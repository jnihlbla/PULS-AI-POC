000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5021100.                                                
000300 AUTHOR.         MARKUS ASPFJÄLL                                          
000400 DATE-WRITTEN.   98/06/12.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*       PROGRAMMET HAR TRE FUNKTIONER:                                    
000900*       1. HÄMTA FELINFO FRÅN WDR9BASEN OCH PRESENTERA DENNA INFO         
001000*       PÅ SKÄRMEN                                                        
001100*       2. KUNNA S-MÄRKA EN DETALJRAD FÖR ATT PÅ SÅ VIS STARTA            
001200*       BILD 5212 FÖR ATT FÅ MER INFO OM SPECIFIK POST                    
001300*      (3. MÖJLIGHET ATT SPARA URVAL PÅ EXTRAKTFIL) BORTTAGEN             
001400*                                                                         
001500*        PROGRAMMET LÄSER      WLSAPA (WDR9)                              
001900*                                                                         
002000*    INDATA.                                                              
002100*        TRANSAKTION: W5T211                                              
002200*        MID:         W5I21101                                            
002300*                                                                         
002400*    UTDATA.                                                              
002500*        MOD:         W5O21101                                            
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100 WORKING-STORAGE SECTION.                                                 
003200*  -----CHECKED BY WY2000                                                 
003300                                                                          
003400 77  IDPGM                       PIC X(08)   VALUE 'W5021100'.            
003500                                                                          
003600*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003700 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003800                                                                          
003900 77  JA                          PIC X       VALUE 'J'.                   
004000 77  NEJ                         PIC X       VALUE 'N'.                   
004100 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   VALUE +33 COMP SYNC.         
004200 77  WS-EXTRACT-SW               PIC X       VALUE 'J'.                   
004300     88 EXTRACT-OK                           VALUE 'J'.                   
004400                                                                          
004500                                                                          
004600*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004700 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004800 77  MAX-INDX                    PIC S9(4)  VALUE +13   COMP SYNC.        
004900     EJECT                                                                
005000*    --- GENERELLA ARBETSFÄLT                                             
005001                                                                          
005002 01  WS-FROM-DAREGDAT                PIC 9(8)        VALUE ZERO.          
005003 01  FILLER REDEFINES WS-FROM-DAREGDAT.                                   
005004     03  WS-FROM-DAREGDAT-TISEKEL    PIC 9(2).                            
005005     03  WS-FROM-DAREGDAT-TIAAMMDD   PIC 9(6).                            
005006                                                                          
005007 01  WS-TOM-DAREGDAT                 PIC 9(8)        VALUE ZERO.          
005008 01  FILLER REDEFINES WS-TOM-DAREGDAT.                                    
005009     03  WS-TOM-DAREGDAT-TISEKEL     PIC 9(2).                            
005010     03  WS-TOM-DAREGDAT-TIAAMMDD    PIC 9(6).                            
005020                                                                          
005030 01  DAGENS-DATUM                    PIC 9(8).                            
005040 01  W-FROM-DATUM                    PIC 9(8).                            
005050 01  W-TOM-DATUM                     PIC 9(8).                            
005060 01  W-ERROR                         PIC S9(7) VALUE ZERO.                
005070 01  WS-IDCPYTXT                      PIC X(8)  VALUE 'W510EKFA'.         
005092                                                                          
005093                                                                          
005094*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005095                                                                          
005096 77  ALLT-SW                     PIC X       VALUE 'J'.                   
005097     88  ALLT-OK                             VALUE 'J'.                   
005098     88  ALLT-NOT-OK                         VALUE 'N'.                   
005099                                                                          
005100 77  BYT-SW                      PIC X       VALUE 'N'.                   
005200     88  BYT-BILD                            VALUE 'J'.                   
005300     88  BYT-EJ-BILD                         VALUE 'N'.                   
005400                                                                          
005500 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005600     88  NYCKLAR-OK                          VALUE 'J'.                   
005700     88  NYCKLAR-FEL                         VALUE 'N'.                   
005701                                                                          
005702 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005703     88  INDATA-OK                           VALUE 'J'.                   
005704     88  INDATA-FEL                          VALUE 'N'.                   
005705                                                                          
005706 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005707     88  EGEN-MID                            VALUE '5211'.                
005708     88  GODK-MID                            VALUE '5211' '5212'          
005709                                                   '5213' '5214'          
005710                                                   '5215' '5216'          
005720                                                   '5217' '5218'          
005730                                                   '5219'.                
005740     88  HELP-MID                            VALUE '0551'.                
005750     88  DETALJ-MID                          VALUE '5212'.                
005760 01  W-INDX                      PIC 99      VALUE ZERO.                  
005770                                                                          
005780     EJECT                                                                
005790*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
005800 01  GENERELLA-SUBPROGRAM.                                                
005900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006000     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
006100     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006400     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
006500     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
006600     EJECT                                                                
006700*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
006800*01 -COPY WMEDAREA                                                        
006900     SKIP3                                                                
007000 01  MESSAGE-CODES.                                                       
007100     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
007200     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
007300     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
007400     03  ERR-RECORD-MISSING      PIC X(3)    VALUE '078'.                 
007500     03  WRONG-INPUT-FIELD       PIC X(3)    VALUE '194'.                 
007600     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
007700     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
007800     EJECT                                                                
007900*01  -COPY WDATAREA                                                       
008000     EJECT                                                                
008100*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008200*                                                                         
008300 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008400*01 -COPY WMSGINIT                                                        
008500     EJECT                                                                
008600 01  FILLER                      PIC X(16)   VALUE 'WORKAREA'.            
008700*01  -COPY WORKAREA                                                       
008800     EJECT                                                                
008900                                                                          
009000 01  FILLER                      PIC X(16)   VALUE 'WWIDFTG '.            
009010*01  -COPY WWIDFTG                                                        
009011     EJECT                                                                
009012                                                                          
009013*                                                                         
009014*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
009015*                                                                         
009016********GÖR MERA ÄNDRINGAR HÄR SEDAN************                          
009017 01  FILLER                      PIC X(16) VALUE 'SPAR-AREA '.            
009018 01  SPAR-AREA.                                                           
009019     03  SPAR-IDPGM              PIC X(8).                                
009020     03  SPAR-IDPGM-TAB          PIC X(8).                                
009021     03  SPAR-DAREGDAT           PIC 9(8).                                
009022     03  SPAR-TIKLOCK            PIC S9(9) COMP-3.                        
009023     03  SPAR-IDSEKVNR           PIC S9(3) COMP-3.                        
009024     03  SPAR-FROM-DAREGDAT      PIC 9(8).                                
009025     03  SPAR-TOM-DAREGDAT       PIC 9(8).                                
009026     03  SPAR-BILD               PIC X(4).                                
009027     03  SPAR-TOM-DATUM          PIC 9(8).                                
009028     03  SPAR-FROM-DATUM         PIC 9(8).                                
009029     03  SPAR-IDTRANS           PIC X(4).                                 
009030     03  FILLER                 PIC X(16) VALUE 'ENTER-NYCKLAR'.          
009031     03  SPAR-ENTER.                                                      
009040         05  SPAR-DAREGDAT-MIN-ENTER PIC 9(8).                            
009050         05  SPAR-DAREGDAT-MAX-ENTER PIC 9(8).                            
009051         05  SPAR-IDPGM-ENTER        PIC X(8).                            
009052         05  SPAR-DAREGDAT-ENTER     PIC 9(8).                            
009053         05  SPAR-TIKLOCK-ENTER      PIC S9(9) COMP-3.                    
009054         05  SPAR-IDSEKVNR-ENTER     PIC S9(3) COMP-3.                    
009055     03  FILLER                 PIC X(16) VALUE 'NEXT-NYCKLAR'.           
009056     03  SPAR-NEXT.                                                       
009057         05  SPAR-DAREGDAT-MIN-NEXT  PIC 9(8).                            
009058         05  SPAR-DAREGDAT-MAX-NEXT  PIC 9(8).                            
009059         05  SPAR-DAREGDAT-NEXT      PIC 9(8).                            
009060         05  SPAR-TIKLOCK-NEXT       PIC 9(9) COMP-3.                     
009070         05  SPAR-IDSEKVNR-NEXT      PIC S9(3) COMP-3.                    
009080         05  SPAR-IDPGM-NEXT         PIC X(8).                            
009090     03  FEL-TABELL.                                                      
009091       05  FEL-WDR901   OCCURS 13.                                        
009092         07  FEL-WDR901KY-TAB.                                            
009093           09  FIL-IDPGM-TAB     PIC X(8).                                
009094           09  FIL-DAREGDAT-TAB  PIC 9(8).                                
009095           09  FIL-TIKLOCK-TAB   PIC S9(9)  COMP-3.                       
009096           09  FIL-IDSEKVNR-TAB  PIC S9(3)  COMP-3.                       
009097     03   SPAR-ERROR             PIC S9(7).                               
009098                                                                          
009099     EJECT                                                                
009100*    ----BMP PARAMETRAR -------------                                     
009110 01  FILLER                      PIC X(16)  VALUE 'BMP-PRM'.              
009120 01  BMP-PARAMETRAR.                                                      
009121     03 SKICKA-IDPGM             PIC X(8).                                
009122     03 SKICKA-FLFDATUM          PIC 9(8).                                
009123     03 SKICKA-FLTDATUM          PIC 9(8).                                
009124     03 SKICKA-IDUSER            PIC X(8).                                
009125                                                                          
009126     EJECT                                                                
009127*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
009128*                                                                         
009129 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009130     SKIP3                                                                
009140*01  MID -COPY W5I21101                                                   
009150     EJECT                                                                
009160 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009170     SKIP3                                                                
009180*01  -COPY WMSGAREA                                                       
009190     EJECT                                                                
009200     03  MOD REDEFINES MSG-AREA.                                          
009300*      05  -COPY W5O21101                                                 
009400     EJECT                                                                
009500 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
009600     SKIP3                                                                
009700*01  -COPY WMFSAREA                                                       
009800     EJECT                                                                
009900 01  W-PROG-TO-PROG-SW1.                                                  
010000*    03 -COPY WMSGSOP                                                     
010100                                                                          
010200 01  W-PROG-TO-PROG-SW2.                                                  
010300     03  M-SW-LL-5212            PIC S9(4)   VALUE +240 COMP SYNC.        
010400     03  M-SW-Z1-Z2-5212         PIC X(2)    VALUE LOW-VALUE.             
010500     03  M-SW-KDTRANS-5212       PIC X(8)    VALUE 'W5T212  '.            
010600     03  M-SW-IDTRANS-5212       PIC X(4)    VALUE '5211'.                
010700     03  M-SW-KDMFSTYP-5212      PIC X(1)    VALUE '2'.                   
010800                                                                          
010810*    03  MID -COPY W5I21201 -PRE 5212-                                    
010820     EJECT                                                                
010830*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010840*                                                                         
010850     EJECT                                                                
010860 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010870     SKIP3                                                                
010880 01  NYCKLAR-TILL-DLI.                                                    
010890*    --- VÄRDE PÅ BLÄDDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN            
010900                                                                          
011000     03  W-WDR901KY-X.                                                    
011100         05  W-IDPGM                 PIC X(8).                            
011200         05  W-DAREGDAT              PIC 9(8).                            
011300         05  W-TIKLOCK               PIC S9(9) COMP-3.                    
011400         05  W-IDSEKVNR              PIC S9(3) COMP-3.                    
011500         05  W-IDCPYTXT              PIC X(8) VALUE 'W510EKFA'.           
011510     03  W-WDR901KY-MIN-X.                                                
011511         05  W-IDPGM-MIN             PIC X(8) VALUE LOW-VALUE.            
011512         05  W-DAREGDAT-MIN          PIC 9(8) VALUE ZERO.                 
011513         05  W-TIKLOCK-MIN           PIC S9(9) COMP-3 VALUE ZERO.         
011514         05  W-IDSEKVNR-MIN          PIC S9(3) COMP-3 VALUE ZERO.         
011515         05  W-IDCPYTXT-MIN          PIC X(8) VALUE 'W510EKFA'.           
011516     03  W-WDR901KY-MAX-X.                                                
011517         05  W-IDPGM-MAX             PIC X(8) VALUE HIGH-VALUE.           
011518         05  W-DAREGDAT-MAX          PIC 9(8) VALUE 99999999.             
011519         05  W-TIKLOCK-MAX       PIC S9(9) COMP-3 VALUE 999999999.        
011520         05  W-IDSEKVNR-MAX          PIC S9(3) COMP-3 VALUE 999.          
011530         05  W-IDCPYTXT-MAX          PIC X(8) VALUE 'W510EKFA'.           
011540                                                                          
011550                                                                          
011560     SKIP2                                                                
011570*    --- STATUS-KOD FRÅN IMS                                              
011580 01  STATUS-WS                   PIC XX.                                  
011590     88  SEGMENT-FINNS                       VALUE '  '.                  
011600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011800     88  BASEN-SLUT                          VALUE 'GB'.                  
011900     SKIP2                                                                
012000 01  GODK-STATUSKODER.                                                    
012100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012200     SKIP3                                                                
012300 01  SSA1                        PIC X(256).                              
012400 01  SSA2                        PIC X(128).                              
012500     EJECT                                                                
012600*    --- IMS FUNKTIONSKODER                                               
012700*01  -COPY W0003                                                          
012800     EJECT                                                                
012900*    ---  DLI INPUT-OUTPUT AREA                                           
013000                                                                          
013100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLSAPA01 '.                   
013200 01  DLI-IO-WLSAPA01.                                                     
013300*    03  -COPY WDR901                                                     
013400*    05  -COPY W510EKHA -RED FIL-WDR901-DATA                              
013500                                                                          
013600     EJECT                                                                
013700 LINKAGE SECTION.                                                         
013800*01  -COPY W0009   -PRE MSG-                                              
013900*01  -COPY W0009   -PRE ALT1-                                             
014000     EJECT                                                                
014010*01  -COPY W0009   -PRE ALT2-                                             
014020     EJECT                                                                
014030*01  -COPY W0008   -PRE USEA-                                             
014040     05  FILLER                  PIC X.                                   
014050                                                                          
014060*01  -COPY W0008  -PRE SAPA-                                              
014070     05  FILLER                  PIC X.                                   
014080     EJECT                                                                
014090 PROCEDURE DIVISION  USING MSG-PCB ALT1-PCB ALT2-PCB                      
014100                                                USEA-PCB SAPA-PCB.        
014200 MAIN SECTION.                                                            
014300     ENTRY 'DLITCBL' USING MSG-PCB ALT1-PCB ALT2-PCB                      
014400                                            USEA-PCB SAPA-PCB.            
014500                                                                          
014600     PERFORM IMS-GET-MSG                                                  
014700     IF SEGMENT-FINNS                                                     
014800       PERFORM A-INIT                                                     
014900       PERFORM B-KOLLA-NYCKLAR                                            
015000       IF NYCKLAR-OK                                                      
015403          IF MFS-FIRST AND NOT DETALJ-MID                                 
015404             PERFORM C-FOERSTA-SIDA                                       
015405             PERFORM IMS-GN-SAPA                                          
015406          ELSE                                                            
015407             IF MFS-NEXT                                                  
015408               PERFORM D-NAESTA-SIDA                                      
015409               PERFORM IMS-GU-SAPA                                        
015410             ELSE                                                         
015411               PERFORM E-SAMMA-SIDA                                       
015412               IF BYT-BILD                                                
015413                 PERFORM H-BYT-BILD                                       
015414               ELSE                                                       
015415                 PERFORM IMS-GU-SAPA                                      
015416               END-IF                                                     
015417             END-IF                                                       
015418          END-IF                                                          
015419          IF ALLT-OK                                                      
015420             PERFORM F-LAES-VISA-INFO                                     
015421          END-IF                                                          
015423       END-IF                                                             
015424*    --- IF ANSWER TO SCREEN:       MSG-KVLL = MOD-LENGTH + 4             
015425*    --- IF PROGRAM-TO-PROGRAM-SWITCH:       = MOD-LENGTH + 17            
015426       IF BYT-EJ-BILD                                                     
015427         COMPUTE MSG-KVLL = LENGTH OF MOD-W5O21101 + 4                    
015428         PERFORM IMS-INSERT-MSG                                           
015429       END-IF                                                             
015430     END-IF                                                               
015440                                                                          
015450     MOVE ZERO TO RETURN-CODE                                             
015460     GOBACK                                                               
015470     .                                                                    
015480     EJECT                                                                
015490 A-INIT SECTION.                                                          
015500                                                                          
015600                                                                          
015700     IF MSG-DUBBLA-TRANSKODER                                             
015800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I21101                 
015900       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
016000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
016100     ELSE                                                                 
016200       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W5I21101                  
016300       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
016400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
016500     END-IF                                                               
016600                                                                          
016700     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
016800     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
016900     MOVE MFS-IDTRANS TO W-IDTRANS                                        
017000                                                                          
017100     MOVE LOW-VALUE TO MSG-AREA                                           
017200     MOVE 'W5O211N1' TO MFS-IDMOD                                         
017300     MOVE '5211' TO MOD-IDTRANS                                           
017400     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
017500                                                                          
017600     IF EGEN-MID OR HELP-MID                                              
017700       CONTINUE                                                           
017800     ELSE                                                                 
017900       MOVE SPACE TO MFS-KDTRTYP                                          
018000       MOVE '7' TO MFS-IDPFK                                              
018100     END-IF                                                               
018200     .                                                                    
018300     EJECT                                                                
018400 B-KOLLA-NYCKLAR SECTION.                                                 
018500                                                                          
018600     MOVE ALL '+'             TO MSGI-WMSGINIT                            
018700     MOVE '001'               TO MSGI-KDCALL                              
018800     MOVE MSG-LTERM-NAME           TO MSGI-IDLTERM-USER                   
018900     MOVE MSG-SIGNON-USERID        TO MSGI-IDUSER                         
019000     MOVE '5211'                   TO MSGI-IDTRANS                        
019100     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
019200     MOVE MSGI-SPAR-AREA      TO SPAR-AREA                                
019300     MOVE SPACE               TO SPAR-BILD                                
019400     MOVE MSGI-IDLAND-SPR     TO MED-IDSKYLT                              
019500                                                                          
019600     MOVE JA TO NYCKLAR-SW                                                
019700                                                                          
019800     MOVE MFS-RENSA-FAELT   TO MOD-FROM-DAREGDAT-IN                       
019900                               MOD-TOM-DAREGDAT-IN                        
020000                               MOD-IDPGM-IN                               
020100     IF EGEN-MID AND MID-FROM-DAREGDAT-IN NOT =  ALL '+'                  
020200         MOVE MID-FROM-DAREGDAT-IN TO SPAR-FROM-DAREGDAT                  
020300     ELSE                                                                 
020400       IF MID-FROM-DAREGDAT-IN NUMERIC                                    
020500         MOVE MID-FROM-DAREGDAT-IN TO SPAR-FROM-DAREGDAT                  
020600       END-IF                                                             
020700     END-IF                                                               
020800     IF EGEN-MID AND MID-TOM-DAREGDAT-IN NOT = ALL '+'                    
020900         MOVE MID-TOM-DAREGDAT-IN TO SPAR-TOM-DAREGDAT                    
021000     ELSE                                                                 
021100       IF MID-TOM-DAREGDAT-IN NUMERIC                                     
021200         MOVE MID-TOM-DAREGDAT-IN TO SPAR-TOM-DAREGDAT                    
021300       END-IF                                                             
021400     END-IF                                                               
021410     IF EGEN-MID AND MID-IDPGM-IN NOT = ALL '+'                           
021420       MOVE MID-IDPGM-IN TO SPAR-IDPGM                                    
021430     ELSE                                                                 
021440       IF MID-IDPGM-IN NOT = ALL '+'                                      
021450         MOVE MID-IDPGM-IN TO SPAR-IDPGM                                  
021460       ELSE                                                               
021470         MOVE MFS-RENSA-FAELT TO MOD-IDPGM-UT                             
021471       END-IF                                                             
021472     END-IF                                                               
021473                                                                          
021474     IF MID-FROM-DAREGDAT-IN NOT = ALL '+'                                
021475       MOVE '7'         TO MFS-IDPFK                                      
021476       MOVE SPACE       TO MFS-KDTRTYP                                    
021477     END-IF                                                               
021478                                                                          
021479     IF MID-TOM-DAREGDAT-IN NOT = ALL '+'                                 
021480       MOVE '7'         TO MFS-IDPFK                                      
021490       MOVE SPACE       TO MFS-KDTRTYP                                    
021491     END-IF                                                               
021492     IF MID-IDPGM-IN        NOT = ALL '+'                                 
021493       MOVE '7'         TO MFS-IDPFK                                      
021494       MOVE SPACE       TO MFS-KDTRTYP                                    
021495     END-IF                                                               
021496                                                                          
021497*    -- RAKNA UT NYTT DATUM OM MID ALL + OCH MFS-FIRST----*               
021498     IF GODK-MID                                                          
021499       IF MID = ALL '+' AND MFS-ENTER                                     
021500         MOVE FUNCTION CURRENT-DATE(1:8)    TO DAGENS-DATUM               
021510         MOVE DAGENS-DATUM                  TO W-FROM-DATUM               
021520         MOVE MSGI-IDDC            TO WORK-IDDC                           
021536         MOVE 2                    TO WORK-KVWORKD                        
021537         MOVE DAGENS-DATUM(3:6)    TO WORK-TIAAMMDD-TOM                   
021538         MOVE 003                  TO WORK-KDCALL                         
021539         CALL WORKDAY   USING WORK-KDCALL                                 
021540                              WORK-DATE-AREA                              
021541                              WORK-KDSVAR                                 
021542         IF WORK-KDSVAR-FEL                                               
021543           MOVE 'FEL FRÅN WORKDAY '                                       
021544                                   TO FELTEXT                             
021545           CALL ABEND USING RKOD-ABEND-MED-DUMP                           
021546         ELSE                                                             
021547                                                                          
021548           MOVE 0101               TO W-FROM-DATUM(5:4)                   
021549*          MOVE WORK-TIAAMMDD-FOM  TO W-FROM-DATUM(3:6)                   
021550*****KONTROLLERA MED DATKONV********                                      
021551           MOVE W-FROM-DATUM       TO DAT-I-TIDATUM                       
021552           MOVE 'AAMMDD'           TO DAT-KDDATFORM                       
021553           CALL WDATKONV USING DAT-KDDATFORM                              
021554                               DAT-I-TIDATUM                              
021560                               DAT-O-TIDATUM                              
021561                               DAT-KDSVAR                                 
021562                                                                          
021563           IF DAT-KDSVAR-OK                                               
021564             MOVE DAT-TIAAMMDD      TO WS-FROM-DAREGDAT-TIAAMMDD          
021565             MOVE DAGENS-DATUM(3:6) TO WS-TOM-DAREGDAT-TIAAMMDD           
021566             MOVE DAT-TISEKEL       TO WS-FROM-DAREGDAT-TISEKEL           
021567                                       WS-TOM-DAREGDAT-TISEKEL            
021568             MOVE WS-FROM-DAREGDAT  TO MOD-FROM-DAREGDAT-UT               
021569                                       W-DAREGDAT-MIN                     
021570                                       SPAR-FROM-DAREGDAT                 
021571                                       SPAR-DAREGDAT-ENTER                
021572                                       SPAR-DAREGDAT-NEXT                 
021573             MOVE WS-TOM-DAREGDAT   TO MOD-TOM-DAREGDAT-UT                
021574                                       W-DAREGDAT-MAX                     
021575                                       SPAR-TOM-DAREGDAT                  
021576             MOVE SPACE             TO SPAR-IDPGM                         
021577             MOVE '7'         TO MFS-IDPFK                                
021578             MOVE SPACE       TO MFS-KDTRTYP                              
021579           ELSE                                                           
021580             MOVE NEJ TO NYCKLAR-SW                                       
021581           END-IF                                                         
021582         END-IF                                                           
021583       END-IF                                                             
021584     END-IF                                                               
021585                                                                          
021586*----KONTROLL AV MID-FROM-DAREGDAT-IN-------------------*                 
021587                                                                          
021588     IF GODK-MID AND SPAR-FROM-DAREGDAT NOT = ALL '+'                     
021589       MOVE SPAR-FROM-DAREGDAT  TO W-FROM-DATUM                           
021590*****KONTROLLERA MED DATKONV********                                      
021600       MOVE W-FROM-DATUM              TO DAT-I-TIDATUM                    
021700       MOVE 'AAMMDD' TO DAT-KDDATFORM                                     
021701       CALL WDATKONV USING DAT-KDDATFORM                                  
021702                           DAT-I-TIDATUM                                  
021703                           DAT-O-TIDATUM                                  
021704                           DAT-KDSVAR                                     
021705                                                                          
021706       IF DAT-KDSVAR-OK                                                   
021707         MOVE DAT-TIAAMMDD     TO WS-FROM-DAREGDAT-TIAAMMDD               
021708         MOVE DAT-TISEKEL      TO WS-FROM-DAREGDAT-TISEKEL                
021709         MOVE WS-FROM-DAREGDAT TO MOD-FROM-DAREGDAT-UT                    
021710                                  W-DAREGDAT-MIN                          
021711                                  SPAR-FROM-DAREGDAT                      
021712                                                                          
021713       ELSE                                                               
021714         MOVE NEJ TO NYCKLAR-SW                                           
021715       END-IF                                                             
021716     END-IF                                                               
021717                                                                          
021718     IF GODK-MID AND SPAR-TOM-DAREGDAT NOT = ALL '+'                      
021719       MOVE SPAR-TOM-DAREGDAT   TO W-TOM-DATUM                            
021720*****KONTROLLERA MED DATKONV********                                      
021721       MOVE W-TOM-DATUM              TO DAT-I-TIDATUM                     
021722       MOVE 'AAMMDD' TO DAT-KDDATFORM                                     
021723       CALL WDATKONV USING DAT-KDDATFORM                                  
021724                           DAT-I-TIDATUM                                  
021725                           DAT-O-TIDATUM                                  
021726                           DAT-KDSVAR                                     
021727                                                                          
021728       IF DAT-KDSVAR-OK                                                   
021729         MOVE DAT-TIAAMMDD     TO WS-TOM-DAREGDAT-TIAAMMDD                
021730         MOVE DAT-TISEKEL      TO WS-TOM-DAREGDAT-TISEKEL                 
021731         MOVE WS-TOM-DAREGDAT  TO MOD-TOM-DAREGDAT-UT                     
021732                                  W-DAREGDAT-MAX                          
021733                                  SPAR-TOM-DAREGDAT                       
021734       ELSE                                                               
021735         MOVE NEJ TO NYCKLAR-SW                                           
021736       END-IF                                                             
021737     END-IF                                                               
021738     IF SPAR-FROM-DAREGDAT > SPAR-TOM-DAREGDAT                            
021739       MOVE NEJ TO NYCKLAR-SW                                             
021740     END-IF                                                               
021741                                                                          
021745                                                                          
021746*----SLUT KONTROLL DAREGDAT                                               
021747                                                                          
021749     IF MSGI-IDFTG NOT = WC-IDFTG-PV                                      
021750       MOVE NEJ TO NYCKLAR-SW                                             
021751     END-IF                                                               
021752                                                                          
021753*----KONTROLL AV IDPGM-------------------------*                          
021754     IF GODK-MID AND MID-IDPGM-IN NOT = ALL '+'                           
021755       MOVE MID-IDPGM-IN    TO W-IDPGM                                    
021756                            MOD-IDPGM-UT                                  
021757                            W-IDPGM-MAX                                   
021758                            W-IDPGM-MIN                                   
021759                            SPAR-IDPGM                                    
021760     END-IF                                                               
021770     IF MID-IDPGM-IN = ALL '+' AND                                        
021771        MID-FROM-DAREGDAT-IN NOT = ALL '+'                                
021772       MOVE LOW-VALUE TO W-IDPGM-MIN                                      
021773       MOVE HIGH-VALUE TO W-IDPGM-MAX                                     
021774       MOVE SPACE      TO SPAR-IDPGM                                      
021775     END-IF                                                               
021776     IF MID-IDPGM-IN = ALL '+' AND                                        
021777        MID-TOM-DAREGDAT-IN NOT = ALL '+'                                 
021778       MOVE LOW-VALUE TO W-IDPGM-MIN                                      
021779       MOVE HIGH-VALUE TO W-IDPGM-MAX                                     
021780       MOVE SPACE      TO SPAR-IDPGM                                      
021790     END-IF                                                               
021791     IF MFS-FIRST AND MID = ALL '+'                                       
021792       IF SPAR-IDPGM > SPACE                                              
021793         MOVE SPAR-IDPGM       TO W-IDPGM-MIN                             
021794                                  W-IDPGM-MAX                             
021795                                  MOD-IDPGM-UT                            
021796       END-IF                                                             
021797     END-IF                                                               
021798*----SLUT KONTROLL IDPGM-----------------------*                          
021799                                                                          
021800*--- KONTROLL AV SELECT-RAD---------                                      
021810     MOVE +1 TO INDX                                                      
021811     PERFORM UNTIL INDX > MAX-INDX                                        
021812       IF MID-CMD (INDX) NOT = '+' AND ' '                                
021813         IF MID-CMD (INDX) = 'S'                                          
021814            MOVE INDX TO W-INDX                                           
021815            MOVE 14   TO INDX                                             
021816            MOVE JA   TO BYT-SW                                           
021817         ELSE                                                             
021818           IF NOT DETALJ-MID                                              
021819              MOVE NEJ TO INDATA-SW                                       
021820              MOVE MFS-ALFA-FAELT-FEL TO MOD-CMD-ATTR (INDX)              
021821           END-IF                                                         
021822         END-IF                                                           
021823       END-IF                                                             
021824       ADD 1 TO INDX                                                      
021825     END-PERFORM                                                          
021826*----SLUT KONTROLL AV SELECT-RAD------                                    
021827                                                                          
021828     IF NOT GODK-MID                                                      
021829       MOVE MFS-RENSA-FAELT TO MID-FROM-DAREGDAT-IN                       
021830                               MID-TOM-DAREGDAT-IN                        
021831                               MID-IDPGM-IN                               
021832       MOVE NEJ TO ALLT-SW                                                
021833     END-IF                                                               
021834                                                                          
021839     IF INDATA-FEL                                                        
021840       MOVE NEJ TO ALLT-SW                                                
021841       MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                          
021842       CALL WMEDKONV USING MED-WMEDAREA                                   
021843       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
021844       PERFORM MFS-ROER-EJ-FAELT-IN                                       
021845       PERFORM MFS-ROER-EJ-FAELT-UT                                       
021846     END-IF                                                               
021847                                                                          
021848     IF NYCKLAR-FEL                                                       
021849       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
021850       CALL WMEDKONV USING MED-WMEDAREA                                   
021851       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
021852       PERFORM MFS-RENSA-FAELT-IN                                         
021853       PERFORM MFS-RENSA-FAELT-UT                                         
021854     END-IF                                                               
021860     .                                                                    
021870     EJECT                                                                
021880 C-FOERSTA-SIDA SECTION.                                                  
021890                                                                          
021900     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
022000     CALL WMEDKONV USING MED-WMEDAREA                                     
022100     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
022200                                                                          
022300     PERFORM MFS-RENSA-FAELT-IN                                           
022400     .                                                                    
022500     EJECT                                                                
022600 D-NAESTA-SIDA SECTION.                                                   
022610                                                                          
022611     IF SPAR-IDTRANS = '5211'                                             
022612       MOVE SPAR-DAREGDAT-NEXT     TO W-DAREGDAT                          
022613       MOVE SPAR-IDSEKVNR-NEXT     TO W-IDSEKVNR                          
022614       MOVE SPAR-TIKLOCK-NEXT      TO W-TIKLOCK                           
022615       MOVE SPAR-IDPGM-NEXT        TO W-IDPGM                             
022616       MOVE SPAR-FROM-DAREGDAT     TO MOD-FROM-DAREGDAT-UT                
022617       MOVE SPAR-TOM-DAREGDAT      TO MOD-TOM-DAREGDAT-UT                 
022618       MOVE SPAR-IDPGM             TO MOD-IDPGM-UT                        
022619     ELSE                                                                 
022620       PERFORM MFS-RENSA-FAELT-IN                                         
022630     END-IF                                                               
022631     .                                                                    
022632     EJECT                                                                
022633 E-SAMMA-SIDA SECTION.                                                    
022634                                                                          
022635     IF SPAR-IDTRANS = '5211' OR '0551'                                   
022636       MOVE SPAR-DAREGDAT-ENTER      TO W-DAREGDAT                        
022637       MOVE SPAR-IDPGM-ENTER         TO W-IDPGM                           
022638       MOVE SPAR-TIKLOCK-ENTER       TO W-TIKLOCK                         
022639       MOVE SPAR-IDSEKVNR-ENTER      TO W-IDSEKVNR                        
022640     ELSE                                                                 
022641       PERFORM MFS-RENSA-FAELT-IN                                         
022642     END-IF                                                               
022643     .                                                                    
022644     EJECT                                                                
022645 F-LAES-VISA-INFO SECTION.                                                
022646                                                                          
022647                                                                          
022648     IF SEGMENT-SAKNAS OR BASEN-SLUT                                      
022649                                                                          
022650       MOVE ERR-RECORD-MISSING TO MED-IDMFSFEL                            
022660       CALL WMEDKONV USING MED-WMEDAREA                                   
022670       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
022680     ELSE                                                                 
022690       MOVE SPACE                   TO SPAR-IDPGM-NEXT                    
022700       MOVE ZERO                    TO SPAR-DAREGDAT-NEXT                 
022800       MOVE ZERO                    TO SPAR-TIKLOCK-NEXT                  
022900       MOVE ZERO                    TO SPAR-IDSEKVNR-NEXT                 
022910                                                                          
022920       MOVE FIL-IDPGM               TO SPAR-IDPGM-ENTER                   
022930       MOVE FIL-DAREGDAT            TO SPAR-DAREGDAT-ENTER                
022940       MOVE FIL-TIKLOCK             TO SPAR-TIKLOCK-ENTER                 
022950       MOVE FIL-IDSEKVNR            TO SPAR-IDSEKVNR-ENTER                
022960                                                                          
022970       MOVE +1 TO INDX                                                    
022980       PERFORM UNTIL INDX > MAX-INDX OR SEGMENT-SAKNAS                    
022990                                     OR BASEN-SLUT                        
023000         IF SEGMENT-FINNS                                                 
023010           IF INDX = 1                                                    
023011             MOVE FIL-DAREGDAT      TO SPAR-DAREGDAT-MIN-ENTER            
023012             MOVE FIL-TIKLOCK       TO SPAR-TIKLOCK-ENTER                 
023013             MOVE FIL-IDSEKVNR      TO SPAR-IDSEKVNR-ENTER                
023014             MOVE FIL-IDPGM         TO SPAR-IDPGM-ENTER                   
023015           END-IF                                                         
023016           IF DETALJ-MID                                                  
023017             IF SPAR-IDPGM-TAB           = FIL-IDPGM    AND               
023018                SPAR-DAREGDAT            = FIL-DAREGDAT AND               
023019                SPAR-IDSEKVNR            = FIL-IDSEKVNR AND               
023020                SPAR-TIKLOCK             = FIL-TIKLOCK                    
023021               MOVE 'S'                  TO MOD-CMD (INDX)                
023022             END-IF                                                       
023023           END-IF                                                         
023024           PERFORM FA-LAEGG-UT-FEL-TO-MOD                                 
023025            ADD +1 TO INDX                                                
023026            ADD +1 TO W-ERROR                                             
023027         ELSE                                                             
023028           MOVE MFS-RENSA-FAELT TO MOD-CMD (INDX)                         
023029                             MOD-DAREGDAT (INDX)                          
023030                             MOD-KDEKHHT (INDX)                           
023031                             MOD-KDEKSHT (INDX)                           
023032                             MOD-KDEKNIVA (INDX)                          
023033                             MOD-IDPGM    (INDX)                          
023034                             MOD-IDTRANS-RAD  (INDX)                      
023035                             MOD-BEFEL    (INDX)                          
023036           ADD +1 TO INDX                                                 
023037         END-IF                                                           
023038       PERFORM IMS-GN-SAPA                                                
023039       END-PERFORM                                                        
023040                                                                          
023041       IF SEGMENT-FINNS                                                   
023042         MOVE FIL-TIKLOCK       TO SPAR-TIKLOCK-NEXT                      
023043         MOVE FIL-DAREGDAT      TO SPAR-DAREGDAT-NEXT                     
023044         MOVE FIL-IDSEKVNR      TO SPAR-IDSEKVNR-NEXT                     
023045         MOVE FIL-IDPGM         TO SPAR-IDPGM-NEXT                        
023046                                                                          
023047         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
023048         CALL WMEDKONV USING MED-WMEDAREA                                 
023049         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
023050       END-IF                                                             
023060                                                                          
023061*-----RAKNAR ANTAL ERRORS SOM FINNS PÅ BASEN--------*                     
023062                                                                          
023063       IF MFS-FIRST AND NOT DETALJ-MID                                    
023064         PERFORM UNTIL BASEN-SLUT OR SEGMENT-SAKNAS                       
023065           PERFORM IMS-GN-SAPA                                            
023066           ADD +1 TO W-ERROR                                              
023067         END-PERFORM                                                      
023068         MOVE W-ERROR TO SPAR-ERROR                                       
023069                         MOD-ERROR-NR                                     
023070       ELSE                                                               
023071         MOVE SPAR-ERROR TO MOD-ERROR-NR                                  
023072       END-IF                                                             
023073     END-IF                                                               
023074                                                                          
023075                                                                          
023076       MOVE '002'                TO MSGI-KDCALL                           
023077       MOVE '5211'               TO SPAR-IDTRANS                          
023078       MOVE SPAR-AREA            TO MSGI-SPAR-AREA                        
023079       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
023080                                                                          
023090     .                                                                    
023100     EJECT                                                                
023200 FA-LAEGG-UT-FEL-TO-MOD SECTION.                                          
023210     MOVE FIL-DAREGDAT              TO MOD-DAREGDAT (INDX)                
023211     MOVE EKH-KDEKHHT               TO MOD-KDEKHHT (INDX)                 
023212     MOVE EKH-KDEKSHT               TO MOD-KDEKSHT (INDX)                 
023213     MOVE EKH-KDEKNIVA              TO MOD-KDEKNIVA (INDX)                
023214     MOVE FIL-IDPGM                 TO MOD-IDPGM (INDX)                   
023215     MOVE EKH-IDTRANS               TO MOD-IDTRANS-RAD (INDX)             
023216     MOVE EKH-BEFELSAP              TO MOD-BEFEL (INDX)                   
023217**** SPAR NYCKLAR I TABELL ********                                       
023218     MOVE FIL-IDPGM                 TO FIL-IDPGM-TAB (INDX)               
023219     MOVE FIL-DAREGDAT              TO FIL-DAREGDAT-TAB (INDX)            
023220     MOVE FIL-TIKLOCK               TO FIL-TIKLOCK-TAB (INDX)             
023221     MOVE FIL-IDSEKVNR              TO FIL-IDSEKVNR-TAB (INDX)            
023222                                                                          
023223     .                                                                    
023224     EJECT                                                                
023225                                                                          
023226 H-BYT-BILD SECTION.                                                      
023227     SKIP2                                                                
023228     MOVE '5212'                    TO SPAR-BILD                          
023229                                                                          
023230* ---HÄMTAR RÄTT RAD-VÄRDE TILL 5212-BILDEN                               
023240     MOVE W-INDX TO INDX                                                  
023250     MOVE FIL-IDPGM-TAB    (INDX)  TO SPAR-IDPGM-TAB                      
023260     MOVE FIL-DAREGDAT-TAB (INDX)  TO SPAR-DAREGDAT                       
023270     MOVE FIL-TIKLOCK-TAB  (INDX)  TO SPAR-TIKLOCK                        
023280     MOVE FIL-IDSEKVNR-TAB (INDX)  TO SPAR-IDSEKVNR                       
023290     MOVE '002'               TO MSGI-KDCALL                              
023300     MOVE '5211'              TO SPAR-IDTRANS                             
023301     MOVE SPAR-AREA           TO MSGI-SPAR-AREA                           
023302     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
023303                                                                          
023304* ---SKICKAR VÄRDE TILL 5212-MID FÖR ATT SEDAN                            
023305* ---STARTA UPP DETTA PROGRAM                                             
023306     MOVE LOW-VALUE              TO 5212-MID-W5I21201                     
023307     MOVE SPAR-FROM-DAREGDAT     TO 5212-MID-FROM-DAREGDAT                
023308     MOVE SPAR-TOM-DAREGDAT      TO 5212-MID-TOM-DAREGDAT                 
023309     MOVE SPAR-IDPGM             TO 5212-MID-IDPGM                        
023310     COMPUTE MSG-KVLL = LENGTH OF MOD-W5O21101 + 17                       
023311     PERFORM IMS-INSERT-ALT2-MSG                                          
023312     MOVE NEJ TO ALLT-SW                                                  
023313     .                                                                    
023314     EJECT                                                                
025323 MFS-RENSA-FAELT-UT SECTION.                                              
025324                                                                          
025325*    --- ALLA UTDATA-FÄLT                                                 
025326*    --- INKL. BLÄDDRINGSNYCKLAR                                          
025327     MOVE +1 TO INDX                                                      
025328     PERFORM UNTIL INDX > MAX-INDX                                        
025329     MOVE MFS-RENSA-FAELT TO MOD-CMD (INDX)                               
025330                             MOD-DAREGDAT (INDX)                          
025340                             MOD-KDEKHHT  (INDX)                          
025350                             MOD-KDEKSHT  (INDX)                          
025360                             MOD-KDEKNIVA (INDX)                          
025370                             MOD-IDPGM    (INDX)                          
025380                             MOD-IDTRANS-RAD  (INDX)                      
025390                             MOD-BEFEL    (INDX)                          
025400     ADD +1 TO INDX                                                       
025500     END-PERFORM                                                          
025600     MOVE MFS-RENSA-FAELT TO                                              
025700                             MOD-FROM-DAREGDAT-UT                         
025800                             MOD-TOM-DAREGDAT-UT                          
025810                             MOD-IDPGM-UT                                 
025820                             MOD-ERROR-NR                                 
025830     .                                                                    
026010     SKIP3                                                                
026020 MFS-RENSA-FAELT-IN SECTION.                                              
026030                                                                          
026040*    --- ALLA INDATA-FÄLT                                                 
026050     MOVE MFS-RENSA-FAELT TO MOD-FROM-DAREGDAT-IN                         
026060                             MOD-TOM-DAREGDAT-IN                          
026070                             MOD-IDPGM-IN                                 
026080                             MOD-ERROR-NR                                 
026090     .                                                                    
026100     EJECT                                                                
026200 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
026300                                                                          
026400*    --- ALLA UTDATA-FÄLT                                                 
026500*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
026600     MOVE MFS-ROER-EJ-FAELT TO MOD-FROM-DAREGDAT-UT                       
026700                               MOD-TOM-DAREGDAT-UT                        
026800                                                                          
026900                                                                          
027000                                                                          
027100                               MOD-IDPGM-UT                               
027200     MOVE +1 TO INDX                                                      
027300     PERFORM UNTIL INDX > MAX-INDX                                        
027400       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
027500       ADD +1 TO INDX                                                     
027501     END-PERFORM                                                          
027502     SKIP2                                                                
027503     .                                                                    
027504     EJECT                                                                
027505 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
027506                                                                          
027507*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
027508     MOVE MFS-ROER-EJ-FAELT TO MOD-CMD      (INDX)                        
027509                               MOD-DAREGDAT (INDX)                        
027510                               MOD-KDEKHHT  (INDX)                        
027511                               MOD-KDEKSHT  (INDX)                        
027512                               MOD-KDEKNIVA (INDX)                        
027513                               MOD-IDPGM    (INDX)                        
027514                               MOD-IDTRANS-RAD  (INDX)                    
027515                               MOD-BEFEL    (INDX)                        
027516     .                                                                    
027517     SKIP3                                                                
027518 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
027519                                                                          
027520*    --- ALLA INDATA-FÄLT                                                 
027530     MOVE MFS-ROER-EJ-FAELT TO MOD-FROM-DAREGDAT-IN                       
027540                               MOD-TOM-DAREGDAT-IN                        
027550                                                                          
027560                                                                          
027570                                                                          
027580                               MOD-IDPGM-IN                               
027590     .                                                                    
027600     EJECT                                                                
027700* --- IMS SEKTIONER ---                                                   
027800 IMS-GET-MSG SECTION.                                                     
027900                                                                          
028000     MOVE '  QC' TO GODK-STATUSKODER                                      
028100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
028200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
028300     PERFORM IMS-STATUSKONTROLL                                           
028400     .                                                                    
028500     SKIP3                                                                
028600 IMS-INSERT-MSG SECTION.                                                  
028700                                                                          
028800     IF MSGI-IDLAND-SPR = 'GB'                                            
028900       MOVE 'N' TO MFS-KDHUVOMR                                           
029000     END-IF                                                               
029100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
029200     MOVE SPACE TO GODK-STATUSKODER                                       
029300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
029400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
029500     PERFORM IMS-STATUSKONTROLL                                           
029600     .                                                                    
029700*    EJECT                                                                
029800*IMS-INSERT-ALT1-MSG SECTION.                                             
029900*                                                                         
030000*    MOVE SPACE TO GODK-STATUSKODER                                       
030100*    CALL CBLTDLI USING ISRT ALT1-PCB W-PROG-TO-PROG-SW1                  
030200*    MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
030300*    PERFORM IMS-STATUSKONTROLL                                           
030400*    .                                                                    
030500     EJECT                                                                
030600 IMS-INSERT-ALT2-MSG SECTION.                                             
030700                                                                          
030800     MOVE SPACE TO GODK-STATUSKODER                                       
030900     CALL CBLTDLI USING ISRT ALT2-PCB W-PROG-TO-PROG-SW2                  
031000     MOVE ALT2-STATUS-CODE TO STATUS-WS                                   
031100     PERFORM IMS-STATUSKONTROLL                                           
031200     .                                                                    
031300     EJECT                                                                
031400 IMS-GU-SAPA SECTION.                                                     
031500                                                                          
031600                                                                          
031700     STRING 'WLSAPA01(WDR901KY =' W-WDR901KY-X ')'                        
031800                                                                          
031900          DELIMITED BY SIZE INTO SSA1                                     
032000     MOVE '  GE' TO GODK-STATUSKODER                                      
032100     CALL CBLTDLI USING GU SAPA-PCB DLI-IO-WLSAPA01 SSA1                  
032110     MOVE SAPA-STATUS-CODE TO STATUS-WS                                   
032120     PERFORM IMS-STATUSKONTROLL                                           
032130     .                                                                    
032140     EJECT                                                                
032150 IMS-GN-SAPA SECTION.                                                     
032160                                                                          
032170     STRING 'WLSAPA01(DAREGDAT>=' W-DAREGDAT-MIN                          
032180                    '&DAREGDAT<=' W-DAREGDAT-MAX                          
032190                    '&IDPGM   >=' W-IDPGM-MIN                             
032191                    '&IDPGM   <=' W-IDPGM-MAX                             
032192                    '&IDCPYTXT =' WS-IDCPYTXT ')'                         
032193                                                                          
032194          DELIMITED BY SIZE INTO SSA1                                     
032195     MOVE '  GBGE' TO GODK-STATUSKODER                                    
032196     CALL CBLTDLI USING GN SAPA-PCB DLI-IO-WLSAPA01 SSA1                  
032197     MOVE SAPA-STATUS-CODE TO STATUS-WS                                   
032198     PERFORM IMS-STATUSKONTROLL                                           
032199     .                                                                    
032200     EJECT                                                                
032210 IMS-STATUSKONTROLL SECTION.                                              
032220                                                                          
032230     SET STATUS-IX TO 1                                                   
032240     SEARCH GODK-STATUS                                                   
032250       AT END                                                             
032260         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
032270         DELIMITED BY SIZE INTO FELTEXT                                   
032280         CALL FELLOG                                                      
032290       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
032300         CONTINUE                                                         
032400     END-SEARCH                                                           
032500     .                                                                    
