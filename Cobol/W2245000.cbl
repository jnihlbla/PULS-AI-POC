000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2245000.                                                
000300 AUTHOR.         KJELLSON GÖRAN.                                          
000400 DATE-WRITTEN.   12/12/28.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        OMRÄKNING AV LEVERANSTIDER FÖR KINA OCH USA                      
000900*                                                                         
001000*        PROGRAMMET LÄSER      WDG3                                       
001100*        PROGRAMMET UPPDATERAR WDG3 (HTR 2203/04)                         
001200*        PROGRAMMET LÄSER      WDK6                                       
001210*        PROGRAMMET LÄSER      WDK7                                       
001300*        PROGRAMMET LÄSER      WDF1                                       
001400*                                                                         
001500*    ABENDKODER:                                                          
001600*        U0016 -  . . . .                                                 
001700*        U1000 -  . . . .                                                 
001800*                                                                         
001900                                                                          
002000                                                                          
002100 ENVIRONMENT DIVISION.                                                    
002200 INPUT-OUTPUT SECTION.                                                    
002300 FILE-CONTROL.                                                            
002310*          --- PARAMETERKORT IN - SYSIN CARD FRÅN JCL                     
002320     SELECT PARMIN                     ASSIGN TO W22450D1.                
002400                                                                          
002500 DATA DIVISION.                                                           
002600 FILE SECTION.                                                            
002610 FD  PARMIN                                                               
002620     RECORDING       F                                                    
002630     BLOCK CONTAINS  0.                                                   
002640                                                                          
002650 01  FILLER                 PIC X(80).                                    
002660     EJECT                                                                
002700                                                                          
002800 WORKING-STORAGE SECTION.                                                 
002900                                                                          
003000 77  IDPGM                       PIC X(8)    VALUE 'W2245000'.            
003100 77  JA                          PIC X       VALUE 'J'.                   
003200 77  NEJ                         PIC X       VALUE 'N'.                   
003300 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
003400 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
003500                                                                          
003600*01  -COPY W200BEFT                                                       
003700                                                                          
003710*01  -COPY WWDC99                                                         
003711*01  -COPY WWDCKONS                                                       
003712                                                                          
003713*    --- VALID NDC LAND CODES                                             
003714*                                                                         
003715*01  -COPY WWDCLAND                                                       
003720                                                                          
003800 01  W-BEFT                      PIC S9(3)   COMP-3 VALUE ZERO.           
003810 01  W-KVDAGAR-FFH               PIC 9(3)    VALUE ZERO.                  
003900                                                                          
004000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004100 01  FILLER REDEFINES DAGENS-DATUM.                                       
004200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
004300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
004400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
004500                                                                          
004510 01  PARM-AREA.                                                           
004520     03  PARM-IDDC-FOM           PIC X(02) VALUE SPACE.                   
004530     03  PARM-IDDC-TOM           PIC X(02) VALUE SPACE.                   
004540     03  FILLER                  PIC X(76) VALUE SPACE.                   
004550                                                                          
004600 01  DYNAMISKA-SUBPROGRAM.                                                
004700*                                                                         
004800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
004900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
005100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005200                                                                          
005300*    --- PARAMETRAR TILL POSTSUM                                          
005400*                                                                         
005500*01  -COPY W0005   -PRE  POSTSUM-                                         
005600                                                                          
005700*    --- PARAMETRAR TILL ABEND                                            
005800                                                                          
005900 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006000 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006100 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006200                                                                          
006300 01  FELTEXT.                                                             
006400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006600     EJECT                                                                
006700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
006800*                                                                         
006900                                                                          
007000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007100                                                                          
007110 01  CHKP-VAR.                                                            
007120 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
007130 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
007140 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
007150 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
007160 03  CHKP-ANT                    PIC S9(5)   VALUE +0.                    
007171 03  CHKP-MAX                    PIC S9(3)   VALUE +900.                  
007180                                                                          
007200 01  NYCKLAR-TILL-DLI.                                                    
007300     03  W-WDG3KEY-2203-X.                                                
007400         05  W-IDHTYP-2203       PIC X(4)    VALUE '2203'.                
007500         05  W-IDDC-2203         PIC X(2)    VALUE SPACE.                 
007600         05  FILLER-WDG3         PIC X(24)   VALUE LOW-VALUE.             
007700     03  W-WDG3KEY-2213-X.                                                
007710         05  W-IDHTYP-2213       PIC X(4)    VALUE '2213'.                
007720         05  W-IDDC-2213         PIC X(2)    VALUE SPACE.                 
007730         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
007800     03  W-IDHTYP                PIC X(4)    VALUE '2213'.                
008500     03  W-IDARTNR-X.                                                     
008600         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
008610     03  W-IDARTNR-2214-X.                                                
008620         05  W-IDARTNR-2214      PIC S9(9)   VALUE ZERO COMP-3.           
008700     03  W-IDDC-X.                                                        
008800         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
008810     03  W-IDDC-MIN-X.                                                    
008821         05  W-IDDC-MIN          PIC X(2)    VALUE LOW-VALUE.             
008830     03  W-IDDC-MAX-X.                                                    
008841         05  W-IDDC-MAX          PIC X(2)    VALUE HIGH-VALUE.            
008850     03  W-IDDC-REF-X.                                                    
008860         05  W-IDDC-REF          PIC X(2)    VALUE SPACE.                 
008900     03  W-IDLAND-X.                                                      
009010         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
009100     03  W-IDLEVNR-X.                                                     
009200         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
009210     03  W-WDGXKEY-4579-X.                                                
009220          05 W-IDHTYP-4579       PIC X(4)    VALUE '4579'.                
009230          05 W-IDPGM             PIC X(8)    VALUE 'W2245000'.            
009240          05 FILLER              PIC X(18)   VALUE LOW-VALUE.             
009300                                                                          
009400*    --- STATUS-KOD FRÅN IMS                                              
009500 01  STATUS-WS                   PIC XX.                                  
009600     88  SEGMENT-FINNS                       VALUE '  '.                  
009700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
009800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009810     88  BASEN-SLUT                          VALUE 'GB'.                  
009820     88  IMS-EJ-OK                           VALUE 'XD'.                  
009900                                                                          
010000 01  GODK-STATUSKODER.                                                    
010100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010200                                                                          
010300 01  ALL-SSA.                                                             
010310     03 SSA1                     PIC X(128).                              
010400     03 SSA2                     PIC X(64).                               
010500     03 SSA3                     PIC X(64).                               
010600                                                                          
010700                                                                          
010800*    --- IMS FUNKTIONSKODER                                               
010900*01  -COPY W0003                                                          
011000                                                                          
011100*    ---  DLI INPUT-OUTPUT AREA                                           
011200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDG301'.                      
011300 01  DLI-IO-WDG301.                                                       
011400*    03  -COPY WDGX01DC -PRE G301-                                        
011500     EJECT                                                                
011600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2214'.                    
011700 01  DLI-IO-WDGX2214.                                                     
011800*    03  -COPY WDGX2214                                                   
011900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDG302-I'.                    
012000 01  DLI-IO-WDGX2204.                                                     
012100*    03  -COPY WDGX2204                                                   
012101                                                                          
012110 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
012120 01  DLI-IO-WDK611.                                                       
012130*    03  -COPY WDK611                                                     
012200                                                                          
012300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
012400 01  DLI-IO-WDK701.                                                       
012500*    03  -COPY WDK701                                                     
012900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
013000 01  DLI-IO-WDK711.                                                       
013100*    03  -COPY WDK711                                                     
013110 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK722'.                      
013120 01  DLI-IO-WDK722.                                                       
013130*    03  -COPY WDK722                                                     
013200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK712'.                      
013300 01  DLI-IO-WDK712.                                                       
013400*    03  -COPY WDK712                                                     
013500                                                                          
013600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF101'.                      
013700 01  DLI-IO-WDF101.                                                       
013800*    03  -COPY WDF101                                                     
013900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF116'.                      
014000 01  DLI-IO-WDF116.                                                       
014100*    03  -COPY WDF116                                                     
014101                                                                          
014102 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4580'.                    
014103 01  DLI-IO-WDGX4580.                                                     
014104*    03  -COPY WDGX4580                                                   
014110                                                                          
014300                                                                          
014400 LINKAGE SECTION.                                                         
014500                                                                          
014600                                                                          
014700*01  -COPY W0009  -PRE  MSG-                                              
014710                                                                          
014720*01  -COPY W0008  -PRE 2213-                                              
014800     05  FILLER                  PIC X.                                   
014900                                                                          
015000*01  -COPY W0008  -PRE 2203-                                              
015100     05  FILLER                  PIC X.                                   
015200                                                                          
015300*01  -COPY W0008  -PRE WDK6-                                              
015400     05  FILLER                  PIC X.                                   
015500                                                                          
015510*01  -COPY W0008  -PRE WDK7-                                              
015520     05  FILLER                  PIC X.                                   
015530                                                                          
015600*01  -COPY W0008  -PRE WDF1-                                              
015700     05  FILLER                  PIC X.                                   
015800                                                                          
015810*01  -COPY W0008  -PRE 4579-                                              
015820     05  FILLER                  PIC X.                                   
015830                                                                          
015900                                                                          
016000 PROCEDURE DIVISION  USING  MSG-PCB 2213-PCB 2203-PCB                     
016100                           WDK6-PCB WDK7-PCB WDF1-PCB 4579-PCB.           
016200 MAIN SECTION.                                                            
016300     ENTRY 'DLITCBL' USING  MSG-PCB 2213-PCB 2203-PCB                     
016400                           WDK6-PCB WDK7-PCB WDF1-PCB 4579-PCB.           
016500                                                                          
016600                                                                          
016800     PERFORM A-INIT                                                       
016801                                                                          
016810     PERFORM S01-LAS-PARMIN                                               
016900                                                                          
017000     PERFORM IMS-GN-WDG301-2213                                           
017100     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
017101                                                                          
017107        IF G301-IDDC < PARM-IDDC-FOM                                      
017108        OR G301-IDDC > PARM-IDDC-TOM                                      
017110           CONTINUE                                                       
017120        ELSE                                                              
017300           PERFORM IMS-GHNP-WDG302-2214                                   
017400           PERFORM UNTIL SEGMENT-SAKNAS                                   
017504                                                                          
017505              MOVE G301-IDDC    TO W-IDDC                                 
017506                                   WS-IDDC                                
017510              MOVE 2214-IDARTNR TO W-IDARTNR                              
017520              PERFORM IMS-GU-WDK701                                       
017521                                                                          
017530              PERFORM B-UPPDATERA-WDK712                                  
017702                                                                          
017704              PERFORM IMS-GNP-WDK711                                      
017705              IF SEGMENT-FINNS                                            
017710                 PERFORM IMS-GHNP-WDK722                                  
017711                 IF SEGMENT-FINNS                                         
017712                    PERFORM C-UPPDATERA-WDK722                            
017713                    PERFORM D-SKAPA-WDG3-2204                             
017714                 END-IF                                                   
017715              END-IF                                                      
017716                                                                          
017733*--- ÖVRIGA NDC I KINA SKALL UPPDATERA PSS SOM DC71                       
017734*--- OM LOKALT ANSKAFFADE (IDDC-REF = SPACE)                              
017736              IF NDC-CN-71                                                
017737                 MOVE '72'  TO W-IDDC-MIN                                 
017738                 MOVE '79'  TO W-IDDC-MAX                                 
017739                                                                          
017740                 PERFORM IMS-GU-WDK701                                    
017741                 PERFORM IMS-GNP-WDK711-REF                               
017742                 PERFORM UNTIL SEGMENT-SAKNAS                             
017746                    MOVE SLAG-IDDC TO W-IDDC                              
017747                    PERFORM IMS-GHNP-WDK722                               
017748                    IF SEGMENT-FINNS                                      
017749                       PERFORM C-UPPDATERA-WDK722                         
017750                       PERFORM D-SKAPA-WDG3-2204                          
017751                    END-IF                                                
017753                    PERFORM IMS-GNP-WDK711-REF                            
017754                 END-PERFORM                                              
017760              END-IF                                                      
017900                                                                          
017910              PERFORM IMS-DLET-WDG302-2214                                
017920              IF CHKP-ANT > CHKP-MAX                                      
017930                 PERFORM X-TAG-CHECKPOINT                                 
017931              ELSE                                                        
017932                PERFORM IMS-GHNP-WDG302-2214                              
017940              END-IF                                                      
018100                                                                          
018200           END-PERFORM                                                    
018210        END-IF                                                            
018300        PERFORM IMS-GN-WDG301-2213                                        
018400     END-PERFORM                                                          
018500                                                                          
018600     PERFORM Z-FINIT                                                      
018700                                                                          
018800     MOVE ZERO TO RETURN-CODE                                             
018900     GOBACK                                                               
019000     .                                                                    
019100                                                                          
019200                                                                          
019300 A-INIT SECTION.                                                          
019400                                                                          
019500     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
019501                                                                          
019510     OPEN INPUT  PARMIN                                                   
019600     ACCEPT DAGENS-DATUM  FROM DATE                                       
019700                                                                          
019900     MOVE IDPGM        TO POSTSUM-PROGNAMN                                
019910                                                                          
019920     PERFORM IMS-RESTART                                                  
019921                                                                          
019930     PERFORM IMS-GHU-RESTART                                              
019940     IF 4580-IDWDGX2213 NOT = SPACE                                       
019950        MOVE 4580-IDWDGX2213 TO W-WDG3KEY-2213-X                          
019960        PERFORM IMS-GU-WDG301-2213                                        
019991     END-IF                                                               
020000     .                                                                    
020100                                                                          
020200                                                                          
020300 B-UPPDATERA-WDK712 SECTION.                                              
020400                                                                          
020500     MOVE 'B-UPD-WDK712    ' TO CURRENT-SECTION                           
020700                                                                          
020720     PERFORM S10-SOK-IDLAND                                               
020730                                                                          
020800     MOVE ZERO               TO W-BEFT                                    
020900     PERFORM IMS-GHNP-WDK712                                              
021040                                                                          
021100     IF SEGMENT-FINNS                                                     
021200        IF LART-BEFT = ZERO                                               
021300           PERFORM IMS-GU-WDK611                                          
021400           IF SEGMENT-FINNS                                               
021500              MOVE CLAG-BEFT   TO W-BEFT                                  
021700           END-IF                                                         
021800        ELSE                                                              
021900           MOVE LART-BEFT      TO W-BEFT                                  
022100        END-IF                                                            
022110                                                                          
022120        SEARCH ALL BEFT-TAB                                               
022130           AT END                                                         
022140              MOVE ZERO        TO LART-KVDAGAR-INLEV                      
022150           WHEN BEFT-BEFT(BEFT-IX) = W-BEFT                               
022160              MOVE BEFT-KVDAGAR-INLEV(BEFT-IX)                            
022170                               TO LART-KVDAGAR-INLEV                      
022180        END-SEARCH                                                        
022191        PERFORM IMS-REPL-WDK712                                           
022192     ELSE                                                                 
022194        MOVE ZERO              TO LART-KVDAGAR-INLEV                      
022195     END-IF                                                               
022196     .                                                                    
022197                                                                          
022198                                                                          
022200 C-UPPDATERA-WDK722 SECTION.                                              
022300                                                                          
022400     MOVE 'C-UPD-WDK722    ' TO CURRENT-SECTION                           
022500                                                                          
023000     MOVE XLAG-IDLEVNR-SHIP TO W-IDLEVNR                                  
023100     PERFORM IMS-GU-WDF101                                                
023110     PERFORM IMS-GNP-WDF116                                               
023115                                                                          
023120     IF SEGMENT-SAKNAS                                                    
023130        MOVE ZERO TO NDC-KVDAGAR-TT                                       
023140     END-IF                                                               
023300                                                                          
023302     IF XLAG-TIMANLED < DAGENS-DATUM                                      
023303        MOVE SLAG-IDLEVNR    TO W-IDLEVNR                                 
023304        PERFORM IMS-GU-WDF101                                             
023310        MOVE ZERO            TO XLAG-TIMANLED                             
023312        MOVE LEV-KVVECKOR-LT TO XLAG-KVVECKOR-LT                          
023320     END-IF                                                               
023330                                                                          
023400     COMPUTE XLAG-KVDAGAR-FFH =                                           
023500             LART-KVDAGAR-INLEV + NDC-KVDAGAR-TT                          
023600                                                                          
023700     COMPUTE W-KVDAGAR-FFH ROUNDED = XLAG-KVDAGAR-FFH / 5                 
023800     COMPUTE XLAG-KVVECKOR-FT =                                           
023900             XLAG-KVVECKOR-LT + W-KVDAGAR-FFH                             
024000                                                                          
024100     PERFORM IMS-REPL-WDK722                                              
024500     .                                                                    
024600                                                                          
024700                                                                          
025169 D-SKAPA-WDG3-2204  SECTION.                                              
025170                                                                          
025171     MOVE 'D-NY-WDG3-2204  ' TO CURRENT-SECTION                           
025180                                                                          
025200     MOVE W-IDDC             TO W-IDDC-2203                               
025300                                                                          
025400     MOVE W-IDARTNR          TO 2204-IDARTNR                              
025500     MOVE +22                TO 2204-KDLPORS                              
025600                                                                          
025700     PERFORM IMS-ISRT-WDG302-2204                                         
025800     .                                                                    
025900                                                                          
026000                                                                          
026100 Z-FINIT SECTION.                                                         
026200                                                                          
026300     MOVE 'Z-FINIT         ' TO CURRENT-SECTION                           
026400                                                                          
026410     CLOSE PARMIN                                                         
026420                                                                          
026500     MOVE 'S'        TO POSTSUM-OPKOD                                     
026600     CALL POSTSUM USING POSTSUM-PARM                                      
026610                                                                          
026620     PERFORM IMS-GHU-RESTART                                              
026630     MOVE SPACE      TO 4580-IDWDGX2213                                   
026640     ACCEPT 4580-TIUPPDAT FROM DATE                                       
026650     ACCEPT 4580-TIUPPTID FROM TIME                                       
026660     PERFORM IMS-REPL-RESTART                                             
026700     .                                                                    
026800                                                                          
026801 S01-LAS-PARMIN   SECTION.                                                
026802                                                                          
026803     READ PARMIN INTO PARM-AREA                                           
026804     .                                                                    
026805     EJECT                                                                
026810 S10-SOK-IDLAND SECTION.                                                  
026900                                                                          
026901     MOVE 'S10-SOK-IDLAND  ' TO CURRENT-SECTION                           
026902                                                                          
026903     SEARCH ALL DC-LAND                                                   
026904        AT END                                                            
026905            MOVE SPACE           TO W-IDLAND                              
026908         WHEN DCLAND-IDDC (DCLAND-IX) = G301-IDDC                         
026909            MOVE DCLAND-IDLANDX2 (DCLAND-IX)                              
026910                                 TO W-IDLAND                              
026911      END-SEARCH                                                          
026913     .                                                                    
026914     EJECT                                                                
026915 X-TAG-CHECKPOINT   SECTION.                                              
026916                                                                          
026917     PERFORM IMS-GHU-RESTART                                              
026918     MOVE DLI-IO-WDG301     TO 4580-IDWDGX2213                            
026919     ACCEPT 4580-TIUPPDAT FROM DATE                                       
026920     ACCEPT 4580-TIUPPTID FROM TIME                                       
026921     PERFORM IMS-REPL-RESTART                                             
026922                                                                          
026923*INNAN MAN TAR CHECKPOINT MÅSTE LÄSAS NÄSTA G302-2214 SOM LIGGER          
026924*EFTER DEN SENAST BORTTAGNA 2214-SEG. DETTA FÖR ATT VETA VILKEN           
026925*ARTIKEL MAN SKA OMPOSITIONERA EFTER CHECKPOINT !!!                       
026926     PERFORM IMS-GHNP-WDG302-2214                                         
026927                                                                          
026930     PERFORM IMS-CHECKPOINT                                               
026940     MOVE ZERO TO CHKP-ANT                                                
026941                                                                          
026942     MOVE DLI-IO-WDG301   TO W-WDG3KEY-2213-X                             
026943     PERFORM IMS-GU-WDG301-2213                                           
026944     MOVE 2214-IDARTNR    TO W-IDARTNR-2214                               
026945     PERFORM IMS-GHNP-WDG302-2214-KVAL                                    
026950     .                                                                    
026960                                                                          
026970                                                                          
026980                                                                          
027000* --- IMS SEKTIONER ---                                                   
027100                                                                          
027200                                                                          
027210 IMS-RESTART SECTION.                                                     
027211     MOVE 'IMS-RESTART     ' TO CURRENT-IMS-SECTION                       
027220                                                                          
027230     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
027240     MOVE '  '  TO GODK-STATUSKODER                                       
027250     CALL CBLTDLI USING XRST MSG-PCB                                      
027260                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
027270                        CHKP-AREA-LENGTH CHKP-AREA                        
027280     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
027290     PERFORM IMS-STATUSKONTROLL                                           
027291     .                                                                    
027292                                                                          
027293                                                                          
027294 IMS-GHU-RESTART  SECTION.                                                
027295     MOVE 'IMS-GHU-RESTART '  TO CURRENT-IMS-SECTION                      
027296                                                                          
027297     MOVE SPACE          TO ALL-SSA                                       
027298     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-4579-X ')'                    
027299          DELIMITED BY SIZE INTO SSA1                                     
027300     MOVE 'WDR470   '    TO SSA2                                          
027301     MOVE '    '         TO GODK-STATUSKODER                              
027302     CALL CBLTDLI USING GHU 4579-PCB DLI-IO-WDGX4580 SSA1 SSA2            
027303     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
027304     PERFORM IMS-STATUSKONTROLL                                           
027305     .                                                                    
027306                                                                          
027307                                                                          
027308 IMS-REPL-RESTART SECTION.                                                
027309     MOVE 'IMS-REPL-RESTART'  TO CURRENT-IMS-SECTION                      
027310                                                                          
027311     MOVE '  '             TO GODK-STATUSKODER                            
027312     CALL CBLTDLI USING REPL 4579-PCB DLI-IO-WDGX4580                     
027313     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
027314     PERFORM IMS-STATUSKONTROLL                                           
027315     .                                                                    
027316                                                                          
027317                                                                          
027318 IMS-CHECKPOINT SECTION.                                                  
027319     MOVE 'IMS-CHECKPOINT  ' TO CURRENT-IMS-SECTION                       
027320                                                                          
027321     MOVE SPACE  TO CHKP-MSG-IO-AREA                                      
027322     MOVE '  XD' TO GODK-STATUSKODER                                      
027323     CALL CBLTDLI USING CHKP MSG-PCB                                      
027324                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
027325                        CHKP-AREA-LENGTH CHKP-AREA                        
027326     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
027327     PERFORM IMS-STATUSKONTROLL                                           
027328                                                                          
027329     IF IMS-EJ-OK                                                         
027330       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
027331       DISPLAY FELTEXT                                                    
027332       CALL FELLOG                                                        
027333     END-IF                                                               
027334     .                                                                    
027335                                                                          
027336                                                                          
027340 IMS-GU-WDG301-2213 SECTION.                                              
027400                                                                          
027500     MOVE 'GU-WDG301-2213  ' TO CURRENT-IMS-SECTION                       
027600                                                                          
027700     MOVE SPACE               TO ALL-SSA                                  
027800     STRING 'WDG301  (WDG3KEY  =' W-WDG3KEY-2213-X ')'                    
027900          DELIMITED BY SIZE INTO SSA1                                     
028000     MOVE '    '              TO GODK-STATUSKODER                         
028100     CALL CBLTDLI USING GU 2213-PCB DLI-IO-WDG301 SSA1                    
028200     MOVE 2213-STATUS-CODE    TO STATUS-WS                                
028300     PERFORM IMS-STATUSKONTROLL                                           
029100     .                                                                    
029200                                                                          
029210 IMS-GN-WDG301-2213 SECTION.                                              
029220                                                                          
029230     MOVE 'GN-WDG301-2213  ' TO CURRENT-IMS-SECTION                       
029240                                                                          
029250     MOVE SPACE               TO ALL-SSA                                  
029260     STRING 'WDG301  (IDHTYP   =' W-IDHTYP ')'                            
029270          DELIMITED BY SIZE INTO SSA1                                     
029280     MOVE '  GEGB'            TO GODK-STATUSKODER                         
029290     CALL CBLTDLI USING GN 2213-PCB DLI-IO-WDG301 SSA1                    
029291     MOVE 2213-STATUS-CODE    TO STATUS-WS                                
029292     PERFORM IMS-STATUSKONTROLL                                           
029293                                                                          
029294     IF SEGMENT-FINNS                                                     
029295        MOVE 'READ  '   TO POSTSUM-FDNAMN                                 
029296        MOVE 'WDG3  '   TO POSTSUM-DDNAMN2                                
029297        MOVE '2213  '   TO POSTSUM-TRANSTYP                               
029298        CALL POSTSUM USING POSTSUM-PARM                                   
029299     END-IF                                                               
029300     .                                                                    
029301                                                                          
029310 IMS-GHNP-WDG302-2214 SECTION.                                            
029400                                                                          
029500     MOVE 'GHNP-WDG302-2214' TO CURRENT-IMS-SECTION                       
029600                                                                          
029610     MOVE SPACE               TO ALL-SSA                                  
029700     MOVE 'WDG302 '           TO SSA1                                     
029800     MOVE '  GE'              TO GODK-STATUSKODER                         
029900     CALL CBLTDLI USING GHNP 2213-PCB DLI-IO-WDGX2214 SSA1                
030000     MOVE 2213-STATUS-CODE    TO STATUS-WS                                
030100     PERFORM IMS-STATUSKONTROLL                                           
030200                                                                          
030300     IF SEGMENT-FINNS                                                     
030400        MOVE 'READ  '   TO POSTSUM-FDNAMN                                 
030500        MOVE 'WDG3  '   TO POSTSUM-DDNAMN2                                
030600        MOVE '2214  '   TO POSTSUM-TRANSTYP                               
030700        CALL POSTSUM USING POSTSUM-PARM                                   
030800     END-IF                                                               
030900     .                                                                    
031000                                                                          
031010 IMS-GHNP-WDG302-2214-KVAL SECTION.                                       
031020                                                                          
031030     MOVE 'GHNP-WDG302-2214-KVAL' TO CURRENT-IMS-SECTION                  
031040                                                                          
031050     MOVE SPACE               TO ALL-SSA                                  
031051     STRING 'WDG302  (IDARTNR  =' W-IDARTNR-2214-X ')'                    
031052          DELIMITED BY SIZE INTO SSA1                                     
031070     MOVE '  GE'              TO GODK-STATUSKODER                         
031080     CALL CBLTDLI USING GHNP 2213-PCB DLI-IO-WDGX2214 SSA1                
031090     MOVE 2213-STATUS-CODE    TO STATUS-WS                                
031091     PERFORM IMS-STATUSKONTROLL                                           
031099     .                                                                    
031100                                                                          
031101                                                                          
031110 IMS-DLET-WDG302-2214 SECTION.                                            
031120                                                                          
031130     MOVE 'DLET-WDG302-2214' TO CURRENT-IMS-SECTION                       
031140                                                                          
031141     MOVE SPACE               TO ALL-SSA                                  
031150     MOVE '    '              TO GODK-STATUSKODER                         
031160     CALL CBLTDLI USING DLET 2213-PCB DLI-IO-WDGX2214                     
031170     MOVE 2213-STATUS-CODE    TO STATUS-WS                                
031180     PERFORM IMS-STATUSKONTROLL                                           
031181     ADD +1 TO CHKP-ANT                                                   
031190     .                                                                    
031191                                                                          
031192                                                                          
031200 IMS-ISRT-WDG302-2204 SECTION.                                            
031300                                                                          
031400     MOVE 'ISRT-WDG302-2204' TO CURRENT-IMS-SECTION                       
031500                                                                          
031510     MOVE SPACE               TO ALL-SSA                                  
031600     STRING 'WDG301  (WDG3KEY  =' W-WDG3KEY-2203-X ')'                    
031700          DELIMITED BY SIZE INTO SSA1                                     
031800     MOVE 'WDG302 '           TO SSA2                                     
031900     MOVE '  II'              TO GODK-STATUSKODER                         
032000     CALL CBLTDLI USING ISRT 2203-PCB DLI-IO-WDGX2204 SSA1 SSA2           
032100     MOVE 2203-STATUS-CODE TO STATUS-WS                                   
032200     PERFORM IMS-STATUSKONTROLL                                           
032210     ADD +1 TO CHKP-ANT                                                   
032300                                                                          
032400     MOVE 'ISRT  '   TO POSTSUM-FDNAMN                                    
032500     MOVE 'WDG3  '   TO POSTSUM-DDNAMN2                                   
032600     MOVE '2204  '   TO POSTSUM-TRANSTYP                                  
032700     CALL POSTSUM USING POSTSUM-PARM                                      
032800     .                                                                    
032900                                                                          
033000                                                                          
033010 IMS-GU-WDK611 SECTION.                                                   
033020                                                                          
033030     MOVE 'GU-WDK611       ' TO CURRENT-IMS-SECTION                       
033040                                                                          
033050     MOVE SPACE               TO ALL-SSA                                  
033060     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
033070          DELIMITED BY SIZE INTO SSA1                                     
033080     STRING 'WDK611 '                                                     
033090          DELIMITED BY SIZE INTO SSA2                                     
033091     MOVE '  GE'            TO GODK-STATUSKODER                           
033092     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
033093     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
033094     PERFORM IMS-STATUSKONTROLL                                           
033095     .                                                                    
033096                                                                          
033097                                                                          
033100 IMS-GU-WDK701 SECTION.                                                   
033200                                                                          
033300     MOVE 'GU-WDK701       ' TO CURRENT-IMS-SECTION                       
033400                                                                          
033410     MOVE SPACE               TO ALL-SSA                                  
033500     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
033600          DELIMITED BY SIZE INTO SSA1                                     
033900     MOVE '  '              TO GODK-STATUSKODER                           
034000     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
034100     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
034200     PERFORM IMS-STATUSKONTROLL                                           
034300     .                                                                    
034400                                                                          
034500                                                                          
034600 IMS-GNP-WDK711     SECTION.                                              
034610                                                                          
034611     MOVE 'GNP-WDK711      ' TO CURRENT-IMS-SECTION                       
034612                                                                          
034613     MOVE SPACE               TO ALL-SSA                                  
034614     STRING 'WDK711  *F(IDDC     =' W-IDDC-X ')'                          
034617          DELIMITED BY SIZE INTO SSA1                                     
034619     MOVE '  GE'              TO GODK-STATUSKODER                         
034620     CALL CBLTDLI USING GNP  WDK7-PCB DLI-IO-WDK711 SSA1                  
034621     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
034622     PERFORM IMS-STATUSKONTROLL                                           
034623     .                                                                    
034624                                                                          
034625                                                                          
034626 IMS-GNP-WDK711-REF SECTION.                                              
034627                                                                          
034628     MOVE 'GNP-WDK711-REF  ' TO CURRENT-IMS-SECTION                       
034629                                                                          
034634     MOVE SPACE               TO ALL-SSA                                  
034635     STRING 'WDK711  (IDDC    >=' W-IDDC-MIN-X                            
034636                    '&IDDC    <=' W-IDDC-MAX-X                            
034637                    '&IDDCREF  =' W-IDDC-REF-X ')'                        
034638          DELIMITED BY SIZE INTO SSA1                                     
034639     MOVE '  GE'              TO GODK-STATUSKODER                         
034640     CALL CBLTDLI USING GNP  WDK7-PCB DLI-IO-WDK711 SSA1                  
034641     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
034642     PERFORM IMS-STATUSKONTROLL                                           
034643     .                                                                    
034644                                                                          
034645                                                                          
034650 IMS-GHNP-WDK712 SECTION.                                                 
034700                                                                          
034800     MOVE 'GHNP-WDK712     ' TO CURRENT-IMS-SECTION                       
034900                                                                          
034910     MOVE SPACE               TO ALL-SSA                                  
035200     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
035300          DELIMITED BY SIZE INTO SSA1                                     
035400     MOVE '  GE'            TO GODK-STATUSKODER                           
035500     CALL CBLTDLI USING GHNP WDK7-PCB DLI-IO-WDK712 SSA1                  
035600     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
035700     PERFORM IMS-STATUSKONTROLL                                           
035800     .                                                                    
035900                                                                          
036000                                                                          
036100 IMS-REPL-WDK712 SECTION.                                                 
036200                                                                          
036300     MOVE 'REPL-WDK712     ' TO CURRENT-IMS-SECTION                       
036400                                                                          
036410     MOVE SPACE               TO ALL-SSA                                  
036500     MOVE '    '              TO GODK-STATUSKODER                         
036600     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK712                       
036700     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
036800     PERFORM IMS-STATUSKONTROLL                                           
036810     ADD +1 TO CHKP-ANT                                                   
036900     .                                                                    
037000                                                                          
037100                                                                          
038710 IMS-GHNP-WDK722 SECTION.                                                 
038720                                                                          
038730     MOVE 'GHNP-WDK722     ' TO CURRENT-IMS-SECTION                       
038740                                                                          
038750     MOVE SPACE               TO ALL-SSA                                  
038760     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
038770          DELIMITED BY SIZE INTO SSA1                                     
038780     MOVE 'WDK722 '           TO SSA2                                     
038790     MOVE '  GE'              TO GODK-STATUSKODER                         
038791     CALL CBLTDLI USING GHNP WDK7-PCB DLI-IO-WDK722 SSA1 SSA2             
038792     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
038793     PERFORM IMS-STATUSKONTROLL                                           
038795     .                                                                    
038796                                                                          
038797                                                                          
038800 IMS-REPL-WDK722 SECTION.                                                 
038900                                                                          
039000     MOVE 'REPL-WDK722     ' TO CURRENT-IMS-SECTION                       
039100                                                                          
039110     MOVE SPACE               TO ALL-SSA                                  
039200     MOVE '    '              TO GODK-STATUSKODER                         
039300     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK722                       
039400     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
039500     PERFORM IMS-STATUSKONTROLL                                           
039510     ADD +1 TO CHKP-ANT                                                   
039600     .                                                                    
039700                                                                          
039800                                                                          
039900 IMS-GU-WDF101 SECTION.                                                   
040000                                                                          
040100     MOVE 'GU-WDF101       ' TO CURRENT-IMS-SECTION                       
040200                                                                          
040210     MOVE SPACE               TO ALL-SSA                                  
040300     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
040400          DELIMITED BY SIZE INTO SSA1                                     
040700     MOVE '  '                TO GODK-STATUSKODER                         
040800     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-WDF101 SSA1                    
040900     MOVE WDF1-STATUS-CODE    TO STATUS-WS                                
041000     PERFORM IMS-STATUSKONTROLL                                           
041100     .                                                                    
041200                                                                          
041300                                                                          
041310 IMS-GNP-WDF116 SECTION.                                                  
041320                                                                          
041330     MOVE 'GNP-WDF116      ' TO CURRENT-IMS-SECTION                       
041340                                                                          
041350     MOVE SPACE               TO ALL-SSA                                  
041380     STRING 'WDF116  (IDDC     =' W-IDDC-X ')'                            
041390          DELIMITED BY SIZE INTO SSA1                                     
041391     MOVE '  GE'              TO GODK-STATUSKODER                         
041392     CALL CBLTDLI USING GNP WDF1-PCB DLI-IO-WDF116 SSA1                   
041393     MOVE WDF1-STATUS-CODE    TO STATUS-WS                                
041394     PERFORM IMS-STATUSKONTROLL                                           
041395     .                                                                    
041396                                                                          
041397                                                                          
041400 IMS-STATUSKONTROLL SECTION.                                              
041500                                                                          
041600     SET STATUS-IX TO 1                                                   
041700     SEARCH GODK-STATUS                                                   
041800       AT END                                                             
041900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
042000           DELIMITED BY SIZE INTO FELTEXT                                 
042100         DISPLAY FELTEXT                                                  
042200         CALL FELLOG                                                      
042300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
042400         CONTINUE                                                         
042500     END-SEARCH                                                           
042600     .                                                                    
