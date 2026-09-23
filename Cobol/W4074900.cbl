001300 ID DIVISION.                                                             
001400 PROGRAM-ID.     W4074900.                                                
001500 AUTHOR.         SUSANNE OLSSON.                                          
001600 DATE-WRITTEN.   00/01/21.                                                
001700 DATE-COMPILED.                                                           
001800                                                                          
001900*    FUNKTION:                                                            
002000*        BAKGRUNDS MPP SOM SKRIVER UT PROFORMAFAKTURA OCH/ELLER           
002100*        KOLLIFLAGGOR.                                                    
002110*                                                                         
002200*        STARTAS AV PGM W40745                                            
002300*                                                                         
002400*        ANROPAR W006PRS1, GENERELT LISTNINGSPROGRAM VIA SPOOL-API        
002401*        UTAN ÅTERSTART TILL PRINTER.                                     
002403*                                                                         
002404*        PROGRAMMET LÄSER      WDR4                                       
002410*        PROGRAMMET LÄSER      WDB2                                       
002500*                                                                         
002600*    INDATA.                                                              
002700*        TRANSAKTION: W4T749X                                             
002800*        MID:         W4I74901                                            
002900*                                                                         
003000*    UTDATA.                                                              
003100*        PROFORMA OCH KOLLIFLAGGOR FÖR CDC                                
003110*                                      JAPAN                              
003120*                                      AUSTRALIEN                         
003130*                                                                         
003200                                                                          
003300     SKIP3                                                                
003400 ENVIRONMENT DIVISION.                                                    
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700 WORKING-STORAGE SECTION.                                                 
003701                                                                          
003710*    -- CHECKED BY WY2000                                                 
003800 77  IDPGM                       PIC X(08)   VALUE 'W4074900'.            
003900                                                                          
004000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004100 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004200                                                                          
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004401                                                                          
004402 77  INDX                        PIC S9(2)   VALUE ZERO COMP SYNC.        
004403 77  MAX-INDX                    PIC S9(2)   VALUE +50  COMP SYNC.        
004410 77  MAX-KVRADER                 PIC S9(3)   VALUE +38 COMP-3.            
004420 77  W-KVRADER                   PIC S9(3)   VALUE ZERO COMP-3.           
004430 77  W-IDSIDNR                   PIC S9(3)   VALUE ZERO COMP-3.           
004431 77  WS-KVKOLLI                  PIC 9(2)    VALUE ZERO.                  
004440 77  WS-TOTVIKT                  PIC S9(6)V9(1) VALUE ZERO COMP-3.        
004450 77  WS-IDKOLLI                  PIC S9(5)   VALUE ZERO COMP-3.           
004460 77  DUMMY-AREA                  PIC X(50)   VALUE SPACE.                 
004500                                                                          
005600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005700     88  EGEN-MID                            VALUE '4749'.                
005800     EJECT                                                                
005810******************************************************************        
005820 01  KOLLIKOD-TABELL.                                                     
005830     03  KOLLIKOD    OCCURS 50 TIMES.                                     
005840       05  TAB-KDKOLLI           PIC X(8)    VALUE SPACE.                 
005850                                                                          
005860******************************************************************        
005861     EJECT                                                                
005862*      --- VALID IDDC CODES                                               
005863*                                                                         
005865*01    -COPY WWDC99                                                       
005870                                                                          
005880     EJECT                                                                
005900 01  WS-IDPRTLST.                                                         
006000     03 FILLER                   PIC X(3)    VALUE '4PF'.                 
006200     03 WS-IDDC-PR               PIC X(2).                                
006300     03 FILLER                   PIC X(3)    VALUE '1  '.                 
006310                                                                          
006400     EJECT                                                                
006500*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006600 01  GENERELLA-SUBPROGRAM.                                                
006800     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007100     03  W006PRS1                PIC X(8)    VALUE 'W006PRS1'.            
007110     03  W006PRT                 PIC X(8)    VALUE 'W006PRT'.             
007200     EJECT                                                                
008010*01  -COPY W006PRT                                                        
008100     EJECT                                                                
008110     SKIP3                                                                
008120* VARIABLER TILL SUBPROGRAM W006PRS1                                      
008130*01  -COPY W006PRAR                                                       
008140     SKIP2                                                                
008141     EJECT                                                                
008144 01  WS-PROFORMA-RAD.                                                     
008145     03  FILLER                  PIC X(1)    VALUE SPACE.                 
008146     03  WS-RAPP-RAD             PIC X(80).                               
008150                                                                          
008160     EJECT                                                                
008200*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008300*                                                                         
008400 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008500     SKIP3                                                                
008600*01 -COPY WMSGINIT                                                        
008800     EJECT                                                                
008900*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
009000*                                                                         
009100 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009200     SKIP3                                                                
009300*01  MID -COPY W4I74901                                                   
009400     EJECT                                                                
009500 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009600     SKIP3                                                                
009700*01  -COPY WMSGAREA                                                       
009800     EJECT                                                                
010600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010700*                                                                         
010800     EJECT                                                                
010900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011000     SKIP3                                                                
011100 01  NYCKLAR-TILL-DLI.                                                    
011230     03  W-WDGX4101-X.                                                    
011240         05 W-4101-IDHTYP          PIC X(4)    VALUE '4101'.              
011250         05 W-4101-IDDC            PIC X(2)    VALUE SPACE.               
011260         05 W-4101-IDDISTR         PIC S9(5)   VALUE ZERO COMP-3.         
011270         05 W-4101-LOWVALUE        PIC X(21)   VALUE LOW-VALUE.           
011280                                                                          
011291     03  W-WDGX4102-MIN-X.                                                
011292         05 W-4102-IDKUNDNR-MIN    PIC S9(7)   VALUE ZERO COMP-3.         
011293         05 W-4102-IDRAPP-MIN      PIC X(10)   VALUE SPACE.               
011294         05 W-4102-IDKOLLI-MIN     PIC S9(5)   VALUE ZERO COMP-3.         
011295         05 W-4102-IDARTNR-MIN     PIC S9(9)   VALUE ZERO COMP-3.         
011296                                                                          
011297     03  W-WDGX4102-MAX-X.                                                
011298         05 W-4102-IDKUNDNR-MAX    PIC S9(7)   VALUE ZERO COMP-3.         
011299         05 W-4102-IDRAPP-MAX      PIC X(10)   VALUE SPACE.               
011300         05 W-4102-IDKOLLI-MAX     PIC S9(5) VALUE +99999 COMP-3.         
011301         05 W-4102-IDARTNR-MAX  PIC S9(9) VALUE +999999999 COMP-3.        
011302                                                                          
011303     03  W-IDGMT-X.                                                       
011304         05 W-IDDISTR-WDB2         PIC S9(5)   VALUE ZERO COMP-3.         
011305         05 W-IDKUNDNR-WDB2        PIC S9(7)   VALUE ZERO COMP-3.         
011306                                                                          
011310     SKIP2                                                                
011400*    --- STATUS-KOD FRÅN IMS                                              
011500 01  STATUS-WS                   PIC XX.                                  
011600     88  SEGMENT-FINNS                       VALUE '  '.                  
011700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011900     SKIP2                                                                
012000 01  GODK-STATUSKODER.                                                    
012100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012200     SKIP3                                                                
012300 01  SSA1                        PIC X(96).                               
012400 01  SSA2                        PIC X(96).                               
012500     EJECT                                                                
012600*    --- IMS FUNKTIONSKODER                                               
012700*01  -COPY W0003                                                          
012900     EJECT                                                                
013000*    ---  DLI INPUT-OUTPUT AREA                                           
013100                                                                          
013200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4101'.                    
013201 01  DLI-IO-WDGX4101.                                                     
013202*    03  -COPY WDGX4101                                                   
013203     EJECT                                                                
013208 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4102'.                    
013209 01  DLI-IO-WDGX4102.                                                     
013210*    03  -COPY WDGX4102                                                   
013211 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
013212 01  DLI-IO-WDB201.                                                       
013220*    03  -COPY WDB201                                                     
013500     EJECT                                                                
014030*  PRINTRADER FÖR PROFORMAFAKTURA                                         
014040                                                                          
014050 01  LIST-HRAD1.                                                          
014096     03   FILLER                  PIC X(40) VALUE SPACE.                  
014097     03   HRAD1-DATUM             PIC X(6)  VALUE SPACE.                  
014098     03   FILLER                  PIC X(3)  VALUE SPACE.                  
014099     03   HRAD1-IDDISTR           PIC Z(3)9 VALUE ZERO.                   
014100     03   FILLER                  PIC X(1)  VALUE SPACE.                  
014101     03   HRAD1-IDKUNDNR          PIC Z(6)  VALUE ZERO.                   
014102     03   FILLER                  PIC X(14)  VALUE SPACE.                 
014103     03   HRAD1-IDSIDNR           PIC Z(2)9 VALUE ZERO.                   
014110                                                                          
014111                                                                          
014120 01  LIST-HRAD2.                                                          
014130     03   FILLER                  PIC X(4)  VALUE SPACE.                  
014140     03   HRAD2-BEGMT-RAD1        PIC X(35) VALUE SPACE.                  
014141                                                                          
014142                                                                          
014143 01  LIST-HRAD3.                                                          
014144     03   FILLER                  PIC X(4)  VALUE SPACE.                  
014145     03   HRAD3-BEGMT-RAD2        PIC X(35) VALUE SPACE.                  
014146                                                                          
014147                                                                          
014148 01  LIST-HRAD4.                                                          
014149     03   FILLER                  PIC X(4)  VALUE SPACE.                  
014150     03   HRAD4-ADGMT-GATA        PIC X(35) VALUE SPACE.                  
014155                                                                          
014156                                                                          
014157 01  LIST-HRAD5.                                                          
014158     03   FILLER                  PIC X(4)  VALUE SPACE.                  
014159     03   HRAD5-ADGMT-PADR        PIC X(35) VALUE SPACE.                  
014161     03   FILLER                  PIC X(1)  VALUE SPACE.                  
014162     03   HRAD5-IDRAPP            PIC X(10) VALUE SPACE.                  
014163     03   FILLER                  PIC X(8)  VALUE SPACE.                  
014164     03   HRAD5-IDAVS             PIC X(20) VALUE SPACE.                  
014165                                                                          
014166                                                                          
014167 01  LIST-HRAD6.                                                          
014168     03   FILLER                  PIC X(4)  VALUE SPACE.                  
014169     03   HRAD6-ADGMT-LAND        PIC X(35) VALUE SPACE.                  
014170                                                                          
014180                                                                          
014190 01  LIST-LRAD.                                                           
014192     03   FILLER                  PIC X(2)  VALUE SPACE.                  
014193     03   LRAD-IDARTNR            PIC Z(7)9.                              
014194     03   FILLER                  PIC X(1)  VALUE SPACE.                  
014195     03   LRAD-TENOTE             PIC X(40).                              
014196     03   FILLER                  PIC X(1)  VALUE SPACE.                  
014197     03   LRAD-KVANTAL            PIC Z(4)9.                              
014198     03   FILLER                  PIC X(10)  VALUE SPACE.                 
014199     03   LRAD-PRARTBTO           PIC Z(6)9.9(2).                         
014200                                                                          
014201 01  LIST-KRAD1.                                                          
014203     03   FILLER                  PIC X(3)  VALUE SPACE.                  
014204     03   FILLER                  PIC X(18)  VALUE                        
014205                                       'EMBALLAGE/PACKING:'.              
014206     03   FILLER                  PIC X(2)  VALUE SPACE.                  
014207     03   KRAD1-KDKOLLI           PIC X(8).                               
014208                                                                          
014209                                                                          
014210 01  LIST-KRAD2.                                                          
014211     03   FILLER                  PIC X(23)  VALUE SPACE.                 
014212     03   KRAD2-KDKOLLI           PIC X(8).                               
014213                                                                          
014214                                                                          
014215 01  LIST-TRAD1.                                                          
014216     03   FILLER                  PIC X(75) VALUE SPACE.                  
014217     03   TRAD1-KVKOLLI           PIC Z(1)9.                              
014218                                                                          
014219                                                                          
014220 01  LIST-TRAD2.                                                          
014221     03   FILLER                  PIC X(69) VALUE SPACE.                  
014222     03   TRAD2-VKORDBTO          PIC Z(5)9.9(1).                         
014223                                                                          
014224     EJECT                                                                
014225 LINKAGE SECTION.                                                         
014226*01  -COPY W0009   -PRE MSG-                                              
014227     EJECT                                                                
014228*01  -COPY W0009   -PRE ALT-                                              
014229     EJECT                                                                
014230*01  -COPY W0008   -PRE USEA-                                             
014231     05  FILLER                  PIC X.                                   
014232                                                                          
014233*01  -COPY W0008  -PRE 4101-                                              
014234     05  FILLER                  PIC X.                                   
014235                                                                          
014236*01  -COPY W0008  -PRE WDB2-                                              
014237     05  FILLER                  PIC X.                                   
014238     EJECT                                                                
014239 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB USEA-PCB 4101-PCB              
014240                           WDB2-PCB.                                      
014241 MAIN SECTION.                                                            
014242     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB USEA-PCB 4101-PCB              
014250                           WDB2-PCB.                                      
014300                                                                          
014500     PERFORM IMS-GET-MSG                                                  
014600     IF SEGMENT-FINNS                                                     
014700       PERFORM A-INIT                                                     
014711                                                                          
014800       PERFORM B-SKAPA-PROFORMA                                           
014934                                                                          
016000     END-IF                                                               
016200                                                                          
016220     PERFORM Z-FINIT                                                      
016300     MOVE ZERO TO RETURN-CODE                                             
016400     GOBACK                                                               
016500     .                                                                    
016600     EJECT                                                                
016700 A-INIT SECTION.                                                          
016800                                                                          
016900     IF MSG-DUBBLA-TRANSKODER                                             
017000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I74901                 
017300     ELSE                                                                 
017400       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I74901                  
017700     END-IF                                                               
017800                                                                          
018300     MOVE LOW-VALUE TO MSG-AREA                                           
018310                                                                          
018400     MOVE MID-IDDC      TO W-4101-IDDC                                    
018500     MOVE MID-IDDISTR   TO W-4101-IDDISTR                                 
018510                           W-IDDISTR-WDB2                                 
018600                                                                          
018610     MOVE MID-IDKUNDNR  TO W-4102-IDKUNDNR-MIN                            
018611                           W-4102-IDKUNDNR-MAX                            
018612                           W-IDKUNDNR-WDB2                                
018613                                                                          
018620     MOVE MID-IDRAPP    TO W-4102-IDRAPP-MIN                              
018630                           W-4102-IDRAPP-MAX                              
018700                                                                          
018900     PERFORM AA-OPEN-PRINTER                                              
019600     .                                                                    
019700     EJECT                                                                
019710 AA-OPEN-PRINTER         SECTION.                                         
019720                                                                          
019721     MOVE MID-IDDC           TO WS-IDDC-PR                                
019722                                WS-IDDC                                   
019723     MOVE WS-IDPRTLST        TO PRT-IDPRTLST                              
019724                                                                          
019725     EVALUATE TRUE                                                        
019726        WHEN CDC-SE                                                       
019727           MOVE 'W40701'           TO PRT-PFDEF-A4S                       
019728        WHEN NDC-JP                                                       
019729           MOVE 'W40702'           TO PRT-PFDEF-A4S                       
019730        WHEN NDC-AU                                                       
019731           MOVE 'W40703'           TO PRT-PFDEF-A4S                       
019732     END-EVALUATE                                                         
019733                                                                          
019734     MOVE '5'                      TO PRT-COPIES-A4S                      
019735                                                                          
019740     CALL W006PRS1 USING PRT-SPOOL-A4S                                    
019750                         PRT-OPEN                                         
019760                         PRT-IDPRTLST                                     
019770                         ALT-PCB                                          
019780                         DUMMY-AREA                                       
019790                         DUMMY-AREA                                       
019791     .                                                                    
019792     EJECT                                                                
019800 B-SKAPA-PROFORMA SECTION.                                                
019900                                                                          
020000     MOVE ZERO                 TO W-IDSIDNR                               
020010                                  WS-IDKOLLI                              
020020                                  WS-KVKOLLI                              
020100                                                                          
020202     PERFORM IMS-GU-WDB201                                                
020203     IF SEGMENT-FINNS                                                     
020204       MOVE GMT-BEGMT-RAD1     TO HRAD2-BEGMT-RAD1                        
020205       MOVE GMT-BEGMT-RAD2     TO HRAD3-BEGMT-RAD2                        
020206       MOVE GMT-ADGMT-GATA     TO HRAD4-ADGMT-GATA                        
020207       MOVE GMT-ADGMT-PADR     TO HRAD5-ADGMT-PADR                        
020208       MOVE GMT-ADGMT-LAND     TO HRAD6-ADGMT-LAND                        
020209     END-IF                                                               
020210                                                                          
020300     PERFORM IMS-GU-WDGX4101                                              
020301                                                                          
020302     IF SEGMENT-FINNS                                                     
020303       PERFORM IMS-GNP-WDGX4102                                           
020304                                                                          
020305       IF SEGMENT-FINNS                                                   
020306                                                                          
020320         MOVE 4102-IDKOLLI    TO WS-IDKOLLI                               
020330         MOVE +1              TO INDX                                     
020331         MOVE 4102-KDKOLLI    TO TAB-KDKOLLI (INDX)                       
020340         MOVE +1              TO WS-KVKOLLI                               
020350         MOVE +39             TO W-KVRADER                                
020360         COMPUTE WS-TOTVIKT = WS-TOTVIKT + 4102-VKORDBTO-KOLLI            
020400                                                                          
020600         PERFORM UNTIL SEGMENT-SAKNAS                                     
020601           IF 4102-IDKOLLI = WS-IDKOLLI                                   
020603               PERFORM BA-SKAPA-RAD                                       
020604           ELSE                                                           
020605             ADD +1               TO INDX                                 
020606                                                                          
020607             MOVE 4102-IDKOLLI    TO WS-IDKOLLI                           
020608             IF INDX < MAX-INDX                                           
020609               MOVE 4102-KDKOLLI    TO TAB-KDKOLLI (INDX)                 
020610             END-IF                                                       
020611             COMPUTE WS-TOTVIKT = WS-TOTVIKT + 4102-VKORDBTO-KOLLI        
020612             ADD +1               TO WS-KVKOLLI                           
020613                                                                          
020614             PERFORM BA-SKAPA-RAD                                         
020620                                                                          
020640           END-IF                                                         
020650                                                                          
020700           PERFORM IMS-GNP-WDGX4102                                       
020800         END-PERFORM                                                      
020810                                                                          
020820         PERFORM BB-SKAPA-KOLLIKODER                                      
020821         PERFORM BC-SKAPA-TOTALER                                         
020830                                                                          
020900       END-IF                                                             
021000     END-IF                                                               
022200     .                                                                    
022400     EJECT                                                                
022500 BA-SKAPA-RAD       SECTION.                                              
022600                                                                          
023900     MOVE 4102-IDARTNR         TO LRAD-IDARTNR                            
024000     MOVE 4102-TENOTE          TO LRAD-TENOTE                             
024100     MOVE 4102-KVANTAL         TO LRAD-KVANTAL                            
024200     MOVE 4102-PRARTBTO        TO LRAD-PRARTBTO                           
024900                                                                          
024910     IF W-KVRADER              >  MAX-KVRADER                             
024920         PERFORM S01-SKAPA-HUVUD                                          
024930         MOVE PRT-AFTER-8      TO PRT-RADSKIP                             
024940         MOVE +1               TO W-KVRADER                               
024950      ELSE                                                                
024960         MOVE PRT-AFTER-1      TO PRT-RADSKIP                             
024970         ADD +1                TO W-KVRADER                               
024980     END-IF                                                               
024990     MOVE LIST-LRAD            TO WS-RAPP-RAD                             
024991                                                                          
024992     PERFORM S02-SKRIV-RAD                                                
024993                                                                          
024994     .                                                                    
024995     EJECT                                                                
024996 BB-SKAPA-KOLLIKODER   SECTION.                                           
024997                                                                          
024998     MOVE +1   TO INDX                                                    
024999     IF TAB-KDKOLLI (INDX)  = SPACE                                       
025000       MOVE SPACE                TO KRAD1-KDKOLLI                         
025002     ELSE                                                                 
025003       MOVE TAB-KDKOLLI (INDX)   TO KRAD1-KDKOLLI                         
025004     END-IF                                                               
025006                                                                          
025007     IF W-KVRADER              >  MAX-KVRADER  - 6                        
025008         PERFORM S01-SKAPA-HUVUD                                          
025009         MOVE PRT-AFTER-8      TO PRT-RADSKIP                             
025010         MOVE +1               TO W-KVRADER                               
025011      ELSE                                                                
025012         MOVE PRT-AFTER-6      TO PRT-RADSKIP                             
025013         ADD +7                TO W-KVRADER                               
025014     END-IF                                                               
025015     MOVE LIST-KRAD1           TO WS-RAPP-RAD                             
025016                                                                          
025017     PERFORM S02-SKRIV-RAD                                                
025019                                                                          
025020*- ENBART FÖRSTA RADEN SKALL HA MED "EMBALLAGE:" ......                   
025021                                                                          
025022     MOVE +2   TO INDX                                                    
025023     PERFORM UNTIL INDX > MAX-INDX                                        
025024                                                                          
025025     IF TAB-KDKOLLI (INDX)  = SPACE                                       
025026       CONTINUE                                                           
025027     ELSE                                                                 
025028       MOVE TAB-KDKOLLI (INDX)   TO KRAD2-KDKOLLI                         
025029                                                                          
025030       IF W-KVRADER              >  MAX-KVRADER                           
025031           PERFORM S01-SKAPA-HUVUD                                        
025032           MOVE PRT-AFTER-8      TO PRT-RADSKIP                           
025033           MOVE +1               TO W-KVRADER                             
025034        ELSE                                                              
025035           MOVE PRT-AFTER-2      TO PRT-RADSKIP                           
025036           ADD +2                TO W-KVRADER                             
025037       END-IF                                                             
025038       MOVE LIST-KRAD2           TO WS-RAPP-RAD                           
025039                                                                          
025040       PERFORM S02-SKRIV-RAD                                              
025041     END-IF                                                               
025042                                                                          
025043       ADD +1 TO INDX                                                     
025044     END-PERFORM                                                          
025045     .                                                                    
025046     EJECT                                                                
025047 BC-SKAPA-TOTALER   SECTION.                                              
025048                                                                          
025049     MOVE WS-KVKOLLI           TO TRAD1-KVKOLLI                           
025050                                                                          
025051     MOVE PRT-EQUAL-59         TO PRT-RADSKIP                             
025052     ADD +1                    TO W-KVRADER                               
025054     MOVE SPACE                TO WS-RAPP-RAD                             
025055                                                                          
025056     PERFORM S02-SKRIV-RAD                                                
025057                                                                          
025060     MOVE PRT-AFTER-1          TO PRT-RADSKIP                             
025061     ADD +1                    TO W-KVRADER                               
025062     MOVE LIST-TRAD1           TO WS-RAPP-RAD                             
025063                                                                          
025064     PERFORM S02-SKRIV-RAD                                                
025065                                                                          
025066                                                                          
025067     MOVE WS-TOTVIKT           TO TRAD2-VKORDBTO                          
025068                                                                          
025069     MOVE PRT-AFTER-1          TO PRT-RADSKIP                             
025070     ADD +1                    TO W-KVRADER                               
025071     MOVE LIST-TRAD2           TO WS-RAPP-RAD                             
025072                                                                          
025073     PERFORM S02-SKRIV-RAD                                                
025074                                                                          
025075     .                                                                    
025076     EJECT                                                                
025080 Z-FINIT                   SECTION.                                       
025100                                                                          
025200     CALL W006PRS1 USING PRT-SPOOL-A4S                                    
025300                         PRT-CLOSE                                        
025400                         PRT-IDPRTLST                                     
025500                         ALT-PCB                                          
025510                         DUMMY-AREA                                       
025520                         DUMMY-AREA                                       
025800     .                                                                    
025900     EJECT                                                                
025910 S01-SKAPA-HUVUD     SECTION.                                             
025920                                                                          
025960     ACCEPT HRAD1-DATUM           FROM DATE                               
025970     MOVE 4101-IDDISTR            TO HRAD1-IDDISTR                        
025971     MOVE 4102-IDKUNDNR           TO HRAD1-IDKUNDNR                       
025972                                                                          
025973     COMPUTE W-IDSIDNR            =  W-IDSIDNR + 1                        
025974     MOVE W-IDSIDNR               TO HRAD1-IDSIDNR                        
025980                                                                          
025991     MOVE PRT-NYSIDA-RAD7         TO PRT-RADSKIP                          
025994     MOVE LIST-HRAD1              TO WS-RAPP-RAD                          
025995                                                                          
025996     PERFORM S02-SKRIV-RAD                                                
025997                                                                          
026002     MOVE PRT-AFTER-2             TO PRT-RADSKIP                          
026004     MOVE LIST-HRAD2              TO WS-RAPP-RAD                          
026005                                                                          
026008     PERFORM S02-SKRIV-RAD                                                
026009                                                                          
026010     MOVE PRT-AFTER-1             TO PRT-RADSKIP                          
026011     MOVE LIST-HRAD3              TO WS-RAPP-RAD                          
026012                                                                          
026013     PERFORM S02-SKRIV-RAD                                                
026014                                                                          
026015     MOVE PRT-AFTER-1             TO PRT-RADSKIP                          
026017     MOVE LIST-HRAD4              TO WS-RAPP-RAD                          
026018                                                                          
026021     PERFORM S02-SKRIV-RAD                                                
026022                                                                          
026023     MOVE PRT-AFTER-1             TO PRT-RADSKIP                          
026024     MOVE 4102-IDRAPP             TO HRAD5-IDRAPP                         
026025     MOVE 4102-IDAVS              TO HRAD5-IDAVS                          
026027     MOVE LIST-HRAD5              TO WS-RAPP-RAD                          
026028                                                                          
026029     PERFORM S02-SKRIV-RAD                                                
026030                                                                          
026031     MOVE PRT-AFTER-1             TO PRT-RADSKIP                          
026032     MOVE LIST-HRAD6              TO WS-RAPP-RAD                          
026033                                                                          
026034     PERFORM S02-SKRIV-RAD                                                
026035                                                                          
026036     .                                                                    
026037     EJECT                                                                
026040 S02-SKRIV-RAD SECTION.                                                   
026100                                                                          
026200     CALL W006PRS1 USING PRT-SPOOL-A4S                                    
026300                         PRT-WRITE                                        
026400                         PRT-IDPRTLST                                     
026500                         ALT-PCB                                          
026600                         PRT-RADSKIP                                      
026700                         WS-PROFORMA-RAD                                  
026800                                                                          
026900     MOVE SPACE                TO WS-PROFORMA-RAD                         
027000     .                                                                    
027100     EJECT                                                                
029600* --- IMS SEKTIONER ---                                                   
029700     SKIP3                                                                
029800 IMS-GET-MSG SECTION.                                                     
029900                                                                          
030000     MOVE '  QC' TO GODK-STATUSKODER                                      
030100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
030200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
030300     PERFORM IMS-STATUSKONTROLL                                           
030400     .                                                                    
030500     SKIP3                                                                
031702 IMS-GU-WDGX4101 SECTION.                                                 
031703                                                                          
031704     STRING 'WDR401  (WDGXKEY  =' W-WDGX4101-X ')'                        
031705          DELIMITED BY SIZE INTO SSA1                                     
031708     MOVE '  GE' TO GODK-STATUSKODER                                      
031709     CALL CBLTDLI USING GU 4101-PCB DLI-IO-WDGX4101 SSA1                  
031710     MOVE 4101-STATUS-CODE TO STATUS-WS                                   
031711     PERFORM IMS-STATUSKONTROLL                                           
031712     .                                                                    
031713     EJECT                                                                
031714 IMS-GNP-WDGX4102 SECTION.                                                
031715                                                                          
031716     STRING  'WDGX4102(KY4102  >=' W-WDGX4102-MIN-X                       
031717                     '&KY4102  <=' W-WDGX4102-MAX-X ')'                   
031718              DELIMITED BY SIZE INTO SSA1                                 
031719     MOVE '  GE' TO GODK-STATUSKODER                                      
031720     CALL CBLTDLI USING GNP 4101-PCB DLI-IO-WDGX4102 SSA1                 
031721     MOVE 4101-STATUS-CODE TO STATUS-WS                                   
031722     PERFORM IMS-STATUSKONTROLL                                           
031723     .                                                                    
031724     SKIP3                                                                
031747 IMS-GU-WDB201 SECTION.                                                   
031748                                                                          
031749     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
031750          DELIMITED BY SIZE INTO SSA1                                     
031751     MOVE '  GE' TO GODK-STATUSKODER                                      
031752     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
031753     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
031754     PERFORM IMS-STATUSKONTROLL                                           
031760     .                                                                    
031800     EJECT                                                                
031900 IMS-STATUSKONTROLL SECTION.                                              
032000                                                                          
032100     SET STATUS-IX TO 1                                                   
032200     SEARCH GODK-STATUS                                                   
032300       AT END                                                             
032400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
032500         DELIMITED BY SIZE INTO FELTEXT                                   
032600         CALL FELLOG                                                      
032700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
032800         CONTINUE                                                         
032900     END-SEARCH                                                           
033000     .                                                                    
