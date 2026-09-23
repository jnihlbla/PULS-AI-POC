000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W4068100.                                                
000400 AUTHOR.         CAMELIA  & GAVIN                                         
000500 DATE-WRITTEN.   97/01/29.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        SKRIVER 'BILL OF LADING' FÖR NDC I  JAPAN.                       
001000*                                                                         
001100*                                                                         
001200*                                                                         
001300* PROGRAMMET LÄSER             WLGMTA (WDB2)  NAMN PÅ JAPANSKA            
001301*                              WLGMTA (WDB2)  TELNR (IDTFN)               
001310*                              WL4463 (WDR4)  BL BASEN                    
001600*                                                                         
001700*    INDATA.                                                              
001800*        TRANSAKTION: W4T681                                              
001900*        MID:         W4I68101                                            
002000*                                                                         
002100*    UTDATA.                                                              
002200*        MOD:         W4O68101                                            
002300                                                                          
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800 WORKING-STORAGE SECTION.                                                 
002900                                                                          
003000*    -- CHECKED BY WY2000                                                 
003100 77  IDPGM                       PIC X(08)   VALUE 'W4068100'.            
003200                                                                          
003300*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003400 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003500                                                                          
003600 77  JA                         PIC X          VALUE 'J'.                 
003700 77  NEJ                        PIC X          VALUE 'N'.                 
005000                                                                          
005100 77  SPAR-IDDISTR               PIC S9(5)      VALUE ZERO COMP-3.         
005200 77  SPAR-IDKUNDNR              PIC S9(7)      VALUE ZERO COMP-3.         
005300                                                                          
005400 77  WS-VIKT                    PIC S9(6)V9(1) VALUE ZERO COMP-3.         
005500 77  WS-VOLVIKT                 PIC S9(4)V9(3) VALUE ZERO COMP-3.         
005600 77  WS-KOLLI                   PIC S9(3)      VALUE ZERO COMP-3.         
005800                                                                          
005900 77  RKOD-ABEND-MED-DUMP        PIC S9(4)   VALUE +33 COMP SYNC.          
006000                                                                          
006310                                                                          
006400     EJECT                                                                
006500*    --- KONSTANTER                                                       
006800 77  JA                         PIC X          VALUE 'J'.                 
006900 77  NEJ                        PIC X          VALUE 'N'.                 
006910 01  TRANSPORT-NAME-100         PIC X(16) VALUE 'SEINO'.                  
006912 01  TRANSPORT-NAME-150         PIC X(16) VALUE 'SEINO AIR'.              
006920 01  TRANSPORT-NAME-200         PIC X(16) VALUE 'MEITETSU'.               
006921 01  DAILY                      PIC X(14) VALUE 'DAILY'.                  
006922 01  BULK                       PIC X(14) VALUE 'BULK'.                   
006930                                                                          
007400*                                                                         
008300*                                                                         
008400*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
008500                                                                          
008600                                                                          
008700 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
008800     88  NYCKLAR-OK                          VALUE 'J'.                   
008900     88  NYCKLAR-FEL                         VALUE 'N'.                   
009000                                                                          
009800 77  TRAFF-SW                    PIC X       VALUE 'J'.                   
009900     88  TRAFF                               VALUE 'J'.                   
010000                                                                          
010100 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
010200     88  EGEN-MID                            VALUE '4681'.                
010300     88  GODK-MID                            VALUE '4681' '4682'          
010400                                                   '4683' '4684'          
010500                                                   '4685' '4686'          
010600                                                   '4687' '4688'          
010700                                                   '4689'.                
010800     88  HELP-MID                            VALUE '0551'.                
010900     EJECT                                                                
011000*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
011100 01  GENERELLA-SUBPROGRAM.                                                
011200     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
011300     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
011400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011600     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
011700     03  W006PRS1                PIC X(8)    VALUE 'W006PRS1'.            
011800     EJECT                                                                
013800*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
013900*01 -COPY WMEDAREA                                                        
014000     SKIP3                                                                
014100 01  MESSAGE-CODES.                                                       
014200     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
014300     EJECT                                                                
014400*01  -COPY W006PRAR                                                       
014500     EJECT                                                                
014600*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
014700*                                                                         
014800 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
014900     SKIP3                                                                
015000*01 -COPY WMSGINIT                                                        
015100     SKIP3                                                                
015200*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
015300*                                                                         
015400 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
015500     SKIP3                                                                
015600*01  MID -COPY W4I68801                                                   
015700     EJECT                                                                
015800 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
015900     SKIP3                                                                
016000*01  -COPY WMSGAREA                                                       
016100     EJECT                                                                
016200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
016300     SKIP3                                                                
016400*01  -COPY WMFSAREA                                                       
016500     EJECT                                                                
016900 01  FILLER                      PIC X(16)   VALUE 'PRT-AREA  '.          
017000 01  WS-PRT-AREA.                                                         
017100                                                                          
017200     03  WS-PRT-DUMMY          PIC X.                                     
017300     03  WS-PRT-IDPRTLST       PIC X(8)  VALUE '4BL61   '  .              
017400     03  WS-LIST-RAD           PIC X(132).                                
017500*                                                                         
017600 01  FILLER                      PIC X(16)   VALUE 'LIST-RADER'.          
017700 01  LIST-RADER.                                                          
017800                                                                          
017900     03 RUBRIKRAD-1.                                                      
018000       05 FILLER               PIC X(37) VALUE SPACE.                     
018100       05 RUB1-AA              PIC Z9.                                    
018200       05 FILLER               PIC X(4)  VALUE SPACE.                     
018300       05 RUB1-MM              PIC Z9.                                    
018400       05 FILLER               PIC X(4)  VALUE SPACE.                     
018500       05 RUB1-DD              PIC Z9.                                    
018600                                                                          
018700     03 RUBRIKRAD-2.                                                      
018800       05 FILLER               PIC X(70) VALUE SPACE.                     
018900       05 RUB2-CARRIER-J       PIC X(16) VALUE SPACE.                     
019400                                                                          
019500     03 RUBRIKRAD-3.                                                      
019600       05 FILLER               PIC X(11) VALUE SPACE.                     
019700       05 RUB3-DLRNO           PIC Z(8).                                  
019800                                                                          
019900     03 RUBRIKRAD-4.                                                      
020000       05 FILLER               PIC X(72) VALUE SPACE.                     
020100       05 RUB4-ORDKL-J         PIC X(14) VALUE SPACE.                     
020200                                                                          
020210     03 RUBRIKRAD-5.                                                      
020220       05 FILLER               PIC X(13)  VALUE SPACE.                    
020230       05 RUB5-TELNO           PIC X(20).                                 
020240                                                                          
020300     03 RUBRIKRAD-6.                                                      
020400       05 FILLER               PIC X(12) VALUE SPACE.                     
020500       05 RUB6-ADDRESS1-J      PIC X(30) VALUE SPACE.                     
020600                                                                          
020700     03 RUBRIKRAD-7.                                                      
020800       05 FILLER               PIC X(12) VALUE SPACE.                     
020900       05 RUB7-ADDRESS2-J      PIC X(30) VALUE SPACE.                     
020910       05 FILLER               PIC X(29) VALUE SPACE.                     
020920       05 RUB7-VOL-KG          PIC Z(3)9.9.                               
020940       05 RUB7-ACT-KG          PIC Z(4)9.9.                               
021000                                                                          
021010     03 RUBRIKRAD-8.                                                      
021020       05 FILLER               PIC X(11) VALUE SPACE.                     
021030       05 RUB8-DLRNAME-J       PIC X(30) VALUE SPACE.                     
021040                                                                          
021050     03 RUBRIKRAD-9.                                                      
021060       05 FILLER               PIC X(71) VALUE SPACE.                     
021070       05 RUB9-KOLLINO         PIC ZZZ.                                   
021080                                                                          
024100     EJECT                                                                
024200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
024300*                                                                         
024400     SKIP2                                                                
024500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
024600     SKIP3                                                                
024700 01  NYCKLAR-TILL-DLI.                                                    
024800                                                                          
024810     03  W-IDGMTA-X.                                                      
024820         05  W-IDDISTR-WDB2      PIC S9(5)   VALUE ZERO COMP-3.           
024830         05  W-IDKUNDNR-WDB2     PIC S9(7)   VALUE ZERO COMP-3.           
024850                                                                          
025700                                                                          
026100                                                                          
026200     03  W-WDGXKEY-X.                                                     
026300         05  FILLER              PIC X(4)    VALUE '4463'.                
026400         05  FILLER              PIC X(2)    VALUE '61'.                  
026500         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
026600                                                                          
026700     03  W-DASKEPPN-X.                                                    
026800         05  W-DASKEPPN          PIC  9(8)   VALUE ZERO.                  
026900                                                                          
027000     03  W-KY4466-X.                                                      
027100         05  W-IDTRPTNR          PIC S9(3)   VALUE ZERO   COMP-3.         
027200         05  W-IDLBBET           PIC X(12)   VALUE SPACE.                 
027300                                                                          
027400     03  W-KY4468-X.                                                      
027500         05  W-IDDISTR-4468       PIC S9(5)   VALUE ZERO  COMP-3.         
027600         05  W-IDKUNDNR-4468      PIC S9(7)   VALUE ZERO  COMP-3.         
027700         05  W-IDKUNDRF           PIC X(10).                              
027800         05  W-IDKUNDRF-IDORDNR-FILLER REDEFINES W-IDKUNDRF.              
027900           07  W-IDORDNR7         PIC  9(7).                              
028000           07  FILLER             PIC  X(3).                              
028100         05  W-IDPRODNR           PIC S9(7)   VALUE ZERO  COMP-3.         
028200         05  W-IDKOLLI            PIC S9(5)   VALUE ZERO  COMP-3.         
028300     SKIP2                                                                
028400*    --- STATUS-KOD FRÅN IMS                                              
028500 01  STATUS-WS                   PIC XX.                                  
028600     88  SEGMENT-FINNS                       VALUE '  '.                  
028700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
028800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
028900     88  BASEN-SLUT                          VALUE 'GB'.                  
029000     SKIP2                                                                
029100 01  GODK-STATUSKODER.                                                    
029200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
029300     SKIP3                                                                
029400 01  SSA1                        PIC X(128).                              
029500 01  SSA2                        PIC X(64).                               
029600 01  SSA3                        PIC X(64).                               
029700     EJECT                                                                
029800*    --- IMS FUNKTIONSKODER                                               
029900*01  -COPY W0003                                                          
030000     EJECT                                                                
030100*    ---  DLI INPUT-OUTPUT AREA                                           
030193     EJECT                                                                
030200 01  FILLER                      PIC X(16)  VALUE 'DLI-AREA-4463'.        
030300     SKIP3                                                                
030400 01  DLI-IO-AREA-4463.                                                    
030500     03  WL446301.                                                        
030600*        05  -COPY WDGX4463                                               
030700     EJECT                                                                
030800 01  FILLER                      PIC X(16)  VALUE 'DLI-AREA-4464'.        
030900     SKIP3                                                                
031000 01  DLI-IO-AREA-4464.                                                    
031100     03  WL446311.                                                        
031200*        05  -COPY WDGX4464                                               
031300     EJECT                                                                
031400 01  FILLER                      PIC X(16)  VALUE 'DLI-AREA-4466'.        
031500     SKIP3                                                                
031600 01  DLI-IO-AREA-4466.                                                    
031700     03  WL446321.                                                        
031800*        05  -COPY WDGX4466                                               
031900     EJECT                                                                
032000 01  FILLER                      PIC X(16)  VALUE 'DLI-AREA-4468'.        
032100     SKIP3                                                                
032200 01  DLI-IO-AREA-4468.                                                    
032300     03  WL446331.                                                        
032400*        05  -COPY WDGX4468                                               
032500     EJECT                                                                
032600 01  FILLER                      PIC X(16)  VALUE 'DLI-AREA-WDB2'.        
032620 01  DLI-IO-AREA-KUNDREG.                                                 
032630     03  WLGMTA.                                                          
032640*        05  -COPY WDB201                                                 
032650     EJECT                                                                
033600 LINKAGE SECTION.                                                         
033700                                                                          
033800*01  -COPY W0009   -PRE MSG-                                              
033900                                                                          
034000*01  -COPY W0009   -PRE ALT-                                              
034100                                                                          
034200*01  -COPY W0008   -PRE 4463-                                             
034300     05  FILLER                  PIC X.                                   
034400     EJECT                                                                
034500*01  -COPY W0008  -PRE KUND-                                              
034600     05  FILLER                  PIC X.                                   
034900     EJECT                                                                
035000 PROCEDURE DIVISION  USING MSG-PCB  ALT-PCB  4463-PCB                     
035100                           KUND-PCB         .                             
035200     ENTRY 'DLITCBL' USING MSG-PCB  ALT-PCB  4463-PCB                     
035300                           KUND-PCB         .                             
035400                                                                          
035500     PERFORM IMS-GET-MSG                                                  
035600     IF SEGMENT-FINNS                                                     
035700       PERFORM A-INIT                                                     
035800       IF NYCKLAR-OK                                                      
035900                                                                          
036000         IF MID-KDSVAR = 'P' OR 'p' OR 'X' or 'x'                         
036110           PERFORM IMS-GU-WL446321-TRANSP                                 
036200                                                                          
036300           IF SEGMENT-FINNS                                               
036400             PERFORM C-LAES-SKRIV-BILL-OF-LADING                          
036500           END-IF                                                         
036600         END-IF                                                           
036700                                                                          
036800         PERFORM Z-STAENG-PRINTER                                         
036900       END-IF                                                             
037000     END-IF                                                               
037100                                                                          
037200     MOVE ZERO TO RETURN-CODE                                             
037300     GOBACK                                                               
037400     .                                                                    
037500     EJECT                                                                
037600 A-INIT SECTION.                                                          
037700                                                                          
037800     MOVE MSG-INDATA-MINUS-1-TRANSKOD    TO MID-W4I68801                  
037900     MOVE MSG-IDTRANS-1 TO W-IDTRANS                                      
038000                                                                          
038100     MOVE MID-TIDATUM     TO W-DASKEPPN                                   
038111     IF MID-TIDATUM NOT = ZERO                                            
038112       IF MID-TIDATUM < 500000                                            
038120         MOVE 20          TO W-DASKEPPN (1:2)                             
038121       ELSE                                                               
038122         IF MID-TIDATUM < 999999                                          
038123           MOVE 19        TO W-DASKEPPN (1:2)                             
038124         ELSE                                                             
038125           MOVE 99999999  TO W-DASKEPPN                                   
038150         END-IF                                                           
038151       END-IF                                                             
038152     END-IF                                                               
038160                                                                          
038300     MOVE MID-IDTRPTNR    TO W-IDTRPTNR                                   
038400     MOVE MID-IDLBBET     TO W-IDLBBET                                    
038500                                                                          
039000     CALL W006PRS1  USING PRT-FORMS-OVR                                   
039100                          PRT-OPEN                                        
039200                          WS-PRT-IDPRTLST                                 
039300                          ALT-PCB                                         
039400                          WS-PRT-DUMMY                                    
039500                          WS-PRT-DUMMY                                    
039600                                                                          
039700     .                                                                    
039800     EJECT                                                                
039900 C-LAES-SKRIV-BILL-OF-LADING SECTION.                                     
040000                                                                          
040100     PERFORM IMS-GNP-WL446331-KOLLI                                       
040101                                                                          
040200     PERFORM UNTIL SEGMENT-SAKNAS                                         
040210       PERFORM CA-SKAPA-HUVUDET                                           
040300                                                                          
040600       PERFORM UNTIL  SEGMENT-SAKNAS    OR                                
040800                     (4468-IDDISTR  NOT = SPAR-IDDISTR)  OR               
040900                     (4468-IDKUNDNR NOT = SPAR-IDKUNDNR)                  
041000                                                                          
041010         COMPUTE WS-VIKT    = WS-VIKT    + 4468-VKORDBTO-KOLLI            
041020         COMPUTE WS-VOLVIKT = WS-VOLVIKT + 4468-VLORDBTO-KOLLI            
041030         COMPUTE WS-KOLLI   = WS-KOLLI   + 1                              
041200                                                                          
041300         PERFORM IMS-GNP-WL446331-KOLLI                                   
041301                                                                          
041400       END-PERFORM                                                        
041410                                                                          
041500       COMPUTE  WS-VOLVIKT ROUNDED =  WS-VOLVIKT * 280                    
041617       MOVE WS-VOLVIKT TO   RUB7-VOL-KG                                   
041619       MOVE WS-VIKT    TO   RUB7-ACT-KG                                   
041627       MOVE WS-KOLLI   TO   RUB9-KOLLINO                                  
041628                                                                          
041630       PERFORM CC-SKRIV                                                   
041700                                                                          
041800     END-PERFORM                                                          
042100     .                                                                    
042200     EJECT                                                                
042210 CA-SKAPA-HUVUDET SECTION.                                                
042220                                                                          
042300     MOVE 4468-IDDISTR  TO SPAR-IDDISTR                                   
042400                           W-IDDISTR-WDB2                                 
042500     MOVE 4468-IDKUNDNR TO SPAR-IDKUNDNR                                  
042600                           W-IDKUNDNR-WDB2                                
042700     MOVE ZERO          TO WS-VIKT                                        
042800                           WS-VOLVIKT                                     
042900                           WS-KOLLI                                       
042910                                                                          
043000     PERFORM IMS-GU-WLGMTA-TEL                                            
043010     MOVE  GMT-IDTFN            TO       RUB5-TELNO                       
043020     MOVE  GMT-ADGMT-OVR-GATA   TO       RUB6-ADDRESS1-J                  
043030     MOVE  GMT-ADGMT-OVR-PADR   TO       RUB7-ADDRESS2-J                  
043040     MOVE  GMT-BEGMT-OVR-RAD1   TO       RUB8-DLRNAME-J                   
043100                                                                          
043200     MOVE  W-DASKEPPN (3:2)     TO       RUB1-AA                          
043300     MOVE  W-DASKEPPN (5:2)     TO       RUB1-MM                          
043400     MOVE  W-DASKEPPN (7:2)     TO       RUB1-DD                          
043410                                                                          
043500     IF 4466-IDTRPTNR > 99 AND < 150                                      
043600       MOVE TRANSPORT-NAME-100    TO RUB2-CARRIER-J                       
043700     ELSE                                                                 
043800       IF 4466-IDTRPTNR = 150                                             
043900         MOVE TRANSPORT-NAME-150  TO RUB2-CARRIER-J                       
044000       ELSE                                                               
044100         MOVE TRANSPORT-NAME-200  TO RUB2-CARRIER-J                       
044200       END-IF                                                             
044300     END-IF                                                               
044310                                                                          
044400     MOVE  4468-IDKUNDNR        TO       RUB3-DLRNO                       
044500     MOVE  4468-KDORDKL         TO       RUB4-ORDKL-J                     
044600     IF 4468-KDORDKL = 1                                                  
044700       MOVE DAILY               TO       RUB4-ORDKL-J                     
044800     ELSE                                                                 
044900       MOVE BULK                TO       RUB4-ORDKL-J                     
045000     END-IF                                                               
045410     .                                                                    
045420     EJECT                                                                
045500                                                                          
061300 CC-SKRIV SECTION.                                                        
061400                                                                          
062300     MOVE SPACE           TO WS-LIST-RAD                                  
062400     MOVE PRT-NYSIDA-RAD1 TO PRT-RADSKIP                                  
062500     PERFORM S03-SKRIV                                                    
062600                                                                          
062610     MOVE RUBRIKRAD-1     TO WS-LIST-RAD                                  
062620     MOVE PRT-AFTER-1     TO PRT-RADSKIP                                  
062630     PERFORM S03-SKRIV                                                    
062640                                                                          
063010     MOVE RUBRIKRAD-2     TO WS-LIST-RAD                                  
063020     MOVE PRT-AFTER-9     TO PRT-RADSKIP                                  
063030     PERFORM S03-SKRIV                                                    
063040                                                                          
063100     MOVE RUBRIKRAD-3     TO WS-LIST-RAD                                  
063200     MOVE PRT-AFTER-1     TO PRT-RADSKIP                                  
063300     PERFORM S03-SKRIV                                                    
063400                                                                          
063500     MOVE RUBRIKRAD-4     TO WS-LIST-RAD                                  
063600     MOVE PRT-AFTER-2     TO PRT-RADSKIP                                  
063700     PERFORM S03-SKRIV                                                    
063800                                                                          
063810     MOVE RUBRIKRAD-5     TO WS-LIST-RAD                                  
063820     MOVE PRT-AFTER-2     TO PRT-RADSKIP                                  
063830     PERFORM S03-SKRIV                                                    
063840                                                                          
063900     MOVE RUBRIKRAD-6     TO WS-LIST-RAD                                  
064000     MOVE PRT-AFTER-2     TO PRT-RADSKIP                                  
064100     PERFORM S03-SKRIV                                                    
064200                                                                          
064210     MOVE RUBRIKRAD-7     TO WS-LIST-RAD                                  
064220     MOVE PRT-AFTER-1     TO PRT-RADSKIP                                  
064230     PERFORM S03-SKRIV                                                    
064240                                                                          
064250     MOVE RUBRIKRAD-8     TO WS-LIST-RAD                                  
064260     MOVE PRT-AFTER-3     TO PRT-RADSKIP                                  
064270     PERFORM S03-SKRIV                                                    
064280                                                                          
064290     MOVE RUBRIKRAD-9     TO WS-LIST-RAD                                  
064300     MOVE PRT-AFTER-1     TO PRT-RADSKIP                                  
064400     PERFORM S03-SKRIV                                                    
064600     .                                                                    
064700     EJECT                                                                
079400 Z-STAENG-PRINTER SECTION.                                                
079500                                                                          
079600     CALL W006PRS1  USING PRT-FORMS-OVR                                   
079700                          PRT-CLOSE                                       
079800                          WS-PRT-IDPRTLST                                 
079900                          ALT-PCB                                         
080000                          WS-PRT-DUMMY                                    
080100                          WS-PRT-DUMMY                                    
080200                                                                          
080300     .                                                                    
080400     EJECT                                                                
085000 S03-SKRIV SECTION.                                                       
085100                                                                          
085200     CALL W006PRS1 USING PRT-FORMS-OVR                                    
085300                         PRT-WRITE                                        
085400                         WS-PRT-IDPRTLST                                  
085500                         ALT-PCB                                          
085600                         PRT-RADSKIP                                      
085700                         WS-LIST-RAD                                      
085800     .                                                                    
085900     EJECT                                                                
086000                                                                          
086100* --- IMS SEKTIONER ---                                                   
086200     SKIP3                                                                
086300 IMS-GET-MSG SECTION.                                                     
086400                                                                          
086500     MOVE '  QC' TO GODK-STATUSKODER                                      
086600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
086700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
086800     PERFORM IMS-STATUSKONTROLL                                           
086900     .                                                                    
087000     SKIP3                                                                
087092 IMS-GU-WL446321-TRANSP SECTION.                                          
087093                                                                          
087094     STRING 'WL446301(WDGXKEY  =' W-WDGXKEY-X ')'                         
087095          DELIMITED BY SIZE INTO SSA1                                     
087096     STRING 'WL446311(DASKEPPN =' W-DASKEPPN-X ')'                        
087097          DELIMITED BY SIZE INTO SSA2                                     
087098     STRING 'WL446321(KY4466   =' W-KY4466-X ')'                          
087099          DELIMITED BY SIZE INTO SSA3                                     
087100     MOVE '  GE' TO GODK-STATUSKODER                                      
087101     CALL CBLTDLI USING GU 4463-PCB DLI-IO-AREA-4466                      
087102                        SSA1 SSA2 SSA3                                    
087103     MOVE 4463-STATUS-CODE TO STATUS-WS                                   
087104     PERFORM IMS-STATUSKONTROLL                                           
087105     .                                                                    
087106     EJECT                                                                
088501 IMS-GNP-WL446331-KOLLI SECTION.                                          
088502                                                                          
088503     MOVE 'WL446331 ' TO SSA1                                             
088504     MOVE '  GE' TO GODK-STATUSKODER                                      
088505     CALL CBLTDLI USING GNP 4463-PCB DLI-IO-AREA-4468 SSA1                
088506     MOVE 4463-STATUS-CODE TO STATUS-WS                                   
088507     PERFORM IMS-STATUSKONTROLL                                           
088508     .                                                                    
088509     SKIP3                                                                
088510                                                                          
088520 IMS-GU-WLGMTA-TEL SECTION.                                               
088521     STRING 'WLGMTA01(IDGMT    =' W-IDGMTA-X ')'                          
088522          DELIMITED BY SIZE INTO SSA1                                     
088523     MOVE '  ' TO GODK-STATUSKODER                                        
088524     CALL CBLTDLI USING GU KUND-PCB DLI-IO-AREA-KUNDREG                   
088525                        SSA1                                              
088526     MOVE KUND-STATUS-CODE TO STATUS-WS                                   
088527     PERFORM IMS-STATUSKONTROLL                                           
088528     .                                                                    
088529     EJECT                                                                
088530                                                                          
088531                                                                          
088560                                                                          
091700 IMS-STATUSKONTROLL SECTION.                                              
091800                                                                          
091900     SET STATUS-IX TO 1                                                   
092000     SEARCH GODK-STATUS                                                   
092100       AT END                                                             
092200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
092300         DELIMITED BY SIZE INTO FELTEXT                                   
092400         CALL FELLOG                                                      
092500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
092600         CONTINUE                                                         
092700     END-SEARCH                                                           
092800     .                                                                    
