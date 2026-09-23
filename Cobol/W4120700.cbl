000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4120700.                                                
000300 AUTHOR.         KJELLSON GÖRAN.                                          
000400 DATE-WRITTEN.   14/09/09.                                                
000500                                                                          
000600 DATE-COMPILED.                                                           
000700*    FUNKTION:                                                            
000800*        UPPDATERING WDI1 (PIE-TRANSAR HISTORIK)                          
000900*                                                                         
001000*        PROGRAMMET UPPDATERAR WDI1                                       
001100*                   UPPDATERAR HÄNDELSE REGISTER (CHKPOINT)               
001200*                              WDR4                                       
001300*                                                                         
001400 ENVIRONMENT DIVISION.                                                    
001500                                                                          
001600 INPUT-OUTPUT SECTION.                                                    
001700                                                                          
001800 FILE-CONTROL.                                                            
001900                                                                          
002000*          --- WDI111 POSTER FÖR UPPDATERING                              
002100     SELECT W41227                     ASSIGN TO W41207D1.                
002200                                                                          
002300*          --- KVITTOPOSTER TILL PIE                                      
002400     SELECT W41228                     ASSIGN TO W41207D2.                
002500                                                                          
002600*          --- PIE-ORDE TILL ORDERENTRY                                   
002700     SELECT W41210                     ASSIGN TO W41207D3.                
002800                                                                          
002900*          --- PIE-ORDE serviceavtal                                      
003000     SELECT W41229                     ASSIGN TO W41207D4.                
003100                                                                          
003200                                                                          
003300 DATA DIVISION.                                                           
003400 FILE SECTION.                                                            
003500                                                                          
003600 FD  W41227                                                               
003700     RECORDING       F                                                    
003800     BLOCK CONTAINS  0.                                                   
003900                                                                          
004000*01  -COPY W46333      -L.                                                
004100                                                                          
004200                                                                          
004300 FD  W41228                                                               
004400     RECORDING       F                                                    
004500     BLOCK CONTAINS  0.                                                   
004600                                                                          
004700 01  KVITTO-POST-XML PIC X(80).                                           
004800                                                                          
004900                                                                          
005000 FD  W41210                                                               
005100     RECORDING       V                                                    
005200     BLOCK CONTAINS  0.                                                   
005300                                                                          
005400*01  POST -COPY W463VG4  -PRE OE-  -L.                                    
005500                                                                          
005600                                                                          
005700 FD  W41229                                                               
005800     RECORDING       F                                                    
005900     BLOCK CONTAINS  0.                                                   
006000                                                                          
006100*01  POST -COPY W46333   -PRE SERVICE-  -L.                               
006200                                                                          
006300                                                                          
006400                                                                          
006500 WORKING-STORAGE SECTION.                                                 
006600                                                                          
006700 77  IDPGM                       PIC X(8)    VALUE 'W4120700'.            
006800 01  CHKP-VAR.                                                            
006900     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
007000     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
007100     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
007200     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
007300     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
007400     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
007500 77  JA                          PIC X       VALUE 'J'.                   
007600 77  NEJ                         PIC X       VALUE 'N'.                   
007700 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
007800 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
007900 77  POST-ANT                    PIC S9(5)   VALUE +0   COMP-3.           
008000 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   VALUE +16 COMP SYNC.         
008100                                                                          
008200 01  FELTEXT.                                                             
008300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008500                                                                          
008600 77  W41227-EOF-SW               PIC X       VALUE 'N'.                   
008700     88  END-OF-W41227                       VALUE 'J'.                   
008800                                                                          
008900 01  SW-DUBBLETT                 PIC X       VALUE 'N'.                   
009000     88  DUBBLETT                            VALUE 'J'.                   
009100                                                                          
009200 01  WS-NOLLOR                   PIC 9(2)    VALUE ZERO.                  
009300 01  WS-START                    PIC 9(2)    VALUE ZERO.                  
009400 01  WS-LANGD                    PIC 9(2)    VALUE ZERO.                  
009500                                                                          
009600 01  XML-AREA.                                                            
009700     03  XML-HEADER              PIC X(43) VALUE                          
009800         '<?xml version="1.0" encoding="ISO-8859-1"?>'.                   
009900     03  XML-RECEIPT-START.                                               
010000         05  RECEIPT-START-1     PIC X(15) VALUE                          
010100         '<receipt xmlns='.                                               
010200         05  RECEIPT-START-2     PIC X(49) VALUE                          
010300         '"http://webservice.volvo.com/pie/supp/interface">'.             
010400     03  XML-RECEIPT-END         PIC X(10) VALUE '</receipt>'.            
010500     03  XML-HEADER-START        PIC X(10) VALUE '  <header>'.            
010600     03  XML-HEADER-END          PIC X(11) VALUE '  </header>'.           
010700     03  XML-RAD-START           PIC X(18) VALUE SPACE.                   
010800*        '<receiptDetails>'.                                              
010900     03  XML-RAD-END             PIC X(19) VALUE SPACE.                   
011000*        '</receiptDetails>'.                                             
011100     03  XML-VERSION             PIC X(26) VALUE SPACE.                   
011200*        '<version>1.0</version>'.                                        
011300     03  XML-ID.                                                          
011400         05  FILLER              PIC X(08) VALUE '    <id>'.              
011500         05  FILLER              PIC X(07) VALUE 'INT1376'.               
011600         05  FILLER              PIC X(05) VALUE '</id>'.                 
011700     03  XML-TIME-STAMP.                                                  
011800         05  XML-TIME-STAMP-ST   PIC X(15) VALUE SPACE.                   
011900*        '    <timeStamp>'.                                               
012000         05  XML-DATE            PIC X(10) VALUE SPACE.                   
012100         05  FILLER              PIC X(01) VALUE 'T'.                     
012200         05  XML-TIME            PIC X(18) VALUE SPACE.                   
012300         05  FILLER              PIC X(18) VALUE '</timeStamp>'.          
012400     03  XML-ORDER               PIC X(80) VALUE SPACE.                   
012500     03  XML-ORDER-HELP.                                                  
012600         05  XML-ORDER-START     PIC X(13) VALUE '    <orderId>'.         
012700         05  XML-IDPIERAD        PIC X(20) VALUE SPACE.                   
012800         05  XML-ORDER-END       PIC X(10) VALUE '</orderId>'.            
012900     03  XML-STATUS.                                                      
013000         05  FILLER              PIC X(12) VALUE '    <status>'.          
013100         05  XML-KDFEL           PIC 9(01) VALUE ZERO.                    
013200         05  FILLER              PIC X(09) VALUE '</status>'.             
013300                                                                          
013400 01  DAGENS-DATUM-TOT.                                                    
013500     03  DAGENS-DATUM            PIC 9(8)    VALUE ZERO.                  
013600     03  DAGENS-TID              PIC 9(8)    VALUE ZERO.                  
013700     03  DAGENS-UTC              PIC X(5)    VALUE ZERO.                  
013800                                                                          
013900 01  WS-DATUM.                                                            
014000     03  WS-AAR                  PIC 9(4)    VALUE ZERO.                  
014100     03  FILLER                  PIC X(1)    VALUE '-'.                   
014200     03  WS-MONTH                PIC 9(2)    VALUE ZERO.                  
014300     03  FILLER                  PIC X(1)    VALUE '-'.                   
014400     03  WS-DAY                  PIC 9(2)    VALUE ZERO.                  
014500 01  WS-TID.                                                              
014600     03  WS-HOUR                 PIC 9(2)    VALUE ZERO.                  
014700     03  FILLER                  PIC X(1)    VALUE ':'.                   
014800     03  WS-MIN                  PIC 9(2)    VALUE ZERO.                  
014900     03  FILLER                  PIC X(1)    VALUE ':'.                   
015000     03  WS-SEK                  PIC 9(2)    VALUE ZERO.                  
015100     03  FILLER                  PIC X(1)    VALUE '.'.                   
015200     03  WS-TUSEN                PIC 9(2)    VALUE ZERO.                  
015300     03  FILLER                  PIC 9(1)    VALUE ZERO.                  
015400     03  WS-UTC-SIGN             PIC X(1)    VALUE '+'.                   
015500     03  WS-UTC-HH               PIC 9(2)    VALUE 01.                    
015600     03  FILLER                  PIC X(1)    VALUE ':'.                   
015700     03  WS-UTC-MM               PIC 9(2)    VALUE 00.                    
015800                                                                          
015900 01  DYNAMISKA-SUBPROGRAM.                                                
016000*                                                                         
016100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
016200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
016300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
016400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
016500                                                                          
016600*    --- PARAMETRAR TILL POSTSUM                                          
016700*                                                                         
016800*01  -COPY W0005   -PRE  POSTSUM-                                         
016900                                                                          
017000 01  IN-AREA-START               PIC X(24)   VALUE                        
017100                                             'IN-AREA-START'.             
017200*01  AREA -COPY W46333     -PRE IN-                                       
017300*                                                                         
017400                                                                          
017500 01  OE-AREA-START               PIC X(24)   VALUE                        
017600                                             'OE-AREA-START'.             
017700*01  AREA -COPY W463VG4    -PRE OE-                                       
017800*                                                                         
017900                                                                          
018000 01  PIEK-AREA-START             PIC X(24)   VALUE                        
018100                                             'PIEK-AREA-START'.           
018200*01  PIEK-AREA -COPY W41228                                               
018300*                                                                         
018400                                                                          
018500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
018600                                                                          
018700*01  -COPY WDGX01                                                         
018800                                                                          
018900 01  NYCKLAR-TILL-DLI.                                                    
019000     03  W-WDI101KY-X.                                                    
019100         05  W-TIREGDAT          PIC 9(6)    VALUE ZERO.                  
019200         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
019300         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
019400     03  W-IDPIERAD-X.                                                    
019500         05  W-IDPIERAD          PIC X(20)   VALUE SPACE.                 
019600         05  FILLER REDEFINES W-IDPIERAD.                                 
019700             07  W-IDDISTR-FIX   PIC 9(4).                                
019800             07  W-IDKUNDNR-FIX  PIC 9(6).                                
019900             07  W-IDORDNR7-FIX  PIC 9(7).                                
020000             07  W-FILLER-FIX    PIC X(3).                                
020100                                                                          
020200*    --- STATUS-KOD FRÅN IMS                                              
020300 01  STATUS-WS                   PIC XX.                                  
020400     88  SEGMENT-FINNS                       VALUE '  '.                  
020500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
020600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
020700     88  SEGMENT-SLUT                        VALUE 'GB'.                  
020800     88  IMS-EJ-OK                           VALUE 'XD'.                  
020900                                                                          
021000 01  GODK-STATUSKODER.                                                    
021100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
021200                                                                          
021300 01  ALL-SSA.                                                             
021400     03 SSA1                     PIC X(64).                               
021500     03 SSA2                     PIC X(64).                               
021600                                                                          
021700*    --- IMS FUNKTIONSKODER                                               
021800*01  -COPY W0003                                                          
021900                                                                          
022000                                                                          
022100*    ---  DLI INPUT-OUTPUT AREA                                           
022200                                                                          
022300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDI101'.                      
022400 01  DLI-IO-WDI101.                                                       
022500*    03  -COPY WDI101                                                     
022600                                                                          
022700                                                                          
022800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDI111'.                      
022900 01  DLI-IO-WDI111.                                                       
023000*    03  -COPY WDI111                                                     
023100                                                                          
023200                                                                          
023300 01  FILLER         PIC X(16) VALUE '4580-IO-AREA'.                       
023400 01  4580-IO-AREA.                                                        
023500*    03  -COPY WDGX4580                                                   
023600                                                                          
023700                                                                          
023800 LINKAGE SECTION.                                                         
023900                                                                          
024000*01  -COPY W0009   -PRE MSG-                                              
024100                                                                          
024200*01  -COPY W0008  -PRE WDI1-                                              
024300     05  FILLER              PIC X.                                       
024400                                                                          
024500*01  -COPY W0008  -PRE WDI1A-                                             
024600     05  FILLER              PIC X.                                       
024700                                                                          
024800*01  -COPY W0008  -PRE 4579-                                              
024900     05  FILLER              PIC X.                                       
025000                                                                          
025100                                                                          
025200 PROCEDURE DIVISION  USING MSG-PCB WDI1-PCB WDI1A-PCB 4579-PCB.           
025300 MAIN SECTION.                                                            
025400     ENTRY 'DLITCBL' USING MSG-PCB WDI1-PCB WDI1A-PCB 4579-PCB.           
025500                                                                          
025600     PERFORM A-INIT                                                       
025700                                                                          
025800     PERFORM S01-LAES-W41227                                              
025900                                                                          
026000     PERFORM UNTIL END-OF-W41227                                          
026100        IF CHKP-ANT > CHKP-MAX                                            
026200           PERFORM X-TAG-CHECKPOINT                                       
026300        END-IF                                                            
026400                                                                          
026500        PERFORM C-KOLLA-DUBBLETT                                          
026600        IF DUBBLETT                                                       
026700          IF POST-ANT NOT < 4580-KVPOST                                   
026800* VID ÅTERSTART SKALL REDAN BEHANDLADE POSTER INTE HANTERAS               
026900            PERFORM D-REGISTRERA-DUBBLETT                                 
027000          END-IF                                                          
027100        ELSE                                                              
027200          IF POST-ANT NOT < 4580-KVPOST                                   
027300* VID ÅTERSTART SKALL REDAN BEHANDLADE POSTER INTE HANTERAS               
027400            PERFORM E-REGISTRERA-NY-TRANS                                 
027500          END-IF                                                          
027600                                                                          
027700          IF IN-FLSERV = JA                                               
027800             PERFORM S13-SKRIV-W41229                                     
027900* ÄVEN ID ÅTERSTART EFTERSOM DEN DELETATS VID ABEND                       
028000          ELSE                                                            
028100             IF IN-KDSOFT = '2'                                           
028200                PERFORM F-SKRIV-PULS-ORDER                                
028300* ÄVEN ID ÅTERSTART EFTERSOM DEN DELETATS VID ABEND                       
028400             END-IF                                                       
028500          END-IF                                                          
028600        END-IF                                                            
028700                                                                          
028800        PERFORM G-SKAPA-KVITTO                                            
028900* ÄVEN ID ÅTERSTART EFTERSOM DEN DELETATS VID ABEND                       
029000                                                                          
029100        PERFORM S01-LAES-W41227                                           
029200     END-PERFORM                                                          
029300                                                                          
029400     PERFORM Z-FINIT                                                      
029500                                                                          
029600     MOVE ZERO TO RETURN-CODE                                             
029700     GOBACK                                                               
029800     .                                                                    
029900                                                                          
030000 A-INIT SECTION.                                                          
030100     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
030200                                                                          
030300     PERFORM IMS-RESTART                                                  
030400                                                                          
030500     OPEN INPUT  W41227                                                   
030600     OPEN OUTPUT W41228                                                   
030700                 W41210                                                   
030800                 W41229                                                   
030900                                                                          
031000                                                                          
031100     MOVE FUNCTION CURRENT-DATE TO DAGENS-DATUM-TOT                       
031200                                                                          
031300     MOVE DAGENS-DATUM(1:4)  TO WS-AAR                                    
031400     MOVE DAGENS-DATUM(5:2)  TO WS-MONTH                                  
031500     MOVE DAGENS-DATUM(7:2)  TO WS-DAY                                    
031600     MOVE WS-DATUM           TO XML-DATE                                  
031700                                                                          
031800     MOVE DAGENS-TID(1:2)    TO WS-HOUR                                   
031900     MOVE DAGENS-TID(3:2)    TO WS-MIN                                    
032000     MOVE DAGENS-TID(5:2)    TO WS-SEK                                    
032100     MOVE DAGENS-TID(7:2)    TO WS-TUSEN                                  
032200     MOVE WS-TID             TO XML-TIME                                  
032300                                                                          
032400*    Dagens-UTC verkar inte finnas i maskinen                             
032500*    hårdkodar i WS-TID i stället                                         
032600*    MOVE DAGENS-UTC(1:1)    TO WS-UTC-SIGN                               
032700*    MOVE DAGENS-UTC(2:2)    TO WS-UTC-HH                                 
032800*    MOVE DAGENS-UTC(3:2)    TO WS-UTC-MM                                 
032900                                                                          
033000*    LITE FIX EFTERSOM VALUE I PICTURE BESKRIVNINGEN KRÅNGLAR             
033100     MOVE '    <timeStamp>'            TO XML-TIME-STAMP-ST               
033200     MOVE '    <version>1.0</version>' TO XML-VERSION                     
033300     MOVE '  <receiptDetails>'         TO XML-RAD-START                   
033400     MOVE '  </receiptDetails>'        TO XML-RAD-END                     
033500                                                                          
033600     MOVE +0            TO POST-ANT                                       
033700                           CHKP-ANT                                       
033800                                                                          
033900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
034000     PERFORM IMS-LAS-ATERSTART                                            
034100                                                                          
034200     WRITE KVITTO-POST-XML FROM XML-HEADER                                
034300     WRITE KVITTO-POST-XML FROM XML-RECEIPT-START                         
034400     WRITE KVITTO-POST-XML FROM XML-HEADER-START                          
034500     WRITE KVITTO-POST-XML FROM XML-VERSION                               
034600     WRITE KVITTO-POST-XML FROM XML-ID                                    
034700     WRITE KVITTO-POST-XML FROM XML-TIME-STAMP                            
034800     WRITE KVITTO-POST-XML FROM XML-HEADER-END                            
034900     .                                                                    
035000                                                                          
035100 C-KOLLA-DUBBLETT SECTION.                                                
035200     MOVE 'C-KOLLA-DUBBLETT' TO CURRENT-SECTION                           
035300                                                                          
035400     MOVE IN-IDPIERAD      TO W-IDPIERAD                                  
035500                                                                          
035600     PERFORM IMS-GHU-WDI111                                               
035700     IF SEGMENT-FINNS                                                     
035800        MOVE JA  TO SW-DUBBLETT                                           
035900     ELSE                                                                 
036000        MOVE NEJ TO SW-DUBBLETT                                           
036100     END-IF                                                               
036200     .                                                                    
036300                                                                          
036400                                                                          
036500 D-REGISTRERA-DUBBLETT SECTION.                                           
036600     MOVE 'D-REG-DUBBLETT  ' TO CURRENT-SECTION                           
036700                                                                          
036800     MOVE DAGENS-DATUM       TO PIET-TIUPPDAT                             
036900     ADD +1                  TO PIET-KVANTEX                              
037000     MOVE 1                  TO PIET-KDFEL                                
037100     PERFORM IMS-REPL-WDI111                                              
037200     .                                                                    
037300                                                                          
037400                                                                          
037500 E-REGISTRERA-NY-TRANS SECTION.                                           
037600     MOVE 'E-REG-NY-TRANS  ' TO CURRENT-SECTION                           
037700                                                                          
037800     MOVE DAGENS-DATUM     TO PIED-TIREGDAT                               
037900     MOVE IN-IDDISTR       TO PIED-IDDISTR                                
038000     MOVE IN-IDKUNDNR      TO PIED-IDKUNDNR                               
038100     MOVE IN-IDORDNR7      TO PIED-IDORDNR7                               
038200     PERFORM IMS-ISRT-WDI101                                              
038300                                                                          
038400     MOVE IN-IDPIERAD      TO PIET-IDPIERAD                               
038500     IF IN-KDSOFT = 'I'                                                   
038600        MOVE 2             TO PIET-KDSOFT                                 
038700     ELSE                                                                 
038800        MOVE IN-KDSOFT     TO PIET-KDSOFT                                 
038900     END-IF                                                               
039000                                                                          
039100     MOVE ZERO             TO PIET-KDFEL                                  
039200                              PIET-KVANTEX                                
039300                              PIET-TIUPPDAT                               
039400                                                                          
039500     MOVE DAGENS-DATUM     TO W-TIREGDAT                                  
039600     MOVE IN-IDDISTR       TO W-IDDISTR                                   
039700     MOVE IN-IDKUNDNR      TO W-IDKUNDNR                                  
039800                                                                          
039900     PERFORM IMS-ISRT-WDI111                                              
040000     .                                                                    
040100                                                                          
040200                                                                          
040300 F-SKRIV-PULS-ORDER SECTION.                                              
040400     MOVE 'F-SKRIV-PULS-O  ' TO CURRENT-SECTION                           
040500                                                                          
040600     MOVE IN-AREA  TO OE-AREA                                             
040700     PERFORM S11-SKRIV-W41210                                             
040800     .                                                                    
040900                                                                          
041000                                                                          
041100 G-SKAPA-KVITTO SECTION.                                                  
041200     MOVE 'G-SKAPA-KVITTO  ' TO CURRENT-SECTION                           
041300                                                                          
041400     MOVE IN-IDPIERAD      TO XML-IDPIERAD                                
041500     IF IN-KDSOFT = 'I'                                                   
041600*    sw-order till interndistrikt får bara ett kvitto                     
041700        IF DUBBLETT                                                       
041800           MOVE 1          TO XML-KDFEL                                   
041900        ELSE                                                              
042000           MOVE 2          TO XML-KDFEL                                   
042100        END-IF                                                            
042200     ELSE                                                                 
042300        MOVE PIET-KDFEL    TO XML-KDFEL                                   
042400     END-IF                                                               
042500     PERFORM S12-SKRIV-W41228                                             
042600     .                                                                    
042700                                                                          
042800                                                                          
042900 Z-FINIT SECTION.                                                         
043000     MOVE 'Z-FINIT         ' TO CURRENT-SECTION                           
043100                                                                          
043200     WRITE KVITTO-POST-XML FROM XML-RECEIPT-END                           
043300                                                                          
043400     CLOSE W41227                                                         
043500           W41228                                                         
043600           W41210                                                         
043700           W41229                                                         
043800                                                                          
043900*    NOLLA ÅTERSTARTINFORMATIONEN                                         
044000     PERFORM IMS-LAS-ATERSTART                                            
044100     MOVE +0         TO 4580-KVPOST                                       
044200     ACCEPT 4580-TIUPPDAT FROM DATE                                       
044300     ACCEPT 4580-TIUPPTID FROM TIME                                       
044400                                                                          
044500     PERFORM IMS-REPL-ATERSTART                                           
044600                                                                          
044700     MOVE 'S'        TO POSTSUM-OPKOD                                     
044800     CALL POSTSUM USING POSTSUM-PARM                                      
044900     .                                                                    
045000                                                                          
045100                                                                          
045200 S01-LAES-W41227  SECTION.                                                
045300                                                                          
045400     READ W41227 INTO IN-AREA                                             
045500     AT END                                                               
045600        SET END-OF-W41227 TO TRUE                                         
045700                                                                          
045800     NOT AT END                                                           
045900        MOVE 'W41227'   TO POSTSUM-FDNAMN                                 
046000        MOVE 'W41207D1' TO POSTSUM-DDNAMN2                                
046100        MOVE 'PIET'     TO POSTSUM-TRANSTYP                               
046200        CALL POSTSUM USING POSTSUM-PARM                                   
046300                                                                          
046400        ADD +1          TO POST-ANT                                       
046500     END-READ                                                             
046600     .                                                                    
046700                                                                          
046800                                                                          
046900 S11-SKRIV-W41210 SECTION.                                                
047000                                                                          
047100     WRITE OE-POST FROM OE-AREA                                           
047200                                                                          
047300     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
047400     MOVE 'W41210'   TO POSTSUM-FDNAMN                                    
047500     MOVE 'W41207D3' TO POSTSUM-DDNAMN2                                   
047600     CALL POSTSUM USING POSTSUM-PARM                                      
047700     .                                                                    
047800                                                                          
047900                                                                          
048000 S12-SKRIV-W41228 SECTION.                                                
048100                                                                          
048200     WRITE KVITTO-POST-XML FROM XML-RAD-START                             
048300     PERFORM S12A-KOMPRIMERA-ORDERNR                                      
048400     WRITE KVITTO-POST-XML FROM XML-ORDER                                 
048500     WRITE KVITTO-POST-XML FROM XML-STATUS                                
048600     WRITE KVITTO-POST-XML FROM XML-RAD-END                               
048700                                                                          
048800     IF DUBBLETT                                                          
048900        IF IN-KDSOFT = 1                                                  
049000           MOVE 'AFEL' TO POSTSUM-TRANSTYP                                
049100        ELSE                                                              
049200           MOVE 'OFEL' TO POSTSUM-TRANSTYP                                
049300        END-IF                                                            
049400     ELSE                                                                 
049500        IF IN-KDSOFT = 1                                                  
049600           MOVE 'AOK ' TO POSTSUM-TRANSTYP                                
049700        ELSE                                                              
049800           MOVE 'OOK ' TO POSTSUM-TRANSTYP                                
049900        END-IF                                                            
050000     END-IF                                                               
050100                                                                          
050200     MOVE 'W41228'   TO POSTSUM-FDNAMN                                    
050300     MOVE 'W41207D2' TO POSTSUM-DDNAMN2                                   
050400     CALL POSTSUM USING POSTSUM-PARM                                      
050500     .                                                                    
050600                                                                          
050700 S12A-KOMPRIMERA-ORDERNR SECTION.                                         
050800                                                                          
050900     MOVE SPACE TO XML-ORDER                                              
051000     MOVE ZERO  TO WS-NOLLOR                                              
051100                                                                          
051200     INSPECT XML-IDPIERAD TALLYING WS-NOLLOR FOR LEADING '0'              
051300     COMPUTE WS-START = WS-NOLLOR + 1                                     
051400     COMPUTE WS-LANGD = 20 - WS-NOLLOR                                    
051500                                                                          
051600     STRING XML-ORDER-START                                               
051700            XML-IDPIERAD(WS-START:WS-LANGD)                               
051800            XML-ORDER-END                                                 
051900          DELIMITED BY SIZE INTO XML-ORDER                                
052000     .                                                                    
052100                                                                          
052200                                                                          
052300 S13-SKRIV-W41229 SECTION.                                                
052400                                                                          
052500     WRITE SERVICE-POST FROM IN-AREA                                      
052600                                                                          
052700     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
052800     MOVE 'W41229'   TO POSTSUM-FDNAMN                                    
052900     MOVE 'W41207D4' TO POSTSUM-DDNAMN2                                   
053000     CALL POSTSUM USING POSTSUM-PARM                                      
053100     .                                                                    
053200                                                                          
053300 X-TAG-CHECKPOINT   SECTION.                                              
053400                                                                          
053500*    UPPDATERA ÅTERSTARTREGISTRET                                         
053600     PERFORM IMS-LAS-ATERSTART                                            
053700                                                                          
053800     MOVE POST-ANT        TO 4580-KVPOST                                  
053900     ACCEPT 4580-TIUPPDAT FROM DATE                                       
054000     ACCEPT 4580-TIUPPTID FROM TIME                                       
054100                                                                          
054200     PERFORM IMS-REPL-ATERSTART                                           
054300                                                                          
054400     PERFORM IMS-CHECKPOINT                                               
054500     MOVE ZERO TO CHKP-ANT                                                
054600     .                                                                    
054700                                                                          
054800* --- IMS SEKTIONER ---                                                   
054900                                                                          
055000 IMS-ISRT-WDI101 SECTION.                                                 
055100     MOVE 'IMS-ISRT-WDI101 ' TO CURRENT-IMS-SECTION                       
055200                                                                          
055300     MOVE SPACE            TO ALL-SSA                                     
055400     MOVE 'WDI101 '        TO SSA1                                        
055500     MOVE '  II'           TO GODK-STATUSKODER                            
055600     CALL CBLTDLI USING ISRT WDI1-PCB DLI-IO-WDI101 SSA1                  
055700     MOVE WDI1-STATUS-CODE TO STATUS-WS                                   
055800     PERFORM IMS-STATUSKONTROLL                                           
055900     ADD +1 TO CHKP-ANT                                                   
056000     .                                                                    
056100                                                                          
056200                                                                          
056300 IMS-GHU-WDI111 SECTION.                                                  
056400     MOVE 'IMS-GHU-WDI111  ' TO CURRENT-IMS-SECTION                       
056500                                                                          
056600     MOVE SPACE               TO ALL-SSA                                  
056700     STRING 'WDI111  (WDI1ASEQ =' W-IDPIERAD-X ')'                        
056800          DELIMITED BY SIZE INTO SSA1                                     
056900     MOVE '  GE'              TO GODK-STATUSKODER                         
057000     CALL CBLTDLI USING GHU WDI1A-PCB DLI-IO-WDI111 SSA1                  
057100     MOVE WDI1A-STATUS-CODE    TO STATUS-WS                               
057200     PERFORM IMS-STATUSKONTROLL                                           
057300     .                                                                    
057400                                                                          
057500                                                                          
057600 IMS-ISRT-WDI111 SECTION.                                                 
057700     MOVE 'IMS-ISRT-WDI111 ' TO CURRENT-IMS-SECTION                       
057800                                                                          
057900     MOVE SPACE               TO ALL-SSA                                  
058000     STRING 'WDI101  (WDI101KY =' W-WDI101KY-X ')'                        
058100          DELIMITED BY SIZE INTO SSA1                                     
058200     MOVE 'WDI111 '           TO SSA2                                     
058300     MOVE '  '                TO GODK-STATUSKODER                         
058400     CALL CBLTDLI USING ISRT WDI1-PCB DLI-IO-WDI111 SSA1 SSA2             
058500     MOVE WDI1-STATUS-CODE    TO STATUS-WS                                
058600     PERFORM IMS-STATUSKONTROLL                                           
058700     ADD +1 TO CHKP-ANT                                                   
058800     .                                                                    
058900                                                                          
059000                                                                          
059100 IMS-REPL-WDI111 SECTION.                                                 
059200     MOVE 'IMS-REPL-WDI111 ' TO CURRENT-IMS-SECTION                       
059300                                                                          
059400     MOVE SPACE            TO ALL-SSA                                     
059500     MOVE '  '             TO GODK-STATUSKODER                            
059600     CALL CBLTDLI USING REPL WDI1A-PCB DLI-IO-WDI111                      
059700     MOVE WDI1A-STATUS-CODE TO STATUS-WS                                  
059800     PERFORM IMS-STATUSKONTROLL                                           
059900     ADD +1 TO CHKP-ANT                                                   
060000     .                                                                    
060100                                                                          
060200                                                                          
060300                                                                          
060400 IMS-RESTART SECTION.                                                     
060500                                                                          
060600     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
060700     MOVE '  ' TO GODK-STATUSKODER                                        
060800     CALL CBLTDLI USING XRST MSG-PCB                                      
060900                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
061000                        CHKP-AREA-LENGTH CHKP-AREA                        
061100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
061200     PERFORM IMS-STATUSKONTROLL                                           
061300     .                                                                    
061400                                                                          
061500                                                                          
061600 IMS-CHECKPOINT SECTION.                                                  
061700                                                                          
061800     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
061900     MOVE '  XD' TO GODK-STATUSKODER                                      
062000     CALL CBLTDLI USING CHKP MSG-PCB                                      
062100                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
062200                        CHKP-AREA-LENGTH CHKP-AREA                        
062300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
062400     PERFORM IMS-STATUSKONTROLL                                           
062500                                                                          
062600     IF IMS-EJ-OK                                                         
062700       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
062800       DISPLAY FELTEXT                                                    
062900       CALL FELLOG                                                        
063000     END-IF                                                               
063100     .                                                                    
063200                                                                          
063300                                                                          
063400 IMS-LAS-ATERSTART SECTION.                                               
063500                                                                          
063600     MOVE '4579'           TO IDHTYP                                      
063700     MOVE LOW-VALUE        TO NYCKEL-VALFRI                               
063800     MOVE 'W4120700'       TO NYCKEL-VALFRI(1:8)                          
063900     MOVE SPACE            TO ALL-SSA                                     
064000     STRING 'WDR401  (WDGXKEY  =' WDGX01 ')'                              
064100       DELIMITED BY SIZE INTO SSA1                                        
064200     MOVE 'WDR470   '      TO SSA2                                        
064300     MOVE '    '           TO GODK-STATUSKODER                            
064400     CALL CBLTDLI USING GHU 4579-PCB 4580-IO-AREA SSA1 SSA2               
064500     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
064600     PERFORM IMS-STATUSKONTROLL                                           
064700     .                                                                    
064800                                                                          
064900 IMS-REPL-ATERSTART SECTION.                                              
065000     SKIP2                                                                
065100     MOVE '  '             TO GODK-STATUSKODER                            
065200     CALL CBLTDLI USING REPL 4579-PCB 4580-IO-AREA                        
065300     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
065400     PERFORM IMS-STATUSKONTROLL                                           
065500     .                                                                    
065600                                                                          
065700                                                                          
065800 IMS-STATUSKONTROLL SECTION.                                              
065900                                                                          
066000     SET STATUS-IX TO 1                                                   
066100     SEARCH GODK-STATUS                                                   
066200       AT END                                                             
066300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
066400           DELIMITED BY SIZE INTO FELTEXT                                 
066500         DISPLAY FELTEXT                                                  
066600         CALL FELLOG                                                      
066700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
066800         CONTINUE                                                         
066900     END-SEARCH                                                           
067000     .                                                                    
