000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2212E00.                                                
000300 AUTHOR.         OLSSON SUSANNE.                                          
000400 DATE-WRITTEN.   16/11/09.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PROGRAMMET KONTROLLERAR DAGENS SORTERADE 220-LARM OM             
000900*        LEVERANTÖREN SKALL HA AUTOMATISKA MAIL.                          
001000*        SAMMA PGM MEN OLIKA PROCEDURER/RUTINER FÖR CDC OCH KINA.         
001100*        SAKNAS MAILADRESS SÅ SKRIVS EN FELFIL UT.                        
001200*        UPPDATERING AV WDD4 OCH WDD925 SKER I BMP W2212F00.              
001300*                                                                         
001400*        PROGRAMMET LÄSER      WDF1 WDF1A                                 
001500*                              WDP3 (ANSKAFFAR INFO)                      
001600*                              WDB6                                       
001700*                                                                         
001800*    ABENDKODER:                                                          
001900*        U0016 -  . . . .                                                 
002000*        U1000 -  . . . .                                                 
002100*                                                                         
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     SKIP2                                                                
002600 INPUT-OUTPUT SECTION.                                                    
002700                                                                          
002800 FILE-CONTROL.                                                            
002900     SKIP2                                                                
003000*          --- INFIL (ARTIKLAR FÖR AUTOMATISKA MAIL)                      
003100     SELECT W2212D01                   ASSIGN TO W2212ED1.                
003200     SKIP2                                                                
003300*          --- UPPDATERINGSPOSTER TILL PGM W2212F00                       
003400     SELECT W2212F                     ASSIGN TO W2212ED2.                
003500     SKIP2                                                                
003600*          --- LEVERANTÖRER SOM SAKNAR MAILADRESS, FELLISTA               
003700     SELECT W2212ER                    ASSIGN TO W2212ED3.                
003800     SKIP2                                                                
003900*          --- MAIL DATA TILL LEVERANTÖREN VIA WMAILSND                   
004000     SELECT W2212E                     ASSIGN TO W2212ED4.                
004100     EJECT                                                                
004200 DATA DIVISION.                                                           
004300     SKIP2                                                                
004400 FILE SECTION.                                                            
004500     SKIP3                                                                
004600 FD  W2212D01                                                             
004700     RECORDING       F                                                    
004800     BLOCK CONTAINS  0.                                                   
004900                                                                          
005000*01  POST -COPY W2212D -PRE  IN-  -L.                                     
005100     SKIP3                                                                
005200 FD  W2212F                                                               
005300     RECORDING       F                                                    
005400     BLOCK CONTAINS  0.                                                   
005500                                                                          
005600*01  POST -COPY W2212F -PRE  UT2F-  -L.                                   
005700     SKIP3                                                                
005800 FD  W2212E                                                               
005900     RECORDING       F                                                    
006000     BLOCK CONTAINS  0.                                                   
006100                                                                          
006200 01  EMAIL-RECORD     PIC X(80).                                          
006300     SKIP3                                                                
006400 FD  W2212ER                                                              
006500     RECORDING       F                                                    
006600     BLOCK CONTAINS  0.                                                   
006700                                                                          
006800*01  POST -COPY W2212ER -PRE  UTER-  -L.                                  
006900     EJECT                                                                
007000 WORKING-STORAGE SECTION.                                                 
007100                                                                          
007200 77  IDPGM                       PIC X(8)    VALUE 'W2212E00'.            
007300 77  JA                          PIC X       VALUE 'J'.                   
007400 77  NEJ                         PIC X       VALUE 'N'.                   
007500 77  CURRENT-SECTION             PIC X(30)   VALUE SPACE.                 
007600 77  DBS-SECTION                 PIC X(30)   VALUE SPACE.                 
007700 77  SPAR-IDLEVNR                PIC X(5)    VALUE SPACE.                 
007800 77  SPAR-IDDC                   PIC X(2)    VALUE SPACE.                 
007900 77  SPAR-IDANSK                 PIC 9(3)    VALUE ZERO.                  
008000                                                                          
008100 77  TAB-IX                      PIC S9(3)   VALUE +0   COMP-3.           
008200 77  TAB-IX-MAX                  PIC S9(3)   VALUE +900 COMP-3.           
008300                                                                          
008400 77  W2212D01-EOF-SW             PIC X       VALUE 'N'.                   
008500     88  END-OF-W2212D01                     VALUE 'J'.                   
008600                                                                          
008700     EJECT                                                                
008800 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008900 01  FILLER REDEFINES DAGENS-DATUM.                                       
009000     03  DAGENS-DATUM-AAR        PIC 9(2).                                
009100     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
009200     03  DAGENS-DATUM-DAG        PIC 9(2).                                
009300     EJECT                                                                
009400 01  DYNAMISKA-SUBPROGRAM.                                                
009500*                                                                         
009600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
009700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010000     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
010100     SKIP2                                                                
010200*    --- PARAMETRAR TILL ABEND                                            
010300                                                                          
010400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
010500 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
010600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
010700     SKIP2                                                                
010800 01  FELTEXT.                                                             
010900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
011000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
011100     EJECT                                                                
011200*01  -COPY WWDCKONS                                                       
011300     EJECT                                                                
011400*    --- PARAMETRAR TILL POSTSUM                                          
011500*                                                                         
011600*01  -COPY W0005   -PRE  POSTSUM-                                         
011700     EJECT                                                                
011800*    --- PARAMETRAR TILL WDATKONV                                         
011900*                                                                         
012000*01  -COPY WDATAREA                                                       
012100     EJECT                                                                
012200 01  IN-AREA-START               PIC X(24)   VALUE                        
012300                                 'IN-AREA-START  '.                       
012400     SKIP2                                                                
012500                                                                          
012600*01  AREA -COPY W2212D      -PRE IN-                                      
012700     EJECT                                                                
012800 01  UT2F-AREA-START              PIC X(24)   VALUE                       
012900                                 'UT2F-AREA-START  '.                     
013000     SKIP2                                                                
013100                                                                          
013200*01  AREA -COPY W2212F      -PRE UT2F-                                    
013300     EJECT                                                                
013400 01  UTER-AREA-START             PIC X(24)   VALUE                        
013500                                 'UTER-AREA-START  '.                     
013600     SKIP2                                                                
013700                                                                          
013800*01  AREA -COPY W2212ER     -PRE UTER-                                    
013900     EJECT                                                                
014000******************************************************************        
014100*--  TABELL  LISTA W2212E-001 MED ARTIKLAR PER MAIL                       
014200******************************************************************        
014300 01  TAB-AREA-START              PIC X(24)   VALUE 'TAB-AREA  '.          
014400                                                                          
014500 01  LIST-TABELL.                                                         
014600     03  FILLER OCCURS 900 TIMES.                                         
014700         05  TAB-IDARTNR              PIC 9(8).                           
014800         05  TAB-TIAAMMDD             PIC 9(6).                           
014900         05  TAB-KVAVROP              PIC 9(7).                           
015000         05  TAB-KVAVIS               PIC 9(7).                           
015100         05  TAB-DAREGDAT-9KOMPL      PIC 9(8).                           
015200         05  TAB-TIKLOCK-9KOMPL       PIC 9(9).                           
015300     SKIP2                                                                
015400******************************************************************        
015500*--  EMAIL AREA                                                           
015600******************************************************************        
015700 01  MAIL-AREA-START             PIC X(24)   VALUE                        
015800                                 'MAIL-AREA-START  '.                     
015900     SKIP2                                                                
016000                                                                          
016100 01  MAIL-001.                                                            
016200     03  FILLER                  PIC X(80) VALUE ')SEND '.                
016300                                                                          
016400 01  MAIL-002.                                                            
016500     03  FILLER                  PIC X(65)                                
016600             VALUE 'TITLE  Volvo Cars - Action required for non-co        
016700-                  'nforming deliveries'.                                 
016800     03  FILLER                  PIC X(02) VALUE SPACE.                   
017010     03  MAIL-IDLEVNR-MFG-SUBJECT                                         
017020                                 PIC X(05) VALUE SPACE.                   
017030     03  FILLER                  PIC X(08) VALUE SPACE.                   
017100                                                                          
017200 01  MAIL-003.                                                            
017300     03  FILLER                  PIC X(03) VALUE 'TO '.                   
017400     03  MAIL-IDMAIL-MFG         PIC X(60) VALUE SPACE.                   
017500     03  FILLER                  PIC X(17) VALUE SPACE.                   
017600                                                                          
017700 01  MAIL-004.                                                            
017800     03  FILLER                  PIC X(08) VALUE 'REPLYTO '.              
017900     03  MAIL-IDMAIL-ANSK        PIC X(60) VALUE SPACE.                   
018000     03  FILLER                  PIC X(12) VALUE SPACE.                   
018100                                                                          
018200 01  MAIL-004A.                                                           
018300     03  FILLER                  PIC X(03) VALUE 'CC '.                   
018400     03  MAIL-IDMAIL-ANSK-CC     PIC X(60) VALUE SPACE.                   
018500     03  FILLER                  PIC X(12) VALUE SPACE.                   
018600                                                                          
018700 01  MAIL-005.                                                            
018800     03  FILLER                  PIC X(05) VALUE 'MAIL '.                 
018900     03  FILLER                  PIC X(75) VALUE SPACE.                   
019000                                                                          
019100 01  MAIL-006.                                                            
019200     03  FILLER                  PIC X(05) VALUE ')END '.                 
019300     03  FILLER                  PIC X(75) VALUE SPACE.                   
019400                                                                          
019500 01  MAIL-007.                                                            
019600     03  FILLER                  PIC X(80) VALUE SPACE.                   
019700                                                                          
019800 01  MAIL-ERR-TEXT.                                                       
019900     03  FILLER                  PIC X(80) VALUE SPACE.                   
020000                                                                          
020100     SKIP2                                                                
020200 01  MAIL-HDR-1.                                                          
020300     03  FILLER                  PIC X(80)                                
020400     VALUE 'Suppliers to Volvo Cars are required to maintain 100%         
020500-          'delivery performance for'.                                    
020600                                                                          
020700 01  MAIL-HDR-2.                                                          
022900     03  FILLER                  PIC X(80)                                
023000     VALUE 'both service and serial production parts. '.                  
021600                                                                          
027901 01  MAIL-HDR-3.                                                          
027902     03  FILLER                  PIC X(80)                                
027903     VALUE 'No despatch advice (ASN) has been received for followi        
027904-          'ng firm call-offs. Volvo'.                                    
022700                                                                          
027906 01  MAIL-HDR-4.                                                          
022900     03  FILLER                  PIC X(80)                                
027908     VALUE 'Cars requires your immediate recovery action plan.'.          
023200                                                                          
027910 01  MAIL-HDR-5.                                                          
023400     03  FILLER                  PIC X(80)                                
027912     VALUE 'If no reasonable action is provided, Volvo Cars will c        
027913-          'harge you as liquidated '.                                    
023600                                                                          
027915 01  MAIL-HDR-6.                                                          
023800     03  FILLER                  PIC X(80)                                
027917     VALUE 'damages for non-conforming deliveries, in accordance w        
027918-          'ith section 15 in '.                                          
024100                                                                          
027920 01  MAIL-HDR-6A.                                                         
024300     03  FILLER                  PIC X(80)                                
027922     VALUE 'the PMGTC.'.                                                  
024500                                                                          
027924 01  MAIL-HDR-7.                                                          
027925     03  FILLER         PIC X(08)   VALUE 'Supplier'.                     
027926     03  FILLER         PIC X(2)    VALUE SPACE.                          
027927     03  FILLER         PIC X(05)   VALUE 'Buyer'.                        
027928     03  FILLER         PIC X(4)    VALUE SPACE.                          
027929     03  FILLER         PIC X(10)   VALUE 'Part     '.                    
027930     03  FILLER         PIC X(6)    VALUE SPACE.                          
027931     03  FILLER         PIC X(08)   VALUE 'Despatch'.                     
027932     03  FILLER         PIC X(2)    VALUE SPACE.                          
027933     03  FILLER         PIC X(08)   VALUE 'Despatch'.                     
027934     03  FILLER         PIC X(2)    VALUE SPACE.                          
027935     03  FILLER         PIC X(07)   VALUE 'Adviced'.                      
027936     03  FILLER         PIC X(11)   VALUE SPACE.                          
024900                                                                          
027938 01  MAIL-HDR-8.                                                          
027939     03  FILLER         PIC X(31)   VALUE SPACE.                          
027940     03  FILLER         PIC X(06)   VALUE 'number'.                       
027941     03  FILLER         PIC X(6)    VALUE SPACE.                          
027942     03  FILLER         PIC X(04)   VALUE 'date'.                         
027943     03  FILLER         PIC X(10)   VALUE SPACE.                          
027944     03  FILLER         PIC X(09)   VALUE 'quantity '.                    
027945     03  FILLER         PIC X(2)    VALUE SPACE.                          
027946     03  FILLER         PIC X(08)   VALUE 'quantity'.                     
027947     03  FILLER         PIC X(04)   VALUE SPACE.                          
025300                                                                          
027949 01  MAIL-DATA.                                                           
027950     03  FILLER                  PIC X(2)    VALUE SPACE.                 
027951     03  MAIL-IDLEVNR-MFG        PIC X(05)   VALUE SPACE.                 
027952     03  FILLER                  PIC X(2)    VALUE SPACE.                 
027953     03  MAIL-IDLEVNR-DC         PIC X(05)   VALUE SPACE.                 
027954     03  FILLER                  PIC X(2)    VALUE SPACE.                 
027955     03  MAIL-IDARTNR            PIC Z(8).                                
027956     03  FILLER                  PIC X(3)    VALUE SPACE.                 
027957     03  MAIL-TIAAMMDD           PIC 9(6).                                
027958     03  FILLER                  PIC X(02)   VALUE SPACE.                 
027959     03  MAIL-KVAVROP            PIC Z(5)9.                               
027960     03  FILLER                  PIC X(04)   VALUE ' pcs'.                
027961     03  FILLER                  PIC X(02)   VALUE SPACE.                 
027962     03  MAIL-KVAVIS             PIC Z(5)9.                               
027963     03  FILLER                  PIC X(04)   VALUE ' pcs'.                
027964     03  FILLER                  PIC X(10)   VALUE SPACE.                 
025800                                                                          
027966 01  MAIL-HDR-9.                                                          
027967     03  FILLER                  PIC X(80)                                
027968     VALUE "Buyer's Supply Chain Coordinator:".                           
027100                                                                          
027970 01  MAIL-HDR-9A.                                                         
027971     03  FILLER                  PIC X(08)   VALUE 'Name    '.            
027972     03  MAIL-IDNAMN             PIC X(40)   VALUE SPACE.                 
027973     03  FILLER                  PIC X(33)   VALUE SPACE.                 
027500                                                                          
027975 01  MAIL-HDR-9B.                                                         
027976     03  FILLER                  PIC X(08)   VALUE 'E-mail  '.            
027977     03  MAIL-IDMAIL             PIC X(60)   VALUE SPACE.                 
027978     03  FILLER                  PIC X(12)   VALUE SPACE.                 
027900                                                                          
027980 01  MAIL-HDR-9C.                                                         
027981     03  FILLER                  PIC X(08)   VALUE 'Phone   '.            
027982     03  MAIL-IDTFN              PIC X(20)   VALUE SPACE.                 
027983     03  FILLER                  PIC X(52)   VALUE SPACE.                 
028300                                                                          
028400     EJECT                                                                
028500******************************************************************        
028600                                                                          
028700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
028800*                                                                         
028900     EJECT                                                                
029000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
029100     SKIP3                                                                
029200 01  NYCKLAR-TILL-DLI.                                                    
029300     03  W-WDF1A1KY-MIN-X.                                                
029400         05  W-IDLEVNR-MIN       PIC X(5)    VALUE SPACE.                 
029500         05  W-IDDC-KLEV-MIN     PIC X(2)    VALUE SPACE.                 
029600         05  W-KDMAIL-MIN        PIC X(4)    VALUE SPACE.                 
029700         05  W-IDATTENT-MIN      PIC S9(3)   VALUE ZERO COMP-3.           
029800                                                                          
029900     03  W-WDF1A1KY-MAX-X.                                                
030000         05  W-IDLEVNR-MAX       PIC X(5)    VALUE SPACE.                 
030100         05  W-IDDC-KLEV-MAX     PIC X(2)    VALUE SPACE.                 
030200         05  W-KDMAIL-MAX        PIC X(4)    VALUE SPACE.                 
030300         05  W-IDATTENT-MAX      PIC S9(3)   VALUE +999 COMP-3.           
030400                                                                          
030500     03  W-IDATTENT-X.                                                    
030600         05  W-IDATTENT          PIC S9(3)   VALUE ZERO COMP-3.           
030700                                                                          
030800     03  W-IDLEVNR-X.                                                     
030900         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
031000                                                                          
031100     03  W-IDDC-B6-X.                                                     
031200         05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
031300                                                                          
031400     03  W-KDARBTYP-X.                                                    
031500         05  W-KDARBTYP          PIC X(8)    VALUE 'ANSK    '.            
031600                                                                          
031700      03  W-IDPERSON-X.                                                   
031800          05  W-IDPERSON         PIC S9(3)   VALUE ZERO COMP-3.           
031900                                                                          
032000     SKIP2                                                                
032100*    --- STATUS-KOD FRÅN IMS                                              
032200 01  STATUS-WS                   PIC XX.                                  
032300     88  SEGMENT-FINNS                       VALUE '  '.                  
032400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
032500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
032600     88  SEGMENT-SLUT                        VALUE 'GB'.                  
032700     SKIP2                                                                
032800 01  GODK-STATUSKODER.                                                    
032900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
033000     SKIP3                                                                
033100 01  SSA1                        PIC X(96).                               
033200 01  SSA2                        PIC X(64).                               
033300     EJECT                                                                
033400*    --- IMS FUNKTIONSKODER                                               
033500*01  -COPY W0003                                                          
033600     EJECT                                                                
033700*    ---  DLI INPUT-OUTPUT AREA                                           
033800                                                                          
033900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF1A1'.                      
034000 01  DLI-IO-WDF1A1.                                                       
034100*    03  -COPY WDF1A1                                                     
034200     EJECT                                                                
034300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF107'.                      
034400 01  DLI-IO-WDF107.                                                       
034500*    03  -COPY WDF107                                                     
034600     EJECT                                                                
034700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
034800 01  DLI-IO-WDB601.                                                       
034900*    03  -COPY WDB601                                                     
035000     EJECT                                                                
035100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP311'.                      
035200 01  DLI-IO-WDP311.                                                       
035300*    03  -COPY WDP311                                                     
035400     EJECT                                                                
035500 LINKAGE SECTION.                                                         
035600                                                                          
035700                                                                          
035800*01  -COPY W0008  -PRE WDF1A-                                             
035900     05  FILLER                  PIC X.                                   
036000                                                                          
036100*01  -COPY W0008  -PRE WDF1-                                              
036200     05  FILLER                  PIC X.                                   
036300                                                                          
036400*01  -COPY W0008  -PRE WDB6-                                              
036500     05  FILLER                  PIC X.                                   
036600                                                                          
036700*01  -COPY W0008  -PRE WDP3-                                              
036800     05  FILLER                  PIC X.                                   
036900                                                                          
037000     EJECT                                                                
037100 PROCEDURE DIVISION  USING WDF1A-PCB WDF1-PCB WDB6-PCB WDP3-PCB.          
037200 MAIN SECTION.                                                            
037300     ENTRY 'DLITCBL' USING WDF1A-PCB WDF1-PCB WDB6-PCB WDP3-PCB.          
037400                                                                          
037500                                                                          
037600     PERFORM A-INIT                                                       
037700                                                                          
037800     PERFORM S01-LAES-W2212D01                                            
037900     PERFORM UNTIL END-OF-W2212D01                                        
038000                                                                          
038100       MOVE IN-IDLEVNR      TO SPAR-IDLEVNR                               
038200       MOVE IN-IDDC         TO SPAR-IDDC                                  
038300       MOVE IN-IDANSK       TO SPAR-IDANSK                                
038400                                                                          
038500       MOVE IN-IDLEVNR      TO W-IDLEVNR-MIN                              
038600                               W-IDLEVNR-MAX                              
038700       MOVE IN-IDDC         TO W-IDDC-KLEV-MIN                            
038800                               W-IDDC-KLEV-MAX                            
038900       MOVE IN-KDMAIL       TO W-KDMAIL-MIN                               
039000                               W-KDMAIL-MAX                               
039100       MOVE ZERO            TO W-IDATTENT                                 
039200                                                                          
039300       PERFORM IMS-GU-WDF1A1                                              
039400                                                                          
039500       IF SEGMENT-FINNS                                                   
039600         MOVE IN-IDANSK      TO W-IDPERSON                                
039700         PERFORM IMS-GU-WDP311                                            
039800         IF SEGMENT-FINNS                                                 
039900           IF PERS-IDMAIL = SPACE                                         
040000             MOVE PERS-IDNAMN     TO UTER-IDNAMN-ANSK                     
040100             MOVE SPACE           TO UTER-TENOTE                          
040200             MOVE 'ANSK MAIL ADDRESS MISSING ON 0801 '                    
040300                                  TO UTER-TENOTE                          
040400             PERFORM D-SKRIV-FELMEDDELANDE                                
040500             PERFORM S01-LAES-W2212D01                                    
040600                                                                          
040700             PERFORM UNTIL END-OF-W2212D01 OR                             
040800                     IN-IDLEVNR  NOT = SPAR-IDLEVNR  OR                   
040900                     IN-IDDC     NOT = SPAR-IDDC     OR                   
041000                     IN-IDANSK   NOT = SPAR-IDANSK                        
041100                                                                          
041200               MOVE PERS-IDNAMN     TO UTER-IDNAMN-ANSK                   
041300               MOVE SPACE           TO UTER-TENOTE                        
041400               MOVE 'ANSK MAIL ADDRESS MISSING ON 0801 '                  
041500                                    TO UTER-TENOTE                        
041600               PERFORM D-SKRIV-FELMEDDELANDE                              
041700                                                                          
041800               PERFORM S01-LAES-W2212D01                                  
041900             END-PERFORM                                                  
042000           ELSE                                                           
042100             PERFORM B-BEHANDLA                                           
042200             PERFORM C-SKAPA-MAIL                                         
042300           END-IF                                                         
042400         ELSE                                                             
042500           MOVE SPACE           TO UTER-IDNAMN-ANSK                       
042600           MOVE SPACE           TO UTER-TENOTE                            
042700           MOVE 'PURCHASE PLANNER ID MISSING ON 0801 '                    
042800                                TO UTER-TENOTE                            
042900           PERFORM D-SKRIV-FELMEDDELANDE                                  
043000           PERFORM S01-LAES-W2212D01                                      
043100                                                                          
043200           PERFORM UNTIL END-OF-W2212D01 OR                               
043300                   IN-IDLEVNR  NOT = SPAR-IDLEVNR  OR                     
043400                   IN-IDDC     NOT = SPAR-IDDC     OR                     
043500                   IN-IDANSK   NOT = SPAR-IDANSK                          
043600                                                                          
043700             MOVE SPACE           TO UTER-IDNAMN-ANSK                     
043800             MOVE SPACE           TO UTER-TENOTE                          
043900             MOVE 'PURCHASE PLANNER ID MISSING ON 0801 '                  
044000                                  TO UTER-TENOTE                          
044100             PERFORM D-SKRIV-FELMEDDELANDE                                
044200                                                                          
044300             PERFORM S01-LAES-W2212D01                                    
044400           END-PERFORM                                                    
044500         END-IF                                                           
044600       ELSE                                                               
044700*--- LEV.NR SAKNAS PÅ 2119 SKRIV 1 POST PER ARTIKEL ENLIGT PL             
044800         MOVE SPACE           TO UTER-IDNAMN-ANSK                         
044900         MOVE SPACE           TO UTER-TENOTE                              
045000         MOVE 'MFG MAIL STEERING MISSING ON 2119' TO UTER-TENOTE          
045100         PERFORM D-SKRIV-FELMEDDELANDE                                    
045200         PERFORM S01-LAES-W2212D01                                        
045300                                                                          
045400         PERFORM UNTIL END-OF-W2212D01 OR                                 
045500                 IN-IDLEVNR  NOT = SPAR-IDLEVNR  OR                       
045600                 IN-IDDC     NOT = SPAR-IDDC     OR                       
045700                 IN-IDANSK   NOT = SPAR-IDANSK                            
045800                                                                          
045900               MOVE SPACE           TO UTER-IDNAMN-ANSK                   
046000               MOVE SPACE           TO UTER-TENOTE                        
046100               MOVE 'MFG MAIL STEERING MISSING ON 2119'                   
046200                                    TO UTER-TENOTE                        
046300               PERFORM D-SKRIV-FELMEDDELANDE                              
046400                                                                          
046500            PERFORM S01-LAES-W2212D01                                     
046600         END-PERFORM                                                      
046700       END-IF                                                             
046800                                                                          
046900     END-PERFORM                                                          
047000                                                                          
047100     PERFORM Z-FINIT                                                      
047200                                                                          
047300     MOVE ZERO TO RETURN-CODE                                             
047400     GOBACK                                                               
047500     .                                                                    
047600     EJECT                                                                
047700 A-INIT SECTION.                                                          
047800                                                                          
047900     OPEN INPUT  W2212D01                                                 
048000                                                                          
048100     OPEN OUTPUT W2212F                                                   
048200                 W2212ER                                                  
048300                 W2212E                                                   
048400                                                                          
048500     ACCEPT DAGENS-DATUM   FROM DATE                                      
048600     MOVE 'AAMMDD'           TO DAT-KDDATFORM                             
048700     MOVE DAGENS-DATUM       TO DAT-I-TIDATUM                             
048800                                                                          
048900     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
049000                     DAT-O-TIDATUM DAT-KDSVAR                             
049100                                                                          
049200     IF DAT-KDSVAR-OK                                                     
049300        CONTINUE                                                          
049400     ELSE                                                                 
049500         MOVE  ' FEL FRÅN DATUMRUTIN WDATKONV '                           
049600                             TO FELTEXT                                   
049700         PERFORM S99-ABEND                                                
049800     END-IF                                                               
049900                                                                          
050000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
050100                                                                          
050200     MOVE LOW-VALUE   TO W-WDF1A1KY-MIN-X                                 
050300     MOVE HIGH-VALUE  TO W-WDF1A1KY-MAX-X                                 
050400     .                                                                    
050500     EJECT                                                                
050600 B-BEHANDLA  SECTION.                                                     
050700     MOVE 'B-BEHANDLA  '  TO CURRENT-SECTION                              
050800                                                                          
050900     PERFORM S02-RENSA-TABELL                                             
051000     MOVE +1  TO TAB-IX                                                   
051100     PERFORM UNTIL END-OF-W2212D01 OR                                     
051200           IN-IDLEVNR  NOT = SPAR-IDLEVNR  OR                             
051300           IN-IDDC     NOT = SPAR-IDDC     OR                             
051400           IN-IDANSK   NOT = SPAR-IDANSK                                  
051500                                                                          
051600        IF TAB-IX < TAB-IX-MAX                                            
051700          MOVE IN-IDARTNR     TO TAB-IDARTNR (TAB-IX)                     
051800          MOVE IN-TIAAMMDD    TO TAB-TIAAMMDD (TAB-IX)                    
051900          MOVE IN-KVAVROP     TO TAB-KVAVROP (TAB-IX)                     
052000          MOVE IN-KVAVIS      TO TAB-KVAVIS (TAB-IX)                      
052100          MOVE IN-DAREGDAT-9KOMPL TO                                      
052200                              TAB-DAREGDAT-9KOMPL(TAB-IX)                 
052300          MOVE IN-TIKLOCK-9KOMPL  TO                                      
052400                              TAB-TIKLOCK-9KOMPL(TAB-IX)                  
052500        ELSE                                                              
052600          MOVE PERS-IDNAMN     TO UTER-IDNAMN-ANSK                        
052700          MOVE SPACE           TO UTER-TENOTE                             
052800          MOVE 'TOO MANY ROWS FOR MAIL TABLE      '                       
052900                               TO UTER-TENOTE                             
053000          PERFORM D-SKRIV-FELMEDDELANDE                                   
053100        END-IF                                                            
053200                                                                          
053300        ADD +1  TO TAB-IX                                                 
053400                                                                          
053500        PERFORM S01-LAES-W2212D01                                         
053600     END-PERFORM                                                          
053700                                                                          
053800     .                                                                    
053900     EJECT                                                                
054000 C-SKAPA-MAIL  SECTION.                                                   
054100     MOVE 'C-SKAPA-MAIL  '  TO CURRENT-SECTION                            
054200                                                                          
054300     PERFORM IMS-GU-WDF1A1                                                
054400                                                                          
054500     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
054600       MOVE SEQA-IDLEVNR     TO W-IDLEVNR                                 
054700       MOVE SEQA-IDATTENT    TO W-IDATTENT                                
054800       PERFORM IMS-GU-WDF107                                              
054900       IF SEGMENT-FINNS                                                   
055000         IF ATT-IDMAIL NOT = SPACE                                        
055100           PERFORM CA-SKAPA-MAIL-DEST                                     
055200           PERFORM CB-SKRIV-RUBRIKER                                      
055300           PERFORM CC-SKRIV-MAIL-LISTA                                    
055400           PERFORM CD-SKRIV-AVSLUT-MAIL                                   
055500         ELSE                                                             
055600           MOVE SPACE           TO UTER-TENOTE                            
055700           MOVE 'MFG MAIL ADDRESS MISSING '  TO UTER-TENOTE               
055800           PERFORM S03-SKRIV-FEL-POSTER                                   
055900         END-IF                                                           
056000       ELSE                                                               
056100         MOVE SPACE   TO UTER-TENOTE                                      
056200         MOVE 'ATT.NO MISSING ON 2115  '  TO UTER-TENOTE                  
056300         PERFORM S03-SKRIV-FEL-POSTER                                     
056400       END-IF                                                             
056500                                                                          
056600       PERFORM IMS-GN-WDF1A1                                              
056700     END-PERFORM                                                          
056800                                                                          
056900     .                                                                    
057000     EJECT                                                                
057100 CA-SKAPA-MAIL-DEST  SECTION.                                             
057200     MOVE 'CA-SKAPA-MAIL-DEST '  TO CURRENT-SECTION                       
057300                                                                          
057400     WRITE EMAIL-RECORD  FROM MAIL-001                                    
057500                                                                          
057510*Supplier info                                                            
057600     MOVE SEQA-IDLEVNR   TO MAIL-IDLEVNR-MFG-SUBJECT                      
057700*                                                                         
057800     WRITE EMAIL-RECORD  FROM MAIL-002                                    
057900                                                                          
058000     MOVE SPACE          TO MAIL-IDMAIL-MFG                               
058100     MOVE ATT-IDMAIL     TO MAIL-IDMAIL-MFG                               
058200                                                                          
058300     WRITE EMAIL-RECORD  FROM MAIL-003                                    
058400                                                                          
058500     MOVE SPACE          TO MAIL-IDMAIL-ANSK-CC                           
058600     MOVE PERS-IDMAIL    TO MAIL-IDMAIL-ANSK-CC                           
058700     WRITE EMAIL-RECORD  FROM MAIL-004A                                   
058800                                                                          
058900     MOVE SPACE          TO MAIL-IDMAIL-ANSK                              
059000     MOVE PERS-IDMAIL    TO MAIL-IDMAIL-ANSK                              
059100     WRITE EMAIL-RECORD  FROM MAIL-004                                    
059200                                                                          
059300     WRITE EMAIL-RECORD  FROM MAIL-005                                    
059400                                                                          
059500     .                                                                    
059600     EJECT                                                                
059700 CB-SKRIV-RUBRIKER  SECTION.                                              
059800     MOVE 'CB-SKRIV-RUBRIKER '  TO CURRENT-SECTION                        
059900                                                                          
060000     WRITE EMAIL-RECORD  FROM MAIL-HDR-1                                  
060010     WRITE EMAIL-RECORD  FROM MAIL-HDR-2                                  
060100                                                                          
060200     WRITE EMAIL-RECORD  FROM MAIL-007                                    
060300                                                                          
060310     WRITE EMAIL-RECORD  FROM MAIL-HDR-3                                  
060320     WRITE EMAIL-RECORD  FROM MAIL-HDR-4                                  
060330                                                                          
060340     WRITE EMAIL-RECORD  FROM MAIL-007                                    
060380                                                                          
060390     WRITE EMAIL-RECORD  FROM MAIL-HDR-5                                  
060391     WRITE EMAIL-RECORD  FROM MAIL-HDR-6                                  
060392     WRITE EMAIL-RECORD  FROM MAIL-HDR-6A                                 
060393                                                                          
060394     WRITE EMAIL-RECORD  FROM MAIL-007                                    
060395                                                                          
060400     WRITE EMAIL-RECORD  FROM MAIL-HDR-7                                  
060410     WRITE EMAIL-RECORD  FROM MAIL-HDR-8                                  
060500                                                                          
060600     .                                                                    
060700     EJECT                                                                
060800 CC-SKRIV-MAIL-LISTA  SECTION.                                            
060900     MOVE 'CC-SKRIV-MAIL-LISTA '  TO CURRENT-SECTION                      
061000*------------------------------------------------------                   
061100*--  UPPDATERA WDD925 OCH WDD4 FÖR VARJE ARTIKEL I MAIL                   
061200*------------------------------------------------------                   
061300                                                                          
061301*Supplier info                                                            
061302     MOVE SEQA-IDLEVNR   TO MAIL-IDLEVNR-MFG                              
061310*To Fetch the Buyer details                                               
061320     MOVE SEQA-IDDC-KLEV    TO W-IDDC-B6                                  
061330     PERFORM IMS-GU-WDB601                                                
061340     IF SEGMENT-FINNS                                                     
061350       IF W-IDDC-B6 = WC-CDC-SE                                           
061360         MOVE SPACE             TO MAIL-IDLEVNR-DC                        
061370         MOVE 'BP2TW'           TO MAIL-IDLEVNR-DC                        
061380       ELSE                                                               
061390         MOVE SPACE             TO MAIL-IDLEVNR-DC                        
061391         MOVE DCS-IDLEVNR-DC    TO MAIL-IDLEVNR-DC                        
061392       END-IF                                                             
061393     ELSE                                                                 
061394       MOVE SPACE   TO MAIL-ERR-TEXT                                      
061395       MOVE 'Address missing for partner code' TO MAIL-ERR-TEXT           
061396       WRITE EMAIL-RECORD  FROM MAIL-ERR-TEXT                             
061397     END-IF                                                               
061398*                                                                         
061400     MOVE +1  TO TAB-IX                                                   
061500     PERFORM UNTIL TAB-IX > TAB-IX-MAX                                    
061600       IF TAB-IDARTNR(TAB-IX) = ZERO                                      
061700         MOVE TAB-IX-MAX  TO TAB-IX                                       
061800       ELSE                                                               
061900         MOVE TAB-IDARTNR(TAB-IX)  TO MAIL-IDARTNR                        
062000         MOVE TAB-TIAAMMDD(TAB-IX) TO MAIL-TIAAMMDD                       
062100         MOVE TAB-KVAVROP(TAB-IX)  TO MAIL-KVAVROP                        
062200         MOVE TAB-KVAVIS(TAB-IX)   TO MAIL-KVAVIS                         
062300                                                                          
062400         WRITE EMAIL-RECORD  FROM MAIL-DATA                               
062500         PERFORM S13-SKRIV-W2212E                                         
062600                                                                          
062700         MOVE SPAR-IDLEVNR         TO UT2F-IDLEVNR                        
062800         MOVE SPAR-IDDC            TO UT2F-IDDC                           
062900         MOVE TAB-IDARTNR(TAB-IX)  TO UT2F-IDARTNR                        
063000         MOVE TAB-DAREGDAT-9KOMPL(TAB-IX)  TO UT2F-DAREGDAT-9KOMPL        
063100         MOVE TAB-TIKLOCK-9KOMPL(TAB-IX)   TO UT2F-TIKLOCK-9KOMPL         
063200                                                                          
063300         PERFORM S11-SKRIV-W2212F                                         
063400                                                                          
063500       END-IF                                                             
063600       ADD +1  TO TAB-IX                                                  
063700     END-PERFORM                                                          
063800                                                                          
063900     .                                                                    
064000     EJECT                                                                
064100 CD-SKRIV-AVSLUT-MAIL SECTION.                                            
064200     MOVE 'CD-SKRIV-AVSLUT-MAIL ' TO CURRENT-SECTION                      
064300                                                                          
064400     WRITE EMAIL-RECORD  FROM MAIL-007                                    
064500                                                                          
064900                                                                          
065220     WRITE EMAIL-RECORD  FROM MAIL-HDR-9                                  
065300                                                                          
065400     MOVE SPACE             TO MAIL-IDNAMN                                
065500     MOVE PERS-IDNAMN       TO MAIL-IDNAMN                                
065800     WRITE EMAIL-RECORD  FROM MAIL-HDR-9A                                 
065700                                                                          
066000     MOVE SPACE             TO MAIL-IDMAIL                                
066100     MOVE PERS-IDMAIL       TO MAIL-IDMAIL                                
066200     WRITE EMAIL-RECORD  FROM MAIL-HDR-9B                                 
065900                                                                          
066000     MOVE SPACE             TO MAIL-IDTFN                                 
066100     MOVE PERS-IDTFN        TO MAIL-IDTFN                                 
066330     WRITE EMAIL-RECORD  FROM MAIL-HDR-9C                                 
066300                                                                          
066400     WRITE EMAIL-RECORD  FROM MAIL-007                                    
066500                                                                          
070300                                                                          
070400     WRITE EMAIL-RECORD  FROM MAIL-006                                    
070500     .                                                                    
070600     EJECT                                                                
070700 D-SKRIV-FELMEDDELANDE  SECTION.                                          
070800     MOVE 'D-SKRIV-FELMEDDELANDE '  TO CURRENT-SECTION                    
070900                                                                          
071000     MOVE IN-IDARTNR      TO UTER-IDARTNR                                 
071100     MOVE IN-IDLEVNR      TO UTER-IDLEVNR                                 
071200     MOVE IN-IDDC         TO UTER-IDDC                                    
071300     MOVE IN-KDMAIL       TO UTER-KDMAIL                                  
071400     MOVE W-IDATTENT      TO UTER-IDATTENT                                
071500     MOVE IN-IDANSK       TO UTER-IDANSK                                  
071600                                                                          
071700     PERFORM S12-SKRIV-W2212ER                                            
071800     .                                                                    
071900     EJECT                                                                
072000 Z-FINIT SECTION.                                                         
072100                                                                          
072200     CLOSE W2212D01                                                       
072300           W2212F                                                         
072400           W2212ER                                                        
072500           W2212E                                                         
072600     SKIP2                                                                
072700     MOVE 'S' TO POSTSUM-OPKOD                                            
072800     CALL POSTSUM USING POSTSUM-PARM                                      
072900     .                                                                    
073000     EJECT                                                                
073100 S01-LAES-W2212D01  SECTION.                                              
073200                                                                          
073300     READ W2212D01 INTO IN-AREA                                           
073400     AT END                                                               
073500        MOVE HIGH-VALUE TO IN-AREA                                        
073600        SET END-OF-W2212D01 TO TRUE                                       
073700                                                                          
073800     NOT AT END                                                           
073900        MOVE 'W2212D'   TO POSTSUM-FDNAMN                                 
074000        MOVE 'W2212ED1' TO POSTSUM-DDNAMN2                                
074100        MOVE 'IN- '     TO POSTSUM-TRANSTYP                               
074200        CALL POSTSUM USING POSTSUM-PARM                                   
074300     END-READ                                                             
074400     .                                                                    
074500     EJECT                                                                
074600 S02-RENSA-TABELL  SECTION.                                               
074700     MOVE 'S02-RENSA-TABELL  '  TO CURRENT-SECTION                        
074800                                                                          
074900     MOVE +1  TO TAB-IX                                                   
075000     PERFORM UNTIL TAB-IX > TAB-IX-MAX                                    
075100       MOVE +0   TO TAB-IDARTNR(TAB-IX)                                   
075200                    TAB-TIAAMMDD(TAB-IX)                                  
075300                    TAB-KVAVROP(TAB-IX)                                   
075400                    TAB-DAREGDAT-9KOMPL(TAB-IX)                           
075500                    TAB-TIKLOCK-9KOMPL(TAB-IX)                            
075600                                                                          
075700       ADD +1    TO TAB-IX                                                
075800     END-PERFORM                                                          
075900     .                                                                    
076000     EJECT                                                                
076100 S03-SKRIV-FEL-POSTER  SECTION.                                           
076200     MOVE 'S03-SKRIV-FEL-POSTER '  TO CURRENT-SECTION                     
076300                                                                          
076400     MOVE +1  TO TAB-IX                                                   
076500     PERFORM UNTIL TAB-IX > TAB-IX-MAX                                    
076600       IF TAB-IDARTNR(TAB-IX) = ZERO                                      
076700         MOVE TAB-IX-MAX  TO TAB-IX                                       
076800       ELSE                                                               
076900         MOVE TAB-IDARTNR(TAB-IX)  TO UTER-IDARTNR                        
077000         MOVE SPAR-IDLEVNR         TO UTER-IDLEVNR                        
077100         MOVE SPAR-IDDC            TO UTER-IDDC                           
077200         MOVE W-KDMAIL-MIN         TO UTER-KDMAIL                         
077300         MOVE W-IDATTENT           TO UTER-IDATTENT                       
077400         MOVE SPAR-IDANSK          TO UTER-IDANSK                         
077500         MOVE PERS-IDNAMN          TO UTER-IDNAMN-ANSK                    
077600                                                                          
077700         PERFORM S12-SKRIV-W2212ER                                        
077800       END-IF                                                             
077900       ADD +1  TO TAB-IX                                                  
078000     END-PERFORM                                                          
078100     .                                                                    
078200     EJECT                                                                
078300 S11-SKRIV-W2212F SECTION.                                                
078400                                                                          
078500     WRITE UT2F-POST FROM UT2F-AREA                                       
078600                                                                          
078700     MOVE 'UPD '     TO POSTSUM-TRANSTYP                                  
078800     MOVE 'W2212F'   TO POSTSUM-FDNAMN                                    
078900     MOVE 'W2212ED2' TO POSTSUM-DDNAMN2                                   
079000     CALL POSTSUM USING POSTSUM-PARM                                      
079100     .                                                                    
079200     EJECT                                                                
079300 S12-SKRIV-W2212ER SECTION.                                               
079400                                                                          
079500     WRITE UTER-POST FROM UTER-AREA                                       
079600                                                                          
079700     MOVE 'ERR '     TO POSTSUM-TRANSTYP                                  
079800     MOVE 'W2212E'   TO POSTSUM-FDNAMN                                    
079900     MOVE 'W2212ED3' TO POSTSUM-DDNAMN2                                   
080000     CALL POSTSUM USING POSTSUM-PARM                                      
080100     .                                                                    
080200     EJECT                                                                
080300 S13-SKRIV-W2212E SECTION.                                                
080400                                                                          
080500     MOVE 'MAIL'     TO POSTSUM-TRANSTYP                                  
080600     MOVE 'W2212E'   TO POSTSUM-FDNAMN                                    
080700     MOVE 'W2212ED3' TO POSTSUM-DDNAMN2                                   
080800     CALL POSTSUM USING POSTSUM-PARM                                      
080900     .                                                                    
081000     EJECT                                                                
081100 S99-ABEND SECTION.                                                       
081200                                                                          
081300     SKIP2                                                                
081400     MOVE 'S' TO POSTSUM-OPKOD                                            
081500     CALL POSTSUM USING POSTSUM-PARM                                      
081600     CALL ABEND USING RKOD-ABEND                                          
081700     .                                                                    
081800     EJECT                                                                
081900* --- IMS SEKTIONER ---                                                   
082000                                                                          
082100     EJECT                                                                
082200 IMS-GU-WDF1A1  SECTION.                                                  
082300     MOVE 'IMS-GU-WDF1A1 '  TO DBS-SECTION                                
082400                                                                          
082500     MOVE SPACE  TO SSA1                                                  
082600     STRING 'WDF1A1  (WDF1A1KY>=' W-WDF1A1KY-MIN-X                        
082700                    '&WDF1A1KY<=' W-WDF1A1KY-MAX-X ')'                    
082800          DELIMITED BY SIZE INTO SSA1                                     
082900     MOVE '  GE' TO GODK-STATUSKODER                                      
083000     CALL CBLTDLI USING GU WDF1A-PCB DLI-IO-WDF1A1 SSA1                   
083100     MOVE WDF1A-STATUS-CODE TO STATUS-WS                                  
083200     PERFORM IMS-STATUSKONTROLL                                           
083300     .                                                                    
083400     SKIP3                                                                
083500 IMS-GN-WDF1A1  SECTION.                                                  
083600     MOVE 'IMS-GN-WDF1A1 '  TO DBS-SECTION                                
083700                                                                          
083800     MOVE SPACE  TO SSA1                                                  
083900     STRING 'WDF1A1  (WDF1A1KY>=' W-WDF1A1KY-MIN-X                        
084000                    '&WDF1A1KY<=' W-WDF1A1KY-MAX-X ')'                    
084100          DELIMITED BY SIZE INTO SSA1                                     
084200     MOVE '  GEGB' TO GODK-STATUSKODER                                    
084300     CALL CBLTDLI USING GN WDF1A-PCB DLI-IO-WDF1A1 SSA1                   
084400     MOVE WDF1A-STATUS-CODE TO STATUS-WS                                  
084500     PERFORM IMS-STATUSKONTROLL                                           
084600     .                                                                    
084700     SKIP3                                                                
084800 IMS-GU-WDF107 SECTION.                                                   
084900     MOVE 'IMS-GU-WDF107 '  TO DBS-SECTION                                
085000                                                                          
085100     MOVE SPACE  TO SSA1 SSA2                                             
085200     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
085300          DELIMITED BY SIZE INTO SSA1                                     
085400     STRING 'WDF107  (IDATTENT =' W-IDATTENT-X ')'                        
085500          DELIMITED BY SIZE INTO SSA2                                     
085600     MOVE '  GE' TO GODK-STATUSKODER                                      
085700     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-WDF107 SSA1 SSA2               
085800     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
085900     PERFORM IMS-STATUSKONTROLL                                           
086000     .                                                                    
086100     EJECT                                                                
086200 IMS-GU-WDP311 SECTION.                                                   
086300     MOVE 'IMS-GU-WDP311 '  TO DBS-SECTION                                
086400                                                                          
086500     MOVE SPACE  TO SSA1 SSA2                                             
086600     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
086700          DELIMITED BY SIZE INTO SSA1                                     
086800     STRING 'WDP311  (IDPERSON =' W-IDPERSON-X ')'                        
086900          DELIMITED BY SIZE INTO SSA2                                     
087000     MOVE '  GE' TO GODK-STATUSKODER                                      
087100     CALL CBLTDLI USING GU WDP3-PCB DLI-IO-WDP311 SSA1 SSA2               
087200     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
087300     PERFORM IMS-STATUSKONTROLL                                           
087400     .                                                                    
087500     EJECT                                                                
087600 IMS-GU-WDB601 SECTION.                                                   
087700     MOVE 'IMS-GU-WDB601 '  TO DBS-SECTION                                
087800                                                                          
087900     MOVE SPACE  TO SSA1                                                  
088000     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
088100          DELIMITED BY SIZE INTO SSA1                                     
088200     MOVE '  GE' TO GODK-STATUSKODER                                      
088300     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
088400     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
088500     PERFORM IMS-STATUSKONTROLL                                           
088600     .                                                                    
088700     EJECT                                                                
088800 IMS-STATUSKONTROLL SECTION.                                              
088900                                                                          
089000     SET STATUS-IX TO 1                                                   
089100     SEARCH GODK-STATUS                                                   
089200       AT END                                                             
089300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
089400           DELIMITED BY SIZE INTO FELTEXT                                 
089500         DISPLAY FELTEXT                                                  
089600         CALL FELLOG                                                      
089700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
089800         CONTINUE                                                         
089900     END-SEARCH                                                           
090000     .                                                                    
