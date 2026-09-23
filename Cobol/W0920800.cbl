000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W0920800.                                                
000300 AUTHOR.         IDK, GÖTEBORG.                                           
000400 DATE-WRITTEN.   MARS 1979.                                               
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION.                                                            
000900*        LÄSER WDG6 OCH SKAPAR:                                           
001000*        FIL    W09290 MED POSTTYP 092 (101 - 138)                        
001100*        FIL    W09291 MED POSTTYP 221 (310 - 340, 400)                   
001200*        FIL    W09248 MED POSTTYP R48,                                   
001300*        FIL    W092Z3 MED POSTTYP 433                                    
001400*        FIL    W092Z4 MED POSTTYP 432                                    
001500*        FIL    W092Z5 MED POSTTYP 229                                    
001600*        FIL    W092Z7 MED POSTTYP SU  TILL VR                            
001700*        FIL    W092ZF MED POSTTYP 001 NOAC                               
001800*        FIL    W092ZI MED POSTTYP 001,002                                
001900*        FIL    W092ZN MED POSTTYP 666, 667                               
002000*        FIL    W092ZR INKÖPSPOST PV                                      
002100*        FIL    W092ZU MED KDP-POSTER POSTTYP RZU                         
002200*        FIL    W092ZV MED POSTTYP RZV                                    
002300*        FIL    W092ZW MED POSTTYP RY6 PACKADE SV KLASS 4                 
002400*        FIL    W092X3 MED POSTTYP 015 NOAC  BYPASS                       
002500*        FIL    W092X4 MED POSTTYP 020 NOAC  ON-ORDER                     
002600*        FIL    W092X9 MED INFO OM BEHANDLADE SEGMENT                     
002700*        FIL    W092XC MED POSTTYP 201, 203, 204                          
002800*        FIL    W092PP MED POSTTYP RPP                                    
002900*        FIL    W092S1 INNEHÅLLER WDGZ01-POSTENS SRS-INFO                 
003000*        FIL    W092XX INNEHÅLLER WDGZ01-POSTENS RYX-INFO                 
003100*                      TILLFÄLLIG FIL TILL LOGISTIK                       
003200*                                                                         
003300*    EJECT                                                                
003400*       I N D A T A                                                       
003500*                                                                         
003600*       WDG6                                                              
003700*                                                                         
003800*                                                                         
003900*    SKIP3                                                                
004000*         U T D A T A                                                     
004100*                                                                         
004200*    *****************           **************  *********                
004300*    INNEHÅLL I FIL               POSTTYP        COPYTEXT                 
004400*    *****************           **************  *********                
004500*    FELPOSTER                   101-138         W211FEL                  
004600*    ANSKAFFNINGSTRANS.          310-340,400     W211310                  
004700*    EKONOMITRANS.               933             W511933                  
004800*                                934             W511934                  
004900*                                935,936,937     W511935                  
005000*                                940             W511940                  
005100*                                941             W511941                  
005200*    GODSAVISERING.              R31             W211R31                  
005300*    GODSINLÄGGNING.             R32             W211R32                  
005400*    FAKTURERINGSTRANSAR.        R48             W413PR48                 
005500*    KDP-TRANS                   RZU             W092ZU                   
005600*    CROSSINDEX                  RZV             W111RZV                  
005700*    POST TILL INKÖP  PV         RY2             A310TB65                 
005800*    FÄRDIGPACKAD SV KLASS 4     RY6             WDGZRY6                  
005900*    RO FÖRÄNDRADE VIA IMS BILD  RY9             WDGZRY9                  
006000*    POST TILL BORS RO ANN./DEL- 201             W020201                  
006100*    POST TILL BORS 'NY' RO      203             W020203                  
006200*    POST TILL BORS BIP RO       204             W020204                  
006300     EJECT                                                                
006400 ENVIRONMENT DIVISION.                                                    
006500 INPUT-OUTPUT SECTION.                                                    
006600 FILE-CONTROL.                                                            
006700     SKIP2                                                                
006800     SELECT  W09299        ASSIGN TO UT-S-W0920899.                       
006900     SELECT  W09290-FEL    ASSIGN TO UT-S-W09208D2.                       
007000     SELECT  W09291-ANSK   ASSIGN TO UT-S-W09208D3.                       
007100     SELECT  W092Z3        ASSIGN TO UT-S-W09208DA.                       
007200     SELECT  W092Z4        ASSIGN TO UT-S-W09208DB.                       
007300     SELECT  W092Z5        ASSIGN TO UT-S-W09208DC.                       
007400     SELECT  W092Z7        ASSIGN TO UT-S-W09208DE.                       
007500     SELECT  W092ZF        ASSIGN TO UT-S-W09208DM.                       
007600     SELECT  W092ZI        ASSIGN TO YT-S-W09208DP.                       
007700     SELECT  W092ZN        ASSIGN TO UT-S-W09208DU.                       
007800     SELECT  W092ZP        ASSIGN TO UT-S-W09208E1.                       
007900     SELECT  W092ZQ        ASSIGN TO UT-S-W09208E2.                       
008000     SELECT  W092ZR        ASSIGN TO UT-S-W09208E3.                       
008100     SELECT  W092ZU        ASSIGN TO UT-S-W09208E6.                       
008200     SELECT  W092ZV        ASSIGN TO UT-S-W09208E7.                       
008300     SELECT  W092ZW        ASSIGN TO UT-S-W09208EA.                       
008400     SELECT  W092X3        ASSIGN TO UT-S-W09208EE.                       
008500     SELECT  W092X4        ASSIGN TO UT-S-W09208EF.                       
008600     SELECT  W092X9        ASSIGN TO UT-S-W09208EL.                       
008700     SELECT  W092XC        ASSIGN TO UT-S-W09208EO.                       
008800     SELECT  W092S1        ASSIGN TO UT-S-W09208EU.                       
008900     SELECT  W092XX        ASSIGN TO UT-S-W09208XX.                       
009000     EJECT                                                                
009100 DATA DIVISION.                                                           
009200                                                                          
009300 FILE SECTION.                                                            
009400                                                                          
009500 FD  W09299                                                               
009600     BLOCK CONTAINS 0 RECORDS                                             
009700     LABEL RECORDS STANDARD                                               
009800     RECORDING F.                                                         
009900                                                                          
010000 01  FELTRANS                    PIC X(200).                              
010100                                                                          
010200     EJECT                                                                
010300                                                                          
010400 FD  W09290-FEL                                                           
010500     BLOCK CONTAINS 0 RECORDS                                             
010600     LABEL RECORDS STANDARD                                               
010700     RECORDING V.                                                         
010800                                                                          
010900*01  UTPOST  -COPY W211FEL -L -PRE W092-                                  
011000     EJECT                                                                
011100                                                                          
011200 FD  W092ZU                                                               
011300     BLOCK CONTAINS 0 RECORDS                                             
011400     LABEL RECORDS STANDARD                                               
011500     RECORDING F.                                                         
011600                                                                          
011700*01  UTPOST  -COPY W092ZU      -L -PRE KDP-                               
011800     SKIP3                                                                
011900                                                                          
012000 FD  W092ZV                                                               
012100     BLOCK CONTAINS 0 RECORDS                                             
012200     LABEL RECORDS STANDARD                                               
012300     RECORDING F.                                                         
012400                                                                          
012500*01  UTPOST  -COPY W111RZV  -PRE RZV- -L                                  
012600     SKIP3                                                                
012700                                                                          
012800 FD  W092ZR                                                               
012900     BLOCK CONTAINS 0 RECORDS                                             
013000     LABEL RECORDS STANDARD                                               
013100     RECORDING F.                                                         
013200                                                                          
013300*01  UTPOST  -COPY A310TB65 -PRE UTRY2-  -L                               
013400     EJECT                                                                
013500                                                                          
013600 FD  W09291-ANSK                                                          
013700     BLOCK CONTAINS 0 RECORDS                                             
013800     LABEL RECORDS STANDARD                                               
013900     RECORDING F.                                                         
014000                                                                          
014100*01  UTPOST  -COPY W211310  -PRE X221- -L                                 
014200     EJECT                                                                
014300                                                                          
014400 FD  W092Z3                                                               
014500     BLOCK CONTAINS 0 RECORDS                                             
014600     LABEL RECORDS STANDARD                                               
014700     RECORDING MODE F.                                                    
014800                                                                          
014900*01  POST -COPY W430432   -PRE W092Z3- -L                                 
015000     EJECT                                                                
015100                                                                          
015200 FD  W092Z4                                                               
015300     BLOCK CONTAINS 0 RECORDS                                             
015400     LABEL RECORDS STANDARD                                               
015500     RECORDING MODE F.                                                    
015600                                                                          
015700*01  POST -COPY W430432   -PRE W092Z4- -L                                 
015800     SKIP3                                                                
015900                                                                          
016000 FD  W092Z5                                                               
016100     BLOCK CONTAINS 0 RECORDS                                             
016200     LABEL RECORDS STANDARD                                               
016300     RECORDING MODE F.                                                    
016400                                                                          
016500*01  POST -COPY W231229   -PRE W092Z5- -L                                 
016600     EJECT                                                                
016700                                                                          
016800 FD  W092Z7                                                               
016900     BLOCK CONTAINS 0 RECORDS                                             
017000     LABEL RECORDS STANDARD                                               
017100     RECORDING MODE V.                                                    
017200                                                                          
017300*01  POST -COPY W425SU2    -PRE W092Z7- -L                                
017400     EJECT                                                                
017500                                                                          
017600 FD  W092ZF                                                               
017700     BLOCK CONTAINS 0 RECORDS                                             
017800     LABEL RECORDS STANDARD                                               
017900     RECORDING MODE F.                                                    
018000                                                                          
018100*01  UTPOST -COPY W461001   -PRE 001- -L                                  
018200                                                                          
018300     EJECT                                                                
018400                                                                          
018500 FD  W092ZI                                                               
018600     BLOCK CONTAINS 0 RECORDS                                             
018700     LABEL RECORDS STANDARD                                               
018800     RECORDING MODE F.                                                    
018900                                                                          
019000*01  UTPOST -COPY WDGZRZQ   -PRE W001- -L                                 
019100                                                                          
019200*01  UTPOST -COPY WDGZRZR   -PRE R002- -L                                 
019300                                                                          
019400     SKIP3                                                                
019500                                                                          
019600 FD  W092ZN                                                               
019700     BLOCK CONTAINS 0 RECORDS                                             
019800     LABEL RECORDS STANDARD                                               
019900     RECORDING MODE V.                                                    
020000                                                                          
020100*01  POST -COPY W4150354  -PRE W092ZN- -L                                 
020200     EJECT                                                                
020300                                                                          
020400 FD  W092ZP                                                               
020500     BLOCK CONTAINS 0 RECORDS                                             
020600     LABEL RECORDS STANDARD                                               
020700     RECORDING MODE F.                                                    
020800                                                                          
020900*01  UTPOST -COPY W461009  -PRE 009- -L                                   
021000     EJECT                                                                
021100                                                                          
021200 FD  W092ZQ                                                               
021300     BLOCK CONTAINS 0 RECORDS                                             
021400     LABEL RECORDS STANDARD                                               
021500     RECORDING MODE V.                                                    
021600                                                                          
021700*01  UTPOST -COPY W461002  -PRE 002- -L                                   
021800                                                                          
021900*01  UTPOST -COPY W461003  -PRE 003- -L                                   
022000                                                                          
022100*01  UTPOST -COPY W461004  -PRE 004- -L                                   
022200                                                                          
022300*01  UTPOST -COPY W461005  -PRE 005- -L                                   
022400                                                                          
022500*01  UTPOST -COPY W461006  -PRE 006- -L                                   
022600                                                                          
022700*01  UTPOST -COPY W461007  -PRE 007- -L                                   
022800                                                                          
022900*01  UTPOST -COPY W461008  -PRE 008- -L                                   
023000     EJECT                                                                
023100                                                                          
023200 FD  W092ZW                                                               
023300     BLOCK CONTAINS 0 RECORDS                                             
023400     LABEL RECORDS STANDARD                                               
023500     RECORDING MODE F.                                                    
023600                                                                          
023700*01  UTRY6-UTPOST -COPY WDGZRY6       -L                                  
023800     SKIP3                                                                
023900                                                                          
024000 FD  W092X3                                                               
024100     BLOCK CONTAINS 0 RECORDS                                             
024200     LABEL RECORD STANDARD                                                
024300     RECORDING F.                                                         
024400                                                                          
024500*01  POST  -COPY W461015     -PRE W092X3- -L                              
024600     EJECT                                                                
024700                                                                          
024800 FD  W092X4                                                               
024900     BLOCK CONTAINS 0 RECORDS                                             
025000     LABEL RECORD STANDARD                                                
025100     RECORDING F.                                                         
025200                                                                          
025300*01  POST  -COPY W461020     -PRE W092X4- -L                              
025400     EJECT                                                                
025500                                                                          
025600 FD  W092X9                                                               
025700     BLOCK CONTAINS 0 RECORDS                                             
025800     LABEL RECORD STANDARD                                                
025900     RECORDING F.                                                         
026000                                                                          
026100*01  POST  -COPY W092X9   -PRE W092X9- -L                                 
026200     EJECT                                                                
026300                                                                          
026400 FD  W092XC                                                               
026500     RECORDING V                                                          
026600     BLOCK 0                                                              
026700     LABEL RECORD STANDARD.                                               
026800                                                                          
026900*01  W092XC-POST -COPY  W020203       -L                                  
027000     EJECT                                                                
027100                                                                          
027200 FD  W092S1                                                               
027300     BLOCK CONTAINS 0 RECORDS                                             
027400     LABEL RECORDS STANDARD                                               
027500     RECORDING MODE F.                                                    
027600                                                                          
027700*01  POST -COPY WDGZ01    -PRE W092S1- -L                                 
027800     EJECT                                                                
027900                                                                          
028000 FD  W092XX                                                               
028100     BLOCK CONTAINS 0 RECORDS                                             
028200     LABEL RECORDS STANDARD                                               
028300     RECORDING MODE F.                                                    
028400                                                                          
028500*01  POST -COPY WDGZ01    -PRE W092XX- -L                                 
028600     EJECT                                                                
028700 WORKING-STORAGE SECTION.                                                 
028800                                                                          
028900*    -- CHECKED BY WY2000                                                 
029000     SKIP3                                                                
029100*    -- CHDCKED BY WY2000                                                 
029200     SKIP3                                                                
029300 77  PROGRAM-NAMN              PIC X(8) VALUE 'W0920800'.                 
029400 77  JA                        PIC X   VALUE 'J'.                         
029500 77  NEJ                       PIC X   VALUE 'N'.                         
029600                                                                          
029700 01  WS-FAELT.                                                            
029800     03  ARTNR-CL-WS           PIC 9(10).                                 
029900     03  FILLER                REDEFINES ARTNR-CL-WS.                     
030000         05  IDARTNR-WS        PIC 9(9).                                  
030100         05  KDCLAGER-WS       PIC 9(1).                                  
030200     03  PRARTNTO-WS-X.                                                   
030300         05  PRARTNTO-WS       PIC S9(7)V9(2).                            
030400     SKIP3                                                                
030500 01  W-IDKUNDRF.                                                          
030600     03  W-IDKUNDRF-ONR        PIC 9(5).                                  
030700     03  FILLER                PIC X(5).                                  
030800     SKIP3                                                                
030900 01  W-TIAAP-AVBOK             PIC 9(3).                                  
031000 01  FILLER                    REDEFINES W-TIAAP-AVBOK.                   
031100     03  W-TIAAP-AA            PIC 9(2).                                  
031200     03  W-TIAAP-P             PIC 9(1).                                  
031300     SKIP1                                                                
031400 01  WS-IDDISTR                PIC 9(4)   VALUE ZERO.                     
031500 01  WS-IDKUNDNR               PIC 9(6)   VALUE ZERO.                     
031600                                                                          
031700 01  W-TIAVBPER                PIC 9(1).                                  
031800 01  W-RYE-IDORDNR             PIC 9(7).                                  
031900 77  IDSUPPL-SU-WS             PIC 9(5).                                  
032000 77  IDDISTR-SU-WS             PIC 9(4).                                  
032100 77  W-SU-IDDC                 PIC X(2).                                  
032200 77  SKRIV-RY1-SW              PIC X(1)    VALUE 'N'.                     
032300     88  SKRIV-RY1                         VALUE 'J'.                     
032400 01  W-IDDC-B6-X.                                                         
032500     03  W-IDDC-B6             PIC X(2)   VALUE SPACE.                    
032600     SKIP3                                                                
032700*      --- VALID IDDC CODES                                               
032800*                                                                         
032900*01    -COPY WWDC99                                                       
033000*01    -COPY WWDCKONS                                                     
033100       EJECT                                                              
033200 01  DAT.                                                                 
033300     03  DAT-AAMMDD            PIC 9(6).                                  
033400     03  DAT-AAMMDD-X          REDEFINES DAT-AAMMDD.                      
033500         05  DAT-AAMMDD-AA     PIC 9(2).                                  
033600         05  DAT-AAMMDD-MM     PIC 9(2).                                  
033700         05  DAT-AAMMDD-DD     PIC 9(2).                                  
033800     SKIP1                                                                
033900 01  DATUM-FAELT.                                                         
034000     03  KONV-AAMMDD           PIC 9(6).                                  
034100     03  KONV-AAVVD            PIC 9(5).                                  
034200     SKIP2                                                                
034300 01  DAGENS-DATUM              PIC 9(6).                                  
034400                                                                          
034500 01  WDAT-TIAAVV               PIC 9(4).                                  
034600 01  FILLER REDEFINES WDAT-TIAAVV.                                        
034700     03  WDAT-TIAA             PIC 9(2).                                  
034800     03  WDAT-TIVV             PIC 9(2).                                  
034900     EJECT                                                                
035000                                                                          
035100 01  FILLER                    PIC X(16)   VALUE 'WDATAREA'.              
035200     SKIP2                                                                
035300*01  -COPY WDATAREA.                                                      
035800 01  FILLER                    PIC X(16)   VALUE 'W460DIS1 AREA'.         
035900     SKIP2                                                                
036000*01  -COPY W460DIS1.                                                      
036100     EJECT                                                                
036200*- - - - - - - - - - - - FIX FÖR BORTTAG AV TRANSAR                       
036300                                                                          
036400 01  FIX-WDGZKEY.                                                         
036500     03  FILLER                PIC X(6).                                  
036600     03  FIX-TTMM              PIC 9(4).                                  
036700     03  FIX-SSSSL             PIC 9(5).                                  
036800     03  FILLER                PIC X(3).                                  
036900     SKIP3                                                                
037000 01  FL-BORTTAG                PIC X(1)       VALUE 'N'.                  
037100     88  BORTTAG                              VALUE 'J'.                  
037200     EJECT                                                                
037300*- - - - - - - - - - - - DISTRIKTS TESTER                                 
037400                                                                          
037500 01  TEST-IDDISTR              PIC 9(5)   COMP-3.                         
037600*01  FILLER  -COPY WWDIST18    -RED TEST-IDDISTR.                         
037700*01  FILLER  -COPY WWDIST19    -RED TEST-IDDISTR.                         
037800*01  FILLER  -COPY WWDIST20    -RED TEST-IDDISTR.                         
037900*01  FILLER  -COPY WWDIST24    -RED TEST-IDDISTR.                         
038000*01  FILLER  -COPY WWDIST35    -RED TEST-IDDISTR.                         
038100*01  FILLER  -COPY WWDIST93    -RED TEST-IDDISTR.                         
038200*01  FILLER  -COPY WWDIS130    -RED TEST-IDDISTR.                         
038300     EJECT                                                                
038400 01  DYNAMISKA-SUBPROGRAM.                                                
038500     03  POSTSUM               PIC X(8) VALUE 'POSTSUM '.                 
038600     03  CBLTDLI               PIC X(8) VALUE 'CBLTDLI '.                 
038700     03  FELLOG                PIC X(8) VALUE 'FELLOG  '.                 
038800     03  WDATKONV              PIC X(8) VALUE 'WDATKONV'.                 
038900     03  W009KSIF              PIC X(8) VALUE 'W009KSIF'.                 
039100     03  W460DIS1              PIC X(8) VALUE 'W460DIS1'.                 
039200                                                                          
039300     SKIP3                                                                
039400 01  HELP-LOGGPOST.                                                       
039500     03  FILLER                PIC X(3).                                  
039600     03  W-LOGGPOST            PIC X(87).                                 
039700     SKIP2                                                                
039800 01  W009KSIF-PARM.                                                       
039900     03 FLT                    PIC 9(9).                                  
040000     03 LGD                    PIC 9(1).                                  
040100     03 KSIFF                  PIC 9(1).                                  
040200 01  W-PKINR                   PIC X(3).                                  
040300 01  FILLER        REDEFINES W-PKINR.                                     
040400     03  FILLER                PIC 9(2).                                  
040500     03  SISTA-SIFFRAN         PIC 9(1).                                  
040600     EJECT                                                                
040700*01  KDP-AREA    -COPY W10111      -PRE WS-.                              
040800     EJECT                                                                
040900*01  RZV-AREA    -COPY W1011102    -PRE WS-.                              
041000     EJECT                                                                
041100*--------------------------------------- PARAMETRAR TILL POSTSUM          
041200                                                                          
041300*01  -COPY W0005  -PRE POSTSUM-                                           
041400     EJECT                                                                
041500*    *** UTAREOR ***                                                      
041600                                                                          
041700*01  POST   -COPY  W211R31  -PRE R31-                                     
041800     EJECT                                                                
041900 01  W09264-POST.                                                         
042000*    03  -COPY  W092W001 -PRE  W-                                         
042100     SKIP2                                                                
042200     03  LOGGPOSTEN PIC X(90).                                            
042300*    03  POST  -COPY  W211F31  -PRE UTR31- -RED LOGGPOSTEN.               
042400     EJECT                                                                
042500     03  LOGGDAT.                                                         
042600        05  LOGG6-DD               PIC 9(2).                              
042700        05  LOGG6-KLOCK            PIC 9(8).                              
042800                                                                          
042900                                                                          
043000     EJECT                                                                
043100*01  POST   -COPY  W211FEL  -PRE  UTW092-                                 
043200     EJECT                                                                
043300*01  POST   -COPY  W211310  -PRE  UTX221-                                 
043400     EJECT                                                                
043500*01  POST   -COPY  W092ZU   -PRE  UTKDP-                                  
043600     EJECT                                                                
043700*01  POST   -COPY  W111RZV                                                
043800     EJECT                                                                
043900*01  POST   -COPY  A310TB65 -PRE  UTRY2-                                  
044000     EJECT                                                                
044100 01  KONTROLLISTA-AREA.                                                   
044200                                                                          
044300*    03  AREA   -COPY W092W001 -PRE KSORT-                                
044400                                                                          
044500     03  FILLER                  PIC X(90)   VALUE SPACE.                 
044600     EJECT                                                                
044700 01  FILLER                      PIC X(16)   VALUE 'UTRZ1-AREA'.          
044800                                                                          
044900*01  AREA   -COPY  W225P232 -PRE UTRZ1-                                   
045000     EJECT                                                                
045100 01  FILLER                      PIC X(16)   VALUE 'UTRZ3-AREA'.          
045200                                                                          
045300*01  AREA   -COPY  W430432  -PRE UTRZ3-                                   
045400     EJECT                                                                
045500 01  FILLER                      PIC X(16)   VALUE 'UTRZ4-AREA'.          
045600                                                                          
045700*01  AREA   -COPY  W430432  -PRE UTRZ4-                                   
045800     EJECT                                                                
045900 01  FILLER                      PIC X(16)   VALUE 'UTRZ5-AREA'.          
046000                                                                          
046100*01  AREA   -COPY  W231229  -PRE UTRZ5-                                   
046200     EJECT                                                                
046300 01  FILLER                      PIC X(16)   VALUE 'UTRZQ-AREA'.          
046400                                                                          
046500*01  AREA   -COPY  WDGZRZQ  -PRE UTRZQ-.                                  
046600     EJECT                                                                
046700 01  FILLER                      PIC X(16)   VALUE 'UTRZR-AREA'.          
046800                                                                          
046900*01  AREA   -COPY  WDGZRZR  -PRE UTRZR-.                                  
047000     EJECT                                                                
047100 01  FILLER                      PIC X(16)   VALUE 'UTRZT-AREA'.          
047200                                                                          
047300*01  AREA   -COPY  WDGZRZT  -PRE UTRZT-.                                  
047400     EJECT                                                                
047500 01  FILLER                      PIC X(16)   VALUE 'UT001-AREA'.          
047600*01  UT001-AREA   -COPY  W461001                                          
047700     EJECT                                                                
047800 01  FILLER                      PIC X(16)   VALUE 'UT002-AREA'.          
047900*01  UT002-AREA   -COPY  W461002                                          
048000     EJECT                                                                
048100 01  FILLER                      PIC X(16)   VALUE 'UT003-AREA'.          
048200*01  UT003-AREA   -COPY  W461003                                          
048300     EJECT                                                                
048400 01  FILLER                      PIC X(16)   VALUE 'UT004-AREA'.          
048500*01  UT004-AREA   -COPY  W461004                                          
048600     EJECT                                                                
048700 01  FILLER                      PIC X(16)   VALUE 'UT005-AREA'.          
048800*01  UT005-AREA   -COPY  W461005                                          
048900     EJECT                                                                
049000 01  FILLER                      PIC X(16)   VALUE 'UT006-AREA'.          
049100*01  UT006-AREA   -COPY  W461006                                          
049200     EJECT                                                                
049300 01  FILLER                      PIC X(16)   VALUE 'UT007-AREA'.          
049400*01  UT007-AREA   -COPY  W461007                                          
049500     EJECT                                                                
049600 01  FILLER                      PIC X(16)   VALUE 'UT008-AREA'.          
049700*01  UT008-AREA   -COPY  W461008                                          
049800     EJECT                                                                
049900 01  FILLER                      PIC X(16)   VALUE 'UT009-AREA'.          
050000*01  UT009-AREA   -COPY  W461009                                          
050100     EJECT                                                                
050200 01  FILLER                      PIC X(16)   VALUE 'UT015-AREA'.          
050300*01  UT015-AREA   -COPY  W461015                                          
050400     EJECT                                                                
050500 01  FILLER                      PIC X(16)   VALUE 'UT020-AREA'.          
050600*01  UT020-AREA   -COPY  W461020                                          
050700     EJECT                                                                
050800 01  FILLER                      PIC X(16)   VALUE 'SU1-AREA'.            
050900                                                                          
051000*01  AREA   -COPY  W425SU1 -PRE SU1-                                      
051100     EJECT                                                                
051200 01  FILLER                      PIC X(16)   VALUE 'SU2-AREA'.            
051300                                                                          
051400*01  AREA   -COPY  W425SU2 -PRE SU2-                                      
051500     EJECT                                                                
051600 01  FILLER                      PIC X(16)   VALUE 'SU3-AREA'.            
051700                                                                          
051800*01  AREA   -COPY  W425SU3 -PRE SU3-                                      
051900     EJECT                                                                
052000 01  FILLER                      PIC X(16)   VALUE 'SU4-AREA'.            
052100                                                                          
052200*01  AREA   -COPY  W425SU4 -PRE SU4-                                      
052300     EJECT                                                                
052400 01  FILLER                      PIC X(16)   VALUE 'SU5-AREA'.            
052500                                                                          
052600*01  AREA   -COPY  W425SU5 -PRE SU5-                                      
052700 01  FILLER                      PIC X(16)   VALUE 'SUA-AREA'.            
052800                                                                          
052900*01  AREA   -COPY  W425SUA -PRE SUA-                                      
053000     EJECT                                                                
053100 01  FILLER                      PIC X(16)   VALUE 'UTSUC-AREA'.          
053200                                                                          
053300*01  AREA   -COPY  W425SUC -PRE UTSUC-                                    
053400     EJECT                                                                
053500 01  FILLER                      PIC X(16)   VALUE 'SUD-AREA'.            
053600                                                                          
053700*01  AREA   -COPY  W425SUD -PRE SUD-                                      
053800     EJECT                                                                
053900 01  FILLER                      PIC X(16)   VALUE 'FEL667-AREA '.        
054000                                                                          
054100*01  AREA   -COPY  W415FEL  -PRE FEL667-.                                 
054200     EJECT                                                                
054300 01  FILLER                      PIC X(16)   VALUE 'FEL666-AREA '.        
054400                                                                          
054500*01  AREA   -COPY  W491FEL  -PRE FEL666-.                                 
054600     EJECT                                                                
054700 01  FILLER                      PIC X(16)   VALUE 'FEL635-AREA '.        
054800                                                                          
054900 01  FEL635-AREA.                                                         
055000     03  FEL635-PRARTNTO-X.                                               
055100         05  FEL635-PRARTNTO     PIC 9(8)V9(2).                           
055200     03  FEL635-PRARTSJK-X.                                               
055300         05  FEL635-PRARTSJK     PIC 9(8)V9(2).                           
055400     EJECT                                                                
055500 01  FILLER                      PIC X(16)   VALUE 'RY1-AREA'.            
055600                                                                          
055700*01  RY1-AREA   -COPY  WDGZRY1                                            
055800     EJECT                                                                
055900 01  FILLER                      PIC X(16)   VALUE 'RY1S-AREA'.           
056000                                                                          
056100*01  RY1S-AREA  -COPY  WDGZRY1S                                           
056200     EJECT                                                                
056300 01  FILLER                      PIC X(16)   VALUE 'RY1X-AREA'.           
056400                                                                          
056500*01  RY1S-AREA  -COPY  W092P001 -PRE RY1X-                                
056600     EJECT                                                                
056700 01  FILLER                      PIC X(16)   VALUE 'RY5-AREA'.            
056800                                                                          
056900*01  RY5-AREA   -COPY  WDGZRY5                                            
057000     EJECT                                                                
057100 01  FILLER                      PIC X(16)   VALUE 'RY5S-AREA'.           
057200                                                                          
057300*01  RY5S-AREA  -COPY  WDGZRY5S                                           
057400     EJECT                                                                
057500 01  FILLER                      PIC X(16)   VALUE 'RY6-AREA'.            
057600                                                                          
057700*01  RY6-AREA   -COPY  WDGZRY6                                            
057800     EJECT                                                                
057900 01  FILLER                      PIC X(16)   VALUE 'RYA-AREA'.            
058000                                                                          
058100*01  RYA-AREA   -COPY  WDGZRYA                                            
058200     EJECT                                                                
058300 01  FILLER                      PIC X(16)   VALUE 'RYB-AREA'.            
058400                                                                          
058500*01  RYB-AREA   -COPY  WDGZRYB                                            
058600     EJECT                                                                
058700 01  FILLER                      PIC X(16)   VALUE 'RYC-AREA'.            
058800                                                                          
058900*01  RYC-AREA   -COPY  WDGZRYC                                            
059000     EJECT                                                                
059100                                                                          
059200 01  FILLER                      PIC X(16)   VALUE 'RYCS-AREA'.           
059300                                                                          
059400*01  RYCS-AREA  -COPY  WDGZRYCS                                           
059500     EJECT                                                                
059600                                                                          
059700 01  FILLER                      PIC X(16)   VALUE 'RYCX-AREA'.           
059800                                                                          
059900*01  RYCS-AREA  -COPY  W092P001 -PRE RYCX-                                
060000     EJECT                                                                
060100                                                                          
060200 01  FILLER                      PIC X(16)   VALUE 'RYE-AREA'.            
060300*01  RYE-AREA   -COPY  WDGZRYE                                            
060400     EJECT                                                                
060500                                                                          
060600 01  FILLER                      PIC X(16)   VALUE 'RYES-AREA'.           
060700                                                                          
060800*01  RYES-AREA  -COPY  WDGZRYES                                           
060900     EJECT                                                                
061000 01  FILLER                      PIC X(16)   VALUE 'RYI-AREA'.            
061100                                                                          
061200*01  RYI-AREA   -COPY  WDGZRYI                                            
061300     EJECT                                                                
061400 01  FILLER                      PIC X(16)   VALUE 'RYJ-AREA'.            
061500                                                                          
061600*01  RYJ-AREA   -COPY  WDGZRYJ                                            
061700     EJECT                                                                
061800 01  FILLER                      PIC X(16)   VALUE 'RZA-AREA'.            
061900                                                                          
062000*01  RZA-AREA   -COPY  WDGZRZA                                            
062100     EJECT                                                                
062200 01  FILLER                      PIC X(16)   VALUE 'RZAS-AREA'.           
062300                                                                          
062400*01  RZAS-AREA  -COPY  W092P001 -PRE RZAS-                                
062500     EJECT                                                                
062600 01  FILLER                      PIC X(16)   VALUE 'RZB-AREA'.            
062700                                                                          
062800*01  RZB-AREA   -COPY  WDGZRZB                                            
062900     EJECT                                                                
063000 01  FILLER                      PIC X(16)   VALUE 'SUC-AREA'.            
063100                                                                          
063200*01  SUC-AREA   -COPY  WDGZSUC                                            
063300     EJECT                                                                
063400*01  AREA  -COPY  W440300  -PRE RY4-                                      
063500     EJECT                                                                
063600*01  RY9S-AREA  -COPY  WDGZRY9S.                                          
063700     EJECT                                                                
063800*01  RY9-AREA  -COPY  WDGZRY9.                                            
063900     EJECT                                                                
064000 01  FILLER                    PIC X(16) VALUE 'UTX9-AREA'.               
064100*01  AREA  -COPY W092X9  -PRE UTX9-.                                      
064200     EJECT                                                                
064300 01  FILLER                    PIC X(16) VALUE 'IN-AREA RYK  '.           
064400*01  RYK-AREA -COPY WDGZRYK                                               
064500     EJECT                                                                
064600 01  BORS-AREA-START           PIC X(16) VALUE 'UT-AREA BORS'.            
064700     SKIP2                                                                
064800 01  BORS-AREA.                                                           
064900                                                                          
065000     03  UXC-IDPTYP            PIC X(3)  VALUE SPACE.                     
065100     03  FILLER                PIC X(30) VALUE SPACE.                     
065200     SKIP2                                                                
065300*01  FILLER   -PRE UXCW201- -COPY W020201 -RED BORS-AREA.                 
065400     EJECT                                                                
065500*01  FILLER   -PRE UXCW203- -COPY W020203 -RED BORS-AREA.                 
065600     EJECT                                                                
065700*01  FILLER   -PRE UXCR204- -COPY W020204 -RED BORS-AREA.                 
065800     EJECT                                                                
065900 01  SRS-AREA-START            PIC X(16) VALUE 'UT-AREA BORS'.            
066000     SKIP2                                                                
066100 01  SRS-AREA.                                                            
066200                                                                          
066300     03  FILLER               PIC X(200) VALUE SPACE.                     
066400     SKIP2                                                                
066500*01  FILLER   -PRE UTSRS-   -COPY WDGZ01  -RED SRS-AREA.                  
066600     EJECT                                                                
066700 01  XXX-AREA-START            PIC X(16) VALUE 'UT-AREA XXX'.             
066800     SKIP2                                                                
066900 01  XXX-AREA.                                                            
067000                                                                          
067100     03  FILLER               PIC X(200) VALUE SPACE.                     
067200     SKIP2                                                                
067300*01  FILLER   -PRE UTXXX-   -COPY WDGZ01  -RED XXX-AREA.                  
067400     EJECT                                                                
067500*--------------------------------------- ARBETSAREOR FÖR IMS              
067600                                                                          
067700 01  IMS-WS.                                                              
067800     03  FILLER              PIC X(8)    VALUE 'IMS-WS'.                  
067900     SKIP3                                                                
068000*--------------------------------------- STATUSKOD FRÅN IMS               
068100     SKIP1                                                                
068200     03  STATUS-WS           PIC X(2).                                    
068300         88  SEGMENT-FINNS               VALUE '  '.                      
068400         88  BASEN-SLUT                  VALUE 'GB'.                      
068500     SKIP3                                                                
068600     03  GODK-STATUSKODER.                                                
068700         05  GODK-STATUS OCCURS 10 INDEXED BY STATUS-IX                   
068800                             PIC X(2).                                    
068900     SKIP2                                                                
069000 01  SSA1                    PIC X(64).                                   
069100     EJECT                                                                
069200*    -COPY W0003                                                          
069300     EJECT                                                                
069400 01  DLI-IO-AREA.                                                         
069500     03  IO-AREA               PIC X(200) VALUE SPACE.                    
069600                                                                          
069700*    03  WLZZAC01  -COPY WDGZ01 -PRE LOGG6-  -RED IO-AREA.                
069800     EJECT                                                                
069900 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
070000 01   DLI-IO-AREA-B601.                                                   
070100*     03  -COPY WDB601                                                    
070200     EJECT                                                                
070300 LINKAGE SECTION.                                                         
070400*    -COPY W0008 -PRE WLZZAC-.                                            
070500     05 FILLER               PIC X.                                       
070600     EJECT                                                                
070700*01  -COPY W0008      -PRE WDB6-                                          
070800     05  FILLER                  PIC X.                                   
070900     EJECT                                                                
071000 PROCEDURE DIVISION USING  WLZZAC-PCB WDB6-PCB.                           
071100 MAIN SECTION.                                                            
071200     ENTRY 'DLITCBL' USING WLZZAC-PCB WDB6-PCB.                           
071300                                                                          
071400     PERFORM A-INIT                                                       
071500                                                                          
071600     PERFORM AB-OPEN-WDG6-FILER                                           
071700     PERFORM C-REDIGERA-WDG6-FILER                                        
071800     PERFORM ZB-CLOSE-WDG6-FILER                                          
071900                                                                          
072000     PERFORM Z-FINIT                                                      
072100                                                                          
072200     MOVE ZERO TO RETURN-CODE                                             
072300     GOBACK                                                               
072400     .                                                                    
072500     EJECT                                                                
072600 A-INIT SECTION.                                                          
072700                                                                          
072800     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
072900     ACCEPT DAGENS-DATUM FROM DATE                                        
073000     .                                                                    
073100     EJECT                                                                
073200 AB-OPEN-WDG6-FILER SECTION.                                              
073300                                                                          
073400      OPEN OUTPUT                W09290-FEL                               
073500                                 W09291-ANSK                              
073600                                 W092Z3                                   
073700                                 W092Z4                                   
073800                                 W092Z5                                   
073900                                 W092Z7                                   
074000                                 W092ZF                                   
074100                                 W092ZI                                   
074200                                 W092ZN                                   
074300                                 W092ZP                                   
074400                                 W092ZQ                                   
074500                                 W092ZR                                   
074600                                 W092ZU                                   
074700                                 W092ZV                                   
074800                                 W092ZW                                   
074900                                 W09299                                   
075000                                 W092X3                                   
075100                                 W092X4                                   
075200                                 W092X9                                   
075300                                 W092XC                                   
075400                                 W092S1                                   
075500                                 W092XX                                   
075600     .                                                                    
075700     EJECT                                                                
075800 C-REDIGERA-WDG6-FILER SECTION.                                           
075900                                                                          
076000     PERFORM S02-LAES-WDG6                                                
076100     PERFORM UNTIL BASEN-SLUT                                             
076200                                                                          
076300*- - - DENNA SECTION KAN ANVÄNDAS OM MAN VILL UNDANTAGA VISSA             
076400*      POSTER OCH FÅ DESSA DISPLAYADE I KÖRNINGSLOGGEN.                   
076500*      PERFORM FIX-TEST-BORTTAG                                           
076600*      IF NOT BORTTAG                                                     
076700                                                                          
076800       PERFORM S20-SKRIV-LOGGFIL                                          
076900                                                                          
077000       EVALUATE LOGG6-IDPTYP                                              
077100         WHEN '310' PERFORM C01-SKAPA-30-OR-31-OR-32-POST                 
077200         WHEN 'R30' PERFORM C01-SKAPA-30-OR-31-OR-32-POST                 
077300         WHEN 'R31' PERFORM C01-SKAPA-30-OR-31-OR-32-POST                 
077400         WHEN 'R32' PERFORM C01-SKAPA-30-OR-31-OR-32-POST                 
077500         WHEN '092' PERFORM C02-SKAPA-W092POSTER                          
077600         WHEN '221' PERFORM C03-SKRIV-221-POST                            
077700         WHEN 'RY1' PERFORM C10-BEHANDLA-RY1-AVVIK                        
077800         WHEN 'RY5' PERFORM C11-BEHANDLA-RY5-ANNUL                        
077900         WHEN 'RY6' PERFORM C45-SKRIV-RY6-POST                            
078000         WHEN 'RZA' PERFORM C19-BEHANDLA-RZA                              
078100         WHEN 'RZQ' PERFORM C34-SKRIV-TULL-POST-001                       
078200         WHEN 'RZR' PERFORM C35-SKRIV-TULL-POST-002                       
078300         WHEN 'RZU' PERFORM C39-SKRIV-KDP-POST                            
078400         WHEN 'RZV' PERFORM C40-SKRIV-RZV-POST                            
078500         WHEN 'RY2' PERFORM C41-SKRIV-RY2-POST                            
078600         WHEN 'RY4' PERFORM C47-SKRIV-RY4-POST                            
078700         WHEN 'RY9' PERFORM C48-SKRIV-RY9-POST                            
078800         WHEN 'RYA' PERFORM C60-SKRIV-RYA-POST                            
078900         WHEN 'RYC' PERFORM C61-SKRIV-RYC-POST                            
079000         WHEN 'RYE' PERFORM C63-SKRIV-RYE-POST                            
079100         WHEN 'RYX' PERFORM S5X-SKRIV-W092XX-POST                         
079200         WHEN 'SUC' PERFORM C66-SKRIV-SUC-POST                            
079300         WHEN 'RYB' PERFORM C67-SKRIV-RYB-POST                            
079400         WHEN 'RYI' PERFORM C69-SKRIV-RYI-POST                            
079500         WHEN 'RYJ' PERFORM C70-SKRIV-RYJ-POST                            
079600         WHEN 'RYK' PERFORM C71-SKRIV-RYK-POST                            
079700         WHEN OTHER                                                       
079800                    WRITE FELTRANS FROM IO-AREA                           
079900       END-EVALUATE                                                       
080000                                                                          
080100*      END-IF                                                             
080200                                                                          
080300       PERFORM S02-LAES-WDG6                                              
080400     END-PERFORM                                                          
080500     .                                                                    
080600     EJECT                                                                
080700*FIX-944 SECTION.                                                         
080800*    SKIP2                                                                
080900*    DISPLAY '944 POSTER'                                                 
081000*    .                                                                    
081100     SKIP2                                                                
081200*FIX-TEST-BORTTAG SECTION.                                                
081300*    SKIP3                                                                
081400*     MOVE NEJ                  TO FL-BORTTAG                             
081500*                                                                         
081600*     IF LOGG6-IDPTYP           = 'RZD'                                   
081700*         OR                      'RY9'                                   
081800*         OR                      'R51'                                   
081900*         OR                      'R52'                                   
082000*         OR                      'R55'                                   
082100*         OR                      'RZ4'                                   
082200*         OR                      'RZ9'                                   
082300*         OR                      'RZA'                                   
082400*         OR                      'RZB'                                   
082500*         OR                      'R50'                                   
082600*         OR                      'RZF'                                   
082700*         OR                      'RZG'                                   
082800*         OR                      'RZH'                                   
082900*         OR                      'RZI'                                   
083000*         OR                      'RZJ'                                   
083100*         MOVE LOGG6-WDGZKEY    TO FIX-WDGZKEY                            
083200*                                                                         
083300*         IF FIX-TTMM           = 1017                                    
083400*             OR                  1018                                    
083500*             OR                  1028                                    
083600*             OR                  1448                                    
083700*             MOVE JA           TO FL-BORTTAG                             
083800*         END-IF                                                          
083900*                                                                         
084000*         IF BORTTAG                                                      
084100*             DISPLAY ' '                                                 
084200*             DISPLAY '***  B O R T T A G E N   T R A N S  ***'           
084300*             DISPLAY LOGG6-WDGZKEY                                       
084400*             EVALUATE LOGG6-IDPTYP                                       
084500*               WHEN 'R50' PERFORM FIX01-KONTROLLISTA                     
084600*               WHEN 'R51' PERFORM FIX01-KONTROLLISTA                     
084700*               WHEN 'R52' PERFORM FIX01-KONTROLLISTA                     
084800*               WHEN 'R55' PERFORM FIX01-KONTROLLISTA                     
084900*               WHEN 'RZA' PERFORM FIX04-RZA                              
085000*               WHEN 'RZD' PERFORM FIX07-RZD                              
085100*            END-EVALUATE                                                 
085200*         END-IF                                                          
085300*     END-IF                                                              
085400*    .                                                                    
085500*    EJECT                                                                
085600*FIX01-KONTROLLISTA SECTION.                                              
085700*    SKIP2                                                                
085800*     DISPLAY '* KONTROLLISTA'                                            
085900*     DISPLAY 'SORT-DEL ' LOGG6-SORTPOST                                  
086000*     DISPLAY 'DATA-DEL ' LOGG6-LOGGPOST                                  
086100*    .                                                                    
086200*    EJECT                                                                
086300*FIX04-RZA SECTION.                                                       
086400*    SKIP2                                                                
086500*     MOVE LOGG6-LOGGPOST        TO RZA-AREA                              
086600*                                                                         
086700*     DISPLAY                    RZA-IDPTYP                               
086800*     DISPLAY                    RZA-IDDISTR                              
086900*     DISPLAY                    RZA-IDKUNDNR                             
087000*     DISPLAY                    RZA-IDDC                                 
087100*     DISPLAY                    RZA-IDORDNR                              
087200*     DISPLAY                    RZA-IDARTNR                              
087300*     DISPLAY                    RZA-KDORDKL                              
087400*     DISPLAY                    RZA-KDFAKTYP                             
087500*     DISPLAY                    RZA-KVBEART                              
087600*     DISPLAY                    RZA-FLVR                                 
087700*    .                                                                    
087800     EJECT                                                                
087900*FIX05-RZB SECTION.                                                       
088000*    SKIP2                                                                
088100*     MOVE LOGG6-LOGGPOST        TO RZB-AREA                              
088200*                                                                         
088300*     DISPLAY                    RZB-IDPTYP                               
088400*     DISPLAY                    RZB-IDARTNR                              
088500*     DISPLAY                    RZB-KDCLAGER                             
088600*     DISPLAY                    RZB-KVAVBART                             
088700*     .                                                                   
088800     EJECT                                                                
088900*FIX07-RZD SECTION.                                                       
089000*    SKIP2                                                                
089100*     MOVE LOGG6-LOGGPOST        TO RZD-AREA                              
089200*                                                                         
089300*     DISPLAY                    RZD-IDPTYP                               
089400*     DISPLAY                    RZD-IDARTNR                              
089500*     DISPLAY                    RZD-KDOI                                 
089600*     DISPLAY                    RZD-KVOI                                 
089700*     DISPLAY                    RZD-TIAAP-AVBOK                          
089800*     DISPLAY                    RZD-IDDC                                 
089900*     DISPLAY                    RZD-IDDC-DAY                             
090000*    .                                                                    
090100     EJECT                                                                
090200 C01-SKAPA-30-OR-31-OR-32-POST SECTION.                                   
090300                                                                          
090400     MOVE ZERO TO W-W092W001                                              
090500     MOVE SPACE TO W-IDFELKODX W-FILLER2 LOGGPOSTEN                       
090600     IF LOGG6-IDPTYP = 'R31' OR '310'                                     
090700        MOVE LOGG6-LOGGPOST TO R31-POST                                   
090800        MOVE R31-IDPTYP TO UTR31-IDPTYP W-IDPTYP                          
090900        MOVE +1            TO UTR31-KDCLAGER W-KDCLAGER                   
091000*       MOVE R31-KDCLAGER  TO UTR31-KDCLAGER W-KDCLAGER                   
091100        MOVE R31-IDARTNR TO UTR31-IDARTNR W-SORTBGP                       
091200        MOVE R31-IDPLFORM TO UTR31-IDPLFORM                               
091300        MOVE R31-IDLEVNR-INL TO UTR31-IDLEVNR-INL                         
091400        MOVE R31-KDRT TO UTR31-KDRT                                       
091500        MOVE R31-TIAVSDAT TO UTR31-TIAVSDAT                               
091600        MOVE R31-IDKONTO TO UTR31-IDKONTO                                 
091700        MOVE R31-IDAVINR TO UTR31-IDAVINR                                 
091800        MOVE R31-KVAVIS TO UTR31-KVAVIS                                   
091900     END-IF                                                               
092000     MOVE LOGG6-TIAAMMDD TO LOGG6-DD                                      
092100     MOVE LOGG6-TIKLOCK TO LOGG6-KLOCK                                    
092200                                                                          
092300     .                                                                    
092400     EJECT                                                                
092500 C02-SKAPA-W092POSTER SECTION.                                            
092600     SKIP2                                                                
092700     MOVE LOGG6-SORTPOST TO UTW092-SORT-FLT                               
092800     MOVE LOGG6-LOGGPOST TO HELP-LOGGPOST                                 
092900     MOVE W-LOGGPOST TO UTW092-FELMED                                     
093000                                                                          
093100     PERFORM C02A-SKRIV-W092-POST                                         
093200     .                                                                    
093300     SKIP3                                                                
093400 C02A-SKRIV-W092-POST SECTION.                                            
093500     SKIP2                                                                
093600     WRITE  W092-UTPOST  FROM UTW092-POST                                 
093700     MOVE 'W09290' TO POSTSUM-FDNAMN                                      
093800     MOVE 'W09208D2' TO POSTSUM-DDNAMN2                                   
093900     MOVE LOGG6-IDPTYP TO POSTSUM-TRANSTYP                                
094000     CALL POSTSUM USING POSTSUM-PARM                                      
094100     .                                                                    
094200     EJECT                                                                
094300 C03-SKRIV-221-POST  SECTION.                                             
094400     SKIP2                                                                
094500     MOVE LOGG6-LOGGPOST TO UTX221-POST                                   
094600                                                                          
094700     WRITE X221-UTPOST FROM UTX221-POST                                   
094800                                                                          
094900                                                                          
095000     MOVE 'W09291' TO POSTSUM-FDNAMN                                      
095100     MOVE 'W09208D3' TO POSTSUM-DDNAMN2                                   
095200     MOVE LOGG6-IDPTYP TO POSTSUM-TRANSTYP                                
095300     CALL POSTSUM USING POSTSUM-PARM                                      
095400     .                                                                    
095500     EJECT                                                                
095600 C10-BEHANDLA-RY1-AVVIK         SECTION.                                  
095700*    HÄR BEHANDLAS RY1-TRANSAKTION SOM SKAPATS                            
095800*    VID FYSISK AVVIKELSE .                                               
095900*    FÖR VARJE RY1 TRANS KONTROLLERAS VILKEN ELLER VILKA                  
096000*    "RZ1-RZ8"-TRANSAKTIONER SOM SKALL SKAPAS.                            
096100*    DENNA ÄNDRING GJORD I SAMBAND MED KOLLIVIS PACKNING -86.             
096200                                                                          
096300                                                                          
096400     MOVE LOGG6-LOGGPOST        TO RY1-AREA                               
096500     MOVE LOGG6-SORTPOST        TO RY1S-WDGZRY1S                          
096600                                                                          
096700                                                                          
096800     MOVE RY1-IDARTNR           TO  FLT                                   
096900                                                                          
097000     PERFORM C10A-GEN-EV-RZ1                                              
097100     PERFORM S50-SKRIV-W092S1-POST                                        
097200     PERFORM C10B-GEN-EV-RZ3                                              
097300     IF RY1S-FLLSBOK            =  NEJ                                    
097400         CONTINUE                                                         
097500      ELSE                                                                
097600         PERFORM C10C-GEN-RZ5                                             
097700     END-IF                                                               
097800     IF RY1-KDORDBEK            =  +80 OR +81 OR +93                      
097900         PERFORM C10E-SKAPA-SU3-POST                                      
098000     END-IF                                                               
098100     IF RY1-KDORDBEK            =  +90 OR +91                             
098200         PERFORM C10F-SKAPA-SU4-POST                                      
098300     END-IF                                                               
098400     IF RY1-KDORDBEK            =  +80 OR +81 OR +90 OR                   
098500                                   +91 OR +93                             
098600         PERFORM C10G-MOVE-BRIST-NOAC                                     
098700     END-IF                                                               
098800     PERFORM C10H-GEN-EV-009-POST                                         
098900     MOVE RY1S-IDDC             TO WS-IDDC                                
099000                                   W-IDDC-B6                              
099100     PERFORM IMS-GU-WDB601                                                
099200     IF DCS-CDC                                                           
099300     OR DCS-CDC-TR                                                        
099400        MOVE 1                  TO RY1X-KDCLAGER-S                        
099500     ELSE                                                                 
099600        MOVE 2                  TO RY1X-KDCLAGER-S                        
099700     END-IF                                                               
099800     MOVE RY1S-IDDISTR          TO RY1X-IDDISTR-S                         
099900     MOVE RY1S-IDKUNDNR         TO RY1X-IDKUNDNR-S                        
100000     MOVE RY1S-KDFRAKT          TO RY1X-KDFRAKT-S                         
100100     MOVE RY1S-KDORDKL          TO RY1X-KDORDKL                           
100200     MOVE ZERO                  TO RY1X-KDFELMRK                          
100300                                   RY1X-IDFELKOD                          
100400     MOVE RY1-IDPTYP            TO RY1X-IDPTYP-S                          
100500     MOVE RY1-IDKUNDRF          TO W-IDKUNDRF                             
100600     MOVE W-IDKUNDRF-ONR        TO RY1X-IDORDNR-S                         
100700     MOVE RY1-IDARTNR           TO RY1X-SORTBGP                           
100800     MOVE RY1X-RY1S-AREA        TO LOGG6-SORTPOST                         
100900     .                                                                    
101000     EJECT                                                                
101100 C10A-GEN-EV-RZ1                SECTION.                                  
101200                                                                          
101300     MOVE RY1S-IDDC             TO WS-IDDC                                
101400                                   W-IDDC-B6                              
101500     PERFORM IMS-GU-WDB601                                                
101600     IF  (DCS-CDC                                                         
101700     OR   DCS-CDC-TR                                                      
101800     OR   DCS-SDC)                                                        
101900     MOVE RY1S-IDDISTR          TO  TEST-IDDISTR                          
102000     IF (RY1-KDORDTYP           = 3                                       
102100         OR RY1-KDORDING        = 3                                       
102200         OR RY1-FLDIRLEV        = JA                                      
102300         OR DIST19-SATS                                                   
102400         OR DIST20-EMBALLAGE                                              
102500         OR DIST93-BYTESRADIO-C2                                          
102600         OR (RY1-IDKUNDRF-RO    NOT = SPACE                               
102700         AND                    NOT = '00000     '                        
102800         AND RY1-TIRODAT        > ZERO)                                   
102900         OR RY1-KVAVART         = ZERO                                    
103000         OR RY1S-FLORDSPE       = JA                                      
103100         OR RY1-KVAVBART = RY1-KVLEVART)                                  
103200        CONTINUE                                                          
103300     ELSE                                                                 
103400*------------------------------------------- FYSISK AVVIKELSE M.M         
103500         MOVE SPACE             TO UTRZ1-AREA                             
103600                                                                          
103700         MOVE NEJ               TO SKRIV-RY1-SW                           
103800         MOVE '232'             TO UTRZ1-IDPTYP                           
103900         MOVE RY1S-IDDISTR      TO UTRZ1-IDDISTR                          
104000         IF CDC                                                           
104100            MOVE 1              TO UTRZ1-KDCLAGER                         
104200         ELSE                                                             
104300            MOVE 2              TO UTRZ1-KDCLAGER                         
104400         END-IF                                                           
104500         MOVE RY1S-KDORDKL      TO UTRZ1-KDORDKL                          
104600         MOVE RY1-IDARTNR       TO UTRZ1-IDARTNR                          
104700         IF RY1-KVAVBART        < RY1-KVBEART - RY1S-KVSLATT              
104800***        OM RAD LÅG UNDER SLATTGRÄNS REDAN VID UTSKRIFT                 
104900             IF RY1-FLRESTN     =  NEJ                                    
105000                 MOVE ZERO      TO UTRZ1-RERORAD                          
105100              ELSE                                                        
105200                 COMPUTE UTRZ1-RERORAD = RY1-KVAVART /                    
105300                                         RY1-KVBEART                      
105400             END-IF                                                       
105500             COMPUTE UTRZ1-REFYSAVV =                                     
105600                (RY1-KVAVBART - RY1-KVLEVART) / RY1-KVAVBART              
105700             MOVE ZERO          TO UTRZ1-REINKORD                         
105800             MOVE ZERO          TO UTRZ1-REAVBRAD                         
105900             MOVE JA            TO SKRIV-RY1-SW                           
106000          ELSE                                                            
106100             IF  ( RY1-KVAVBART >= RY1-KVBEART - RY1S-KVSLATT )           
106200             AND ( RY1-KVLEVART <  RY1-KVBEART - RY1S-KVSLATT )           
106300                 IF RY1-FLRESTN = NEJ                                     
106400                     MOVE ZERO          TO UTRZ1-RERORAD                  
106500                  ELSE                                                    
106600                     COMPUTE UTRZ1-RERORAD = RY1-KVAVART /                
106700                                             RY1-KVBEART                  
106800                 END-IF                                                   
106900                 COMPUTE UTRZ1-REFYSAVV =                                 
107000                    (RY1-KVBEART - RY1-KVLEVART) / RY1-KVBEART            
107100                 MOVE ZERO              TO UTRZ1-REAVBRAD                 
107200                 MOVE ZERO              TO UTRZ1-REINKORD                 
107300                 MOVE JA                TO SKRIV-RY1-SW                   
107400             END-IF                                                       
107500         END-IF                                                           
107600                                                                          
107700     END-IF                                                               
107800     END-IF                                                               
107900     .                                                                    
108000     EJECT                                                                
108100 C10B-GEN-EV-RZ3 SECTION.                                                 
108200     SKIP3                                                                
108300     IF RY1-KVAVART             > ZERO                                    
108400*------------------------------------------- FYSISK AVVIKELSE             
108500         MOVE SPACE             TO UTRZ3-AREA                             
108600                                                                          
108700         MOVE '433'             TO UTRZ3-IDPTYP                           
108800         MOVE RY1-IDARTNR       TO UTRZ3-IDARTNR                          
108900         MOVE RY1S-IDDC         TO UTRZ3-IDDC                             
109000         MOVE RY1S-IDDISTR      TO UTRZ3-IDDISTR                          
109100         MOVE RY1S-KDORDKL      TO UTRZ3-KDORDKL                          
109200         IF  RY1-TIORDREG       = ZERO                                    
109300             MOVE ZERO          TO UTRZ3-TIAVBPER                         
109400                                   UTRZ3-TIAVBAR                          
109500         ELSE                                                             
109600             MOVE RY1-TIORDREG  TO DAT-AAMMDD                             
109700             MOVE DAT-AAMMDD-AA TO UTRZ3-TIAVBAR                          
109800             PERFORM S11-HAEMTA-VOLVOPERIOD                               
109900             MOVE W-TIAVBPER    TO UTRZ3-TIAVBPER                         
110000         END-IF                                                           
110100         COMPUTE UTRZ3-KVBEART  = RY1-KVAVART * -1                        
110200         MOVE ZERO              TO UTRZ3-KDQPACK                          
110300                                                                          
110400         WRITE W092Z3-POST      FROM UTRZ3-AREA                           
110500                                                                          
110600         MOVE 'W092Z3'          TO POSTSUM-FDNAMN                         
110700         MOVE 'W09208DA'        TO POSTSUM-DDNAMN2                        
110800         MOVE UTRZ3-IDPTYP      TO POSTSUM-TRANSTYP                       
110900         CALL POSTSUM USING     POSTSUM-PARM                              
111000     END-IF                                                               
111100     .                                                                    
111200     EJECT                                                                
111300 C10C-GEN-RZ5 SECTION.                                                    
111400     SKIP3                                                                
111500     MOVE RY1S-IDDC             TO WS-IDDC                                
111600                                   W-IDDC-B6                              
111700     PERFORM IMS-GU-WDB601                                                
111800     IF RY1-KVAVBART = RY1-KVLEVART                                       
111900     AND (DCS-SDC                                                         
112000     OR   DCS-NDC)                                                        
112200        CONTINUE                                                          
112300     ELSE                                                                 
112400       MOVE RY1S-IDDISTR          TO TEST-IDDISTR                         
112500       MOVE SPACE                 TO UTRZ5-AREA                           
112600                                                                          
112700       MOVE '229'                 TO UTRZ5-IDPTYP                         
112800       MOVE RY1-IDARTNR           TO UTRZ5-IDARTNR                        
112900       IF CDC                                                             
113000          MOVE 1                  TO UTRZ5-KDCLAGER                       
113100       ELSE                                                               
113200          MOVE 2                  TO UTRZ5-KDCLAGER                       
113300       END-IF                                                             
113400       IF RY1-KDORDTYP            = 3                                     
113500           MOVE 7                 TO UTRZ5-KDRORELS                       
113600       ELSE                                                               
113700         IF DIST35-REFILL AND NOT                                         
113710           (DIST35-REFILL-NA OR                                           
113720            DIST35-REFILL-CN)                                             
113800           MOVE 2                 TO UTRZ5-KDRORELS                       
113900         ELSE                                                             
114000           MOVE 8                 TO UTRZ5-KDRORELS                       
114100         END-IF                                                           
114200       END-IF                                                             
114300       COMPUTE UTRZ5-KVANTAL      = (RY1-KVAVBART                         
114400                                  -  RY1-KVLEVART) * -1                   
114500       MOVE '2'                   TO UTRZ5-KDUPPD                         
114600                                                                          
114700       WRITE W092Z5-POST          FROM UTRZ5-AREA                         
114800                                                                          
114900       MOVE 'W092Z5'              TO POSTSUM-FDNAMN                       
115000       MOVE 'W09208DC'            TO POSTSUM-DDNAMN2                      
115100       MOVE UTRZ5-IDPTYP          TO POSTSUM-TRANSTYP                     
115200       CALL POSTSUM USING         POSTSUM-PARM                            
115300     END-IF                                                               
115400     .                                                                    
115500     EJECT                                                                
115600 C10E-SKAPA-SU3-POST      SECTION.                                        
115700                                                                          
115800     MOVE RY1S-IDDISTR          TO TEST-IDDISTR                           
115900     IF NOT DIST24-NORGE                                                  
116000       MOVE SPACE                 TO SU3-AREA                             
116100       MOVE 'SU3'                 TO SU3-IDPTYP                           
116200       MOVE RY1-KVAVART           TO SU3-KVBEART-002                      
116300       MOVE RY1-KDORDBEK          TO SU3-KDRESTR                          
116400       MOVE RY1S-IDDC             TO W-SU-IDDC                            
116500                                                                          
116600       PERFORM S40-RED-IDSUPPL-SU                                         
116700                                                                          
116800       MOVE RY1S-IDDISTR          TO SU3-IDDISTR                          
116900       MOVE RY1S-IDKUNDNR         TO SU3-IDKUNDNR                         
117000       MOVE IDSUPPL-SU-WS         TO SU3-IDSUPPL                          
117100                                                                          
117200       IF RY1-IDKUNDRF-RO  (1:5)  >  ZERO                                 
117300           MOVE RY1-IDKUNDRF-RO (1:5) TO SU3-IDORDNR-002                  
117400       ELSE                                                               
117500           MOVE RY1-IDKUNDRF (1:5)    TO SU3-IDORDNR-002                  
117600       END-IF                                                             
117700       MOVE RY1-IDARTNR           TO SU3-IDARTNR                          
117800       MOVE RY1-REKSIFFR          TO SU3-REKSIFFR                         
117900       MOVE RY1-KDFAKTYP          TO SU3-KDFAKTYP                         
118000       MOVE 2                     TO SU3-KDKVFOR                          
118100       MOVE ZERO                  TO SU3-KDRO                             
118200       MOVE RY1S-KDORDKL          TO SU3-KDORDER                          
118300       MOVE RY1-IDKUNDRF (1:5)    TO SU3-IDORDNR7-LEV                     
118400       MOVE ZERO                  TO SU3-KVQPACK-1                        
118500       MOVE NEJ                   TO SU3-TID-ERS                          
118600       MOVE RY1S-KDORDKL          TO SU3-KDORDKL                          
118700       MOVE ZERO                  TO SU3-FIKTIV-KVANT                     
118800       IF RY1-KDTPOTYP            =  +1                                   
118900           IF RY1S-IDSYSTEM       =  'VR  '                               
119000               MOVE 1             TO SU3-KDVRTPO                          
119100            ELSE                                                          
119200               MOVE 2             TO SU3-KDVRTPO                          
119300           END-IF                                                         
119400        ELSE                                                              
119500           MOVE ZERO              TO SU3-KDVRTPO                          
119600       END-IF                                                             
119700       MOVE RY1-KDVRINFO          TO SU3-KDVRINFO                         
119800                                                                          
119900       MOVE LOGG6-TIAAMMDD        TO SU3-TIAAMMDD                         
120000       MOVE LOGG6-TIKLOCK         TO SU3-TIKLOCK                          
120100                                                                          
120200       WRITE W092Z7-POST          FROM SU3-AREA                           
120300                                                                          
120400       MOVE 'W092Z7'              TO POSTSUM-FDNAMN                       
120500       MOVE 'W09208DE'            TO POSTSUM-DDNAMN2                      
120600       MOVE SU3-IDPTYP            TO POSTSUM-TRANSTYP                     
120700       CALL POSTSUM USING         POSTSUM-PARM                            
120800     END-IF                                                               
120900     .                                                                    
121000     EJECT                                                                
121100 C10F-SKAPA-SU4-POST      SECTION.                                        
121200                                                                          
121300     MOVE RY1S-IDDISTR          TO TEST-IDDISTR                           
121400     IF NOT DIST24-NORGE                                                  
121500       MOVE SPACE                 TO SU4-AREA                             
121600       MOVE 'SU4'                 TO SU4-IDPTYP                           
121700       MOVE RY1-KDORDBEK          TO SU4-KDRESTR                          
121800       MOVE RY1S-IDDC             TO W-SU-IDDC                            
121900                                                                          
122000       PERFORM S40-RED-IDSUPPL-SU                                         
122100                                                                          
122200       MOVE RY1S-IDDISTR          TO SU4-IDDISTR                          
122300       MOVE RY1S-IDKUNDNR         TO SU4-IDKUNDNR                         
122400       MOVE IDSUPPL-SU-WS         TO SU4-IDSUPPL                          
122500                                                                          
122600       IF RY1-IDKUNDRF-RO (1:5)   >  ZERO                                 
122700           MOVE RY1-IDKUNDRF-RO (1:5) TO SU4-IDORDNR-002                  
122800           MOVE 1                     TO SU4-KDRO                         
122900       ELSE                                                               
123000           MOVE RY1-IDKUNDRF (1:5)    TO SU4-IDORDNR-002                  
123100           MOVE ZERO                  TO SU4-KDRO                         
123200       END-IF                                                             
123300       MOVE RY1-IDARTNR           TO SU4-IDARTNR                          
123400       MOVE RY1-REKSIFFR          TO SU4-REKSIFFR                         
123500       MOVE RY1-KVAVART           TO SU4-KVRO-002                         
123600       MOVE RY1S-KDORDKL          TO SU4-KDORDER                          
123700       MOVE RY1-KDFAKTYP          TO SU4-KDFAKTYP                         
123800       MOVE RY1-IDKUNDRF (1:5)    TO SU4-IDORDNR7-LEV                     
123900       MOVE NEJ                   TO SU4-TID-ERS                          
124000       MOVE RY1-TIDISPIN          TO SU4-TIDISPIN                         
124100       MOVE RY1-KDVRINFO          TO SU4-KDVRINFO                         
124200                                                                          
124300       MOVE LOGG6-TIAAMMDD        TO SU4-TIAAMMDD-REG                     
124400       MOVE LOGG6-TIKLOCK         TO SU4-TIKLOCK-REG                      
124500                                                                          
124600       WRITE W092Z7-POST          FROM SU4-AREA                           
124700                                                                          
124800       MOVE 'W092Z7'              TO POSTSUM-FDNAMN                       
124900       MOVE 'W09208DE'            TO POSTSUM-DDNAMN2                      
125000       MOVE SU4-IDPTYP            TO POSTSUM-TRANSTYP                     
125100       CALL POSTSUM USING         POSTSUM-PARM                            
125200     END-IF                                                               
125300     .                                                                    
125400     EJECT                                                                
125500 C10G-MOVE-BRIST-NOAC SECTION.                                            
125600     SKIP2                                                                
125700     MOVE '007'                TO OBLAG-IDPTYP                            
125800     MOVE RY1S-IDDISTR         TO OBLAG-IDDISTR                           
125900     MOVE RY1S-IDKUNDNR        TO OBLAG-IDKUNDNR                          
126000     MOVE RY1S-KDFRAKT         TO OBLAG-KDFRAKT                           
126100     MOVE RY1-IDKUNDRF (1:5)   TO OBLAG-IDORDNR                           
126200     IF RY1-FLTILLK            = NEJ                                      
126300       IF RY1-IDKUNDRF-RO (1:5)  = +0                                     
126400         MOVE +0               TO OBLAG-KDLIDEL                           
126500       ELSE                                                               
126600         MOVE +1               TO OBLAG-KDLIDEL                           
126700       END-IF                                                             
126800     ELSE                                                                 
126900       MOVE +2                 TO OBLAG-KDLIDEL                           
127000     END-IF                                                               
127100     MOVE RY1S-IDDC            TO OBLAG-IDDC                              
127200     MOVE RY1-IDARTNR          TO OBLAG-IDARTNR                           
127300     MOVE SPACE                TO OBLAG-BEART                             
127400     MOVE RY1-BERADREF         TO OBLAG-BERADREF                          
127500     MOVE RY1-BEVOLREF         TO OBLAG-BEVOLREF                          
127600     MOVE RY1-IDKUNDRF-RO (1:5) TO OBLAG-IDRONR                           
127700     MOVE RY1-REKSIFFR         TO OBLAG-REKSIFFR                          
127800     MOVE RY1-TIRODAT          TO OBLAG-TIRODAT                           
127900     MOVE RY1-TIORDREG         TO OBLAG-TIORDREG                          
128000     MOVE RY1-KDORDBEK         TO OBLAG-KDRESTR                           
128100     MOVE RY1-KVBEART          TO OBLAG-KVBEART                           
128200     MOVE RY1-KVLEVART         TO OBLAG-KVAVBART                          
128300     MOVE RY1-KDDSP            TO OBLAG-KDDSP                             
128400     IF RY1-KDORDBEK = +80 OR +81                                         
128500        MOVE ZERO              TO OBLAG-KVRO                              
128600     ELSE                                                                 
128700        MOVE RY1-KVAVART       TO OBLAG-KVRO                              
128800     END-IF                                                               
128900     MOVE RY1-KDFAKTYP         TO OBLAG-KDFAKTYP                          
129000     MOVE RY1-KDKVBRYT         TO OBLAG-KDKVBRYT                          
129100     MOVE LOGG6-TIAAMMDD       TO OBLAG-TIAAMMDD                          
129200     MOVE LOGG6-TIKLOCK        TO OBLAG-TIKLOCK                           
129300                                                                          
129400     MOVE RY1-TIDISPIN         TO OBLAG-TIDISPIN                          
129500     MOVE '007'                TO POSTSUM-TRANSTYP                        
129600     WRITE 007-UTPOST          FROM UT007-AREA                            
129700                                                                          
129800     MOVE 'W092ZQ'              TO POSTSUM-FDNAMN                         
129900     MOVE 'W09208E2'            TO POSTSUM-DDNAMN2                        
130000     CALL POSTSUM USING         POSTSUM-PARM                              
130100     .                                                                    
130200     EJECT                                                                
130300 C10H-GEN-EV-009-POST   SECTION.                                          
130400                                                                          
130500*------  SERVICEGRADSFILER TILL NOAC                                      
130600*------  FÖR DIREKTTRANSPORT DISTRIKT                                     
130700                                                                          
130800     MOVE RY1S-IDDISTR         TO TEST-IDDISTR DIS1-IDDISTR               
130900                                                                          
131000     CALL W460DIS1 USING DIS1-W460DIS1                                    
131100                                                                          
131200     IF (DIS1-KDSVAR = JA OR DIS130-NOAC)                                 
131300        IF (RY1-KDFAKTYP       = 'F' OR 'P') OR                           
131400           (RY1-IDKUNDRF-RO (1:5) NOT = ZERO AND                          
131500            RY1-TIRODAT           NOT = ZERO )                            
131600           CONTINUE                                                       
131700        ELSE                                                              
131800              IF (RY1-KDORDBEK     = +80 OR +90)                          
131900              AND RY1-KVLEVART     = ZERO                                 
132000                 PERFORM C10HA-MOVE-SERVICEGRAD-NOAC                      
132100              END-IF                                                      
132200        END-IF                                                            
132300     END-IF                                                               
132400     .                                                                    
132500     EJECT                                                                
132600 C10HA-MOVE-SERVICEGRAD-NOAC SECTION.                                     
132700     SKIP2                                                                
132800     MOVE '009'                 TO SERV-IDPTYP                            
132900     MOVE RY1S-IDDC             TO SERV-IDDC                              
133000     MOVE RY1S-IDDISTR          TO SERV-IDDISTR                           
133100     MOVE RY1S-IDKUNDNR         TO SERV-IDKUNDNR                          
133200     IF RY1-IDKUNDRF-RO (1:5)   NOT = ZERO                                
133300         MOVE RY1-IDKUNDRF-RO (1:5) TO SERV-IDORDNR                       
133400     ELSE                                                                 
133500         MOVE RY1-IDKUNDRF (1:5)    TO SERV-IDORDNR                       
133600     END-IF                                                               
133700     MOVE RY1S-KDORDKL          TO SERV-KDORDKL                           
133800     MOVE RY1-IDARTNR           TO SERV-IDARTNR                           
133900     MOVE RY1-REKSIFFR          TO SERV-REKSIFFR                          
134000     MOVE RY1-TIORDREG          TO SERV-TIORDREG                          
134100     MOVE ZERO                  TO SERV-KDPRODSL                          
134200     MOVE RY1-KVBEART           TO SERV-KVBEART                           
134300     MOVE RY1-KDFAKTYP          TO SERV-KDFAKTYP                          
134400                                                                          
134500     WRITE 009-UTPOST           FROM UT009-AREA                           
134600                                                                          
134700     MOVE 'W092ZP'              TO POSTSUM-FDNAMN                         
134800     MOVE 'W09208E1'            TO POSTSUM-DDNAMN2                        
134900     MOVE '009'                 TO POSTSUM-TRANSTYP                       
135000     CALL POSTSUM USING         POSTSUM-PARM                              
135100     .                                                                    
135200     EJECT                                                                
135300 C11-BEHANDLA-RY5-ANNUL         SECTION.                                  
135400                                                                          
135500*    HÄR BEHANDLAS RY5-TRANSAKTION SOM SKAPATS                            
135600*    VID ANNULATIONER.                                                    
135700*    FÖR VARJE RY5 TRANS KONTROLLERAS VIKEN ELLER VILKA                   
135800*    "RZ2-RZ7"-TRANSAKTIONER SOM SKALL SKAPAS.                            
135900*    DENNA ÄNDRING GJORD I SAMBAND MED KOLLIVIS PACKNING -86.             
136000                                                                          
136100     MOVE LOGG6-LOGGPOST        TO RY5-AREA                               
136200     MOVE LOGG6-SORTPOST        TO RY5S-WDGZRY5S-CTX                      
136300                                                                          
136400                                                                          
136500     IF RY5-FLLSBOK            = NEJ                                      
136600         CONTINUE                                                         
136700      ELSE                                                                
136800         PERFORM C11A-GEN-RZ5-ANNUL                                       
136900     END-IF                                                               
137000     PERFORM C11C-GEN-RZ7-ANNUL                                           
137100     PERFORM C11D-MOVE-STOPPAD-NOAC                                       
137200     MOVE RY5S-IDDISTR         TO TEST-IDDISTR                            
137300     IF RY5-FLORDSPE           =  JA    OR                                
137400        RY5-KDORDTYP           = 3      OR                                
137500        RY5-KDORDING           = 3      OR                                
137600        RY5-FLDIRLEV           = JA     OR                                
137700        DIST18-SKROT                    OR                                
137800        DIST19-SATS                     OR                                
137900        DIST20-EMBALLAGE                OR                                
138000        DIST93-BYTESRADIO-C2            OR                                
138100        (RY5-IDKUNDRF-RO       NOT = SPACE                                
138200         AND                   NOT = '00000     '                         
138300         AND  RY5-TIRODAT      > ZERO)             OR                     
138400        RY5-KVANNANT           = ZERO                                     
138500         CONTINUE                                                         
138600      ELSE                                                                
138700         PERFORM C11E-GEN-RZ1-ANNUL                                       
138800     END-IF                                                               
138900     PERFORM S50-SKRIV-W092S1-POST                                        
139000     MOVE RY5-IDDC              TO WS-IDDC                                
139100                                   W-IDDC-B6                              
139200     PERFORM IMS-GU-WDB601                                                
139300     IF DCS-CDC                                                           
139400     OR DCS-CDC-TR                                                        
139500        MOVE 1                  TO RY1X-KDCLAGER-S                        
139600     ELSE                                                                 
139700        MOVE 2                  TO RY1X-KDCLAGER-S                        
139800     END-IF                                                               
139900     MOVE RY5-IDPTYP            TO RY1X-IDPTYP-S                          
140000     MOVE RY5S-IDDISTR          TO RY1X-IDDISTR-S                         
140100     MOVE RY5S-IDKUNDNR         TO RY1X-IDKUNDNR-S                        
140200     MOVE RY5-KDFRAKT           TO RY1X-KDFRAKT-S                         
140300     MOVE RY5-IDKUNDRF (1:5)    TO RY1X-IDORDNR-S                         
140400     MOVE RY5-KDORDKL           TO RY1X-KDORDKL                           
140500     MOVE RY5-IDARTNR           TO RY1X-SORTBGP                           
140600     MOVE RY1X-RY1S-AREA        TO LOGG6-SORTPOST                         
140700     .                                                                    
140800     EJECT                                                                
140900 C11A-GEN-RZ5-ANNUL SECTION.                                              
141000     SKIP3                                                                
141100****  VID ANNULLATION                                                     
141200     MOVE RY5-IDDC                TO WS-IDDC                              
141300                                   W-IDDC-B6                              
141400     PERFORM IMS-GU-WDB601                                                
141500     IF  (DCS-SDC                                                         
141600     OR   DCS-NDC)                                                        
141800       CONTINUE                                                           
141900     ELSE                                                                 
142000       MOVE SPACE                 TO UTRZ5-AREA                           
142100                                                                          
142200       MOVE '229'                 TO UTRZ5-IDPTYP                         
142300       MOVE RY5-IDARTNR           TO UTRZ5-IDARTNR                        
142400       IF CDC                                                             
142500          MOVE 1                  TO UTRZ5-KDCLAGER                       
142600       ELSE                                                               
142700          MOVE 2                  TO UTRZ5-KDCLAGER                       
142800       END-IF                                                             
142900       IF DIST18-SKROT                                                    
143000           MOVE 7                 TO UTRZ5-KDRORELS                       
143100       ELSE                                                               
143200         IF DIST35-REFILL AND NOT                                         
143210           (DIST35-REFILL-NA OR                                           
143220            DIST35-REFILL-CN)                                             
143300           MOVE 2                 TO UTRZ5-KDRORELS                       
143400         ELSE                                                             
143500           MOVE 8                 TO UTRZ5-KDRORELS                       
143600         END-IF                                                           
143700       END-IF                                                             
143800       COMPUTE UTRZ5-KVANTAL      = (RY5-KVAVBART                         
143900                                  -  RY5-KVANNANT) * -1                   
144000       MOVE '2'                   TO UTRZ5-KDUPPD                         
144100                                                                          
144200       WRITE W092Z5-POST          FROM UTRZ5-AREA                         
144300                                                                          
144400       MOVE 'W092Z5'              TO POSTSUM-FDNAMN                       
144500       MOVE 'W09208DC'            TO POSTSUM-DDNAMN2                      
144600       MOVE UTRZ5-IDPTYP          TO POSTSUM-TRANSTYP                     
144700       CALL POSTSUM USING         POSTSUM-PARM                            
144800     END-IF                                                               
144900     .                                                                    
145000     EJECT                                                                
145100 C11C-GEN-RZ7-ANNUL   SECTION.                                            
145200                                                                          
145300     MOVE RY5S-IDDISTR          TO TEST-IDDISTR                           
145400     IF NOT DIST24-NORGE                                                  
145500       MOVE SPACE             TO SU3-AREA                                 
145600       MOVE 'SU3'             TO SU3-IDPTYP                               
145700*      COMPUTE SU3-KVBEART-002 = RY5-KVLEVART                             
145800*                             +  RY5-KVAVART                              
145900       MOVE RY5-KVANNANT      TO SU3-KVBEART-002                          
146000       MOVE +83               TO SU3-KDRESTR                              
146100                                                                          
146200       MOVE RY1S-IDDC             TO W-SU-IDDC                            
146300                                                                          
146400       PERFORM S40-RED-IDSUPPL-SU                                         
146500                                                                          
146600       MOVE RY5S-IDDISTR          TO SU3-IDDISTR                          
146700       MOVE RY5S-IDKUNDNR         TO SU3-IDKUNDNR                         
146800       MOVE IDSUPPL-SU-WS         TO SU3-IDSUPPL                          
146900                                                                          
147000       IF RY5-IDKUNDRF-RO (1:5)   >  ZERO                                 
147100           MOVE RY5-IDKUNDRF-RO (1:5)      TO SU3-IDORDNR-002             
147200           MOVE 1                          TO SU3-KDRO                    
147300       ELSE                                                               
147400           MOVE RY5-IDKUNDRF (1:5)         TO SU3-IDORDNR-002             
147500           MOVE ZERO                       TO SU3-KDRO                    
147600       END-IF                                                             
147700       MOVE RY5-IDARTNR           TO SU3-IDARTNR                          
147800       MOVE RY5-REKSIFFR          TO SU3-REKSIFFR                         
147900       MOVE RY5-KDFAKTYP          TO SU3-KDFAKTYP                         
148000       MOVE 2                     TO SU3-KDKVFOR                          
148100       MOVE RY5-KDORDKL           TO SU3-KDORDER                          
148200       MOVE RY5-IDKUNDRF (1:5)    TO SU3-IDORDNR7-LEV                     
148300       MOVE ZERO                  TO SU3-KVQPACK-1                        
148400       IF RY5-KDARTERS            =  ZERO                                 
148500           MOVE NEJ               TO SU3-TID-ERS                          
148600        ELSE                                                              
148700           MOVE RY5-KDARTERS      TO SU3-TID-ERS                          
148800       END-IF                                                             
148900       MOVE RY5-KDORDKL           TO SU3-KDORDKL                          
149000       MOVE ZERO                  TO SU3-FIKTIV-KVANT                     
149100       IF RY5S-KDTPOTYP           =  +1                                   
149200           IF RY5S-IDSYSTEM       =  'VR  '                               
149300               MOVE 1             TO SU3-KDVRTPO                          
149400            ELSE                                                          
149500               MOVE 2             TO SU3-KDVRTPO                          
149600           END-IF                                                         
149700        ELSE                                                              
149800           MOVE ZERO              TO SU3-KDVRTPO                          
149900       END-IF                                                             
150000       MOVE RY5-KDVRINFO          TO SU3-KDVRINFO                         
150100                                                                          
150200       MOVE LOGG6-TIAAMMDD        TO SU3-TIAAMMDD                         
150300       MOVE LOGG6-TIKLOCK         TO SU3-TIKLOCK                          
150400                                                                          
150500       WRITE W092Z7-POST          FROM SU3-AREA                           
150600                                                                          
150700       MOVE 'W092Z7'              TO POSTSUM-FDNAMN                       
150800       MOVE 'W09208DE'            TO POSTSUM-DDNAMN2                      
150900       MOVE SU3-IDPTYP            TO POSTSUM-TRANSTYP                     
151000       CALL POSTSUM USING         POSTSUM-PARM                            
151100     END-IF                                                               
151200     .                                                                    
151300     EJECT                                                                
151400 C11D-MOVE-STOPPAD-NOAC SECTION.                                          
151500     SKIP2                                                                
151600     MOVE '008'                 TO OBSTOP-IDPTYP                          
151700     MOVE RY5S-IDDISTR          TO OBSTOP-IDDISTR                         
151800     MOVE RY5S-IDKUNDNR         TO OBSTOP-IDKUNDNR                        
151900     MOVE RY5-KDFRAKT           TO OBSTOP-KDFRAKT                         
152000     MOVE RY5-IDKUNDRF (1:5)    TO OBSTOP-IDORDNR                         
152100     IF RY5-KDARTERS            = +0                                      
152200       IF RY5-IDKUNDRF-RO (1:5) = ZERO OR SPACE                           
152300         MOVE +0                TO OBSTOP-KDLIDEL                         
152400       ELSE                                                               
152500         MOVE +1                TO OBSTOP-KDLIDEL                         
152600       END-IF                                                             
152700     ELSE                                                                 
152800       MOVE +2                  TO OBSTOP-KDLIDEL                         
152900     END-IF                                                               
153000     MOVE RY5-IDDC              TO OBSTOP-IDDC                            
153100     MOVE RY5-IDARTNR           TO OBSTOP-IDARTNR                         
153200     MOVE SPACE                 TO OBSTOP-BEART                           
153300     MOVE RY5-BERADREF          TO OBSTOP-BERADREF                        
153400     MOVE RY5-BEVOLREF          TO OBSTOP-BEVOLREF                        
153500     IF RY5-IDKUNDRF-RO (1:5)   >  ZERO                                   
153600         MOVE RY5-IDKUNDRF-RO (1:5)  TO OBSTOP-IDRONR                     
153700     ELSE                                                                 
153800         MOVE ZERO                   TO OBSTOP-IDRONR                     
153900     END-IF                                                               
154000     MOVE RY5-KDDSP             TO OBSTOP-KDDSP                           
154100     MOVE RY5-REKSIFFR          TO OBSTOP-REKSIFFR                        
154200     MOVE RY5-TIRODAT           TO OBSTOP-TIRODAT                         
154300     MOVE RY5-TIORDREG          TO OBSTOP-TIORDREG                        
154400     MOVE RY5-KDKVBRYT          TO OBSTOP-KDKVBRYT                        
154500     MOVE 83                    TO OBSTOP-KDRESTR                         
154600     MOVE RY5-KVANNANT          TO OBSTOP-KVBEART                         
154700     MOVE RY5-KDFAKTYP          TO OBSTOP-KDFAKTYP                        
154800     MOVE LOGG6-TIAAMMDD        TO OBSTOP-TIAAMMDD                        
154900     MOVE LOGG6-TIKLOCK         TO OBSTOP-TIKLOCK                         
155000                                                                          
155100     MOVE '008'                 TO POSTSUM-TRANSTYP                       
155200     WRITE 008-UTPOST           FROM UT008-AREA                           
155300                                                                          
155400     MOVE 'W092ZQ'              TO POSTSUM-FDNAMN                         
155500     MOVE 'W09208E2'            TO POSTSUM-DDNAMN2                        
155600     CALL POSTSUM USING         POSTSUM-PARM                              
155700     .                                                                    
155800     EJECT                                                                
155900 C11E-GEN-RZ1-ANNUL   SECTION.                                            
156000     SKIP2                                                                
156100     MOVE RY5-IDDC              TO WS-IDDC                                
156200                                   W-IDDC-B6                              
156300     PERFORM IMS-GU-WDB601                                                
156400     IF  (DCS-CDC                                                         
156500     OR   DCS-CDC-TR                                                      
156600     OR   DCS-SDC)                                                        
156700     MOVE SPACE                 TO UTRZ1-AREA                             
156800                                                                          
156900     MOVE '232'                 TO UTRZ1-IDPTYP                           
157000     MOVE RY5-IDARTNR           TO UTRZ1-IDARTNR                          
157100     MOVE RY5S-IDDISTR          TO UTRZ1-IDDISTR                          
157200     IF CDC                                                               
157300        MOVE 1                  TO UTRZ1-KDCLAGER                         
157400     ELSE                                                                 
157500        MOVE 2                  TO UTRZ1-KDCLAGER                         
157600     END-IF                                                               
157700     MOVE RY5-KDORDKL           TO UTRZ1-KDORDKL                          
157800     MOVE -1                    TO UTRZ1-REINKORD                         
157900     MOVE ZERO                  TO UTRZ1-REFYSAVV                         
158000     IF RY5-KVAVBART >= RY5-KVBEART - RY5S-KVSLATT                        
158100***    OM RAD LÅG INOM SLATT FÖRE ANNULLATION                             
158200         MOVE -1                TO UTRZ1-REAVBRAD                         
158300         MOVE ZERO              TO UTRZ1-RERORAD                          
158400     ELSE                                                                 
158500***    OM RAD HADE BRIST UTANFÖR SLATT FÖRE ANNULLATION                   
158600         COMPUTE UTRZ1-REAVBRAD     =                                     
158700           (RY5-KVAVBART / RY5-KVBEART) * -1                              
158800         END-COMPUTE                                                      
158900         COMPUTE UTRZ1-RERORAD      =                                     
159000           ((RY5-KVBEART - RY5-KVAVBART) / RY5-KVBEART) * -1              
159100         END-COMPUTE                                                      
159200     END-IF                                                               
159300                                                                          
159400     IF RY5-KVANNANT < RY5-KVAVBART                                       
159500***    END. TRANS VID DELANNULLATION                                      
159600         MOVE '232'                 TO UTRZ1-IDPTYP                       
159700         MOVE RY5-IDARTNR           TO UTRZ1-IDARTNR                      
159800         MOVE RY5S-IDDISTR          TO UTRZ1-IDDISTR                      
159900         IF CDC                                                           
160000            MOVE 1                  TO UTRZ1-KDCLAGER                     
160100         ELSE                                                             
160200            MOVE 2                  TO UTRZ1-KDCLAGER                     
160300         END-IF                                                           
160400         MOVE RY5-KDORDKL           TO UTRZ1-KDORDKL                      
160500         MOVE +1                    TO UTRZ1-REINKORD                     
160600         MOVE ZERO                  TO UTRZ1-REFYSAVV                     
160700         IF RY5-KVAVBART - RY5-KVANNANT >=                                
160800            RY5-KVBEART - RY5S-KVSLATT                                    
160900***         RAD LIGGER INOM SLATT ÄVEN EFTER DELANN.                      
161000             MOVE ZERO              TO UTRZ1-RERORAD                      
161100             MOVE +1                TO UTRZ1-REAVBRAD                     
161200         ELSE                                                             
161300             COMPUTE UTRZ1-RERORAD  =                                     
161400                        (RY5-KVBEART - RY5-KVAVBART) /                    
161500                        (RY5-KVBEART - RY5-KVANNANT)                      
161600                        ON SIZE ERROR MOVE ZERO TO UTRZ1-RERORAD          
161700             END-COMPUTE                                                  
161800             COMPUTE UTRZ1-REAVBRAD =                                     
161900                        (RY5-KVBEART - RY5-KVANNANT -                     
162000                         (RY5-KVBEART - RY5-KVAVBART)) /                  
162100                        (RY5-KVBEART  - RY5-KVANNANT)                     
162200                        ON SIZE ERROR MOVE ZERO TO UTRZ1-REAVBRAD         
162300             END-COMPUTE                                                  
162400         END-IF                                                           
162500                                                                          
162600     END-IF                                                               
162700     END-IF                                                               
162800     .                                                                    
162900     EJECT                                                                
163000 C19-BEHANDLA-RZA SECTION.                                                
163100     SKIP2                                                                
163200     MOVE LOGG6-LOGGPOST       TO RZA-AREA                                
163300     MOVE LOGG6-SORTPOST       TO RZAS-W092P001-CTX                       
163400                                                                          
163500     EVALUATE RZA-IDSYSTEM                                                
163600         WHEN 'VR'                                                        
163700             IF RZA-KDTPOTYP = 1 OR 2                                     
163800                 PERFORM C19A-SKAPA-SU1-POST                              
163900             END-IF                                                       
164000             PERFORM C19B-SKAPA-015-NOAC-POST                             
164100             PERFORM C19C-SKAPA-020-NOAC-POST                             
164200         WHEN 'VIPS'                                                      
164300             IF RZA-KDTPOTYP = 1 OR 2                                     
164400                 PERFORM C19B-SKAPA-015-NOAC-POST                         
164500                 PERFORM C19C-SKAPA-020-NOAC-POST                         
164600             END-IF                                                       
164700             PERFORM C19A-SKAPA-SU1-POST                                  
164800         WHEN 'VDI'                                                       
164900             IF RZA-KDTPOTYP = 1 OR 2                                     
165000                 PERFORM C19B-SKAPA-015-NOAC-POST                         
165100                 PERFORM C19C-SKAPA-020-NOAC-POST                         
165200             END-IF                                                       
165300             PERFORM C19A-SKAPA-SU1-POST                                  
165400         WHEN OTHER                                                       
165500             PERFORM C19A-SKAPA-SU1-POST                                  
165600             PERFORM C19B-SKAPA-015-NOAC-POST                             
165700             PERFORM C19C-SKAPA-020-NOAC-POST                             
165800     END-EVALUATE                                                         
165900     .                                                                    
166000     EJECT                                                                
166100 C19A-SKAPA-SU1-POST  SECTION.                                            
166200                                                                          
166300     MOVE RZAS-IDDISTR-S        TO TEST-IDDISTR                           
166400                                   WS-IDDISTR                             
166500     IF NOT DIST24-NORGE                                                  
166600       MOVE SPACE                 TO  SU1-AREA                            
166700       MOVE 'SU1'                 TO  SU1-IDPTYP                          
166800       MOVE RZA-KVBEART           TO  SU1-KVBEART-002                     
166900                                                                          
167000       MOVE RZA-IDDC              TO W-SU-IDDC                            
167100                                                                          
167200       PERFORM S40-RED-IDSUPPL-SU                                         
167300                                                                          
167400       MOVE WS-IDDISTR            TO SU1-IDDISTR                          
167500       MOVE RZAS-IDKUNDNR-S       TO WS-IDKUNDNR                          
167600       MOVE WS-IDKUNDNR           TO SU1-IDKUNDNR                         
167700       MOVE IDSUPPL-SU-WS         TO SU1-IDSUPPL                          
167800                                                                          
167900       MOVE RZA-IDKUNDRF          TO W-IDKUNDRF                           
168000       IF RZA-IDKUNDRF (1:7)     NUMERIC                                  
168100           MOVE RZA-IDKUNDRF (1:7)  TO SU1-IDORDNR-002                    
168200        ELSE                                                              
168300           MOVE RZA-IDKUNDRF (1:5)  TO SU1-IDORDNR-002                    
168400       END-IF                                                             
168500       MOVE RZA-IDARTNR           TO SU1-IDARTNR                          
168600       MOVE RZA-REKSIFFR          TO SU1-REKSIFFR                         
168700       MOVE RZA-KVBEART           TO SU1-KVBEART-002                      
168800       MOVE RZA-KDORDKL           TO SU1-KDORDER                          
168900       MOVE RZA-KDTPOTYP          TO SU1-KDTPOTYP                         
169000       MOVE RZA-KDFAKTYP          TO SU1-KDFAKTYP                         
169100       MOVE RZA-KDMANPR           TO SU1-KDMANPR                          
169200       MOVE RZA-KDVRTPO           TO SU1-KDVRTPO                          
169300       MOVE RZA-KDVRINFO          TO SU1-KDVRINFO                         
169400                                                                          
169500       MOVE LOGG6-TIAAMMDD        TO SU1-TIAAMMDD-REG                     
169600       MOVE LOGG6-TIKLOCK         TO SU1-TIKLOCK-REG                      
169700                                                                          
169800       WRITE W092Z7-POST          FROM SU1-AREA                           
169900                                                                          
170000       MOVE 'W092Z7'              TO POSTSUM-FDNAMN                       
170100       MOVE 'W09208DE'            TO POSTSUM-DDNAMN2                      
170200       MOVE SU1-IDPTYP            TO POSTSUM-TRANSTYP                     
170300       CALL POSTSUM USING         POSTSUM-PARM                            
170400     END-IF                                                               
170500     .                                                                    
170600     EJECT                                                                
170700 C19B-SKAPA-015-NOAC-POST  SECTION.                                       
170800                                                                          
170900     MOVE SPACE                TO BYPASS-W461015                          
171000                                                                          
171100     MOVE '015'                TO BYPASS-IDPTYP                           
171200     MOVE RZAS-IDDISTR-S       TO BYPASS-IDDISTR                          
171300     MOVE RZAS-IDKUNDNR-S      TO BYPASS-IDKUNDNR                         
171400     IF RZA-IDKUNDRF (1:7)     NUMERIC                                    
171500         MOVE RZA-IDKUNDRF (1:7)   TO BYPASS-IDORDNR                      
171600      ELSE                                                                
171700         MOVE RZA-IDKUNDRF (1:5)   TO BYPASS-IDORDNR                      
171800     END-IF                                                               
171900     MOVE RZA-KDORDKL          TO BYPASS-KDORDKL                          
172000     MOVE RZA-IDARTNR          TO BYPASS-IDARTNR                          
172100     MOVE RZA-REKSIFFR         TO BYPASS-REKSIFFR                         
172200     MOVE RZA-BERADREF         TO BYPASS-BERADREF                         
172300     MOVE RZA-BEVOLREF         TO BYPASS-BEVOLREF                         
172400     MOVE RZA-KVBEART          TO BYPASS-KVBEART                          
172500     MOVE RZA-KDFAKTYP         TO BYPASS-KDFAKTYP                         
172600     MOVE RZA-KDDSP            TO BYPASS-KDDSP                            
172700     MOVE RZA-FLABON           TO BYPASS-FLABON                           
172800     MOVE RZA-KDTPOTYP         TO BYPASS-KDTPOTYP                         
172900                                                                          
173000     WRITE W092X3-POST          FROM BYPASS-W461015                       
173100                                                                          
173200     MOVE 'W092X3'              TO POSTSUM-FDNAMN                         
173300     MOVE 'W09208EE'            TO POSTSUM-DDNAMN2                        
173400     MOVE BYPASS-IDPTYP         TO POSTSUM-TRANSTYP                       
173500     CALL POSTSUM USING         POSTSUM-PARM                              
173600     .                                                                    
173700     EJECT                                                                
173800 C19C-SKAPA-020-NOAC-POST  SECTION.                                       
173900     MOVE SPACE                TO ONORD-W461020                           
174000                                                                          
174100     MOVE '020'                TO ONORD-IDPTYP                            
174200     MOVE RZAS-IDDISTR-S       TO ONORD-IDDISTR                           
174300     MOVE RZAS-IDKUNDNR-S      TO ONORD-IDKUNDNR                          
174400     IF RZA-IDKUNDRF (1:7)     NUMERIC                                    
174500         MOVE RZA-IDKUNDRF (1:7)  TO ONORD-IDORDNR                        
174600      ELSE                                                                
174700         MOVE RZA-IDKUNDRF (1:5)  TO ONORD-IDORDNR                        
174800     END-IF                                                               
174900     MOVE RZA-IDDC             TO ONORD-IDDC                              
175000     MOVE RZA-KDORDKL          TO ONORD-KDORDKL                           
175100     MOVE RZA-BEVARREF         TO ONORD-BEVARREF                          
175200     MOVE RZA-BEVOLREF         TO ONORD-BEVOLREF                          
175300     MOVE RZA-BERADREF         TO ONORD-BERADREF                          
175400     MOVE RZA-IDARTNR          TO ONORD-IDARTNR                           
175500     MOVE RZA-REKSIFFR         TO ONORD-REKSIFFR                          
175600     MOVE RZA-KVBEART          TO ONORD-KVBEART                           
175700     MOVE RZA-TIORDREG         TO ONORD-TIORDREG                          
175800     MOVE ZERO                 TO ONORD-IDRONR                            
175900     MOVE ZERO                 TO ONORD-TIRODAT                           
176000     MOVE RZA-PRARTNTO         TO ONORD-PRARTNTO                          
176100     MOVE RZA-PRARTBTO-EXP     TO ONORD-PRARTBTO-EXP                      
176200     MOVE RZA-KDPRODSL         TO ONORD-KDPRODSL                          
176300     MOVE RZA-IDFKNGRP         TO ONORD-IDFKNGRP                          
176400     MOVE RZA-FLINVEST         TO ONORD-FLINVEST                          
176500     MOVE RZA-KDDSP            TO ONORD-KDDSP                             
176600     MOVE RZA-KDMANPR          TO ONORD-KDMANPR                           
176700     MOVE RZA-FLABON           TO ONORD-FLABON                            
176800     MOVE RZA-KDTPOTYP         TO ONORD-KDTPOTYP                          
176900                                                                          
177000     WRITE W092X4-POST          FROM ONORD-W461020                        
177100                                                                          
177200     MOVE 'W092X4'              TO POSTSUM-FDNAMN                         
177300     MOVE 'W09208EF'            TO POSTSUM-DDNAMN2                        
177400     MOVE ONORD-IDPTYP          TO POSTSUM-TRANSTYP                       
177500     CALL POSTSUM USING         POSTSUM-PARM                              
177600     .                                                                    
177700     EJECT                                                                
177800 C34-SKRIV-TULL-POST-001 SECTION.                                         
177900     SKIP2                                                                
178000     MOVE LOGG6-LOGGPOST TO UTRZQ-AREA                                    
178100     MOVE '001' TO UTRZQ-IDPTYP                                           
178200                                                                          
178300     WRITE W001-UTPOST   FROM UTRZQ-AREA                                  
178400                                                                          
178500                                                                          
178600     MOVE 'W092ZI'    TO POSTSUM-FDNAMN                                   
178700     MOVE 'W09208DP'  TO POSTSUM-DDNAMN2                                  
178800     MOVE LOGG6-IDPTYP TO POSTSUM-TRANSTYP                                
178900     CALL POSTSUM USING POSTSUM-PARM                                      
179000     .                                                                    
179100     EJECT                                                                
179200 C35-SKRIV-TULL-POST-002 SECTION.                                         
179300     SKIP2                                                                
179400     MOVE LOGG6-LOGGPOST TO UTRZR-AREA                                    
179500                                                                          
179600                                                                          
179700     MOVE '002' TO UTRZR-IDPTYP                                           
179800                                                                          
179900     WRITE R002-UTPOST  FROM  UTRZR-AREA                                  
180000                                                                          
180100     MOVE 'W092ZI'    TO POSTSUM-FDNAMN                                   
180200     MOVE 'W09208DP'  TO POSTSUM-DDNAMN2                                  
180300     MOVE LOGG6-IDPTYP TO POSTSUM-TRANSTYP                                
180400     CALL POSTSUM USING POSTSUM-PARM                                      
180500     .                                                                    
180600     EJECT                                                                
180700 C39-SKRIV-KDP-POST SECTION.                                              
180800     SKIP2                                                                
180900     MOVE LOGG6-LOGGPOST        TO WS-KDP-AREA                            
181000                                                                          
181100     MOVE WS-KDP-IDARTNR        TO UTKDP-IDARTNR                          
181200     MOVE WS-KDP-KDUART         TO UTKDP-KDUART                           
181300     MOVE LOGG6-TIKLOCK         TO UTKDP-TIKLOCK                          
181400     WRITE KDP-UTPOST FROM UTKDP-POST                                     
181500                                                                          
181600                                                                          
181700     MOVE 'W092ZU'              TO POSTSUM-FDNAMN                         
181800     MOVE 'W09208E6'            TO POSTSUM-DDNAMN2                        
181900     MOVE LOGG6-IDPTYP          TO POSTSUM-TRANSTYP                       
182000     CALL POSTSUM USING         POSTSUM-PARM                              
182100     .                                                                    
182200     EJECT                                                                
182300 C40-SKRIV-RZV-POST SECTION.                                              
182400     SKIP2                                                                
182500     MOVE LOGG6-LOGGPOST        TO WS-RZV-AREA                            
182600     MOVE WS-RZV-IDARTNR        TO UTRZV-IDARTNR                          
182700     MOVE WS-RZV-IDLEVNR        TO UTRZV-IDLEVNR                          
182800     MOVE WS-RZV-BELEV          TO UTRZV-BELEV                            
182900     MOVE WS-RZV-IDBERED        TO UTRZV-IDBERED                          
183000     WRITE RZV-UTPOST FROM UTRZV-W111RZV                                  
183100                                                                          
183200                                                                          
183300     MOVE 'W092ZV'              TO POSTSUM-FDNAMN                         
183400     MOVE 'W09208E7'            TO POSTSUM-DDNAMN2                        
183500     MOVE LOGG6-IDPTYP          TO POSTSUM-TRANSTYP                       
183600     CALL POSTSUM USING         POSTSUM-PARM                              
183700     .                                                                    
183800     EJECT                                                                
183900 C41-SKRIV-RY2-POST SECTION.                                              
184000     SKIP2                                                                
184100     MOVE LOGG6-LOGGPOST        TO UTRY2-A310B65                          
184200     MOVE 'B65'                 TO UTRY2-KT                               
184300     WRITE UTRY2-UTPOST FROM       UTRY2-A310B65                          
184400                                                                          
184500                                                                          
184600     MOVE 'W092ZR'              TO POSTSUM-FDNAMN                         
184700     MOVE 'W09208E3'            TO POSTSUM-DDNAMN2                        
184800     MOVE LOGG6-IDPTYP          TO POSTSUM-TRANSTYP                       
184900                                                                          
185000     CALL POSTSUM USING         POSTSUM-PARM                              
185100     .                                                                    
185200     EJECT                                                                
185300 C45-SKRIV-RY6-POST SECTION.                                              
185400     SKIP2                                                                
185500     MOVE LOGG6-LOGGPOST        TO RY6-AREA                               
185600     WRITE UTRY6-UTPOST FROM       RY6-AREA                               
185700                                                                          
185800                                                                          
185900     MOVE 'W092ZW'              TO POSTSUM-FDNAMN                         
186000     MOVE 'W09208EA'            TO POSTSUM-DDNAMN2                        
186100     MOVE LOGG6-IDPTYP          TO POSTSUM-TRANSTYP                       
186200                                                                          
186300     CALL POSTSUM USING         POSTSUM-PARM                              
186400     .                                                                    
186500     EJECT                                                                
186600 C47-SKRIV-RY4-POST SECTION.                                              
186700     SKIP2                                                                
186800     MOVE LOGG6-LOGGPOST        TO RY4-AREA                               
186900                                                                          
187000     MOVE RY4-IDDISTR           TO TEST-IDDISTR                           
187100     IF NOT DIST24-NORGE                                                  
187200       MOVE SPACE                 TO SU5-AREA                             
187300       MOVE 'SU5'                 TO SU5-IDPTYP                           
187400                                                                          
187500       MOVE RY4-IDDC              TO W-SU-IDDC                            
187600                                                                          
187700       PERFORM S40-RED-IDSUPPL-SU                                         
187800                                                                          
187900       MOVE RY4-IDDISTR           TO SU5-IDDISTR                          
188000       MOVE RY4-IDKUNDNR          TO SU5-IDKUNDNR                         
188100       MOVE IDSUPPL-SU-WS         TO SU5-IDSUPPL                          
188200                                                                          
188300       MOVE RY4-IDRONR            TO SU5-IDRONR                           
188400       MOVE RY4-IDARTNR           TO SU5-IDARTNR                          
188500                                                                          
188600       MOVE RY4-IDARTNR           TO  FLT                                 
188700       PERFORM S25-CALL-W009KSIF                                          
188800       MOVE KSIFF                 TO SU5-REKSIFFR                         
188900                                                                          
189000       MOVE RY4-KVRO              TO SU5-KVRO                             
189100       MOVE RY4-KDORDKL           TO SU5-KDORDER                          
189200       MOVE RY4-KDFAKTYP          TO SU5-KDFAKTYP                         
189300       MOVE RY4-KDORDKL           TO SU5-KDORDKL                          
189400       MOVE RY4-KDVRINFO          TO SU5-KDVRINFO                         
189500                                                                          
189600       MOVE LOGG6-TIAAMMDD        TO SU5-TIAAMMDD-REG                     
189700       MOVE LOGG6-TIKLOCK         TO SU5-TIKLOCK-REG                      
189800                                                                          
189900       WRITE W092Z7-POST          FROM SU5-AREA                           
190000                                                                          
190100                                                                          
190200       MOVE 'W092Z7'              TO POSTSUM-FDNAMN                       
190300       MOVE 'W09208DE'            TO POSTSUM-DDNAMN2                      
190400       MOVE SU5-IDPTYP            TO POSTSUM-TRANSTYP                     
190500       CALL POSTSUM USING         POSTSUM-PARM                            
190600     END-IF                                                               
190700     .                                                                    
190800     EJECT                                                                
190900 C48-SKRIV-RY9-POST SECTION.                                              
191000     SKIP2                                                                
191100     MOVE LOGG6-SORTPOST       TO  RY9S-WDGZRY9S                          
191200     MOVE LOGG6-LOGGPOST       TO  RY9-WDGZRY9                            
191300                                                                          
191400     MOVE RY9-IDARTNR          TO  FLT                                    
191500     PERFORM S25-CALL-W009KSIF                                            
191600                                                                          
191700                                                                          
191800     IF (RY9-KDTPOTYP          =   1 OR 2 OR 3 OR 4 OR 5)                 
191900         IF RY9-KDSTARAD       =   '1'                                    
192000             PERFORM C48B-SKAPA-SUD-POST                                  
192100         END-IF                                                           
192200     END-IF                                                               
192300                                                                          
192400     IF RY9-KDTPOTYP          =  6                                        
192500         IF RY9S-KDORDBEK      =  71 OR 77                                
192600             PERFORM C48C-MOVE-BRIST-NOAC                                 
192700             PERFORM C48F-SKAPA-SU4-POST                                  
192800         END-IF                                                           
192900     END-IF                                                               
193000                                                                          
193100     IF  RY9-KDTPOTYP          =  6  OR                                   
193200         RY9-KDSTARAD          = '2' OR                                   
193300         RY9-KDSTARAD          = '3'                                      
193400         IF RY9S-KDORDBEK      = 85 OR 87 OR 83                           
193500             PERFORM C48E-MOVE-STOPPAD-NOAC                               
193600             PERFORM C48D-SKAPA-SU3-POST                                  
193700         END-IF                                                           
193800     END-IF                                                               
193900                                                                          
194000     IF (RY9S-KDORDBEK = 85  OR                                           
194100         RY9S-KDORDBEK = 87) AND                                          
194200         RY9-TIRODAT   > ZERO                                             
194300       PERFORM C48G-SKAPA-BORS-RO-POST                                    
194400     END-IF                                                               
194500     .                                                                    
194600     EJECT                                                                
194700 C48B-SKAPA-SUD-POST   SECTION.                                           
194800                                                                          
194900     MOVE LOGG6-LOGGPOST        TO  RY9-AREA                              
195000                                                                          
195100     MOVE RY9S-IDDISTR          TO TEST-IDDISTR                           
195200     IF NOT DIST24-NORGE                                                  
195300       MOVE SPACE                 TO SUD-AREA                             
195400       MOVE 'SUD'                 TO SUD-IDPTYP                           
195500                                                                          
195600       MOVE RY9S-IDDC             TO W-SU-IDDC                            
195700                                                                          
195800       PERFORM S40-RED-IDSUPPL-SU                                         
195900                                                                          
196000       MOVE RY9S-IDDISTR          TO SUD-IDDISTR                          
196100       MOVE RY9S-IDKUNDNR         TO SUD-IDKUNDNR                         
196200       MOVE IDSUPPL-SU-WS         TO SUD-IDSUPPL                          
196300                                                                          
196400       MOVE RY9-IDKUNDRF (1:5)    TO SUD-IDORDNR-002                      
196500       MOVE RY9-IDARTNR           TO SUD-IDARTNR                          
196600       MOVE KSIFF                 TO SUD-REKSIFFR                         
196700       MOVE RY9-KVART             TO SUD-KVBEART-002                      
196800       MOVE RY9-TITPO             TO SUD-TITPO                            
196900       MOVE RY9-KDTPOTYP          TO SUD-KDTPOTYP                         
197000       MOVE RY9-KDVRTPO           TO SUD-KDVRTPO                          
197100       MOVE RY9-KDVRINFO          TO SUD-KDVRINFO                         
197200                                                                          
197300       MOVE LOGG6-TIAAMMDD        TO SUD-TIAAMMDD                         
197400       MOVE LOGG6-TIKLOCK         TO SUD-TIKLOCK                          
197500                                                                          
197600       WRITE W092Z7-POST FROM SUD-AREA                                    
197700                                                                          
197800       MOVE 'W092Z7'              TO POSTSUM-FDNAMN                       
197900       MOVE 'W09208DE'            TO POSTSUM-DDNAMN2                      
198000       MOVE LOGG6-IDPTYP          TO POSTSUM-TRANSTYP                     
198100       CALL POSTSUM USING         POSTSUM-PARM                            
198200     END-IF                                                               
198300     .                                                                    
198400     EJECT                                                                
198500 C48C-MOVE-BRIST-NOAC  SECTION.                                           
198600                                                                          
198700     MOVE LOGG6-LOGGPOST       TO  RY9-AREA                               
198800                                                                          
198900     MOVE '007'                TO OBLAG-IDPTYP                            
199000     MOVE RY9S-IDDISTR         TO OBLAG-IDDISTR                           
199100     MOVE RY9S-IDKUNDNR        TO OBLAG-IDKUNDNR                          
199200     MOVE RY9S-KDFRAKT         TO OBLAG-KDFRAKT                           
199300     MOVE RY9-IDKUNDRF (1:5)   TO OBLAG-IDORDNR                           
199400     MOVE +0                   TO OBLAG-KDLIDEL                           
199500     MOVE RY9S-IDDC            TO OBLAG-IDDC                              
199600     MOVE RY9-IDARTNR          TO OBLAG-IDARTNR                           
199700     MOVE KSIFF                TO OBLAG-REKSIFFR                          
199800     MOVE SPACE                TO OBLAG-BEART                             
199900     MOVE RY9-BERADREF         TO OBLAG-BERADREF                          
200000     MOVE RY9-BEVOLREF         TO OBLAG-BEVOLREF                          
200100     MOVE ZERO                 TO OBLAG-IDRONR                            
200200     MOVE RY9-TIRODAT          TO OBLAG-TIRODAT                           
200300     MOVE RY9-TIREGDAT         TO OBLAG-TIORDREG                          
200400     MOVE RY9S-KDORDBEK        TO OBLAG-KDRESTR                           
200500     MOVE RY9-KVART            TO OBLAG-KVBEART                           
200600     MOVE ZERO                 TO OBLAG-KVAVBART                          
200700     MOVE RY9-KDDSP            TO OBLAG-KDDSP                             
200800     MOVE RY9-KVART            TO OBLAG-KVRO                              
200900     MOVE RY9-KDFAKTYP         TO OBLAG-KDFAKTYP                          
201000     MOVE RY9-KDKVBRYT         TO OBLAG-KDKVBRYT                          
201100     MOVE RY9-TITPO            TO OBLAG-TIDISPIN                          
201200     MOVE LOGG6-TIAAMMDD       TO OBLAG-TIAAMMDD                          
201300     MOVE LOGG6-TIKLOCK        TO OBLAG-TIKLOCK                           
201400                                                                          
201500     MOVE '007'                 TO POSTSUM-TRANSTYP                       
201600     WRITE 007-UTPOST           FROM UT007-AREA                           
201700                                                                          
201800     MOVE 'W092ZQ'              TO POSTSUM-FDNAMN                         
201900     MOVE 'W09208E2'            TO POSTSUM-DDNAMN2                        
202000     CALL POSTSUM USING         POSTSUM-PARM                              
202100     .                                                                    
202200     EJECT                                                                
202300 C48D-SKAPA-SU3-POST    SECTION.                                          
202400                                                                          
202500     MOVE RY9S-IDDISTR          TO TEST-IDDISTR                           
202600     IF NOT DIST24-NORGE                                                  
202700       MOVE SPACE                 TO SU3-AREA                             
202800       MOVE 'SU3'                 TO SU3-IDPTYP                           
202900       MOVE RY9-KVART             TO SU3-KVBEART-002                      
203000                                                                          
203100       MOVE RY9S-KDORDBEK         TO SU3-KDRESTR                          
203200                                                                          
203300       IF RY9S-KDORDBEK = 83                                              
203400          MOVE 0                  TO SU3-KDRO                             
203500       ELSE                                                               
203600          IF RY9-KDSTARAD            =  '2'                               
203700             MOVE 2                 TO SU3-KDRO                           
203800          ELSE                                                            
203900            IF RY9-KDTPOTYP = 6 AND RY9S-KDORDBEK = 85                    
204000               MOVE 2               TO SU3-KDRO                           
204100            ELSE                                                          
204200               MOVE 1               TO SU3-KDRO                           
204300            END-IF                                                        
204400          END-IF                                                          
204500       END-IF                                                             
204600                                                                          
204700       MOVE RY9S-IDDC             TO W-SU-IDDC                            
204800                                                                          
204900       PERFORM S40-RED-IDSUPPL-SU                                         
205000                                                                          
205100       MOVE RY9S-IDDISTR          TO SU3-IDDISTR                          
205200       MOVE RY9S-IDKUNDNR         TO SU3-IDKUNDNR                         
205300       MOVE IDSUPPL-SU-WS         TO SU3-IDSUPPL                          
205400                                                                          
205500       MOVE RY9-IDKUNDRF (1:5)    TO SU3-IDORDNR-002                      
205600       MOVE RY9-IDARTNR           TO SU3-IDARTNR                          
205700       MOVE KSIFF                 TO SU3-REKSIFFR                         
205800       MOVE RY9-KDFAKTYP          TO SU3-KDFAKTYP                         
205900       MOVE 2                     TO SU3-KDKVFOR                          
206000       MOVE RY9S-KDORDKL          TO SU3-KDORDER                          
206100       MOVE ZERO                  TO SU3-IDORDNR7-LEV                     
206200       MOVE ZERO                  TO SU3-KVQPACK-1                        
206300       MOVE RY9-FLERS             TO SU3-TID-ERS                          
206400       MOVE RY9S-KDORDKL          TO SU3-KDORDKL                          
206500       MOVE ZERO                  TO SU3-FIKTIV-KVANT                     
206600       MOVE RY9-KDVRTPO           TO SU3-KDVRTPO                          
206700       MOVE RY9-KDVRINFO          TO SU3-KDVRINFO                         
206800                                                                          
206900       MOVE LOGG6-TIAAMMDD        TO SU3-TIAAMMDD                         
207000       MOVE LOGG6-TIKLOCK         TO SU3-TIKLOCK                          
207100                                                                          
207200       WRITE W092Z7-POST          FROM SU3-AREA                           
207300                                                                          
207400       MOVE 'W092Z7'              TO POSTSUM-FDNAMN                       
207500       MOVE 'W09208DE'            TO POSTSUM-DDNAMN2                      
207600       MOVE SU3-IDPTYP            TO POSTSUM-TRANSTYP                     
207700       CALL POSTSUM USING         POSTSUM-PARM                            
207800     END-IF                                                               
207900     .                                                                    
208000     EJECT                                                                
208100 C48E-MOVE-STOPPAD-NOAC SECTION.                                          
208200                                                                          
208300     MOVE '008'                 TO OBSTOP-IDPTYP                          
208400     MOVE RY9S-IDDISTR          TO OBSTOP-IDDISTR                         
208500     MOVE RY9S-IDKUNDNR         TO OBSTOP-IDKUNDNR                        
208600     MOVE RY9S-KDFRAKT          TO OBSTOP-KDFRAKT                         
208700     MOVE RY9-IDKUNDRF   (1:5)  TO OBSTOP-IDORDNR                         
208800     MOVE ZERO                  TO OBSTOP-KDLIDEL                         
208900     MOVE RY9S-IDDC             TO OBSTOP-IDDC                            
209000     MOVE RY9-IDARTNR           TO OBSTOP-IDARTNR                         
209100     MOVE KSIFF                 TO OBSTOP-REKSIFFR                        
209200     MOVE SPACE                 TO OBSTOP-BEART                           
209300     MOVE RY9-BERADREF          TO OBSTOP-BERADREF                        
209400     MOVE RY9-BEVOLREF          TO OBSTOP-BEVOLREF                        
209500     MOVE RY9-IDKUNDRF    (1:5) TO OBSTOP-IDRONR                          
209600                                                                          
209700     MOVE RY9-TIRODAT           TO OBSTOP-TIRODAT                         
209800     MOVE RY9-TIREGDAT          TO OBSTOP-TIORDREG                        
209900     MOVE RY9-KDDSP             TO OBSTOP-KDDSP                           
210000     MOVE RY9-KDKVBRYT          TO OBSTOP-KDKVBRYT                        
210100     MOVE RY9S-KDORDBEK         TO OBSTOP-KDRESTR                         
210200     MOVE RY9-KVART             TO OBSTOP-KVBEART                         
210300     MOVE RY9-KDFAKTYP          TO OBSTOP-KDFAKTYP                        
210400     MOVE LOGG6-TIAAMMDD        TO OBSTOP-TIAAMMDD                        
210500     MOVE LOGG6-TIKLOCK         TO OBSTOP-TIKLOCK                         
210600                                                                          
210700     MOVE '008'                 TO POSTSUM-TRANSTYP                       
210800     WRITE 008-UTPOST           FROM UT008-AREA                           
210900                                                                          
211000     MOVE 'W092ZQ'              TO POSTSUM-FDNAMN                         
211100     MOVE 'W09208E2'            TO POSTSUM-DDNAMN2                        
211200     CALL POSTSUM USING         POSTSUM-PARM                              
211300     .                                                                    
211400     EJECT                                                                
211500 C48F-SKAPA-SU4-POST   SECTION.                                           
211600                                                                          
211700     MOVE RY9S-IDDISTR          TO TEST-IDDISTR                           
211800     IF NOT DIST24-NORGE                                                  
211900       MOVE SPACE                 TO SU4-AREA                             
212000       MOVE 'SU4'                 TO SU4-IDPTYP                           
212100                                                                          
212200       MOVE ZERO                  TO SU4-KDRESTR                          
212300                                                                          
212400       MOVE RY9S-IDDC             TO W-SU-IDDC                            
212500                                                                          
212600       PERFORM S40-RED-IDSUPPL-SU                                         
212700                                                                          
212800       MOVE RY9S-IDDISTR          TO SU4-IDDISTR                          
212900       MOVE RY9S-IDKUNDNR         TO SU4-IDKUNDNR                         
213000       MOVE IDSUPPL-SU-WS         TO SU4-IDSUPPL                          
213100                                                                          
213200       MOVE RY9-IDKUNDRF (1:5)    TO SU4-IDORDNR-002                      
213300       MOVE RY9-IDARTNR           TO SU4-IDARTNR                          
213400       MOVE KSIFF                 TO SU4-REKSIFFR                         
213500       MOVE RY9-KVART             TO SU4-KVRO-002                         
213600       MOVE RY9S-KDORDBEK         TO SU4-KDRESTR                          
213700       MOVE ZERO                  TO SU4-KDRO                             
213800       MOVE RY9S-KDORDKL          TO SU4-KDORDER                          
213900       MOVE RY9-KDFAKTYP          TO SU4-KDFAKTYP                         
214000       MOVE RY9-IDKUNDRF (1:5)    TO SU4-IDORDNR7-LEV                     
214100       MOVE NEJ                   TO SU4-TID-ERS                          
214200       MOVE RY9-TITPO             TO SU4-TIDISPIN                         
214300       MOVE RY9-KDVRINFO          TO SU4-KDVRINFO                         
214400                                                                          
214500       MOVE LOGG6-TIAAMMDD        TO SU4-TIAAMMDD-REG                     
214600       MOVE LOGG6-TIKLOCK         TO SU4-TIKLOCK-REG                      
214700                                                                          
214800       WRITE W092Z7-POST          FROM SU4-AREA                           
214900                                                                          
215000       MOVE 'W092Z7'              TO POSTSUM-FDNAMN                       
215100       MOVE 'W09208DE'            TO POSTSUM-DDNAMN2                      
215200       MOVE SU4-IDPTYP            TO POSTSUM-TRANSTYP                     
215300       CALL POSTSUM USING         POSTSUM-PARM                            
215400     END-IF                                                               
215500     .                                                                    
215600     EJECT                                                                
215700 C48G-SKAPA-BORS-RO-POST SECTION.                                         
215800                                                                          
215900     MOVE '201'                 TO UXC-IDPTYP                             
216000     MOVE RY9S-IDDISTR          TO UXCW201-IDDISTR                        
216100     MOVE RY9S-IDORDER          TO UXCW201-IDORDER                        
216200     MOVE RY9-IDARTNR           TO UXCW201-IDARTNR                        
216300     MOVE RY9-KVART             TO UXCW201-KVLEVART                       
216400                                                                          
216500     WRITE W092XC-POST          FROM UXCW201-W020201                      
216600                                                                          
216700     MOVE 'W092XC'              TO POSTSUM-FDNAMN                         
216800     MOVE 'W09208EO'            TO POSTSUM-DDNAMN2                        
216900     MOVE UXC-IDPTYP            TO POSTSUM-TRANSTYP                       
217000     CALL POSTSUM USING         POSTSUM-PARM                              
217100     .                                                                    
217200     EJECT                                                                
217300 C60-SKRIV-RYA-POST   SECTION.                                            
217400     SKIP2                                                                
217500     MOVE LOGG6-LOGGPOST        TO  RYA-AREA                              
217600                                                                          
217700     MOVE RYA-IDDISTR           TO TEST-IDDISTR                           
217800     IF NOT DIST24-NORGE                                                  
217900       MOVE SPACE                 TO SUD-AREA                             
218000       MOVE 'SUD'                 TO SUD-IDPTYP                           
218100                                                                          
218200       MOVE WC-CDC-SE             TO W-SU-IDDC                            
218300                                                                          
218400       PERFORM S40-RED-IDSUPPL-SU                                         
218500                                                                          
218600       MOVE RYA-IDDISTR           TO SUD-IDDISTR                          
218700       MOVE RYA-IDKUNDNR          TO SUD-IDKUNDNR                         
218800       MOVE IDSUPPL-SU-WS         TO SUD-IDSUPPL                          
218900                                                                          
219000       MOVE RYA-IDKUNDRF (1:7)    TO SUD-IDORDNR-002                      
219100       MOVE RYA-IDARTNR           TO SUD-IDARTNR                          
219200       MOVE RYA-REKSIFFR          TO SUD-REKSIFFR                         
219300       MOVE RYA-KVBEART           TO SUD-KVBEART-002                      
219400       MOVE RYA-TITPO             TO SUD-TITPO                            
219500       MOVE RYA-KDTPOTYP          TO SUD-KDTPOTYP                         
219600       MOVE RYA-KDVRTPO           TO SUD-KDVRTPO                          
219700       MOVE RYA-KDVRINFO          TO SUD-KDVRINFO                         
219800                                                                          
219900       MOVE LOGG6-TIAAMMDD        TO SUD-TIAAMMDD                         
220000       MOVE LOGG6-TIKLOCK         TO SUD-TIKLOCK                          
220100                                                                          
220200       WRITE W092Z7-POST FROM SUD-AREA                                    
220300                                                                          
220400                                                                          
220500       MOVE 'W092Z7'              TO POSTSUM-FDNAMN                       
220600       MOVE 'W09208DE'            TO POSTSUM-DDNAMN2                      
220700       MOVE LOGG6-IDPTYP          TO POSTSUM-TRANSTYP                     
220800       CALL POSTSUM USING         POSTSUM-PARM                            
220900     END-IF                                                               
221000     .                                                                    
221100     EJECT                                                                
221200 C61-SKRIV-RYC-POST     SECTION.                                          
221300     SKIP2                                                                
221400                                                                          
221500     MOVE LOGG6-LOGGPOST       TO  RYC-WDGZRYC                            
221600     MOVE LOGG6-SORTPOST       TO  RYCS-WDGZRYCS                          
221700                                                                          
221800     PERFORM C61A-SKAPA-RYC-SORTPOST                                      
221900     PERFORM C61B-SKAPA-SU3-POST                                          
222000     PERFORM C61C-MOVE-STOPPAD-NOAC                                       
222100     .                                                                    
222200     EJECT                                                                
222300 C61A-SKAPA-RYC-SORTPOST     SECTION.                                     
222400     SKIP2                                                                
222500                                                                          
222600     MOVE SPACE                TO  RYCX-RYCS-AREA                         
222700     MOVE RYC-IDPTYP           TO  RYCX-IDPTYP-S                          
222800     MOVE RYC-IDDISTR          TO  RYCX-IDDISTR-S                         
222900     MOVE RYC-IDKUNDNR         TO  RYCX-IDKUNDNR-S                        
223000     MOVE RYCS-IDDC            TO  WS-IDDC                                
223100                                   W-IDDC-B6                              
223200     PERFORM IMS-GU-WDB601                                                
223300     IF DCS-CDC                                                           
223400     OR DCS-CDC-TR                                                        
223500        MOVE 1                 TO  RYCX-KDCLAGER-S                        
223600     ELSE                                                                 
223700        MOVE 2                 TO  RYCX-KDCLAGER-S                        
223800     END-IF                                                               
223900     MOVE RYC-KDFRAKT          TO  RYCX-KDFRAKT-S                         
224000     MOVE RYC-IDKUNDRF (1:5)   TO  RYCX-IDORDNR-S                         
224100     MOVE RYC-KDORDKL          TO  RYCX-KDORDKL                           
224200     MOVE RYC-IDARTNR          TO  RYCX-SORTBGP                           
224300     MOVE RYCX-RYCS-AREA       TO  LOGG6-SORTPOST                         
224400     .                                                                    
224500     EJECT                                                                
224600 C61B-SKAPA-SU3-POST      SECTION.                                        
224700                                                                          
224800     MOVE RYC-IDDISTR           TO TEST-IDDISTR                           
224900     IF NOT DIST24-NORGE                                                  
225000       MOVE SPACE                 TO SU3-AREA                             
225100       MOVE 'SU3'                 TO SU3-IDPTYP                           
225200       MOVE RYC-KVANNANT          TO SU3-KVBEART-002                      
225300       MOVE RYC-KDORDBEK          TO SU3-KDRESTR                          
225400       MOVE RYCS-IDDC             TO W-SU-IDDC                            
225500                                                                          
225600       PERFORM S40-RED-IDSUPPL-SU                                         
225700                                                                          
225800       MOVE RYC-IDDISTR           TO SU3-IDDISTR                          
225900       MOVE RYC-IDKUNDNR          TO SU3-IDKUNDNR                         
226000       MOVE IDSUPPL-SU-WS         TO SU3-IDSUPPL                          
226100                                                                          
226200       MOVE RYC-IDKUNDRF (1:7)    TO SU3-IDORDNR-002                      
226300       MOVE RYC-IDARTNR           TO SU3-IDARTNR                          
226400       MOVE RYC-REKSIFFR          TO SU3-REKSIFFR                         
226500       MOVE RYC-KDFAKTYP          TO SU3-KDFAKTYP                         
226600       MOVE 2                     TO SU3-KDKVFOR                          
226700       MOVE ZERO                  TO SU3-KDRO                             
226800       MOVE RYC-KDORDKL           TO SU3-KDORDER                          
226900       MOVE RYC-IDKUNDRF (1:7)    TO SU3-IDORDNR7-LEV                     
227000       MOVE ZERO                  TO SU3-KVQPACK-1                        
227100       MOVE RYC-FLTILLK           TO SU3-TID-ERS                          
227200       MOVE RYC-KDORDKL           TO SU3-KDORDKL                          
227300       MOVE ZERO                  TO SU3-FIKTIV-KVANT                     
227400       MOVE RYC-KDVRTPO           TO SU3-KDVRTPO                          
227500       MOVE RYC-KDVRINFO          TO SU3-KDVRINFO                         
227600                                                                          
227700       MOVE LOGG6-TIAAMMDD        TO SU3-TIAAMMDD                         
227800       MOVE LOGG6-TIKLOCK         TO SU3-TIKLOCK                          
227900       WRITE W092Z7-POST          FROM SU3-AREA                           
228000                                                                          
228100       MOVE 'W092Z7'              TO POSTSUM-FDNAMN                       
228200       MOVE 'W09208DE'            TO POSTSUM-DDNAMN2                      
228300       MOVE SU3-IDPTYP            TO POSTSUM-TRANSTYP                     
228400       CALL POSTSUM USING         POSTSUM-PARM                            
228500     END-IF                                                               
228600     .                                                                    
228700     EJECT                                                                
228800 C61C-MOVE-STOPPAD-NOAC SECTION.                                          
228900     SKIP2                                                                
229000     MOVE '008'                 TO OBSTOP-IDPTYP                          
229100     MOVE RYC-IDDISTR           TO OBSTOP-IDDISTR                         
229200     MOVE RYC-IDKUNDNR          TO OBSTOP-IDKUNDNR                        
229300     MOVE RYC-KDFRAKT           TO OBSTOP-KDFRAKT                         
229400     MOVE RYC-IDKUNDRF (1:7)    TO OBSTOP-IDORDNR                         
229500     IF RYC-IDKUNDRF-RO (1:7) = ZERO OR SPACE                             
229600       MOVE +0                  TO OBSTOP-KDLIDEL                         
229700     ELSE                                                                 
229800       MOVE +1                  TO OBSTOP-KDLIDEL                         
229900     END-IF                                                               
230000     MOVE RYCS-IDDC             TO OBSTOP-IDDC                            
230100     MOVE RYC-IDARTNR           TO OBSTOP-IDARTNR                         
230200     MOVE RYC-REKSIFFR          TO OBSTOP-REKSIFFR                        
230300     MOVE SPACE                 TO OBSTOP-BEART                           
230400     MOVE RYC-BERADREF          TO OBSTOP-BERADREF                        
230500     MOVE RYC-BEVOLREF          TO OBSTOP-BEVOLREF                        
230600     MOVE RYC-IDKUNDRF-RO (1:7) TO OBSTOP-IDRONR                          
230700     MOVE RYC-TIRODAT           TO OBSTOP-TIRODAT                         
230800     MOVE RYC-TIORDREG          TO OBSTOP-TIORDREG                        
230900     MOVE RYC-KDDSP             TO OBSTOP-KDDSP                           
231000     MOVE RYC-KDKVBRYT          TO OBSTOP-KDKVBRYT                        
231100     MOVE RYC-KDORDBEK          TO OBSTOP-KDRESTR                         
231200     MOVE RYC-KVANNANT          TO OBSTOP-KVBEART                         
231300     MOVE RYC-KDFAKTYP          TO OBSTOP-KDFAKTYP                        
231400     MOVE LOGG6-TIAAMMDD        TO OBSTOP-TIAAMMDD                        
231500     MOVE LOGG6-TIKLOCK         TO OBSTOP-TIKLOCK                         
231600                                                                          
231700     MOVE '008'                 TO POSTSUM-TRANSTYP                       
231800     WRITE 008-UTPOST           FROM UT008-AREA                           
231900                                                                          
232000     MOVE 'W092ZQ'              TO POSTSUM-FDNAMN                         
232100     MOVE 'W09208E2'            TO POSTSUM-DDNAMN2                        
232200     CALL POSTSUM USING         POSTSUM-PARM                              
232300     .                                                                    
232400     EJECT                                                                
232500 C63-SKRIV-RYE-POST      SECTION.                                         
232600                                                                          
232700     MOVE LOGG6-SORTPOST       TO  RYES-WDGZRYES-CTX                      
232800     MOVE LOGG6-LOGGPOST       TO  RYE-WDGZRYE                            
232900                                                                          
233000     IF  RYE-KDORDBEK          =   51 OR 52 OR 53 OR 55 OR 57 OR          
233100                                   58 OR 66 OR 67 OR 80 OR                
233200                                   81 OR 92 OR 43 OR 44                   
233300          PERFORM C63A-SKAPA-SU3-POST                                     
233400     END-IF                                                               
233500                                                                          
233600     IF  RYE-KDORDBEK          =   54                                     
233700          PERFORM C63B-SKAPA-SU2-POST                                     
233800     END-IF                                                               
233900                                                                          
234000     IF  RYE-KDORDBEK          =   90 OR 91                               
234100          PERFORM C63C-SKAPA-SU4-POST                                     
234200     END-IF                                                               
234300                                                                          
234400     IF  RYE-KDORDBEK          =   80 OR 81 OR 90 OR 91 OR 92             
234500          PERFORM C63D-MOVE-BRIST-NOAC                                    
234600     END-IF                                                               
234700                                                                          
234800     IF RYE-KDORDBEK           =  51 OR 52 OR 53 OR 54 OR 55 OR           
234900                                  57 OR 58 OR 66 OR 67                    
235000          PERFORM C63E-MOVE-STOPPAD-NOAC                                  
235100     END-IF                                                               
235200                                                                          
235300     IF RYE-KDORDBEK           =  80 OR 90                                
235400         IF RYE-KVRO           =  RYE-KVBEART-Q                           
235500             PERFORM C63F-SKAPA-EV-232-POST                               
235600         END-IF                                                           
235700     END-IF                                                               
235800                                                                          
235900     IF RYE-KDORDBEK           =  ZERO                                    
236000         PERFORM C63F-SKAPA-EV-232-POST                                   
236100         PERFORM C63G-SKAPA-229-POST                                      
236200         IF RYE-KVAVBART       >  ZERO                                    
236300             PERFORM C63H-SKAPA-432-POST                                  
236400         END-IF                                                           
236500     END-IF                                                               
236600     PERFORM S50-SKRIV-W092S1-POST                                        
236700                                                                          
236800     IF RYE-KDORDBEK           > ZERO AND RYE-KVAVBART = ZERO             
236900         IF RYE-FLLSBOK        = NEJ OR                                   
237000           (RYE-IDKUNDRF-RO (1:7) NOT = ZERO AND                          
237100            RYE-TIRODAT           NOT = ZERO )                            
237200             CONTINUE                                                     
237300          ELSE                                                            
237400             PERFORM C63I-SKAPA-009-POST                                  
237500         END-IF                                                           
237600     END-IF                                                               
237700                                                                          
237800     IF RYE-KDORDBEK           = 10                                       
237900         PERFORM C63J-SKAPA-001-POST                                      
238000         PERFORM C63K-SKAPA-SUA-POST                                      
238100     END-IF                                                               
238200                                                                          
238300     IF RYE-KDORDBEK           = 43 OR 44                                 
238400         PERFORM C63L-SKAPA-006-POST                                      
238500     END-IF                                                               
238600     .                                                                    
238700     EJECT                                                                
238800 C63A-SKAPA-SU3-POST      SECTION.                                        
238900                                                                          
239000     MOVE RYES-IDDISTR          TO TEST-IDDISTR                           
239100     IF NOT DIST24-NORGE                                                  
239200       MOVE SPACE                 TO SU3-AREA                             
239300       MOVE 'SU3'                 TO SU3-IDPTYP                           
239400       MOVE RYE-KVRO              TO SU3-KVBEART-002                      
239500                                                                          
239600       MOVE RYE-KDORDBEK          TO SU3-KDRESTR                          
239700                                                                          
239800       MOVE RYE-IDDC              TO W-SU-IDDC                            
239900                                                                          
240000       PERFORM S40-RED-IDSUPPL-SU                                         
240100                                                                          
240200       MOVE RYES-IDDISTR          TO SU3-IDDISTR                          
240300       MOVE RYES-IDKUNDNR         TO SU3-IDKUNDNR                         
240400       MOVE IDSUPPL-SU-WS         TO SU3-IDSUPPL                          
240500                                                                          
240600       PERFORM S30-FIXA-RYE-IDORDNR                                       
240700       MOVE W-RYE-IDORDNR         TO SU3-IDORDNR-002                      
240800       MOVE RYE-IDARTNR           TO SU3-IDARTNR                          
240900       MOVE RYE-REKSIFFR          TO SU3-REKSIFFR                         
241000       MOVE RYE-KDFAKTYP          TO SU3-KDFAKTYP                         
241100       MOVE 2                     TO SU3-KDKVFOR                          
241200       IF RYE-IDKUNDRF-RO (1:7) > ZERO                                    
241300          MOVE 1                  TO SU3-KDRO                             
241400       ELSE                                                               
241500          MOVE ZERO               TO SU3-KDRO                             
241600       END-IF                                                             
241700       MOVE RYES-KDORDKL          TO SU3-KDORDER                          
241800       MOVE RYE-IDKUNDRF (1:7)    TO SU3-IDORDNR7-LEV                     
241900       MOVE RYES-KVQPACK-1        TO SU3-KVQPACK-1                        
242000       MOVE RYE-FLTILLK           TO SU3-TID-ERS                          
242100       MOVE RYES-KDORDKL          TO SU3-KDORDKL                          
242200       MOVE ZERO                  TO SU3-FIKTIV-KVANT                     
242300       MOVE RYE-KDVRTPO           TO SU3-KDVRTPO                          
242400       MOVE RYE-KDVRINFO          TO SU3-KDVRINFO                         
242500                                                                          
242600       MOVE LOGG6-TIAAMMDD        TO SU3-TIAAMMDD                         
242700       MOVE LOGG6-TIKLOCK         TO SU3-TIKLOCK                          
242800                                                                          
242900       WRITE W092Z7-POST          FROM SU3-AREA                           
243000                                                                          
243100       MOVE 'W092Z7'              TO POSTSUM-FDNAMN                       
243200       MOVE 'W09208DE'            TO POSTSUM-DDNAMN2                      
243300       MOVE SU3-IDPTYP            TO POSTSUM-TRANSTYP                     
243400       CALL POSTSUM USING         POSTSUM-PARM                            
243500     END-IF                                                               
243600     .                                                                    
243700     EJECT                                                                
243800 C63B-SKAPA-SU2-POST      SECTION.                                        
243900                                                                          
244000     MOVE RYES-IDDISTR          TO TEST-IDDISTR                           
244100     IF NOT DIST24-NORGE                                                  
244200       MOVE SPACE                 TO SU2-AREA                             
244300       MOVE 'SU2'                 TO SU2-IDPTYP                           
244400                                                                          
244500       MOVE RYE-IDDC              TO W-SU-IDDC                            
244600                                                                          
244700       PERFORM S40-RED-IDSUPPL-SU                                         
244800                                                                          
244900       MOVE RYES-IDDISTR          TO SU2-IDDISTR                          
245000       MOVE RYES-IDKUNDNR         TO SU2-IDKUNDNR                         
245100       MOVE IDSUPPL-SU-WS         TO SU2-IDSUPPL                          
245200                                                                          
245300       PERFORM S30-FIXA-RYE-IDORDNR                                       
245400       MOVE W-RYE-IDORDNR         TO SU2-IDORDNR-002                      
245500       MOVE RYE-IDARTNR           TO SU2-IDARTNR-ERS                      
245600       MOVE RYE-REKSIFFR          TO SU2-REKSIFFR-ERS                     
245700       MOVE ZERO                  TO SU2-IDARTNR-TILLK                    
245800       MOVE ZERO                  TO SU2-REKSIFFR-TILLK                   
245900       MOVE RYE-KVBEART-Q         TO SU2-KVBEART-002                      
246000       MOVE ZERO                  TO SU2-DIERS                            
246100       MOVE RYE-KVAVBART          TO SU2-KVLEVART-002                     
246200       MOVE RYE-KDORDBEK          TO SU2-KDRESTR                          
246300       IF RYE-IDKUNDRF-RO (1:7) > ZERO                                    
246400          MOVE 1                  TO SU2-KDRO                             
246500       ELSE                                                               
246600          MOVE ZERO               TO SU2-KDRO                             
246700       END-IF                                                             
246800       MOVE RYES-KDORDKL          TO SU2-KDORDER                          
246900       MOVE 1                     TO SU2-FLVRERS                          
247000       MOVE RYES-KDERS            TO SU2-KDERS                            
247100       MOVE RYES-KDORDKL          TO SU2-KDORDKL                          
247200       MOVE RYE-KDFAKTYP          TO SU2-KDFAKTYP                         
247300       MOVE RYE-IDKUNDRF (1:7)    TO SU2-IDORDNR7-LEV                     
247400       MOVE RYES-KDTPOTYP         TO SU2-KDTPOTYP                         
247500       MOVE RYE-KDVRTPO           TO SU2-KDVRTPO                          
247600       MOVE RYE-KDVRINFO          TO SU2-KDVRINFO                         
247700                                                                          
247800       MOVE LOGG6-TIAAMMDD        TO SU2-TIAAMMDD                         
247900       MOVE LOGG6-TIKLOCK         TO SU2-TIKLOCK                          
248000                                                                          
248100       WRITE W092Z7-POST          FROM SU2-AREA                           
248200                                                                          
248300       MOVE 'W092Z7'              TO POSTSUM-FDNAMN                       
248400       MOVE 'W09208DE'            TO POSTSUM-DDNAMN2                      
248500       MOVE SU2-IDPTYP            TO POSTSUM-TRANSTYP                     
248600       CALL POSTSUM USING         POSTSUM-PARM                            
248700     END-IF                                                               
248800     .                                                                    
248900     EJECT                                                                
249000 C63C-SKAPA-SU4-POST      SECTION.                                        
249100                                                                          
249200     MOVE RYES-IDDISTR          TO TEST-IDDISTR                           
249300     IF NOT DIST24-NORGE                                                  
249400       MOVE SPACE                 TO SU4-AREA                             
249500       MOVE 'SU4'                 TO SU4-IDPTYP                           
249600       MOVE RYE-KDORDBEK          TO SU4-KDRESTR                          
249700                                                                          
249800       MOVE RYE-IDDC              TO W-SU-IDDC                            
249900                                                                          
250000       PERFORM S40-RED-IDSUPPL-SU                                         
250100                                                                          
250200       MOVE RYES-IDDISTR          TO SU4-IDDISTR                          
250300       MOVE RYES-IDKUNDNR         TO SU4-IDKUNDNR                         
250400       MOVE IDSUPPL-SU-WS         TO SU4-IDSUPPL                          
250500                                                                          
250600       PERFORM S30-FIXA-RYE-IDORDNR                                       
250700       MOVE W-RYE-IDORDNR         TO SU4-IDORDNR-002                      
250800       MOVE RYE-IDARTNR           TO SU4-IDARTNR                          
250900       MOVE RYE-REKSIFFR          TO SU4-REKSIFFR                         
251000       MOVE RYE-KVRO              TO SU4-KVRO-002                         
251100       IF RYE-IDKUNDRF-RO (1:7) > ZERO                                    
251200          MOVE 1                  TO SU4-KDRO                             
251300       ELSE                                                               
251400          MOVE 0                  TO SU4-KDRO                             
251500       END-IF                                                             
251600       MOVE RYES-KDORDKL          TO SU4-KDORDER                          
251700       MOVE RYE-KDFAKTYP          TO SU4-KDFAKTYP                         
251800       MOVE RYE-IDKUNDRF (1:7)    TO SU4-IDORDNR7-LEV                     
251900       MOVE NEJ                   TO SU4-TID-ERS                          
252000       MOVE RYE-TIDISPIN          TO SU4-TIDISPIN                         
252100       MOVE RYE-KDVRINFO          TO SU4-KDVRINFO                         
252200                                                                          
252300       MOVE LOGG6-TIAAMMDD        TO SU4-TIAAMMDD-REG                     
252400       MOVE LOGG6-TIKLOCK         TO SU4-TIKLOCK-REG                      
252500                                                                          
252600       WRITE W092Z7-POST          FROM SU4-AREA                           
252700                                                                          
252800       MOVE 'W092Z7'              TO POSTSUM-FDNAMN                       
252900       MOVE 'W09208DE'            TO POSTSUM-DDNAMN2                      
253000       MOVE SU4-IDPTYP            TO POSTSUM-TRANSTYP                     
253100       CALL POSTSUM USING         POSTSUM-PARM                            
253200     END-IF                                                               
253300     .                                                                    
253400     EJECT                                                                
253500 C63D-MOVE-BRIST-NOAC     SECTION.                                        
253600     SKIP2                                                                
253700     MOVE '007'                TO OBLAG-IDPTYP                            
253800     MOVE RYES-IDDISTR         TO OBLAG-IDDISTR                           
253900     MOVE RYES-IDKUNDNR        TO OBLAG-IDKUNDNR                          
254000     MOVE RYES-KDFRAKT         TO OBLAG-KDFRAKT                           
254100     MOVE RYE-IDKUNDRF (1:7)   TO OBLAG-IDORDNR                           
254200     IF RYE-FLTILLK            = NEJ                                      
254300       IF RYE-IDKUNDRF-RO (1:7)  = +0                                     
254400         MOVE +0               TO OBLAG-KDLIDEL                           
254500       ELSE                                                               
254600         MOVE +1               TO OBLAG-KDLIDEL                           
254700       END-IF                                                             
254800     ELSE                                                                 
254900       MOVE +2                 TO OBLAG-KDLIDEL                           
255000     END-IF                                                               
255100     MOVE RYE-IDDC             TO OBLAG-IDDC                              
255200     MOVE RYE-IDARTNR          TO OBLAG-IDARTNR                           
255300     MOVE SPACE                TO OBLAG-BEART                             
255400     MOVE RYE-BERADREF         TO OBLAG-BERADREF                          
255500     MOVE RYE-BEVOLREF         TO OBLAG-BEVOLREF                          
255600     PERFORM S30-FIXA-RYE-IDORDNR                                         
255700     MOVE W-RYE-IDORDNR        TO OBLAG-IDRONR                            
255800     MOVE RYE-REKSIFFR         TO OBLAG-REKSIFFR                          
255900     MOVE RYE-TIRODAT          TO OBLAG-TIRODAT                           
256000     MOVE RYE-TIORDREG         TO OBLAG-TIORDREG                          
256100     MOVE RYE-KDORDBEK         TO OBLAG-KDRESTR                           
256200     MOVE RYE-KVBEART-Q        TO OBLAG-KVBEART                           
256300     MOVE RYE-KVAVBART         TO OBLAG-KVAVBART                          
256400     MOVE RYE-KDDSP            TO OBLAG-KDDSP                             
256500     IF RYE-KDORDBEK = +80 OR +81                                         
256600        MOVE ZERO              TO OBLAG-KVRO                              
256700     ELSE                                                                 
256800        MOVE RYE-KVRO          TO OBLAG-KVRO                              
256900     END-IF                                                               
257000     MOVE RYE-KDFAKTYP         TO OBLAG-KDFAKTYP                          
257100     MOVE RYE-TIDISPIN         TO OBLAG-TIDISPIN                          
257200     MOVE RYE-KDKVBRYT         TO OBLAG-KDKVBRYT                          
257300     MOVE LOGG6-TIAAMMDD       TO OBLAG-TIAAMMDD                          
257400     MOVE LOGG6-TIKLOCK        TO OBLAG-TIKLOCK                           
257500                                                                          
257600     MOVE '007'                 TO POSTSUM-TRANSTYP                       
257700     WRITE 007-UTPOST           FROM UT007-AREA                           
257800                                                                          
257900     MOVE 'W092ZQ'              TO POSTSUM-FDNAMN                         
258000     MOVE 'W09208E2'            TO POSTSUM-DDNAMN2                        
258100     CALL POSTSUM USING         POSTSUM-PARM                              
258200     .                                                                    
258300     EJECT                                                                
258400 C63E-MOVE-STOPPAD-NOAC   SECTION.                                        
258500                                                                          
258600     SKIP2                                                                
258700     MOVE '008'                 TO OBSTOP-IDPTYP                          
258800     MOVE RYES-IDDISTR          TO OBSTOP-IDDISTR                         
258900     MOVE RYES-IDKUNDNR         TO OBSTOP-IDKUNDNR                        
259000     MOVE RYES-KDFRAKT          TO OBSTOP-KDFRAKT                         
259100     MOVE RYE-IDKUNDRF (1:7)    TO OBSTOP-IDORDNR                         
259200     IF RYE-FLTILLK             = NEJ                                     
259300       IF RYE-IDKUNDRF-RO (1:7) = +0                                      
259400         MOVE +0                TO OBSTOP-KDLIDEL                         
259500       ELSE                                                               
259600         MOVE +1                TO OBSTOP-KDLIDEL                         
259700       END-IF                                                             
259800     ELSE                                                                 
259900       MOVE +2                  TO OBSTOP-KDLIDEL                         
260000     END-IF                                                               
260100     MOVE RYE-IDDC              TO OBSTOP-IDDC                            
260200     MOVE RYE-IDARTNR           TO OBSTOP-IDARTNR                         
260300     MOVE SPACE                 TO OBSTOP-BEART                           
260400     MOVE RYE-BERADREF          TO OBSTOP-BERADREF                        
260500     MOVE RYE-BEVOLREF          TO OBSTOP-BEVOLREF                        
260600     PERFORM S30-FIXA-RYE-IDORDNR                                         
260700     MOVE W-RYE-IDORDNR         TO OBSTOP-IDRONR                          
260800     MOVE RYE-REKSIFFR          TO OBSTOP-REKSIFFR                        
260900     MOVE RYE-TIRODAT           TO OBSTOP-TIRODAT                         
261000     MOVE RYE-TIORDREG          TO OBSTOP-TIORDREG                        
261100     MOVE RYE-KDDSP             TO OBSTOP-KDDSP                           
261200     MOVE RYE-KDKVBRYT          TO OBSTOP-KDKVBRYT                        
261300     MOVE RYE-KDORDBEK          TO OBSTOP-KDRESTR                         
261400     MOVE RYE-KVBEART-Q         TO OBSTOP-KVBEART                         
261500     MOVE RYE-KDFAKTYP          TO OBSTOP-KDFAKTYP                        
261600     MOVE LOGG6-TIAAMMDD        TO OBSTOP-TIAAMMDD                        
261700     MOVE LOGG6-TIKLOCK         TO OBSTOP-TIKLOCK                         
261800                                                                          
261900     MOVE '008'                 TO POSTSUM-TRANSTYP                       
262000     WRITE 008-UTPOST           FROM UT008-AREA                           
262100                                                                          
262200     MOVE 'W092ZQ'              TO POSTSUM-FDNAMN                         
262300     MOVE 'W09208E2'            TO POSTSUM-DDNAMN2                        
262400     CALL POSTSUM USING         POSTSUM-PARM                              
262500     .                                                                    
262600     EJECT                                                                
262700 C63F-SKAPA-EV-232-POST      SECTION.                                     
262800                                                                          
262900     MOVE RYE-IDDC          TO WS-IDDC                                    
263000                               W-IDDC-B6                                  
263100     PERFORM IMS-GU-WDB601                                                
263200     IF  (DCS-CDC                                                         
263300     OR   DCS-CDC-TR                                                      
263400     OR   DCS-SDC)                                                        
263500     MOVE RYES-IDDISTR          TO  TEST-IDDISTR                          
263600     IF RYE-KDORDING         =  3      OR                                 
263700        DIST18-SKROT                   OR                                 
263800        DIST19-SATS                    OR                                 
263900        DIST20-EMBALLAGE               OR                                 
264000        DIST93-BYTESRADIO-C2           OR                                 
264100        RYE-FLDIRLEV         =  JA     OR                                 
264200        RYE-FLORDSPE         =  JA     OR                                 
264300       (RYE-TIRODAT          >  ZERO   AND                                
264400       (RYE-IDKUNDRF-RO  NOT =  SPACE  AND                                
264500        RYE-IDKUNDRF-RO  NOT = '0000000   '))                             
264600       CONTINUE                                                           
264700     ELSE                                                                 
264800       MOVE SPACE           TO UTRZ1-AREA                                 
264900                                                                          
265000       MOVE '232'           TO UTRZ1-IDPTYP                               
265100       MOVE RYES-IDDISTR    TO UTRZ1-IDDISTR                              
265200       IF CDC                                                             
265300          MOVE 1            TO UTRZ1-KDCLAGER                             
265400       ELSE                                                               
265500          MOVE 2            TO UTRZ1-KDCLAGER                             
265600       END-IF                                                             
265700       MOVE RYES-KDORDKL    TO UTRZ1-KDORDKL                              
265800       MOVE RYE-IDARTNR     TO UTRZ1-IDARTNR                              
265900       MOVE +1              TO UTRZ1-REINKORD                             
266000       IF RYE-KVAVBART >= RYE-KVBEART-Q - RYES-KVSLATT                    
266100         MOVE 1             TO UTRZ1-REAVBRAD                             
266200         MOVE ZERO          TO UTRZ1-RERORAD                              
266300       ELSE                                                               
266400         COMPUTE UTRZ1-REAVBRAD = RYE-KVAVBART  / RYE-KVBEART-Q           
266500         COMPUTE UTRZ1-RERORAD  =                                         
266600                 (RYE-KVBEART-Q - RYE-KVAVBART) / RYE-KVBEART-Q           
266700       END-IF                                                             
266800       MOVE ZERO            TO UTRZ1-REFYSAVV                             
266900                                                                          
267000     END-IF                                                               
267100     END-IF                                                               
267200     .                                                                    
267300     EJECT                                                                
267400 C63G-SKAPA-229-POST      SECTION.                                        
267500     SKIP3                                                                
267600     MOVE RYE-IDDC               TO WS-IDDC                               
267700                                   W-IDDC-B6                              
267800     PERFORM IMS-GU-WDB601                                                
267900     IF  (DCS-SDC                                                         
268000     OR   DCS-NDC)                                                        
268200       CONTINUE                                                           
268300     ELSE                                                                 
268400       MOVE RYES-IDDISTR          TO TEST-IDDISTR                         
268500       MOVE SPACE                 TO UTRZ5-AREA                           
268600                                                                          
268700       MOVE '229'                 TO UTRZ5-IDPTYP                         
268800       MOVE RYE-IDARTNR           TO UTRZ5-IDARTNR                        
268900       IF CDC                                                             
269000          MOVE 1                  TO UTRZ5-KDCLAGER                       
269100       ELSE                                                               
269200          MOVE 2                  TO UTRZ5-KDCLAGER                       
269300       END-IF                                                             
269400       IF DIST18-SKROT                                                    
269500          MOVE 7                  TO UTRZ5-KDRORELS                       
269600       ELSE                                                               
269700         IF DIST35-REFILL AND NOT                                         
269710           (DIST35-REFILL-NA OR                                           
269720            DIST35-REFILL-CN)                                             
269800           MOVE 2                 TO UTRZ5-KDRORELS                       
269900         ELSE                                                             
270000           MOVE 8                 TO UTRZ5-KDRORELS                       
270100         END-IF                                                           
270200       END-IF                                                             
270300       MOVE RYE-KVAVBART          TO UTRZ5-KVANTAL                        
270400       MOVE '2'                   TO UTRZ5-KDUPPD                         
270500                                                                          
270600       WRITE W092Z5-POST          FROM UTRZ5-AREA                         
270700                                                                          
270800       MOVE 'W092Z5'              TO POSTSUM-FDNAMN                       
270900       MOVE 'W09208DC'            TO POSTSUM-DDNAMN2                      
271000       MOVE UTRZ5-IDPTYP          TO POSTSUM-TRANSTYP                     
271100       CALL POSTSUM USING         POSTSUM-PARM                            
271200     END-IF                                                               
271300     .                                                                    
271400     EJECT                                                                
271500 C63H-SKAPA-432-POST      SECTION.                                        
271600                                                                          
271700     MOVE SPACE                 TO UTRZ4-AREA                             
271800                                                                          
271900     MOVE '432'                 TO UTRZ4-IDPTYP                           
272000     MOVE RYE-IDDC              TO UTRZ4-IDDC                             
272100     MOVE RYE-IDARTNR           TO UTRZ4-IDARTNR                          
272200     MOVE RYE-KVAVBART          TO UTRZ4-KVBEART                          
272300     MOVE ZERO                  TO UTRZ4-KDQPACK                          
272400     MOVE RYES-IDDISTR          TO UTRZ4-IDDISTR                          
272500     MOVE RYES-KDORDKL          TO UTRZ4-KDORDKL                          
272600                                                                          
272700     MOVE 'AAMMDD'             TO  DAT-KDDATFORM                          
272800     MOVE RYE-TIORDREG         TO  DAT-I-TIDATUM                          
272900                                                                          
273000     CALL WDATKONV USING       DAT-KDDATFORM                              
273100                               DAT-I-TIDATUM                              
273200                               DAT-O-TIDATUM                              
273300                               DAT-KDSVAR                                 
273400                                                                          
273500     MOVE DAT-TIAA             TO   UTRZ4-TIAVBAR                         
273600     MOVE DAT-TIP              TO   UTRZ4-TIAVBPER                        
273700     WRITE W092Z4-POST         FROM UTRZ4-AREA                            
273800                                                                          
273900     MOVE 'W092Z4'              TO POSTSUM-FDNAMN                         
274000     MOVE 'W09208DC'            TO POSTSUM-DDNAMN2                        
274100     MOVE UTRZ5-IDPTYP          TO POSTSUM-TRANSTYP                       
274200     CALL POSTSUM USING         POSTSUM-PARM                              
274300     .                                                                    
274400     EJECT                                                                
274500                                                                          
274600 C63I-SKAPA-009-POST      SECTION.                                        
274700                                                                          
274800     MOVE '009'                 TO SERV-IDPTYP                            
274900     MOVE RYE-IDDC              TO SERV-IDDC                              
275000     MOVE RYES-IDDISTR          TO SERV-IDDISTR                           
275100     MOVE RYES-IDKUNDNR         TO SERV-IDKUNDNR                          
275200     IF RYE-IDKUNDRF-RO (1:7)   NOT = ZERO                                
275300         MOVE RYE-IDKUNDRF-RO (1:7) TO SERV-IDORDNR                       
275400     ELSE                                                                 
275500         MOVE RYE-IDKUNDRF (1:7)    TO SERV-IDORDNR                       
275600     END-IF                                                               
275700     MOVE RYES-KDORDKL          TO SERV-KDORDKL                           
275800     MOVE RYE-IDARTNR           TO SERV-IDARTNR                           
275900     MOVE RYE-REKSIFFR          TO SERV-REKSIFFR                          
276000     MOVE RYE-TIORDREG          TO SERV-TIORDREG                          
276100     MOVE RYE-KDPRODSL          TO SERV-KDPRODSL                          
276200     MOVE RYE-KVBEART-Q         TO SERV-KVBEART                           
276300     MOVE RYE-KDFAKTYP          TO SERV-KDFAKTYP                          
276400                                                                          
276500     WRITE 009-UTPOST           FROM UT009-AREA                           
276600                                                                          
276700     MOVE 'W092ZP'              TO POSTSUM-FDNAMN                         
276800     MOVE 'W09208E1'            TO POSTSUM-DDNAMN2                        
276900     MOVE '009'                 TO POSTSUM-TRANSTYP                       
277000     CALL POSTSUM USING         POSTSUM-PARM                              
277100     .                                                                    
277200     EJECT                                                                
277300 C63J-SKAPA-001-POST     SECTION.                                         
277400                                                                          
277500     MOVE '001'                 TO BIP-IDPTYP                             
277600     MOVE RYE-IDDC              TO BIP-IDDC                               
277700     MOVE RYES-IDDISTR          TO BIP-IDDISTR                            
277800     MOVE RYES-IDKUNDNR         TO BIP-IDKUNDNR                           
277900     MOVE RYE-IDKUNDRF (1:7)    TO BIP-IDORDNR                            
278000     MOVE RYES-KDORDKL          TO BIP-KDORDKL                            
278100     MOVE RYE-IDARTNR           TO BIP-IDARTNR                            
278200     MOVE RYE-REKSIFFR          TO BIP-REKSIFFR                           
278300                                                                          
278400     MOVE RYE-BERADREF          TO BIP-BERADREF                           
278500     PERFORM S30-FIXA-RYE-IDORDNR                                         
278600     MOVE W-RYE-IDORDNR         TO BIP-IDRONR                             
278700     MOVE RYE-TIRODAT           TO BIP-TIRODAT                            
278800     MOVE RYE-BEVOLREF          TO BIP-BEVOLREF                           
278900     MOVE RYE-KVBEART-Q         TO BIP-KVLEVART                           
279000     MOVE RYE-KDFAKTYP          TO BIP-KDFAKTYP                           
279100     MOVE RYE-KDORDBEK          TO BIP-KDRESTR                            
279200     MOVE LOGG6-TIAAMMDD        TO BIP-TIAAMMDD                           
279300     MOVE LOGG6-TIKLOCK         TO BIP-TIKLOCK                            
279400                                                                          
279500     MOVE '001'                 TO POSTSUM-TRANSTYP                       
279600     WRITE 001-UTPOST           FROM UT001-AREA                           
279700                                                                          
279800     MOVE 'W092ZF'              TO POSTSUM-FDNAMN                         
279900     MOVE 'W09208DM'            TO POSTSUM-DDNAMN2                        
280000     CALL POSTSUM USING         POSTSUM-PARM                              
280100     .                                                                    
280200     EJECT                                                                
280300 C63K-SKAPA-SUA-POST     SECTION.                                         
280400                                                                          
280500     MOVE RYES-IDDISTR          TO TEST-IDDISTR                           
280600     IF NOT DIST24-NORGE                                                  
280700       MOVE SPACE                 TO SUA-AREA                             
280800       MOVE 'SUA'                 TO SUA-IDPTYP                           
280900                                                                          
281000       MOVE RYE-IDDC              TO W-SU-IDDC                            
281100                                                                          
281200       PERFORM S40-RED-IDSUPPL-SU                                         
281300                                                                          
281400       MOVE RYES-IDDISTR          TO SUA-IDDISTR                          
281500       MOVE RYES-IDKUNDNR         TO SUA-IDKUNDNR                         
281600       MOVE IDSUPPL-SU-WS         TO SUA-IDSUPPL                          
281700                                                                          
281800       MOVE RYE-IDKUNDRF (1:7)    TO SUA-IDORDNR-002                      
281900       IF RYE-IDKUNDRF-RO (1:7)   >  ZERO                                 
282000           MOVE RYE-IDKUNDRF-RO (1:7) TO SUA-IDRONR-002                   
282100        ELSE                                                              
282200           MOVE ZERO                  TO SUA-IDRONR-002                   
282300       END-IF                                                             
282400       MOVE RYE-IDARTNR           TO SUA-IDARTNR                          
282500       MOVE RYE-REKSIFFR          TO SUA-REKSIFFR                         
282600       MOVE RYE-KVBEART-Q         TO SUA-KVLEVART                         
282700       MOVE RYE-KDORDBEK          TO SUA-KDRESTR                          
282800                                                                          
282900       MOVE LOGG6-TIAAMMDD        TO SUA-TIAAMMDD                         
283000       MOVE LOGG6-TIKLOCK         TO SUA-TIKLOCK                          
283100                                                                          
283200       WRITE W092Z7-POST FROM SUA-AREA                                    
283300                                                                          
283400       MOVE 'W092Z7'              TO POSTSUM-FDNAMN                       
283500       MOVE 'W09208DE'            TO POSTSUM-DDNAMN2                      
283600       MOVE LOGG6-IDPTYP          TO POSTSUM-TRANSTYP                     
283700       CALL POSTSUM USING         POSTSUM-PARM                            
283800     END-IF                                                               
283900     .                                                                    
284000     EJECT                                                                
284100 C63L-SKAPA-006-POST     SECTION.                                         
284200                                                                          
284300     MOVE '006'                 TO OBKVAN-IDPTYP                          
284400     MOVE RYES-IDDISTR          TO OBKVAN-IDDISTR                         
284500     MOVE RYES-IDKUNDNR         TO OBKVAN-IDKUNDNR                        
284600     MOVE RYES-KDFRAKT          TO OBKVAN-KDFRAKT                         
284700     MOVE RYE-IDKUNDRF(1:7)     TO OBKVAN-IDORDNR                         
284800     IF RYE-IDKUNDRF-RO(1:7)    =  ZERO OR SPACE                          
284900         MOVE ZERO              TO OBKVAN-KDLIDEL                         
285000      ELSE                                                                
285100         MOVE +1                TO OBKVAN-KDLIDEL                         
285200     END-IF                                                               
285300     MOVE RYE-IDDC              TO OBKVAN-IDDC                            
285400     MOVE RYE-IDARTNR           TO OBKVAN-IDARTNR                         
285500     MOVE RYE-REKSIFFR          TO OBKVAN-REKSIFFR                        
285600     MOVE SPACE                 TO OBKVAN-BEART                           
285700     MOVE RYE-BERADREF          TO OBKVAN-BERADREF                        
285800     PERFORM S30-FIXA-RYE-IDORDNR                                         
285900     MOVE W-RYE-IDORDNR         TO OBKVAN-IDRONR                          
286000     MOVE RYE-TIRODAT           TO OBKVAN-TIRODAT                         
286100     MOVE RYE-BEVOLREF          TO OBKVAN-BEVOLREF                        
286200     MOVE RYE-KDORDBEK          TO OBKVAN-KDRESTR                         
286300     MOVE RYES-KVBEART          TO OBKVAN-KVBEART                         
286400     MOVE RYE-KVBEART-Q         TO OBKVAN-KVBEART-Q                       
286500     MOVE RYES-KVQPACK-1        TO OBKVAN-KVQPACK-1                       
286600     MOVE RYE-KDKVBRYT          TO OBKVAN-KDKVBRYT                        
286700     MOVE RYE-KDDSP             TO OBKVAN-KDDSP                           
286800     MOVE RYE-KDFAKTYP          TO OBKVAN-KDFAKTYP                        
286900     MOVE LOGG6-TIAAMMDD        TO OBKVAN-TIAAMMDD                        
287000     MOVE LOGG6-TIKLOCK         TO OBKVAN-TIKLOCK                         
287100                                                                          
287200     MOVE '006'                 TO POSTSUM-TRANSTYP                       
287300     WRITE 006-UTPOST           FROM UT006-AREA                           
287400                                                                          
287500     MOVE 'W092ZQ'              TO POSTSUM-FDNAMN                         
287600     MOVE 'W09208E2'            TO POSTSUM-DDNAMN2                        
287700     CALL POSTSUM USING         POSTSUM-PARM                              
287800     .                                                                    
287900     EJECT                                                                
288000 C66-SKRIV-SUC-POST      SECTION.                                         
288100     SKIP2                                                                
288200     MOVE LOGG6-LOGGPOST       TO  SUC-WDGZSUC                            
288300                                                                          
288400     MOVE SUC-IDDISTR          TO  TEST-IDDISTR                           
288500     IF NOT DIST24-NORGE                                                  
288600       MOVE SPACE                TO  UTSUC-AREA                           
288700                                                                          
288800       MOVE 'SUC'                TO  UTSUC-IDPTYP                         
288900       MOVE SUC-IDDC             TO  W-SU-IDDC                            
289000                                                                          
289100       PERFORM S40-RED-IDSUPPL-SU                                         
289200                                                                          
289300       MOVE SUC-IDDISTR          TO UTSUC-IDDISTR                         
289400       MOVE SUC-IDKUNDNR         TO UTSUC-IDKUNDNR                        
289500       MOVE IDSUPPL-SU-WS        TO UTSUC-IDSUPPL                         
289600                                                                          
289700       MOVE SUC-IDORDNR          TO UTSUC-IDORDNR-002                     
289800       MOVE SUC-IDARTNR          TO UTSUC-IDARTNR                         
289900       MOVE SUC-REKSIFFR         TO UTSUC-REKSIFFR                        
290000       MOVE SUC-KVBEART          TO UTSUC-KVBEART                         
290100       MOVE SUC-KDRO             TO UTSUC-KDRO                            
290200       MOVE SUC-KDORDER          TO UTSUC-KDORDER                         
290300       MOVE SUC-KDORDER-NY       TO UTSUC-KDORDER-NY                      
290400       MOVE SUC-KDVRINFO         TO UTSUC-KDVRINFO                        
290500                                                                          
290600       MOVE LOGG6-TIAAMMDD       TO UTSUC-TIAAMMDD                        
290700       MOVE LOGG6-TIKLOCK        TO UTSUC-TIKLOCK                         
290800                                                                          
290900       WRITE W092Z7-POST         FROM UTSUC-AREA                          
291000                                                                          
291100                                                                          
291200       MOVE 'W092Z7'             TO POSTSUM-FDNAMN                        
291300       MOVE 'W09208DE'           TO POSTSUM-DDNAMN2                       
291400       MOVE LOGG6-IDPTYP         TO POSTSUM-TRANSTYP                      
291500       CALL POSTSUM USING        POSTSUM-PARM                             
291600     END-IF                                                               
291700     .                                                                    
291800     EJECT                                                                
291900                                                                          
292000 C67-SKRIV-RYB-POST      SECTION.                                         
292100                                                                          
292200     MOVE LOGG6-LOGGPOST       TO  RYB-AREA                               
292300                                                                          
292400     PERFORM C67A-SKAPA-SU4-POST                                          
292500     PERFORM C67B-SKAPA-SU5-POST                                          
292600     PERFORM C67C-SKAPA-BRIST-POST                                        
292700     .                                                                    
292800     EJECT                                                                
292900 C67A-SKAPA-SU4-POST      SECTION.                                        
293000                                                                          
293100     MOVE RYB-IDDISTR           TO TEST-IDDISTR                           
293200     IF NOT DIST24-NORGE                                                  
293300       MOVE SPACE                 TO SU4-AREA                             
293400       MOVE 'SU4'                 TO SU4-IDPTYP                           
293500       MOVE RYB-KDORDBEK          TO SU4-KDRESTR                          
293600                                                                          
293700       MOVE RYB-IDDC              TO W-SU-IDDC                            
293800                                                                          
293900       PERFORM S40-RED-IDSUPPL-SU                                         
294000                                                                          
294100       MOVE RYB-IDDISTR           TO SU4-IDDISTR                          
294200       MOVE RYB-IDKUNDNR          TO SU4-IDKUNDNR                         
294300       MOVE IDSUPPL-SU-WS         TO SU4-IDSUPPL                          
294400                                                                          
294500       MOVE RYB-IDKUNDRF-RO (1:7) TO SU4-IDORDNR-002                      
294600       MOVE RYB-IDARTNR           TO SU4-IDARTNR                          
294700       MOVE RYB-REKSIFFR          TO SU4-REKSIFFR                         
294800       MOVE RYB-KVRO              TO SU4-KVRO-002                         
294900       MOVE 1                     TO SU4-KDRO                             
295000       MOVE RYB-KDORDKL           TO SU4-KDORDER                          
295100       MOVE RYB-KDFAKTYP          TO SU4-KDFAKTYP                         
295200       MOVE RYB-IDKUNDRF (1:7)    TO SU4-IDORDNR7-LEV                     
295300       MOVE RYB-FLTILLK           TO SU4-TID-ERS                          
295400       MOVE RYB-TIDISPIN          TO SU4-TIDISPIN                         
295500       MOVE RYB-KDVRINFO          TO SU4-KDVRINFO                         
295600                                                                          
295700       MOVE LOGG6-TIAAMMDD        TO SU4-TIAAMMDD-REG                     
295800       MOVE LOGG6-TIKLOCK         TO SU4-TIKLOCK-REG                      
295900                                                                          
296000       WRITE W092Z7-POST          FROM SU4-AREA                           
296100                                                                          
296200       MOVE 'W092Z7'              TO POSTSUM-FDNAMN                       
296300       MOVE 'W09208DE'            TO POSTSUM-DDNAMN2                      
296400       MOVE SU4-IDPTYP            TO POSTSUM-TRANSTYP                     
296500       CALL POSTSUM USING         POSTSUM-PARM                            
296600     END-IF                                                               
296700     .                                                                    
296800     EJECT                                                                
296900 C67B-SKAPA-SU5-POST      SECTION.                                        
297000                                                                          
297100     MOVE LOGG6-LOGGPOST        TO RYB-AREA                               
297200                                                                          
297300     MOVE RYB-IDDISTR           TO TEST-IDDISTR                           
297400     IF NOT DIST24-NORGE                                                  
297500       MOVE SPACE                 TO SU5-AREA                             
297600       MOVE 'SU5'                 TO SU5-IDPTYP                           
297700                                                                          
297800       MOVE RYB-IDDC              TO W-SU-IDDC                            
297900                                                                          
298000       PERFORM S40-RED-IDSUPPL-SU                                         
298100                                                                          
298200       MOVE RYB-IDDISTR           TO SU5-IDDISTR                          
298300       MOVE RYB-IDKUNDNR          TO SU5-IDKUNDNR                         
298400       MOVE IDSUPPL-SU-WS         TO SU5-IDSUPPL                          
298500                                                                          
298600       MOVE RYB-IDKUNDRF-RO (1:7) TO SU5-IDRONR                           
298700       MOVE RYB-IDARTNR           TO SU5-IDARTNR                          
298800       MOVE RYB-REKSIFFR          TO SU5-REKSIFFR                         
298900       MOVE RYB-KVRO              TO SU5-KVRO                             
299000       MOVE RYB-KDORDKL           TO SU5-KDORDER                          
299100       MOVE RYB-KDFAKTYP          TO SU5-KDFAKTYP                         
299200       MOVE RYB-KDORDKL           TO SU5-KDORDKL                          
299300       MOVE RYB-KDVRINFO          TO SU5-KDVRINFO                         
299400                                                                          
299500       MOVE LOGG6-TIAAMMDD        TO SU5-TIAAMMDD-REG                     
299600       MOVE LOGG6-TIKLOCK         TO SU5-TIKLOCK-REG                      
299700                                                                          
299800*      RÄKNAR UPP TIDEN PÅ SU5 MED 1 SEKUND FÖR ATT                       
299900*      DEN MÅSTE KOMMA EFTER SU4                                          
300000                                                                          
300100       COMPUTE SU5-TIKLOCK-REG    =  SU5-TIKLOCK-REG + 100                
300200                                                                          
300300       WRITE W092Z7-POST          FROM SU5-AREA                           
300400                                                                          
300500                                                                          
300600       MOVE 'W092Z7'              TO POSTSUM-FDNAMN                       
300700       MOVE 'W09208DE'            TO POSTSUM-DDNAMN2                      
300800       MOVE SU5-IDPTYP            TO POSTSUM-TRANSTYP                     
300900       CALL POSTSUM USING         POSTSUM-PARM                            
301000     END-IF                                                               
301100     .                                                                    
301200     EJECT                                                                
301300 C67C-SKAPA-BRIST-POST      SECTION.                                      
301400                                                                          
301500     MOVE '007'                TO OBLAG-IDPTYP                            
301600     MOVE RYB-IDDISTR          TO OBLAG-IDDISTR                           
301700     MOVE RYB-IDKUNDNR         TO OBLAG-IDKUNDNR                          
301800     MOVE RYB-KDFRAKT          TO OBLAG-KDFRAKT                           
301900     MOVE RYB-IDKUNDRF (1:7)   TO OBLAG-IDORDNR                           
302000     MOVE +1                   TO OBLAG-KDLIDEL                           
302100     MOVE RYB-IDDC             TO OBLAG-IDDC                              
302200     MOVE RYB-IDARTNR          TO OBLAG-IDARTNR                           
302300     MOVE SPACE                TO OBLAG-BEART                             
302400     MOVE RYB-BERADREF         TO OBLAG-BERADREF                          
302500     MOVE RYB-BEVOLREF         TO OBLAG-BEVOLREF                          
302600     PERFORM S30-FIXA-RYE-IDORDNR                                         
302700     MOVE W-RYE-IDORDNR        TO OBLAG-IDRONR                            
302800     MOVE RYB-REKSIFFR         TO OBLAG-REKSIFFR                          
302900     MOVE RYB-TIRODAT          TO OBLAG-TIRODAT                           
303000     MOVE RYB-TIORDREG         TO OBLAG-TIORDREG                          
303100     MOVE RYB-KDORDBEK         TO OBLAG-KDRESTR                           
303200     MOVE RYB-KVRO             TO OBLAG-KVBEART                           
303300     MOVE ZERO                 TO OBLAG-KVAVBART                          
303400     MOVE RYB-KDDSP            TO OBLAG-KDDSP                             
303500     MOVE RYB-KVRO             TO OBLAG-KVRO                              
303600     MOVE RYB-KDFAKTYP         TO OBLAG-KDFAKTYP                          
303700     MOVE RYB-TIDISPIN         TO OBLAG-TIDISPIN                          
303800     MOVE RYB-KDKVBRYT         TO OBLAG-KDKVBRYT                          
303900                                                                          
304000     MOVE '007'                 TO POSTSUM-TRANSTYP                       
304100     WRITE 007-UTPOST           FROM UT007-AREA                           
304200                                                                          
304300     MOVE 'W092ZQ'              TO POSTSUM-FDNAMN                         
304400     MOVE 'W09208E2'            TO POSTSUM-DDNAMN2                        
304500     CALL POSTSUM USING         POSTSUM-PARM                              
304600     .                                                                    
304700     EJECT                                                                
304800                                                                          
304900 C69-SKRIV-RYI-POST      SECTION.                                         
305000                                                                          
305100     MOVE LOGG6-LOGGPOST       TO  RYI-AREA                               
305200                                                                          
305300     PERFORM C69A-SKAPA-229-POST                                          
305400     PERFORM C69B-SKAPA-432-POST                                          
305500     .                                                                    
305600     EJECT                                                                
305700 C69A-SKAPA-229-POST  SECTION.                                            
305800     SKIP3                                                                
305900     MOVE RYI-IDDC              TO WS-IDDC                                
306000                                   W-IDDC-B6                              
306100     PERFORM IMS-GU-WDB601                                                
306200     IF DCS-CDC                                                           
306300     OR DCS-CDC-TR                                                        
306400*                                                                         
306500     MOVE SPACE                 TO UTRZ5-AREA                             
306600                                                                          
306700     MOVE '229'                 TO UTRZ5-IDPTYP                           
306800     MOVE RYI-IDARTNR           TO UTRZ5-IDARTNR                          
306900     MOVE 1                     TO UTRZ5-KDCLAGER                         
307000     MOVE RYI-KDRORELS          TO UTRZ5-KDRORELS                         
307100     MOVE RYI-KVAVBART          TO UTRZ5-KVANTAL                          
307200     MOVE RYI-KDUPPD            TO UTRZ5-KDUPPD                           
307300                                                                          
307400     WRITE W092Z5-POST          FROM UTRZ5-AREA                           
307500                                                                          
307600     MOVE 'W092Z5'              TO POSTSUM-FDNAMN                         
307700     MOVE 'W09208DC'            TO POSTSUM-DDNAMN2                        
307800     MOVE UTRZ5-IDPTYP          TO POSTSUM-TRANSTYP                       
307900     CALL POSTSUM USING         POSTSUM-PARM                              
308000     END-IF                                                               
308100     .                                                                    
308200     EJECT                                                                
308300 C69B-SKAPA-432-POST      SECTION.                                        
308400                                                                          
308500     MOVE SPACE                 TO UTRZ4-AREA                             
308600                                                                          
308700     MOVE '432'                 TO UTRZ4-IDPTYP                           
308800     MOVE RYI-IDDC              TO UTRZ4-IDDC                             
308900     MOVE RYI-IDARTNR           TO UTRZ4-IDARTNR                          
309000     MOVE RYI-IDDISTR           TO UTRZ4-IDDISTR                          
309100     MOVE RYI-KDORDKL           TO UTRZ4-KDORDKL                          
309200     MOVE RYI-KVAVBART          TO UTRZ4-KVBEART                          
309300     MOVE ZERO                  TO UTRZ4-KDQPACK                          
309400                                                                          
309500     MOVE 'AAMMDD'             TO  DAT-KDDATFORM                          
309600     MOVE RYI-TIUTSKR          TO  DAT-I-TIDATUM                          
309700                                                                          
309800     CALL WDATKONV USING       DAT-KDDATFORM                              
309900                               DAT-I-TIDATUM                              
310000                               DAT-O-TIDATUM                              
310100                               DAT-KDSVAR                                 
310200                                                                          
310300     MOVE DAT-TIAA             TO   UTRZ4-TIAVBAR                         
310400     MOVE DAT-TIP              TO   UTRZ4-TIAVBPER                        
310500     WRITE W092Z4-POST         FROM UTRZ4-AREA                            
310600                                                                          
310700     MOVE 'W092Z4'              TO POSTSUM-FDNAMN                         
310800     MOVE 'W09208DC'            TO POSTSUM-DDNAMN2                        
310900     MOVE UTRZ5-IDPTYP          TO POSTSUM-TRANSTYP                       
311000     CALL POSTSUM USING         POSTSUM-PARM                              
311100     .                                                                    
311200     EJECT                                                                
311300 C70-SKRIV-RYJ-POST      SECTION.                                         
311400                                                                          
311500     MOVE LOGG6-LOGGPOST       TO  RYJ-AREA                               
311600                                                                          
311700     IF RYJ-KDAVVORS           = +1                                       
311800         PERFORM C70A-SKAPA-433-POST                                      
311900     END-IF                                                               
312000                                                                          
312100     IF RYJ-KDAVVORS           = +2                                       
312200         PERFORM C70B-SKAPA-229-POST                                      
312300     END-IF                                                               
312400     .                                                                    
312500     EJECT                                                                
312600 C70A-SKAPA-433-POST  SECTION.                                            
312700     SKIP3                                                                
312800     MOVE SPACE                 TO UTRZ3-AREA                             
312900                                                                          
313000     MOVE '433'                 TO UTRZ3-IDPTYP                           
313100     MOVE RYJ-IDARTNR           TO UTRZ3-IDARTNR                          
313200     MOVE RYJ-IDDC              TO UTRZ3-IDDC                             
313300     MOVE RYJ-IDDISTR           TO UTRZ3-IDDISTR                          
313400     MOVE RYJ-KDORDKL           TO UTRZ3-KDORDKL                          
313500     COMPUTE UTRZ3-KVBEART      = RYJ-KVANNANT * -1                       
313600     MOVE RYJ-TIUTSKR           TO DAT-AAMMDD                             
313700     MOVE DAT-AAMMDD-AA         TO UTRZ3-TIAVBAR                          
313800     PERFORM S11-HAEMTA-VOLVOPERIOD                                       
313900     MOVE W-TIAVBPER            TO UTRZ3-TIAVBPER                         
314000     MOVE ZERO                  TO UTRZ3-KDQPACK                          
314100                                                                          
314200     WRITE W092Z3-POST          FROM UTRZ3-AREA                           
314300                                                                          
314400     MOVE 'W092Z3'              TO POSTSUM-FDNAMN                         
314500     MOVE 'W09208DA'            TO POSTSUM-DDNAMN2                        
314600     MOVE UTRZ3-IDPTYP          TO POSTSUM-TRANSTYP                       
314700     CALL POSTSUM USING         POSTSUM-PARM                              
314800     .                                                                    
314900     EJECT                                                                
315000 C70B-SKAPA-229-POST  SECTION.                                            
315100     SKIP3                                                                
315200     MOVE SPACE                 TO UTRZ5-AREA                             
315300                                                                          
315400     MOVE '229'                 TO UTRZ5-IDPTYP                           
315500     MOVE RYJ-IDARTNR           TO UTRZ5-IDARTNR                          
315600     MOVE RYJ-IDDC              TO WS-IDDC                                
315700                                   W-IDDC-B6                              
315800     PERFORM IMS-GU-WDB601                                                
315900     IF DCS-CDC                                                           
316000     OR DCS-CDC-TR                                                        
316100        MOVE 1                  TO UTRZ5-KDCLAGER                         
316200     ELSE                                                                 
316300        MOVE 2                  TO UTRZ5-KDCLAGER                         
316400     END-IF                                                               
316500     MOVE RYJ-KDRORELS          TO UTRZ5-KDRORELS                         
316600     COMPUTE UTRZ5-KVANTAL      =  RYJ-KVANNANT  * -1                     
316700     MOVE RYJ-KDUPPD            TO UTRZ5-KDUPPD                           
316800                                                                          
316900     WRITE W092Z5-POST          FROM UTRZ5-AREA                           
317000                                                                          
317100     MOVE 'W092Z5'              TO POSTSUM-FDNAMN                         
317200     MOVE 'W09208DC'            TO POSTSUM-DDNAMN2                        
317300     MOVE UTRZ5-IDPTYP          TO POSTSUM-TRANSTYP                       
317400     CALL POSTSUM USING         POSTSUM-PARM                              
317500     .                                                                    
317600     EJECT                                                                
317700 C71-SKRIV-RYK-POST SECTION.                                              
317800                                                                          
317900     MOVE LOGG6-LOGGPOST        TO  RYK-WDGZRYK                           
318000                                                                          
318100     IF RYK-TIRODAT  = ZERO AND                                           
318200        RYK-KDORDBEK = 92                                                 
318300       PERFORM C71A-RED-SKRIV-UXCW201-POST                                
318400     ELSE                                                                 
318500       IF RYK-KDORDBEK = ZERO                                             
318600         PERFORM C71B-RED-SKRIV-UXCR204-POST                              
318700       ELSE                                                               
318800         IF RYK-KDORDBEK = 90 OR 91 OR 92                                 
318900           PERFORM C71C-RED-SKRIV-UXCW203-POST                            
319000         END-IF                                                           
319100       END-IF                                                             
319200     END-IF                                                               
319300                                                                          
319400     MOVE 'W092XC'              TO POSTSUM-FDNAMN                         
319500     MOVE 'W09208EO'            TO POSTSUM-DDNAMN2                        
319600     MOVE UXC-IDPTYP            TO POSTSUM-TRANSTYP                       
319700     CALL POSTSUM USING         POSTSUM-PARM                              
319800     .                                                                    
319900     EJECT                                                                
320000 C71A-RED-SKRIV-UXCW201-POST SECTION.                                     
320100                                                                          
320200     MOVE '201'                 TO UXC-IDPTYP                             
320300     MOVE RYK-IDDISTR           TO UXCW201-IDDISTR                        
320400     MOVE RYK-IDORDER           TO UXCW201-IDORDER                        
320500     MOVE RYK-IDARTNR           TO UXCW201-IDARTNR                        
320600     MOVE RYK-KVLEVART          TO UXCW201-KVLEVART                       
320700                                                                          
320800     WRITE W092XC-POST          FROM UXCW201-W020201                      
320900     .                                                                    
321000     EJECT                                                                
321100 C71B-RED-SKRIV-UXCR204-POST SECTION.                                     
321200                                                                          
321300     MOVE '204'                 TO UXC-IDPTYP                             
321400     MOVE RYK-IDDISTR           TO UXCR204-IDDISTR                        
321500     MOVE RYK-IDORDER           TO UXCR204-IDORDER                        
321600     MOVE RYK-IDARTNR           TO UXCR204-IDARTNR                        
321700     MOVE RYK-TIRODAT           TO UXCR204-TIRODAT                        
321800     MOVE RYK-KVLEVART          TO UXCR204-KVLEVART                       
321900                                                                          
322000     WRITE W092XC-POST          FROM UXCR204-W020204                      
322100     .                                                                    
322200     EJECT                                                                
322300 C71C-RED-SKRIV-UXCW203-POST SECTION.                                     
322400                                                                          
322500     MOVE '203'                 TO UXC-IDPTYP                             
322600     MOVE RYK-IDDISTR           TO UXCW203-IDDISTR                        
322700     MOVE RYK-IDORDER           TO UXCW203-IDORDER                        
322800     MOVE RYK-IDARTNR           TO UXCW203-IDARTNR                        
322900     MOVE RYK-TIRODAT           TO UXCW203-TIRODAT                        
323000     MOVE RYK-KVLEVART          TO UXCW203-KVLEVART                       
323100     MOVE RYK-KVBEART-Q         TO UXCW203-KVBEART-Q                      
323200     MOVE RYK-KDORDKL           TO UXCW203-KDORDKL                        
323300     MOVE RYK-KDPRODSL          TO UXCW203-KDPRODSL                       
323400                                                                          
323500     WRITE W092XC-POST          FROM UXCW203-W020203                      
323600     .                                                                    
323700     EJECT                                                                
323800 S02-LAES-WDG6 SECTION.                                                   
323900     SKIP2                                                                
324000     PERFORM IMS-GET-WDG601                                               
324100                                                                          
324200     IF  SEGMENT-FINNS                                                    
324300         MOVE 'INFIL'        TO POSTSUM-FDNAMN                            
324400         MOVE 'WDG6'         TO POSTSUM-DDNAMN2                           
324500         MOVE LOGG6-IDPTYP   TO POSTSUM-TRANSTYP                          
324600         CALL POSTSUM USING POSTSUM-PARM                                  
324700     END-IF                                                               
324800     .                                                                    
324900     EJECT                                                                
325000 S11-HAEMTA-VOLVOPERIOD  SECTION.                                         
325100     SKIP2                                                                
325200     MOVE 'AAMMDD'       TO DAT-KDDATFORM                                 
325300     MOVE DAT-AAMMDD     TO DAT-I-TIDATUM                                 
325400                                                                          
325500     CALL WDATKONV USING DAT-KDDATFORM                                    
325600                         DAT-I-TIDATUM                                    
325700                         DAT-O-TIDATUM                                    
325800                         DAT-KDSVAR                                       
325900                                                                          
326000     MOVE DAT-TIP        TO W-TIAVBPER                                    
326100     .                                                                    
326200     EJECT                                                                
326300 S20-SKRIV-LOGGFIL     SECTION.                                           
326400     SKIP2                                                                
326500     MOVE LOGG6-TIAAMMDD       TO  UTX9-TIAAMMDD                          
326600     MOVE LOGG6-TIKLOCK        TO  UTX9-TIKLOCK                           
326700     MOVE LOGG6-IDLOGLOP       TO  UTX9-IDLOGLOP                          
326800     MOVE LOGG6-IDPTYP         TO  UTX9-IDPTYP                            
326900                                                                          
327000     WRITE W092X9-POST FROM UTX9-AREA                                     
327100                                                                          
327200      MOVE 'W092X9'            TO POSTSUM-FDNAMN                          
327300      MOVE 'W09208EL'          TO POSTSUM-DDNAMN2                         
327400      MOVE LOGG6-IDPTYP        TO POSTSUM-TRANSTYP                        
327500      CALL POSTSUM USING       POSTSUM-PARM                               
327600     .                                                                    
327700     EJECT                                                                
327800 S25-CALL-W009KSIF     SECTION.                                           
327900     SKIP2                                                                
328000     MOVE 9                    TO  LGD                                    
328100     CALL W009KSIF             USING  FLT LGD KSIFF                       
328200     .                                                                    
328300     EJECT                                                                
328400 S30-FIXA-RYE-IDORDNR  SECTION.                                           
328500                                                                          
328600     IF RYE-IDKUNDRF-RO (1:7)   >  ZERO                                   
328700         MOVE RYE-IDKUNDRF-RO (1:7) TO W-RYE-IDORDNR                      
328800      ELSE                                                                
328900         IF RYE-IDKUNDRF (1:7) > ZERO                                     
329000             MOVE RYE-IDKUNDRF(1:7) TO W-RYE-IDORDNR                      
329100         END-IF                                                           
329200     END-IF                                                               
329300     .                                                                    
329400     EJECT                                                                
329500 S40-RED-IDSUPPL-SU SECTION.                                              
329600                                                                          
329700*    REDIGERA IDSUPPL FÖR SU-POSTER                                       
329800                                                                          
329900                                                                          
330000     MOVE W-SU-IDDC        TO WS-IDDC                                     
330100                              W-IDDC-B6                                   
330200     PERFORM IMS-GU-WDB601-GE                                             
330300     IF SEGMENT-FINNS                                                     
330400       IF DCS-CDC                                                         
330500*    IF  CDC-SE                                                           
330600         MOVE 711            TO IDSUPPL-SU-WS                             
330700       ELSE                                                               
330810         IF DCS-SDC                                                       
330900             MOVE 729        TO IDSUPPL-SU-WS                             
331000         ELSE                                                             
331100             MOVE 711        TO IDSUPPL-SU-WS                             
331200         END-IF                                                           
331300       END-IF                                                             
331400     ELSE                                                                 
331500       MOVE 711              TO IDSUPPL-SU-WS                             
331600     END-IF                                                               
331700     .                                                                    
331800     EJECT                                                                
331900 ZB-CLOSE-WDG6-FILER SECTION.                                             
332000                                                                          
332100      CLOSE W09290-FEL                                                    
332200            W09291-ANSK                                                   
332300            W092Z3                                                        
332400            W092Z4                                                        
332500            W092Z5                                                        
332600            W092Z7                                                        
332700            W092ZF                                                        
332800            W092ZI                                                        
332900            W092ZN                                                        
333000            W092ZP                                                        
333100            W092ZQ                                                        
333200            W092ZR                                                        
333300            W092ZU                                                        
333400            W092ZV                                                        
333500            W092ZW                                                        
333600            W09299                                                        
333700            W092X3                                                        
333800            W092X4                                                        
333900            W092X9                                                        
334000            W092XC                                                        
334100            W092S1                                                        
334200            W092XX                                                        
334300     .                                                                    
334400     EJECT                                                                
334500 Z-FINIT SECTION.                                                         
334600                                                                          
334700      MOVE 'S'                   TO POSTSUM-OPKOD                         
334800      CALL POSTSUM USING         POSTSUM-PARM                             
334900     .                                                                    
335000     EJECT                                                                
335100 S50-SKRIV-W092S1-POST SECTION.                                           
335200                                                                          
335300     MOVE  LOGG6-WDGZ01 TO    UTSRS-WDGZ01                                
335400     WRITE W092S1-POST  FROM  SRS-AREA                                    
335500                                                                          
335600     MOVE 'W092S1'      TO    POSTSUM-FDNAMN                              
335700     MOVE 'W09208EU'    TO    POSTSUM-DDNAMN2                             
335800     MOVE 'S1  '        TO    POSTSUM-TRANSTYP                            
335900     CALL  POSTSUM      USING POSTSUM-PARM                                
336000     .                                                                    
336100     EJECT                                                                
336200*                                                                         
336300*------ INLAGD TILLFÄLLIGT FÖR LOGISTIK                                   
336400*       RYX SKAPAD I W40373                                               
336500*                                                                         
336600 S5X-SKRIV-W092XX-POST SECTION.                                           
336700                                                                          
336800     MOVE  LOGG6-WDGZ01 TO    UTXXX-WDGZ01                                
336900     WRITE W092XX-POST  FROM  XXX-AREA                                    
337000                                                                          
337100     MOVE 'W092XX'      TO    POSTSUM-FDNAMN                              
337200     MOVE 'W09208XX'    TO    POSTSUM-DDNAMN2                             
337300     MOVE 'RYX '        TO    POSTSUM-TRANSTYP                            
337400     CALL  POSTSUM      USING POSTSUM-PARM                                
337500     .                                                                    
337600     EJECT                                                                
337700*    ***   I M S   S E C T I O N E R  ***                                 
337800                                                                          
337900 IMS-GET-WDG601 SECTION.                                                  
338000                                                                          
338100     MOVE 'WLZZAC01 ' TO SSA1                                             
338200     MOVE '  GB' TO GODK-STATUSKODER                                      
338300     CALL CBLTDLI USING GN WLZZAC-PCB DLI-IO-AREA SSA1                    
338400     MOVE WLZZAC-STATUS-CODE TO STATUS-WS                                 
338500     PERFORM IMS-STATUSKONTROLL                                           
338600     .                                                                    
338700     EJECT                                                                
338800                                                                          
338900 IMS-GU-WDB601    SECTION.                                                
339000     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
339100          DELIMITED BY SIZE INTO SSA1                                     
339200     MOVE '  ' TO GODK-STATUSKODER                                        
339300     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
339400     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
339500     PERFORM IMS-STATUSKONTROLL                                           
339600     .                                                                    
339700     EJECT                                                                
339800                                                                          
339900 IMS-GU-WDB601-GE SECTION.                                                
340000     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
340100          DELIMITED BY SIZE INTO SSA1                                     
340200     MOVE '  GE' TO GODK-STATUSKODER                                      
340300     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
340400     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
340500     PERFORM IMS-STATUSKONTROLL                                           
340600     .                                                                    
340700     EJECT                                                                
340800 IMS-STATUSKONTROLL SECTION.                                              
340900                                                                          
341000     SET STATUS-IX TO 1                                                   
341100     SEARCH GODK-STATUS                                                   
341200       AT END                                                             
341300         CALL FELLOG                                                      
341400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
341500         CONTINUE                                                         
341600     END-SEARCH                                                           
341700     .                                                                    
