000100 ID  DIVISION.                                                            
000200 PROGRAM-ID.    W4605500.                                                 
000300 AUTHOR.        I BENGTSON.                                               
000400 DATE-WRITTEN.  FEB 1985.                                                 
000500*                                                                         
000600*                                                                         
000700*    FUNKTION:                                                            
000800*        PROGRAMMET FRAMSTÄLLER EN FIL MED EN SAMMANSTÄLLNING             
000900*        ÖVER INKOMMNA ORDER SAMT EN FÖRTECKNING ÖVER FELAKTIGA           
001000*        ORDER. PROGRAMMET LÄSER MASTERFILEN W46088 FRAMSTÄLLD I          
001100*        W4600500 , FILEN MED RÄTTA ORDER W46086 FRAMSTÄLLD I             
001200*        W4600500 SAMT FILEN MED FELAKTIGA ORDER W46089 FRAMSTÄLLD        
001300*        I W4600500 SAMT FILEN MED KREDITERINGSTRANSAR W4608S.            
001400*                                                                         
001500*        FILEN SKAPAS FÖR         OCH SÄNDS ÖVER VIA VCOM.                
001600*                                                                         
001700*         -  AUSTRALIEN      7836 IMPORTÖR                                
001800*         -  AUSTRALIEN      7838                                         
001900*         -  BELGIEN         1258                                         
002000*         -  DANMARK          974                                         
002100*         -  ENGLAND         1378                                         
002200*         -  FINLAND         1090                                         
002300*         -  FRANKRIKE       1478                                         
002400*         -  HOLLAND         1678                                         
002500*         -  IRLAND          1778                                         
002600*         -  ITALIEN         1822                                         
002700*         -  JAPAN           5222                                         
002800*         -  KOREA           6124                                         
002900*         -  KINA                                                         
003000*         -  MALAYSIA        5619 IMPORTÖR                                
003100*         -  MALAYSIA        5627                                         
003200*         -  NORGE            878                                         
003300*         -  OSTERRIKE        2378                                        
003400*         -  POLEN            2870 IMPORTÖR                               
003500*         -  POLEN            2878                                        
003600*         -  PORTUGAL         1958                                        
003700*         -  PORTUGAL         1978                                        
003800*         -  SCHWEIZ          2070 IMPORTÖR                               
003900*         -  SCHWEIZ          2078                                        
004000*         -  SPANIEN          2178                                        
004100*         -  SVERIGE           778                                        
004200*         -  SYDAFRIKA                                                    
004300*         -  TAIWAN           6221 IMPORTÖR                               
004400*         -  TAIWAN           6223                                        
004500*         -  TAIWAN2          6222 IMPORTÖR                               
004600*         -  TAIWAN2          6224                                        
004700*         -  THAILAND         6251 IMPORTÖR                               
004800*         -  THAILAND         6225                                        
004900*         -  TYSKLAND         2278                                        
005000*         -  USA              7574                                        
005100*         -  CANADA           7674                                        
005200*         -  BRASILIEN        7030                                        
005300*         -  MEXICO           6587 IMPORTÖR                               
005400*         -  MEXICO           6580                                        
005500*     *****************************************************               
005600*                                                                         
005700*    ABENDKODER:                                                          
005800*        U0016 OM DISTRIKTET INTE FINNS I DDNAMNSTABELLEN                 
005900*                                                                         
006000     EJECT                                                                
006100 ENVIRONMENT DIVISION.                                                    
006200     SKIP2                                                                
006300 CONFIGURATION SECTION.                                                   
006400     SKIP2                                                                
006500 INPUT-OUTPUT SECTION.                                                    
006600                                                                          
006700 FILE-CONTROL.                                                            
006800     SKIP2                                                                
006900*- - - - - - - - - - - - INFILER:                                         
007000     SELECT W46088                       ASSIGN TO UT-S-W46055D1.         
007100     SELECT W46086                       ASSIGN TO UT-S-W46055D2.         
007200     SELECT W46089                       ASSIGN TO UT-S-W46055D3.         
007300     SELECT W4608S                       ASSIGN TO UT-S-W46055D4.         
007400     SELECT SORTFIL                      ASSIGN TO UT-S-W46055DS.         
007500*- - - - - - - - - - - - UTFILER:                                         
007600     SELECT W46055                       ASSIGN TO UT-S-W46055D5.         
007700                                                                          
007800     EJECT                                                                
007900 DATA DIVISION.                                                           
008000     SKIP2                                                                
008100 FILE SECTION.                                                            
008200     SKIP3                                                                
008300 FD  W46088                                                               
008400     LABEL RECORD   STANDARD                                              
008500     RECORDING      V                                                     
008600     BLOCK CONTAINS 0.                                                    
008700     SKIP2                                                                
008800*    -COPY W460001   -L.                                                  
008900     SKIP2                                                                
009000*    -COPY W460002   -L.                                                  
009100     SKIP2                                                                
009200*    -COPY W460003   -L.                                                  
009300 FD  W46086                                                               
009400     LABEL RECORD   STANDARD                                              
009500     RECORDING      V                                                     
009600     BLOCK CONTAINS 0.                                                    
009700     SKIP2                                                                
009800*    -COPY W460001   -L.                                                  
009900     SKIP2                                                                
010000*    -COPY W460002   -L.                                                  
010100     EJECT                                                                
010200 FD  W46089                                                               
010300     LABEL RECORD   STANDARD                                              
010400     RECORDING      V                                                     
010500     BLOCK CONTAINS 0.                                                    
010600     SKIP2                                                                
010700*    -COPY W460001   -L.                                                  
010800     SKIP2                                                                
010900*    -COPY W460002   -L.                                                  
011000     SKIP2                                                                
011100*    -COPY W460003   -L.                                                  
011200     EJECT                                                                
011300 FD  W4608S                                                               
011400     LABEL RECORD   STANDARD                                              
011500     RECORDING      V                                                     
011600     BLOCK CONTAINS 0.                                                    
011700     SKIP2                                                                
011800*    -COPY W460006   -L.                                                  
011900     EJECT                                                                
012000 FD  W46055                                                               
012100     LABEL RECORD   STANDARD                                              
012200     RECORDING      F                                                     
012300     BLOCK CONTAINS 0.                                                    
012400     SKIP2                                                                
012500 01  W46055-001-RAD                 PIC X(121).                           
012600     EJECT                                                                
012700 SD  SORTFIL                                                              
012800     RECORDING V.                                                         
012900     SKIP2                                                                
013000 01  SORT-POST.                                                           
013100     SKIP2                                                                
013200*03  FILLER  -COPY W460001  -PRE SORT-                                    
013300     06  SORT-IDTRANSLOP         PIC 9(5).                                
013400     EJECT                                                                
013500*01  FILLER  -COPY W460002  -PRE SORT-                                    
013600     EJECT                                                                
013700*01  FILLER  -COPY W460003  -PRE SORT-                                    
013800     EJECT                                                                
013900*01  FILLER  -COPY W460006  -PRE SORT-                                    
014000     EJECT                                                                
014100 WORKING-STORAGE SECTION.                                                 
014200*    -- CHECKED BY WY2000                                                 
014300     SKIP3                                                                
014400*- - - - - - - - - - - - - -  GENERERAT PROGRAM-NAMN                      
014500 77   PROGRAM-NAMN           VALUE 'W4605500'                             
014600                                 PIC X(8).                                
014700     SKIP2                                                                
014800 01  GENERELLA-KONSTANTER.                                                
014900*                                                                         
015000     03  JA                      PIC X(1)    VALUE 'J'.                   
015100     03  NEJ                     PIC X(1)    VALUE 'N'.                   
015200     SKIP2                                                                
015300 01  END-OF-FILE-SWITCHAR.                                                
015400*                                                                         
015500     03  W46088-EOF              PIC X(1)    VALUE 'N'.                   
015600     03  W46086-EOF              PIC X(1)    VALUE 'N'.                   
015700     03  W46089-EOF              PIC X(1)    VALUE 'N'.                   
015800     03  W4608S-EOF              PIC X(1)    VALUE 'N'.                   
015900     03  SORTFIL-EOF             PIC X(1)    VALUE 'N'.                   
016000     SKIP2                                                                
016100 01  DYNAMISKA-SUBPROGRAM.                                                
016200*                                                                         
016300   03  ABEND                     PIC X(8)    VALUE 'ABEND'.               
016400   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM'.             
016500   03  DDMOD                     PIC X(8)    VALUE 'DDMOD'.               
016600   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
016700   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
016800   03  W460DIS1                  PIC X(8)    VALUE 'W460DIS1'.            
016900     SKIP3                                                                
017000 01  RETURKODER.                                                          
017100*                                                                         
017200   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4)  COMP SYNC VALUE +16.          
017300     SKIP3                                                                
017400 01  HJALPAREOR.                                                          
017500*                                                                         
017600   03  WS-ANTAL-RADER            PIC S9(7)  VALUE ZERO.                   
017700   03  WS-ANTAL-RADER-OK         PIC S9(9)  VALUE ZERO.                   
017800   03  WS-IDDISTR-1              PIC 9(4)   VALUE ZERO.                   
017900   03  WS-IDDISTR-2              PIC 9(4)   VALUE ZERO.                   
018000   03  WS-TIFILDAT               PIC 9(6)   VALUE ZERO.                   
018100   03  WS-TIHHMMSS               PIC 9(6)   VALUE ZERO.                   
018200   03  WS-IDKUNDNR               PIC 9(6)   VALUE ZERO.                   
018300   03  WR-IDKUNDNR               PIC Z(5)9  VALUE ZERO.                   
018400   03  WS-IDORDNR                PIC 9(7)   VALUE ZERO.                   
018500   03  WR-IDORDNR                PIC Z(6)9  VALUE ZERO.                   
018600   03  WS-BEBETRAD-1             PIC X(35)  VALUE SPACE.                  
018700   03  WS-BEBETRAD-2             PIC X(35)  VALUE SPACE.                  
018800   03  WS-KDFEL                  PIC 9(3)   VALUE ZERO.                   
018900   03  WS-FELTEXT                PIC X(49)  VALUE SPACE.                  
019000   03  WS-SUHASH-RAETT           PIC Z(13)9 VALUE ZERO.                   
019100   03  FLAGGA-DISTR              PIC X(1)   VALUE 'N'.                    
019200   03  RHARAKNARE                PIC 9(5)   VALUE ZERO.                   
019300   03  RHBRAKNARE                PIC 9(5)   VALUE ZERO.                   
019400   03  RHCRAKNARE                PIC 9(5)   VALUE ZERO.                   
019500   03  RHDRAKNARE                PIC 9(5)   VALUE ZERO.                   
019600   03  RHERAKNARE                PIC 9(5)   VALUE ZERO.                   
019700   03  RHFRAKNARE                PIC 9(5)   VALUE ZERO.                   
019800   03  RHTOTAL                   PIC 9(7)   VALUE ZERO.                   
019900   03  W-TOM-MASTER              PIC X(1)   VALUE 'N'.                    
020000     EJECT                                                                
020100 01  FELTEXT-AREA-START          PIC X(24)  VALUE                         
020200                                 'FELTEXT-AREA-START'.                    
020300     SKIP2                                                                
020400 01  FELTEXT-AREA.                                                        
020500     SKIP2                                                                
020600     03  FILLER.                                                          
020700         05  FELTEXT1A           PIC X(16)  VALUE                         
020800             'WRONG HASH-TOTAL'.                                          
020900         05  FELTEXT1B           PIC X(3)   VALUE                         
021000             ' = '.                                                       
021100         05  FELTEXT1C           PIC X(30)  VALUE SPACE.                  
021200     03  FILLER.                                                          
021300         05  FELTEXT2A           PIC X(20)  VALUE                         
021400             'WRONG CHECK-DIGIT = '.                                      
021500         05  FELTEXT2B           PIC X(29)  VALUE SPACE.                  
021600     03  FILLER                  PIC X(49)  VALUE                         
021700         'ORDER LINES MISSING                              '.             
021800     03  FILLER                  PIC X(49)  VALUE                         
021900         'DOUBLE ORDER HEAD                                '.             
022000     03  FILLER                  PIC X(49)  VALUE                         
022100         'LINES WITHOUT ORDER HEAD                         '.             
022200     03  FILLER                  PIC X(49)  VALUE                         
022300         'IDENTITY MISSING ON CUSTOMER FILE                '.             
022400     03  FILLER                  PIC X(49)  VALUE                         
022500         'C-WAREHOUSE MISSING ON CUSTOMER FILE             '.             
022600     03  FILLER                  PIC X(49)  VALUE                         
022700         'FREIGHT-CODE MISSING ON CUSTOMER FILE            '.             
022800     03  FILLER                  PIC X(49)  VALUE                         
022900         'ORDER-IDENTITY ALREADY ON ORDER FILE             '.             
023000     03  FILLER                  PIC X(49)  VALUE                         
023100         'DISTR., CUST. OR ORDERNO. NOT NUMERIC            '.             
023200     03  FILLER                  PIC X(49)  VALUE                         
023300         'ORDERID. ATT. BO-CODE, FR.CODE., LANG.C. NOT NUM.'.             
023400     03  FILLER                  PIC X(49)  VALUE                         
023500         'PARTNO., CHECK-DGT, ORDERQ., BREAKPACK NOT NUM.  '.             
023600     03  FILLER                  PIC X(49)  VALUE                         
023700         'HASH-TOTAL NOT NUMERIC                           '.             
023800     03  FILLER                  PIC X(49)  VALUE                         
023900         'START-CARD MISSING                               '.             
024000     03  FILLER                  PIC X(49)  VALUE                         
024100         'FILE SENT EARLIER                                '.             
024200     03  FILLER                  PIC X(49)  VALUE                         
024300         'WRONG NUMBER OF TRANSACTIONS                     '.             
024400     03  FILLER                  PIC X(49)  VALUE                         
024500         'END-CARD MISSING                                 '.             
024600     03  FILLER                  PIC X(49)  VALUE                         
024700         'HASH-TOTAL-CARD MISSING                          '.             
024800     03  FILLER                  PIC X(49)  VALUE                         
024900         'DOUBLE START-CARD                                '.             
025000     03  FILLER                  PIC X(49)  VALUE                         
025100         'WRONG DISTRICT ON START-CARD                     '.             
025200     03  FILLER                  PIC X(49)  VALUE                         
025300         'THIS DISTRICT SHALL NOT HAVE HASH-TOTAL-CARD     '.             
025400     03  FILLER                  PIC X(49)  VALUE                         
025500         'ONLY ORDERCLASS = 2, 3 AND 4 ARE VALID           '.             
025600     03  FILLER                  PIC X(49)  VALUE                         
025700         'ONLY ORDER CLASS = 3 AND 4 ARE VALID             '.             
025800     03  FILLER                  PIC X(49)  VALUE                         
025900         'TPO WEEK MUST BE > PRESENT WEEK                  '.             
026000     03  FILLER                  PIC X(49)  VALUE                         
026100         'THIS ORDER IS NOT A CORRECT TPO                  '.             
026200     SKIP3                                                                
026300 01  FILLER REDEFINES FELTEXT-AREA.                                       
026400     03  FELTEXT OCCURS 25 INDEXED BY IX-FEL PIC X(49).                   
026500     EJECT                                                                
026600*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
026700                                                                          
026800*01  -COPY W0005       -PRE  POSTSUM-.                                    
026900     EJECT                                                                
027000*- - - - - - - - - - - - - -  PARAMETRAR TILL W460DIS1                    
027100                                                                          
027200*01  -COPY W460DIS1                                                       
027300     EJECT                                                                
027400*01  -COPY W460LISO                                                       
027500     EJECT                                                                
027600*- - - - - - - - - - - - - - - -DISTRIKTSCOPYTEXT                         
027700                                                                          
027800 01  TEST-IDDISTR                PIC 9(5)  COMP-3.                        
027900     SKIP2                                                                
028000*01  FILLER  -COPY WWDIS130  -RED TEST-IDDISTR                            
028100     EJECT                                                                
028200*- - - - - - - - - - - - - -  IMS-AREOR                                   
028300                                                                          
028400 01  IMS-AREA-START              PIC X(24)   VALUE                        
028500                                            'IMS-AREA-START'.             
028600 01  NYCKLAR-TILL-DLI.                                                    
028700   03  W-IDGMT-X.                                                         
028800     05  W-IDDISTR-WDB2          PIC S9(5)                COMP-3.         
028900     05  W-IDKUNDNR-WDB2         PIC S9(7)                COMP-3.         
029000                                                                          
029100   03  W-WDB101KY-X.                                                      
029200     05  W-WDB1-IDPARTNR         PIC X(9)    VALUE SPACE.                 
029300     05  W-WDB1-IDFTG            PIC 9(2)    VALUE ZERO.                  
029400     EJECT                                                                
029500*        ARBETSAREOR TILL IMS-SEKTIONERNA                                 
029600*                                                                         
029700 01  IMS-WS.                                                              
029800   03  FILLER                    PIC X(8)    VALUE 'IMS-WS  '.            
029900     SKIP3                                                                
030000*                            *** STATUSKOD FRÅN IMS                       
030100   03  STATUS-WS                 PIC XX.                                  
030200     88  SEGMENT-FINNS                       VALUE '  '.                  
030300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
030400     SKIP3                                                                
030500   03  GODK-STATUSKODER.                                                  
030600     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
030700     SKIP3                                                                
030800   03  SSA1                      PIC X(64).                               
030900     EJECT                                                                
031000*01  -COPY W0003                                                          
031100     EJECT                                                                
031200 01  DLI-IO-AREA-WDB2.                                                    
031300*  03  WLGMTA01 -COPY WDB201 -PRE WDB201-                                 
031400     EJECT                                                                
031500     SKIP2                                                                
031600 01  DLI-IO-AREA.                                                         
031700*  03  WDB1 -COPY WDB101 -PRE WDB101-                                     
031800     EJECT                                                                
031900     SKIP2                                                                
032000 01  I08-AREA-START              PIC X(24)   VALUE                        
032100                                            'I08-AREA-START'.             
032200 01  I08-AREA.                                                            
032300     SKIP2                                                                
032400*03  FILLER  -COPY W460001  -PRE I08-                                     
032500     EJECT                                                                
032600*01  FILLER  -COPY W460002  -PRE I08-  -RED I08-AREA                      
032700     EJECT                                                                
032800*01  FILLER  -COPY W460003  -PRE I08-  -RED I08-AREA                      
032900     EJECT                                                                
033000*01  FILLER  -COPY W460006  -PRE I08-  -RED I08-AREA                      
033100     EJECT                                                                
033200 01  I26-AREA-START              PIC X(24)   VALUE                        
033300                                            'I26-AREA-START'.             
033400 01  I26-AREA.                                                            
033500     SKIP2                                                                
033600*03  FILLER  -COPY W460001  -PRE I26-                                     
033700     EJECT                                                                
033800*01  FILLER  -COPY W460002  -PRE I26-  -RED I26-AREA                      
033900     EJECT                                                                
034000 01  I36-AREA-START              PIC X(24)   VALUE                        
034100                                            'I36-AREA-START'.             
034200 01  I36-AREA.                                                            
034300     SKIP2                                                                
034400*03  FILLER  -COPY W460001  -PRE I36-                                     
034500     EJECT                                                                
034600*01  FILLER  -COPY W460002  -PRE I36-  -RED I36-AREA                      
034700     EJECT                                                                
034800*01  FILLER  -COPY W460003  -PRE I36-  -RED I36-AREA                      
034900     EJECT                                                                
035000 01  I1S-AREA-START              PIC X(24)   VALUE                        
035100                                            'I1S-AREA-START'.             
035200 01  I1S-AREA.                                                            
035300     SKIP2                                                                
035400*03  FILLER  -COPY W460006  -PRE I1S-                                     
035500     EJECT                                                                
035600 01  SORTWS-AREA-START            PIC X(24) VALUE                         
035700                                  'SORTWS-AREA-START'.                    
035800     SKIP3                                                                
035900 01  SORTWS-AREA.                                                         
036000     SKIP2                                                                
036100*03  FILLER  -PRE SORTWS-  -COPY W460001                                  
036200     06  SORTWS-IDTRANSLOP               PIC 9(5).                        
036300     EJECT                                                                
036400*01  FILLER  -PRE SORTWS-  -COPY W460002  -RED SORTWS-AREA                
036500     EJECT                                                                
036600*01  FILLER  -PRE SORTWS-  -COPY W460003  -RED SORTWS-AREA                
036700     EJECT                                                                
036800 01  W001-AREA-START             PIC X(24)   VALUE                        
036900                                            'W001-AREA-START'.            
037000     SKIP3                                                                
037100 01  W001-HJALPAREOR.                                                     
037200*                                                                         
037300     03  W001-SIDRAKNARE                                                  
037400                                 PIC S9(5)   COMP-3 VALUE ZERO.           
037500     03  W001-RADRAKNARE                                                  
037600                                 PIC S9(3)   COMP-3 VALUE +100.           
037700     03  W001-MAX-RADER-PER-SIDA                                          
037800                                 PIC 9(3)    VALUE 44.                    
037900     SKIP2                                                                
038000     03  W001-RAD.                                                        
038100         05  FILLER              PIC X(121)  VALUE SPACE.                 
038200     EJECT                                                                
038300 01  W001R1-RUBRIK.                                                       
038400*                                                                         
038500     03  W001R1-SKIP             PIC X(1)    VALUE '1'.                   
038600     03  FILLER                  PIC X(1)    VALUE SPACE.                 
038700     03  FILLER           PIC X(17)   VALUE 'VOLVO CAR PARTS'.            
038800     03  W001R1-PROGNAMN         PIC X(13)   VALUE 'W46055-001'.          
038900     03  W001R1-LISTNAMN         PIC X(59)   VALUE                        
039000            'O R D E R  R E C E I V E D  C O N F I R M A T I O N'.        
039100     03  FILLER                  PIC X(5)    VALUE 'TIME'.                
039200     03  W001R1-TIME             PIC XXBXX.                               
039300     03  FILLER                  PIC X(3)    VALUE SPACE.                 
039400     03  FILLER                  PIC X(5)    VALUE 'PAGE'.                
039500     03  W001R1-SIDNR            PIC Z(3)9.                               
039600     03  FILLER                  PIC X(7)    VALUE SPACE.                 
039700     SKIP3                                                                
039800 01  W001R2-RUBRIK.                                                       
039900*                                                                         
040000     03  W001R2-SKIP             PIC X(1)    VALUE ' '.                   
040100     03  FILLER                  PIC X(90)   VALUE SPACE.                 
040200     03  FILLER                  PIC X(5)    VALUE 'DATE'.                
040300     03  W001R2-DATE             PIC XXBXXBXX.                            
040400     03  FILLER                  PIC X(16)   VALUE SPACE.                 
040500     SKIP3                                                                
040600 01  W001R3-RUBRIK.                                                       
040700*                                                                         
040800     03  W001R3-SKIP             PIC X(1)    VALUE ' '.                   
040900     03  FILLER                  PIC X(45)   VALUE SPACE.                 
041000     03  FILLER                  PIC X(12)   VALUE 'TOTAL ORDERS'.        
041100     03  FILLER                  PIC X(62)   VALUE SPACE.                 
041200     EJECT                                                                
041300 01  W001R4-RUBRIK.                                                       
041400*                                                                         
041500     03  W001R4-SKIP             PIC X(1)    VALUE '-'.                   
041600     03  FILLER                  PIC X(18)   VALUE SPACE.                 
041700     03  FILLER                  PIC X(13)   VALUE                        
041800               'IMPORTER NAME'.                                           
041900     03  FILLER                  PIC X(14)   VALUE SPACE.                 
042000     03  FILLER                  PIC X(21)   VALUE                        
042100               'FILE GENERATED: DATE '.                                   
042200     03  W001R4-DATE             PIC XXBXXBXX.                            
042300     03  FILLER                  PIC X(7)   VALUE                         
042400               '  TIME '.                                                 
042500     03  W001R4-TIME             PIC XXBXX.                               
042600     03  FILLER                  PIC X(46)   VALUE SPACE.                 
042700     SKIP2                                                                
042800 01  W001R5-RUBRIK.                                                       
042900*                                                                         
043000     03  W001R5-SKIP             PIC X(1)    VALUE '0'.                   
043100     03  FILLER                  PIC X(18)   VALUE SPACE.                 
043200     03  W001R5-BEBETRAD-1       PIC X(35)   VALUE SPACE.                 
043300     03  FILLER                  PIC X(66)   VALUE SPACE.                 
043400     SKIP2                                                                
043500 01  W001R6-RUBRIK.                                                       
043600*                                                                         
043700     03  W001R6-SKIP             PIC X(1)    VALUE ' '.                   
043800     03  FILLER                  PIC X(18)   VALUE SPACE.                 
043900     03  W001R6-BEBETRAD-2       PIC X(35)   VALUE SPACE.                 
044000     03  FILLER                  PIC X(66)   VALUE SPACE.                 
044100     SKIP2                                                                
044200 01  W001R7-RUBRIK.                                                       
044300*                                                                         
044400     03  W001R7-SKIP             PIC X(1)    VALUE '-'.                   
044500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
044600     03  FILLER                  PIC X(33)   VALUE                        
044700               'SUMMARY OF ORDERS FROM DISTRICT: '.                       
044800     03  W001R7-IDDISTR          PIC 9(4)    VALUE ZERO.                  
044900     03  FILLER                  PIC X(81)   VALUE SPACE.                 
045000     EJECT                                                                
045100 01  W001R8-RUBRIK.                                                       
045200*                                                                         
045300     03  W001R8-SKIP             PIC X(1)    VALUE '-'.                   
045400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
045500     03  FILLER                  PIC X(33)   VALUE                        
045600               'DEALER  ORDERNO.  LINES  LINES-OK'.                       
045700     03  FILLER                  PIC X(85)   VALUE SPACE.                 
045800     SKIP2                                                                
045900 01  W001R9-RUBRIK.                                                       
046000*                                                                         
046100     03  W001R9-SKIP             PIC X(1)    VALUE ' '.                   
046200     03  FILLER                  PIC X(45)   VALUE SPACE.                 
046300     03  FILLER                  PIC X(12)   VALUE 'WRONG ORDERS'.        
046400     03  FILLER                  PIC X(62)   VALUE SPACE.                 
046500     SKIP2                                                                
046600 01  W001R10-RUBRIK.                                                      
046700*                                                                         
046800     03  W001R10-SKIP            PIC X(1)    VALUE '-'.                   
046900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
047000     03  FILLER                  PIC X(16)   VALUE                        
047100                                 'TRANSACTION LIST'.                      
047200     03  FILLER                  PIC X(102)  VALUE SPACE.                 
047300     SKIP2                                                                
047400 01  W001R11-RUBRIK.                                                      
047500*                                                                         
047600     03  W001R11-SKIP            PIC X(1)    VALUE '-'.                   
047700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
047800     03  FILLER                  PIC X(49)   VALUE                        
047900         'TT.DI..DE....OR.....DREF......F.CCALIREF......BN.'.             
048000     03  FILLER                  PIC X(11)   VALUE                        
048100         'TKAMPRF.   '.                                                   
048200     03  W001R11-FEL             PIC X(22)   VALUE                        
048300         'TRNO.  ERR  ERROR-TEXT'.                                        
048400     03  FILLER                  PIC X(39)   VALUE SPACE.                 
048500     EJECT                                                                
048600 01  W001R12-RUBRIK.                                                      
048700*                                                                         
048800     03  W001R12-SKIP            PIC X(1)    VALUE '-'.                   
048900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
049000     03  FILLER                  PIC X(48)   VALUE                        
049100         'TT.DI..DE....OR.....PA.......CLREF......Q.....CC'.              
049200     03  FILLER                  PIC X(12)   VALUE                        
049300         'TITPO.S     '.                                                  
049400     03  W001R12-FEL             PIC X(22)   VALUE                        
049500         'TRNO.  ERR  ERROR-TEXT'.                                        
049600     03  FILLER                  PIC X(39)   VALUE SPACE.                 
049700     SKIP2                                                                
049800 01  W001R13-RUBRIK.                                                      
049900*                                                                         
050000     03  W001R13-SKIP            PIC X(1)    VALUE '-'.                   
050100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
050200     03  FILLER                  PIC X(34)   VALUE                        
050300         'TT.DI..DE....OR.....HASH..........'.                            
050400     03  FILLER                  PIC X(26)   VALUE SPACE.                 
050500     03  W001R13-FEL             PIC X(22)   VALUE                        
050600         'TRNO.  ERR  ERROR-TEXT'.                                        
050700     03  FILLER                  PIC X(39)   VALUE SPACE.                 
050800     SKIP2                                                                
050900 01  W001R14-RUBRIK.                                                      
051000*                                                                         
051100     03  W001R14-SKIP            PIC X(1)    VALUE '-'.                   
051200     03  FILLER                  PIC X(19)   VALUE SPACE.                 
051300     03  FILLER                  PIC X(55)   VALUE                        
051400       'T R A N S M I S S I O N  F R O M  V I P S  T O  N O A C'.         
051500     03  FILLER                  PIC X(46)   VALUE SPACE.                 
051600     SKIP2                                                                
051700 01  W001R15-RUBRIK.                                                      
051800*                                                                         
051900     03  W001R15-SKIP            PIC X(1)    VALUE '-'.                   
052000     03  FILLER                  PIC X(19)   VALUE SPACE.                 
052100     03  FILLER                  PIC X(7)    VALUE 'RECTYPE'.             
052200     03  FILLER                  PIC X(15)   VALUE SPACE.                 
052300     03  FILLER                  PIC X(4)    VALUE 'RCDS'.                
052400     03  FILLER                  PIC X(15)   VALUE SPACE.                 
052500     03  FILLER                  PIC X(11)   VALUE 'DESCRIPTION'.         
052600     03  FILLER                  PIC X(49)   VALUE SPACE.                 
052700     EJECT                                                                
052800 01  W001D1-DETALJ.                                                       
052900*                                                                         
053000     03  W001D1-SKIP             PIC X(1)    VALUE ' '.                   
053100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
053200     03  W001D1-IDKUNDNR         PIC X(6)    VALUE SPACE.                 
053300     03  FILLER                  PIC X(2)    VALUE SPACE.                 
053400     03  W001D1-IDORDNR          PIC X(7)    VALUE SPACE.                 
053500     03  W001D1-ANT-TRANS        PIC Z(7)9   VALUE ZERO.                  
053600     03  W001D1-ANT-TRANS-OK     PIC Z(9)9   VALUE ZERO.                  
053700     03  FILLER                  PIC X(5)    VALUE SPACE.                 
053800     03  W001D1-TEXT             PIC X(20)   VALUE SPACE.                 
053900     03  FILLER                  PIC X(60)   VALUE SPACE.                 
054000     SKIP2                                                                
054100 01  W001D2-DETALJ.                                                       
054200*                                                                         
054300     03  W001D2-SKIP             PIC X(1)    VALUE ' '.                   
054400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
054500     03  W001D2-IDPTYP           PIC X(3)    VALUE SPACE.                 
054600     03  W001D2-IDDISTR          PIC X(4)    VALUE SPACE.                 
054700     03  W001D2-IDKUNDNR         PIC X(6)    VALUE SPACE.                 
054800     03  W001D2-IDORDNR          PIC X(7)    VALUE SPACE.                 
054900     03  W001D2-BEVOLREF         PIC X(10)   VALUE SPACE.                 
055000     03  W001D2-KDFRAKT          PIC 9(2)    VALUE ZERO.                  
055100     03  W001D2-KDORDKL          PIC 9       VALUE ZERO.                  
055200     03  W001D2-KDORDKL-IMP      PIC 9       VALUE ZERO.                  
055300     03  W001D2-KDROPACK         PIC X       VALUE SPACE.                 
055400     03  W001D2-KDSPRAK          PIC X       VALUE SPACE.                 
055500     03  W001D2-BEVARREF         PIC X(10)   VALUE SPACE.                 
055600     03  W001D2-FLRESTN          PIC X       VALUE SPACE.                 
055700     03  W001D2-KDNCNOT          PIC X(2)    VALUE SPACE.                 
055800     03  W001D2-KDTPOTYP         PIC 9(1)    VALUE ZERO.                  
055900     03  W001D2-IDKAMPRF         PIC 9(7)    VALUE ZERO.                  
056000     03  FILLER                  PIC X(3)    VALUE SPACE.                 
056100     03  W001D2-IDTRANSLOP       PIC Z(4)9   VALUE ZERO.                  
056200     03  FILLER                  PIC X(2)    VALUE SPACE.                 
056300     03  W001D2-KDFEL            PIC Z(3)    VALUE ZERO.                  
056400     03  FILLER                  PIC X(2)    VALUE SPACE.                 
056500     03  W001D2-FELTEXT          PIC X(49)   VALUE SPACE.                 
056600     EJECT                                                                
056700 01  W001D3-DETALJ.                                                       
056800*                                                                         
056900     03  W001D3-SKIP             PIC X(1)    VALUE ' '.                   
057000     03  FILLER                  PIC X(1)    VALUE SPACE.                 
057100     03  W001D3-IDPTYP           PIC X(3)    VALUE SPACE.                 
057200     03  W001D3-IDDISTR          PIC X(4)    VALUE SPACE.                 
057300     03  W001D3-IDKUNDNR         PIC X(6)    VALUE SPACE.                 
057400     03  W001D3-IDORDNR          PIC X(7)    VALUE SPACE.                 
057500     03  W001D3-IDARTNR          PIC X(9)    VALUE SPACE.                 
057600     03  W001D3-REKSIFFR         PIC 9       VALUE ZERO.                  
057700     03  W001D3-BERADREF         PIC X(10)   VALUE SPACE.                 
057800     03  W001D3-KVBEART          PIC 9(6)    VALUE ZERO.                  
057900     03  W001D3-KDKVBRYT         PIC 9       VALUE ZERO.                  
058000     03  W001D3-KDDSP            PIC 9       VALUE ZERO.                  
058100     03  W001D3-TITPO            PIC 9(6)    VALUE ZERO.                  
058200     03  W001D3-FLSLATT          PIC X(1)    VALUE ZERO.                  
058300     03  W001D3-FLDIRLEV         PIC X(1)    VALUE ZERO.                  
058400     03  FILLER                  PIC X(4)    VALUE SPACE.                 
058500     03  W001D3-IDTRANSLOP       PIC Z(4)9   VALUE ZERO.                  
058600     03  FILLER                  PIC X(2)    VALUE SPACE.                 
058700     03  W001D3-KDFEL            PIC Z(3)    VALUE ZERO.                  
058800     03  FILLER                  PIC X(2)    VALUE SPACE.                 
058900     03  W001D3-FELTEXT          PIC X(49)   VALUE SPACE.                 
059000     EJECT                                                                
059100 01  W001D4-DETALJ.                                                       
059200*                                                                         
059300     03  W001D4-SKIP             PIC X(1)    VALUE ' '.                   
059400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
059500     03  W001D4-IDPTYP           PIC X(3)    VALUE SPACE.                 
059600     03  W001D4-IDDISTR          PIC X(4)    VALUE SPACE.                 
059700     03  W001D4-IDKUNDNR         PIC X(6)    VALUE SPACE.                 
059800     03  W001D4-IDORDNR          PIC X(7)    VALUE SPACE.                 
059900     03  W001D4-SUHASH           PIC 9(14)   VALUE ZERO.                  
060000     03  FILLER                  PIC X(26)   VALUE SPACE.                 
060100     03  W001D4-IDTRANSLOP       PIC Z(4)9   VALUE ZERO.                  
060200     03  FILLER                  PIC X(2)    VALUE SPACE.                 
060300     03  W001D4-KDFEL            PIC Z(3)    VALUE ZERO.                  
060400     03  FILLER                  PIC X(2)    VALUE SPACE.                 
060500     03  W001D4-FELTEXT          PIC X(49)   VALUE SPACE.                 
060600     SKIP2                                                                
060700 01  W001D5-DETALJ.                                                       
060800*                                                                         
060900     03  W001D5-SKIP             PIC X(1)    VALUE '0'.                   
061000     03  FILLER                  PIC X(23)   VALUE SPACE.                 
061100     03  FILLER                  PIC X(3)    VALUE 'RH0'.                 
061200     03  FILLER                  PIC X(18)   VALUE SPACE.                 
061300     03  FILLER                  PIC X       VALUE '-'.                   
061400     03  FILLER                  PIC X(15)   VALUE SPACE.                 
061500     03  FILLER                  PIC X(12)   VALUE 'START RECORD'.        
061600     03  FILLER                  PIC X(48)   VALUE SPACE.                 
061700     EJECT                                                                
061800 01  W001D6-DETALJ.                                                       
061900*                                                                         
062000     03  W001D6-SKIP             PIC X(1)    VALUE '0'.                   
062100     03  FILLER                  PIC X(23)   VALUE SPACE.                 
062200     03  FILLER                  PIC X(3)    VALUE 'RHA'.                 
062300     03  FILLER                  PIC X(10)   VALUE SPACE.                 
062400     03  W001D6-RHARAKNARE       PIC Z(8)9   VALUE ZERO.                  
062500     03  FILLER                  PIC X(15)   VALUE SPACE.                 
062600     03  FILLER                  PIC X(12)   VALUE 'ORDER HEADER'.        
062700     03  FILLER                  PIC X(48)   VALUE SPACE.                 
062800     SKIP2                                                                
062900 01  W001D7-DETALJ.                                                       
063000*                                                                         
063100     03  W001D7-SKIP             PIC X(1)    VALUE '0'.                   
063200     03  FILLER                  PIC X(23)   VALUE SPACE.                 
063300     03  FILLER                  PIC X(3)    VALUE 'RHB'.                 
063400     03  FILLER                  PIC X(10)   VALUE SPACE.                 
063500     03  W001D7-RHBRAKNARE       PIC Z(8)9   VALUE ZERO.                  
063600     03  FILLER                  PIC X(15)   VALUE SPACE.                 
063700     03  FILLER                  PIC X(12)   VALUE 'ORDER LINE  '.        
063800     03  FILLER                  PIC X(48)   VALUE SPACE.                 
063900     SKIP2                                                                
064000 01  W001D8-DETALJ.                                                       
064100*                                                                         
064200     03  W001D8-SKIP             PIC X(1)    VALUE '0'.                   
064300     03  FILLER                  PIC X(23)   VALUE SPACE.                 
064400     03  FILLER                  PIC X(3)    VALUE 'RHC'.                 
064500     03  FILLER                  PIC X(10)   VALUE SPACE.                 
064600     03  W001D8-RHCRAKNARE       PIC Z(8)9   VALUE ZERO.                  
064700     03  FILLER                  PIC X(15)   VALUE SPACE.                 
064800     03  FILLER                  PIC X(16)   VALUE                        
064900                              'ORDER HASH TOTAL'.                         
065000     03  FILLER                  PIC X(44)   VALUE SPACE.                 
065100     EJECT                                                                
065200 01  W001D9-DETALJ.                                                       
065300*                                                                         
065400     03  W001D9-SKIP             PIC X(1)    VALUE '0'.                   
065500     03  FILLER                  PIC X(23)   VALUE SPACE.                 
065600     03  FILLER                  PIC X(3)    VALUE 'RH9'.                 
065700     03  FILLER                  PIC X(18)   VALUE SPACE.                 
065800     03  FILLER                  PIC X       VALUE '-'.                   
065900     03  FILLER                  PIC X(15)   VALUE SPACE.                 
066000     03  FILLER                  PIC X(12)   VALUE 'STOP RECORD '.        
066100     03  FILLER                  PIC X(48)   VALUE SPACE.                 
066200     SKIP2                                                                
066300 01  W001D10-DETALJ.                                                      
066400*                                                                         
066500     03  W001D10-SKIP            PIC X(1)    VALUE '0'.                   
066600     03  FILLER                  PIC X(23)   VALUE SPACE.                 
066700     03  FILLER                  PIC X(3)    VALUE 'RHD'.                 
066800     03  FILLER                  PIC X(10)   VALUE SPACE.                 
066900     03  W001D10-RHDRAKNARE      PIC Z(8)9   VALUE ZERO.                  
067000     03  FILLER                  PIC X(15)   VALUE SPACE.                 
067100     03  FILLER                  PIC X(16)   VALUE                        
067200                              'DISCREPANCY LINE'.                         
067300     03  FILLER                  PIC X(44)   VALUE SPACE.                 
067400     SKIP2                                                                
067500 01  W001D11-DETALJ.                                                      
067600*                                                                         
067700     03  W001D11-SKIP            PIC X(1)    VALUE '0'.                   
067800     03  FILLER                  PIC X(23)   VALUE SPACE.                 
067900     03  FILLER                  PIC X(3)    VALUE 'RHE'.                 
068000     03  FILLER                  PIC X(10)   VALUE SPACE.                 
068100     03  W001D11-RHERAKNARE      PIC Z(8)9   VALUE ZERO.                  
068200     03  FILLER                  PIC X(15)   VALUE SPACE.                 
068300     03  FILLER                  PIC X(21)   VALUE                        
068400                              'ADDITIONAL COSTS LINE'.                    
068500     03  FILLER                  PIC X(39)   VALUE SPACE.                 
068600     SKIP2                                                                
068700 01  W001D12-DETALJ.                                                      
068800*                                                                         
068900     03  W001D12-SKIP            PIC X(1)    VALUE '0'.                   
069000     03  FILLER                  PIC X(23)   VALUE SPACE.                 
069100     03  FILLER                  PIC X(3)    VALUE 'RHF'.                 
069200     03  FILLER                  PIC X(10)   VALUE SPACE.                 
069300     03  W001D12-RHFRAKNARE      PIC Z(8)9   VALUE ZERO.                  
069400     03  FILLER                  PIC X(15)   VALUE SPACE.                 
069500     03  FILLER                  PIC X(16)   VALUE                        
069600                              'TEXT-STRING     '.                         
069700     03  FILLER                  PIC X(44)   VALUE SPACE.                 
069800     SKIP2                                                                
069900 01  W001T1-TOTAL.                                                        
070000*                                                                         
070100     03  W001T1-SKIP             PIC X(1)    VALUE '-'.                   
070200     03  FILLER                  PIC X(22)   VALUE SPACE.                 
070300     03  FILLER                  PIC X(5)    VALUE 'TOTAL'.               
070400     03  FILLER                  PIC X(10)   VALUE SPACE.                 
070500     03  W001T1-RHTOTAL          PIC Z(8)9   VALUE ZERO.                  
070600     03  FILLER                  PIC X(75)   VALUE SPACE.                 
070700                                                                          
070800 01  KREDIT-TEXT.                                                         
070900     03  FILLER                  PIC X(20)   VALUE                        
071000                              'DISCREPANCY LINES   '.                     
071100     EJECT                                                                
071200 LINKAGE SECTION.                                                         
071300     SKIP2                                                                
071400*01      -COPY W0008     -PRE GMTA-                                       
071500      05 FILLER          PIC X.                                           
071600     EJECT                                                                
071700*01      -COPY W0008     -PRE WDB1-                                       
071800      05 FILLER          PIC X.                                           
071900     EJECT                                                                
072000 PROCEDURE DIVISION USING GMTA-PCB WDB1-PCB.                              
072100     ENTRY 'DLITCBL' USING GMTA-PCB WDB1-PCB.                             
072200     SKIP2                                                                
072300     PERFORM A-INIT                                                       
072400                                                                          
072500     SORT SORTFIL    ASCENDING KEY SORT-OHUV-SORT-IDDISTR                 
072600                                   SORT-OHUV-SORT-TIFILDAT                
072700                                   SORT-OHUV-SORT-TIHHMMSS                
072800                                   SORT-OHUV-IDDISTR                      
072900                                   SORT-OHUV-IDKUNDNR                     
073000                                   SORT-OHUV-IDORDNR                      
073100                                   SORT-OHUV-IDPTYP                       
073200                                   SORT-IDTRANSLOP                        
073300          INPUT PROCEDURE B-KONTROLL                                      
073400          OUTPUT PROCEDURE C-BEARBETA.                                    
073500                                                                          
073600     PERFORM Z-FINIT                                                      
073700     MOVE ZERO TO RETURN-CODE                                             
073800     GOBACK                                                               
073900                                                                          
074000     .                                                                    
074100     EJECT                                                                
074200                                                                          
074300 A-INIT SECTION.                                                          
074400     SKIP2                                                                
074500     OPEN INPUT W46088                                                    
074600                W46086                                                    
074700                W46089                                                    
074800                W4608S                                                    
074900     OPEN OUTPUT W46055                                                   
075000                                                                          
075100     SKIP2                                                                
075200     ACCEPT W001R2-DATE FROM DATE                                         
075300     INSPECT W001R2-DATE REPLACING ALL ' ' BY '.'                         
075400     ACCEPT W001R1-TIME FROM TIME                                         
075500     INSPECT W001R1-TIME REPLACING ALL ' ' BY '.'                         
075600     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
075700     .                                                                    
075800     EJECT                                                                
075900                                                                          
076000 B-KONTROLL SECTION.                                                      
076100     SKIP2                                                                
076200     PERFORM S01-LAES-W46088                                              
076300     IF W46088-EOF = JA                                                   
076400        MOVE JA TO W-TOM-MASTER                                           
076500     ELSE                                                                 
076600       PERFORM UNTIL NOT                                                  
076700         (W46088-EOF = NEJ)                                               
076800           EVALUATE I08-OHUV-IDPTYP                                       
076900                                                                          
077000             WHEN 'RHA'                                                   
077100                   ADD 1 TO RHARAKNARE                                    
077200                   MOVE I08-OHUV-W460001 TO SORTWS-AREA                   
077300                   MOVE I08-OHUV-IDTRANSLOP TO SORTWS-IDTRANSLOP          
077400             WHEN 'RHB'                                                   
077500                   ADD 1 TO RHBRAKNARE                                    
077600                   MOVE I08-ORAD-W460002    TO SORTWS-AREA                
077700                   MOVE I08-ORAD-IDTRANSLOP TO SORTWS-IDTRANSLOP          
077800             WHEN 'RHC'                                                   
077900                   ADD 1 TO RHCRAKNARE                                    
078000                   MOVE I08-OHASH-W460003 TO SORTWS-AREA                  
078100                   MOVE I08-OHASH-IDTRANSLOP TO SORTWS-IDTRANSLOP         
078200                                                                          
078300           END-EVALUATE                                                   
078400                                                                          
078500           PERFORM S12-SORT-RELEASE                                       
078600           PERFORM S01-LAES-W46088                                        
078700       END-PERFORM                                                        
078800     END-IF                                                               
078900     .                                                                    
079000     EJECT                                                                
079100                                                                          
079200 C-BEARBETA SECTION.                                                      
079300     SKIP2                                                                
079400     PERFORM S13-SORT-RETURN                                              
079500     PERFORM S02-LAES-W46086                                              
079600     PERFORM S03-LAES-W46089                                              
079700     PERFORM S07-LAES-W4608S                                              
079800     PERFORM UNTIL NOT                                                    
079900       (SORTFIL-EOF = NEJ OR W4608S-EOF = NEJ)                            
080000       IF W-TOM-MASTER = NEJ                                              
080100         MOVE SORTWS-OHUV-SORT-IDDISTR TO W-IDDISTR-WDB2                  
080200       ELSE                                                               
080300         MOVE I1S-KRED-SORT-IDDISTR    TO W-IDDISTR-WDB2                  
080400       END-IF                                                             
080500       MOVE ZERO           TO W-IDKUNDNR-WDB2                             
080600       PERFORM IMS-GET-WDB201                                             
080700       IF SEGMENT-SAKNAS                                                  
080800*                 DISTRIKT-KUND FINNS INTE PÅ WDB201                      
080900         MOVE SPACE                   TO WS-BEBETRAD-1                    
081000                                         WS-BEBETRAD-2                    
081100       ELSE                                                               
081200         MOVE WDB201-GMT-IDPARTNR     TO W-WDB1-IDPARTNR                  
081300         MOVE WDB201-GMT-IDFTG        TO W-WDB1-IDFTG                     
081400                                                                          
081500         PERFORM IMS-GET-WDB101                                           
081600         IF SEGMENT-FINNS                                                 
081700           MOVE WDB101-BET-BEBETRAD-1 TO WS-BEBETRAD-1                    
081800           MOVE WDB101-BET-BEBETRAD-2 TO WS-BEBETRAD-2                    
081900         ELSE                                                             
082000           MOVE SPACE                 TO WS-BEBETRAD-1                    
082100                                         WS-BEBETRAD-2                    
082200         END-IF                                                           
082300       END-IF                                                             
082400       IF SORTFIL-EOF = NEJ                                               
082500         MOVE SORTWS-OHUV-SORT-IDDISTR  TO WS-IDDISTR-1                   
082600                                          W001R7-IDDISTR                  
082700         MOVE SORTWS-OHUV-SORT-TIFILDAT TO WS-TIFILDAT                    
082800         MOVE SORTWS-OHUV-SORT-TIHHMMSS TO WS-TIHHMMSS                    
082900         MOVE 0                         TO W001-SIDRAKNARE                
083000         MOVE 100                       TO W001-RADRAKNARE                
083100*                     INITIERA DDNAMNET                                   
083200         PERFORM CA-DDNAMNSINITIERING                                     
083300         MOVE SORTWS-OHUV-SORT-TIFILDAT TO W001R4-DATE                    
083400         INSPECT W001R4-DATE REPLACING ALL ' ' BY '.'                     
083500         MOVE SORTWS-OHUV-SORT-TIHHMMSS TO W001R4-TIME                    
083600         INSPECT W001R4-TIME REPLACING ALL ' ' BY '.'                     
083700         PERFORM UNTIL NOT                                                
083800           (SORTFIL-EOF = NEJ AND                                         
083900                 SORTWS-OHUV-SORT-IDDISTR = WS-IDDISTR-1 AND              
084000                 SORTWS-OHUV-SORT-TIFILDAT = WS-TIFILDAT AND              
084100                 SORTWS-OHUV-SORT-TIHHMMSS = WS-TIHHMMSS)                 
084200             MOVE SORTWS-OHUV-IDDISTR TO WS-IDDISTR-2                     
084300             MOVE SORTWS-OHUV-IDKUNDNR TO WS-IDKUNDNR                     
084400                                      WR-IDKUNDNR                         
084500             IF SORTWS-OHUV-IDKUNDNR NOT NUMERIC                          
084600                 MOVE SORTWS-OHUV-IDKUNDNR TO W001D1-IDKUNDNR             
084700             ELSE                                                         
084800                 MOVE WR-IDKUNDNR TO W001D1-IDKUNDNR                      
084900             END-IF                                                       
085000             MOVE SORTWS-OHUV-IDORDNR TO WS-IDORDNR                       
085100                                      WR-IDORDNR                          
085200             IF SORTWS-OHUV-IDORDNR NOT NUMERIC                           
085300                 MOVE SORTWS-OHUV-IDORDNR TO W001D1-IDORDNR               
085400             ELSE                                                         
085500               IF WR-IDORDNR = ZERO                                       
085600                 MOVE SPACE      TO W001D1-IDORDNR                        
085700               ELSE                                                       
085800                 MOVE WR-IDORDNR TO W001D1-IDORDNR                        
085900               END-IF                                                     
086000             END-IF                                                       
086100             PERFORM D-SKAPA-LISTA-TOTALA-ORDER                           
086200         END-PERFORM                                                      
086300       END-IF                                                             
086400       MOVE ZERO TO WS-ANTAL-RADER                                        
086500       MOVE ZERO TO WS-ANTAL-RADER-OK                                     
086600       IF W4608S-EOF = NEJ                                                
086700         IF W-TOM-MASTER = JA                                             
086800           MOVE I1S-KRED-SORT-IDDISTR TO SORTWS-OHUV-SORT-IDDISTR         
086900           PERFORM CA-DDNAMNSINITIERING                                   
087000           MOVE I1S-KRED-SORT-IDDISTR TO W001R7-IDDISTR                   
087100           MOVE I1S-KRED-SORT-TIFILDAT TO W001R4-DATE                     
087200           INSPECT W001R4-DATE REPLACING ALL ' ' BY '.'                   
087300           MOVE I1S-KRED-SORT-TIHHMMSS TO W001R4-TIME                     
087400           INSPECT W001R4-TIME REPLACING ALL ' ' BY '.'                   
087500         END-IF                                                           
087600         MOVE I1S-KRED-SORT-IDDISTR TO WS-IDDISTR-1                       
087700         MOVE I1S-KRED-SORT-TIFILDAT TO WS-TIFILDAT                       
087800         MOVE I1S-KRED-SORT-TIHHMMSS TO WS-TIHHMMSS                       
087900         MOVE 100 TO W001-RADRAKNARE                                      
088000         PERFORM UNTIL NOT                                                
088100           (W4608S-EOF = NEJ AND                                          
088200                 I1S-KRED-SORT-IDDISTR = WS-IDDISTR-1 AND                 
088300                 I1S-KRED-SORT-TIFILDAT = WS-TIFILDAT AND                 
088400                 I1S-KRED-SORT-TIHHMMSS = WS-TIHHMMSS)                    
088500             MOVE I1S-KRED-IDKUNDNR TO WS-IDKUNDNR                        
088600             MOVE I1S-KRED-IDDISTR TO WS-IDDISTR-2                        
088700             PERFORM CB-RHD-POSTER                                        
088800             PERFORM ED-SKRIV-RHD-POST                                    
088900             MOVE ZERO TO WS-ANTAL-RADER                                  
089000             MOVE ZERO TO WS-ANTAL-RADER-OK                               
089100         END-PERFORM                                                      
089200       END-IF                                                             
089300         MOVE 100 TO W001-RADRAKNARE                                      
089400         PERFORM UNTIL NOT                                                
089500           (W46089-EOF = NEJ AND                                          
089600                 I36-OHUV-SORT-IDDISTR = WS-IDDISTR-1 AND                 
089700                 I36-OHUV-SORT-TIFILDAT = WS-TIFILDAT AND                 
089800                 I36-OHUV-SORT-TIHHMMSS = WS-TIHHMMSS)                    
089900             PERFORM E-SKAPA-LISTA-FEL-ORDER                              
090000         END-PERFORM                                                      
090100         PERFORM S06-SKRIV-W46055-001-TOTAL                               
090200         CLOSE W46055                                                     
090300     END-PERFORM                                                          
090400     .                                                                    
090500     EJECT                                                                
090600                                                                          
090700 CA-DDNAMNSINITIERING SECTION.                                            
090800     SKIP2                                                                
090900     IF SORTWS-OHUV-SORT-IDDISTR NOT NUMERIC OR                           
091000        SORTWS-OHUV-SORT-IDDISTR = ZERO                                   
091100         IF SORTWS-OHUV-IDDISTR NOT NUMERIC OR                            
091200            SORTWS-OHUV-IDDISTR = ZERO                                    
091300            CONTINUE                                                      
091400         ELSE                                                             
091500             MOVE SORTWS-OHUV-IDDISTR TO TEST-IDDISTR                     
091600                                        DIS1-IDDISTR                      
091700         END-IF                                                           
091800     ELSE                                                                 
091900         MOVE SORTWS-OHUV-SORT-IDDISTR TO TEST-IDDISTR                    
092000                                          DIS1-IDDISTR                    
092100     END-IF                                                               
092200                                                                          
092300     CALL W460DIS1 USING DIS1-W460DIS1                                    
092400                                                                          
092500     MOVE NEJ TO FLAGGA-DISTR                                             
092600******************   OBS KOMPLETTERA VID NOAC INST. !!                    
092700     EVALUATE TRUE                                                        
092800                                                                          
092900       WHEN DIS130-NOAC-AUSTRALIEN                                        
093000           MOVE JA TO FLAGGA-DISTR                                        
093100                                                                          
093200       WHEN DIS1-IDLANDX2 = ISO-AUSTRALIEN                                
093300           MOVE JA TO FLAGGA-DISTR                                        
093400                                                                          
093500       WHEN DIS1-IDLANDX2 = ISO-BELGIEN                                   
093600           MOVE JA TO FLAGGA-DISTR                                        
093700                                                                          
093800       WHEN DIS1-IDLANDX2 = ISO-BRASILIEN                                 
093900           MOVE JA TO FLAGGA-DISTR                                        
094000                                                                          
094100       WHEN DIS1-IDLANDX2 = ISO-DANMARK                                   
094200           MOVE JA TO FLAGGA-DISTR                                        
094300                                                                          
094400       WHEN DIS1-IDLANDX2 = ISO-ENGLAND                                   
094500           MOVE JA TO FLAGGA-DISTR                                        
094600                                                                          
094700       WHEN DIS1-IDLANDX2 = ISO-FINLAND                                   
094800           MOVE JA TO FLAGGA-DISTR                                        
094900                                                                          
095000       WHEN DIS1-IDLANDX2 = ISO-FRANKRIKE                                 
095100           MOVE JA TO FLAGGA-DISTR                                        
095200                                                                          
095300       WHEN DIS1-IDLANDX2 = ISO-HOLLAND                                   
095400           MOVE JA TO FLAGGA-DISTR                                        
095500                                                                          
095600       WHEN DIS1-IDLANDX2 = ISO-IRLAND                                    
095700           MOVE JA TO FLAGGA-DISTR                                        
095800                                                                          
095900       WHEN DIS1-IDLANDX2 = ISO-ITALIEN                                   
096000           MOVE JA TO FLAGGA-DISTR                                        
096100                                                                          
096200       WHEN DIS1-IDLANDX2 = ISO-JAPAN                                     
096300           MOVE JA TO FLAGGA-DISTR                                        
096400                                                                          
096500       WHEN DIS130-NOAC-KINA OR DIS130-NOAC-KINA-C1                       
096600           MOVE JA TO FLAGGA-DISTR                                        
096700                                                                          
096800       WHEN DIS1-IDLANDX2 = ISO-KINA OR ISO-KINA-C1                       
096900           MOVE JA TO FLAGGA-DISTR                                        
097000                                                                          
097100       WHEN DIS1-IDLANDX2 = ISO-KOREA                                     
097200           MOVE JA TO FLAGGA-DISTR                                        
097300                                                                          
097400       WHEN DIS130-NOAC-MALAYSIA                                          
097500           MOVE JA TO FLAGGA-DISTR                                        
097600                                                                          
097700       WHEN DIS1-IDLANDX2 = ISO-MALAYSIA                                  
097800         MOVE JA TO FLAGGA-DISTR                                          
097900                                                                          
098000       WHEN DIS130-NOAC-MEXICO                                            
098100           MOVE JA TO FLAGGA-DISTR                                        
098200                                                                          
098300       WHEN DIS1-IDLANDX2 = ISO-MEXICO                                    
098400         MOVE JA TO FLAGGA-DISTR                                          
098500                                                                          
098600       WHEN DIS130-NOAC-RYSSLAND                                          
098700           MOVE JA TO FLAGGA-DISTR                                        
098800                                                                          
098900       WHEN DIS1-IDLANDX2 = ISO-RYSSLAND                                  
099000         MOVE JA TO FLAGGA-DISTR                                          
099100                                                                          
099200       WHEN DIS130-NOAC-TURKIET                                           
099300           MOVE JA TO FLAGGA-DISTR                                        
099400                                                                          
099410       WHEN DIS1-IDLANDX2 = ISO-TURKIET                                   
099420           MOVE JA TO FLAGGA-DISTR                                        
099430                                                                          
099800       WHEN DIS1-IDLANDX2 = ISO-NORGE                                     
099900           MOVE JA TO FLAGGA-DISTR                                        
100000                                                                          
100100       WHEN DIS1-IDLANDX2 = ISO-OSTERRIKE                                 
100200           MOVE JA TO FLAGGA-DISTR                                        
100300                                                                          
100400       WHEN DIS130-NOAC-POLEN                                             
100500           MOVE JA TO FLAGGA-DISTR                                        
100600                                                                          
100700       WHEN DIS1-IDLANDX2 = ISO-POLEN                                     
100800           MOVE JA TO FLAGGA-DISTR                                        
100900                                                                          
101000       WHEN DIS1-IDLANDX2 = ISO-PORTUGAL                                  
101100           MOVE JA TO FLAGGA-DISTR                                        
101200                                                                          
101300       WHEN DIS130-NOAC-PORTUGAL                                          
101400           MOVE JA TO FLAGGA-DISTR                                        
101500                                                                          
101600       WHEN DIS1-IDLANDX2 = ISO-SCHWEIZ                                   
101700           MOVE JA TO FLAGGA-DISTR                                        
101800                                                                          
101900       WHEN DIS130-NOAC-SCHWEIZ-PV                                        
102000           MOVE JA TO FLAGGA-DISTR                                        
102100                                                                          
102200       WHEN DIS1-IDLANDX2 = ISO-SPANIEN                                   
102300           MOVE JA TO FLAGGA-DISTR                                        
102400                                                                          
102500       WHEN DIS1-IDLANDX2 = ISO-SVERIGE                                   
102600           MOVE JA TO FLAGGA-DISTR                                        
102700                                                                          
102800       WHEN DIS130-NOAC-SYDAFRIKA                                         
102900           MOVE JA TO FLAGGA-DISTR                                        
103000                                                                          
103100       WHEN DIS1-IDLANDX2 = ISO-SYDAFRIKA                                 
103200           MOVE JA TO FLAGGA-DISTR                                        
103300                                                                          
103400       WHEN DIS130-NOAC-TAIWAN                                            
103500           MOVE JA TO FLAGGA-DISTR                                        
103600                                                                          
103700       WHEN DIS1-IDLANDX2 = ISO-TAIWAN                                    
103800           MOVE JA TO FLAGGA-DISTR                                        
103900                                                                          
104000       WHEN DIS130-NOAC-TAIWAN2                                           
104100           MOVE JA TO FLAGGA-DISTR                                        
104200                                                                          
104300       WHEN DIS1-IDLANDX2 = ISO-TAIWAN2                                   
104400           MOVE JA TO FLAGGA-DISTR                                        
104500                                                                          
104600       WHEN DIS130-NOAC-THAILAND                                          
104700           MOVE JA TO FLAGGA-DISTR                                        
104800                                                                          
104900       WHEN DIS1-IDLANDX2 = ISO-THAILAND                                  
105000           MOVE JA TO FLAGGA-DISTR                                        
105100                                                                          
105200       WHEN DIS1-IDLANDX2 = ISO-TYSKLAND                                  
105300           MOVE JA TO FLAGGA-DISTR                                        
105400                                                                          
105500       WHEN DIS1-IDLANDX2 = ISO-CANADA                                    
105600           MOVE JA TO FLAGGA-DISTR                                        
105610                                                                          
105700       WHEN DIS1-IDLANDX2 = ISO-USA                                       
105800           MOVE JA TO FLAGGA-DISTR                                        
105900                                                                          
105910       WHEN DIS1-IDLANDX2 = ISO-INDIEN                                    
105920           MOVE JA TO FLAGGA-DISTR                                        
105930                                                                          
105940       WHEN DIS1-IDLANDX2 = ISO-UNGERN                                    
105950           MOVE JA TO FLAGGA-DISTR                                        
105960                                                                          
105970       WHEN DIS1-IDLANDX2 = ISO-TJECKIEN                                  
105980           MOVE JA TO FLAGGA-DISTR                                        
105990                                                                          
106000     END-EVALUATE                                                         
106100******************************************************************        
106200     IF FLAGGA-DISTR = NEJ                                                
106300         DISPLAY 'DISTRIKTET FINNS INTE '                                 
106400         DISPLAY 'DISTRIKT : ' DIS1-IDDISTR                               
106500         DISPLAY 'LANDKOD  : ' DIS1-IDLANDX2                              
106600         DISPLAY 'SVARET   : ' DIS1-KDSVAR                                
106700         CALL ABEND USING RKOD-ABEND-UTAN-DUMP                            
106800     END-IF                                                               
106900     .                                                                    
107000     EJECT                                                                
107100                                                                          
107200 CB-RHD-POSTER SECTION.                                                   
107300     SKIP2                                                                
107400     PERFORM UNTIL NOT                                                    
107500       (W4608S-EOF = NEJ AND                                              
107600             I1S-KRED-SORT-IDDISTR = WS-IDDISTR-1 AND                     
107700             I1S-KRED-SORT-TIFILDAT = WS-TIFILDAT AND                     
107800             I1S-KRED-SORT-TIHHMMSS = WS-TIHHMMSS AND                     
107900             I1S-KRED-IDDISTR = WS-IDDISTR-2 AND                          
108000             I1S-KRED-IDKUNDNR = WS-IDKUNDNR)                             
108100         ADD +1 TO WS-ANTAL-RADER                                         
108200                   WS-ANTAL-RADER-OK                                      
108300         PERFORM S07-LAES-W4608S                                          
108400         MOVE KREDIT-TEXT TO W001D1-TEXT                                  
108500     END-PERFORM                                                          
108600     .                                                                    
108700     EJECT                                                                
108800                                                                          
108900 D-SKAPA-LISTA-TOTALA-ORDER SECTION.                                      
109000     SKIP2                                                                
109100     MOVE 0 TO WS-ANTAL-RADER                                             
109200     MOVE 0 TO WS-ANTAL-RADER-OK                                          
109300*                     KONTROLLERA ORDERN. FINNS ORDERPOSTEN               
109400*                     PÅ W46088 OCH W46086 ÄR DET EN RÄTT                 
109500*                     ORDERPOST. FINNS ORDERPOSTEN PÅ                     
109600*                     W46088 OCH W46089 ÄR DET EN FELAKTIG                
109700*                     ORDERPOST.                                          
109800     IF W46086-EOF = NEJ                                                  
109900         IF SORTWS-OHUV-SORT-IDDISTR = I26-OHUV-SORT-IDDISTR              
110000             IF SORTWS-OHUV-SORT-TIFILDAT = I26-OHUV-SORT-TIFILDAT        
110100                 IF SORTWS-OHUV-SORT-TIHHMMSS =                           
110200                    I26-OHUV-SORT-TIHHMMSS                                
110300                     IF SORTWS-OHUV-IDDISTR = I26-OHUV-IDDISTR            
110400                         IF SORTWS-OHUV-IDKUNDNR =                        
110500                            I26-OHUV-IDKUNDNR                             
110600                             IF SORTWS-OHUV-IDORDNR =                     
110700                                I26-OHUV-IDORDNR                          
110800                                 PERFORM DB-RAETT-ORDER                   
110900                             ELSE                                         
111000                                 PERFORM DA-FEL-ORDER                     
111100                             END-IF                                       
111200                         ELSE                                             
111300                             PERFORM DA-FEL-ORDER                         
111400                         END-IF                                           
111500                     ELSE                                                 
111600                         PERFORM DA-FEL-ORDER                             
111700                     END-IF                                               
111800                 ELSE                                                     
111900                     PERFORM DA-FEL-ORDER                                 
112000                 END-IF                                                   
112100             ELSE                                                         
112200                 PERFORM DA-FEL-ORDER                                     
112300             END-IF                                                       
112400         ELSE                                                             
112500             PERFORM DA-FEL-ORDER                                         
112600         END-IF                                                           
112700     ELSE                                                                 
112800         PERFORM DA-FEL-ORDER                                             
112900     END-IF                                                               
113000     MOVE WS-ANTAL-RADER    TO W001D1-ANT-TRANS                           
113100     MOVE WS-ANTAL-RADER-OK TO W001D1-ANT-TRANS-OK                        
113200     MOVE W001D1-DETALJ     TO W001-RAD                                   
113300     PERFORM S04-SKRIV-W46055-001                                         
113400     .                                                                    
113500     EJECT                                                                
113600                                                                          
113700 DA-FEL-ORDER SECTION.                                                    
113800     SKIP2                                                                
113900     IF SORTWS-OHUV-IDPTYP = 'RHD' OR 'RHE' OR 'RHF'                      
114000        CONTINUE                                                          
114100     ELSE                                                                 
114200        PERFORM UNTIL NOT                                                 
114300          (SORTFIL-EOF = NEJ AND                                          
114400                SORTWS-OHUV-SORT-IDDISTR = WS-IDDISTR-1 AND               
114500                SORTWS-OHUV-SORT-TIFILDAT = WS-TIFILDAT AND               
114600                SORTWS-OHUV-SORT-TIHHMMSS = WS-TIHHMMSS AND               
114700                SORTWS-OHUV-IDDISTR = WS-IDDISTR-2 AND                    
114800                SORTWS-OHUV-IDKUNDNR = WS-IDKUNDNR AND                    
114900                SORTWS-OHUV-IDORDNR = WS-IDORDNR)                         
115000            EVALUATE SORTWS-OHUV-IDPTYP                                   
115100                                                                          
115200              WHEN 'RHA'                                                  
115300                      PERFORM S21-BEHANDLA-ORDERHUVUD                     
115400              WHEN 'RHB'                                                  
115500                      PERFORM S22-BEHANDLA-ORDERRADER                     
115600              WHEN 'RHC'                                                  
115700                      PERFORM S23-BEHANDLA-HASHTOTAL                      
115800                                                                          
115900            END-EVALUATE                                                  
116000        END-PERFORM                                                       
116100     END-IF                                                               
116200     .                                                                    
116300     EJECT                                                                
116400                                                                          
116500 DB-RAETT-ORDER SECTION.                                                  
116600     SKIP2                                                                
116700     IF SORTWS-OHUV-IDPTYP = 'RHD' OR 'RHE' OR 'RHF'                      
116800        CONTINUE                                                          
116900     ELSE                                                                 
117000        PERFORM UNTIL NOT                                                 
117100          (SORTFIL-EOF = NEJ AND                                          
117200                SORTWS-OHUV-SORT-IDDISTR = WS-IDDISTR-1 AND               
117300                SORTWS-OHUV-SORT-TIFILDAT = WS-TIFILDAT AND               
117400                SORTWS-OHUV-SORT-TIHHMMSS = WS-TIHHMMSS AND               
117500                SORTWS-OHUV-IDDISTR = WS-IDDISTR-2 AND                    
117600                SORTWS-OHUV-IDKUNDNR = WS-IDKUNDNR AND                    
117700                SORTWS-OHUV-IDORDNR = WS-IDORDNR)                         
117800            EVALUATE SORTWS-OHUV-IDPTYP                                   
117900                                                                          
118000              WHEN 'RHA'                                                  
118100                      PERFORM S21-BEHANDLA-ORDERHUVUD                     
118200              WHEN 'RHB'                                                  
118300                      PERFORM S22-BEHANDLA-ORDERRADER                     
118400              WHEN 'RHC'                                                  
118500                      PERFORM S23-BEHANDLA-HASHTOTAL                      
118600                                                                          
118700            END-EVALUATE                                                  
118800        END-PERFORM                                                       
118900        PERFORM UNTIL NOT                                                 
119000          (W46086-EOF = NEJ AND                                           
119100                I26-OHUV-SORT-IDDISTR = WS-IDDISTR-1 AND                  
119200                I26-OHUV-SORT-TIFILDAT = WS-TIFILDAT AND                  
119300                I26-OHUV-SORT-TIHHMMSS = WS-TIHHMMSS AND                  
119400                I26-OHUV-IDDISTR = WS-IDDISTR-2 AND                       
119500                I26-OHUV-IDKUNDNR = WS-IDKUNDNR AND                       
119600                I26-OHUV-IDORDNR = WS-IDORDNR)                            
119700            IF I26-OHUV-IDPTYP = 'RHB'                                    
119800                ADD 1 TO WS-ANTAL-RADER-OK                                
119900            END-IF                                                        
120000            PERFORM S02-LAES-W46086                                       
120100        END-PERFORM                                                       
120200     END-IF                                                               
120300     .                                                                    
120400     EJECT                                                                
120500                                                                          
120600 E-SKAPA-LISTA-FEL-ORDER SECTION.                                         
120700     SKIP2                                                                
120800     EVALUATE I36-OHUV-IDPTYP                                             
120900                                                                          
121000       WHEN 'RHA'                                                         
121100               PERFORM EA-FLYTTA-RHA-TILL-FELLISTA                        
121200       WHEN 'RHB'                                                         
121300               PERFORM EB-FLYTTA-RHB-TILL-FELLISTA                        
121400       WHEN 'RHC'                                                         
121500               PERFORM EC-FLYTTA-RHC-TILL-FELLISTA                        
121600                                                                          
121700     END-EVALUATE                                                         
121800     .                                                                    
121900     EJECT                                                                
122000                                                                          
122100 EA-FLYTTA-RHA-TILL-FELLISTA SECTION.                                     
122200     SKIP2                                                                
122300     MOVE I36-OHUV-IDPTYP      TO W001D2-IDPTYP                           
122400     MOVE I36-OHUV-IDDISTR     TO W001D2-IDDISTR                          
122500     MOVE I36-OHUV-IDKUNDNR    TO W001D2-IDKUNDNR                         
122600     MOVE I36-OHUV-IDORDNR     TO W001D2-IDORDNR                          
122700     MOVE I36-OHUV-BEVOLREF    TO W001D2-BEVOLREF                         
122800     MOVE I36-OHUV-KDFRAKT     TO W001D2-KDFRAKT                          
122900     MOVE I36-OHUV-KDORDKL     TO W001D2-KDORDKL                          
123000     MOVE I36-OHUV-KDORDKL-IMP TO W001D2-KDORDKL-IMP                      
123100     MOVE I36-OHUV-KDROPACK    TO W001D2-KDROPACK                         
123200     MOVE SPACE                TO W001D2-KDSPRAK                          
123300     MOVE I36-OHUV-BEVARREF    TO W001D2-BEVARREF                         
123400     MOVE I36-OHUV-FLRESTN     TO W001D2-FLRESTN                          
123500     MOVE I36-OHUV-KDNCNOT     TO W001D2-KDNCNOT                          
123600     MOVE I36-OHUV-KDTPOTYP    TO W001D2-KDTPOTYP                         
123700     MOVE I36-OHUV-IDKAMPRF    TO W001D2-IDKAMPRF                         
123800     MOVE I36-OHUV-IDTRANSLOP  TO W001D2-IDTRANSLOP                       
123900     MOVE I36-OHUV-KDFEL       TO W001D2-KDFEL                            
124000                                  WS-KDFEL                                
124100     PERFORM S05B-SKRIV-W46055-001-RUBRIK                                 
124200     IF WS-KDFEL = 1                                                      
124300         MOVE SPACE TO FELTEXT1B                                          
124400         MOVE SPACE TO FELTEXT1C                                          
124500     END-IF                                                               
124600     SET IX-FEL TO WS-KDFEL                                               
124700     MOVE FELTEXT(IX-FEL) TO W001D2-FELTEXT                               
124800     MOVE W001D2-DETALJ TO W001-RAD                                       
124900     PERFORM S05-SKRIV-W46055-001                                         
125000     PERFORM S03-LAES-W46089                                              
125100     PERFORM UNTIL NOT                                                    
125200       (W46089-EOF = NEJ AND                                              
125300             I36-OHUV-SORT-IDDISTR = WS-IDDISTR-1 AND                     
125400             I36-OHUV-SORT-TIFILDAT = WS-TIFILDAT AND                     
125500             I36-OHUV-SORT-TIHHMMSS = WS-TIHHMMSS AND                     
125600             I36-OHUV-IDPTYP = 'RHA')                                     
125700         MOVE I36-OHUV-IDPTYP      TO W001D2-IDPTYP                       
125800         MOVE I36-OHUV-IDDISTR     TO W001D2-IDDISTR                      
125900         MOVE I36-OHUV-IDKUNDNR    TO W001D2-IDKUNDNR                     
126000         MOVE I36-OHUV-IDORDNR     TO W001D2-IDORDNR                      
126100         MOVE I36-OHUV-BEVOLREF    TO W001D2-BEVOLREF                     
126200         MOVE I36-OHUV-KDFRAKT     TO W001D2-KDFRAKT                      
126300         MOVE I36-OHUV-KDORDKL     TO W001D2-KDORDKL                      
126400         MOVE I36-OHUV-KDORDKL-IMP TO W001D2-KDORDKL-IMP                  
126500         MOVE I36-OHUV-KDROPACK    TO W001D2-KDROPACK                     
126600         MOVE SPACE                TO W001D2-KDSPRAK                      
126700         MOVE I36-OHUV-BEVARREF    TO W001D2-BEVARREF                     
126800         MOVE I36-OHUV-FLRESTN     TO W001D2-FLRESTN                      
126900         MOVE I36-OHUV-KDNCNOT     TO W001D2-KDNCNOT                      
127000         MOVE I36-OHUV-KDTPOTYP    TO W001D2-KDTPOTYP                     
127100         MOVE I36-OHUV-IDKAMPRF    TO W001D2-IDKAMPRF                     
127200         MOVE I36-OHUV-IDTRANSLOP  TO W001D2-IDTRANSLOP                   
127300         MOVE I36-OHUV-KDFEL       TO W001D2-KDFEL                        
127400                                      WS-KDFEL                            
127500         IF WS-KDFEL = 1                                                  
127600             MOVE SPACE TO FELTEXT1B                                      
127700             MOVE SPACE TO FELTEXT1C                                      
127800         END-IF                                                           
127900         SET IX-FEL TO WS-KDFEL                                           
128000         MOVE FELTEXT(IX-FEL) TO W001D2-FELTEXT                           
128100         MOVE W001D2-DETALJ   TO W001-RAD                                 
128200         PERFORM S05-SKRIV-W46055-001                                     
128300         PERFORM S03-LAES-W46089                                          
128400     END-PERFORM                                                          
128500     .                                                                    
128600     EJECT                                                                
128700                                                                          
128800 EB-FLYTTA-RHB-TILL-FELLISTA SECTION.                                     
128900     SKIP2                                                                
129000     MOVE I36-ORAD-IDPTYP     TO W001D3-IDPTYP                            
129100     MOVE I36-ORAD-IDDISTR    TO W001D3-IDDISTR                           
129200     MOVE I36-ORAD-IDKUNDNR   TO W001D3-IDKUNDNR                          
129300     MOVE I36-ORAD-IDORDNR    TO W001D3-IDORDNR                           
129400     MOVE I36-ORAD-IDARTNR    TO W001D3-IDARTNR                           
129500     MOVE I36-ORAD-REKSIFFR   TO W001D3-REKSIFFR                          
129600     MOVE I36-ORAD-BERADREF   TO W001D3-BERADREF                          
129700     MOVE I36-ORAD-KVBEART    TO W001D3-KVBEART                           
129800     MOVE I36-ORAD-KDKVBRYT   TO W001D3-KDKVBRYT                          
129900     MOVE I36-ORAD-KDDSP      TO W001D3-KDDSP                             
130000     MOVE I36-ORAD-TITPO      TO W001D3-TITPO                             
130100     MOVE I36-ORAD-FLSLATT    TO W001D3-FLSLATT                           
130200     MOVE I36-ORAD-FLDIRLEV   TO W001D3-FLDIRLEV                          
130300     MOVE I36-ORAD-IDTRANSLOP TO W001D3-IDTRANSLOP                        
130400     MOVE I36-ORAD-KDFEL      TO W001D3-KDFEL                             
130500                                 WS-KDFEL                                 
130600     PERFORM S05B-SKRIV-W46055-001-RUBRIK                                 
130700     IF WS-KDFEL = 1                                                      
130800         MOVE SPACE TO FELTEXT1B                                          
130900         MOVE SPACE TO FELTEXT1C                                          
131000     ELSE                                                                 
131100        IF WS-KDFEL = 2                                                   
131200           MOVE I36-ORAD-REKSIFFR-RAETT TO FELTEXT2B                      
131300        END-IF                                                            
131400     END-IF                                                               
131500     SET IX-FEL TO WS-KDFEL                                               
131600     MOVE FELTEXT(IX-FEL) TO W001D3-FELTEXT                               
131700     MOVE W001D3-DETALJ TO W001-RAD                                       
131800     PERFORM S05-SKRIV-W46055-001                                         
131900     PERFORM S03-LAES-W46089                                              
132000     PERFORM UNTIL NOT                                                    
132100       (W46089-EOF = NEJ AND                                              
132200             I36-OHUV-SORT-IDDISTR = WS-IDDISTR-1 AND                     
132300             I36-OHUV-SORT-TIFILDAT = WS-TIFILDAT AND                     
132400             I36-OHUV-SORT-TIHHMMSS = WS-TIHHMMSS AND                     
132500             I36-OHUV-SORT-IDDISTR = WS-IDDISTR-1 AND                     
132600             I36-OHUV-IDPTYP = 'RHB')                                     
132700         MOVE I36-ORAD-IDPTYP     TO W001D3-IDPTYP                        
132800         MOVE I36-ORAD-IDDISTR    TO W001D3-IDDISTR                       
132900         MOVE I36-ORAD-IDKUNDNR   TO W001D3-IDKUNDNR                      
133000         MOVE I36-ORAD-IDORDNR    TO W001D3-IDORDNR                       
133100         MOVE I36-ORAD-IDARTNR    TO W001D3-IDARTNR                       
133200         MOVE I36-ORAD-REKSIFFR   TO W001D3-REKSIFFR                      
133300         MOVE I36-ORAD-BERADREF   TO W001D3-BERADREF                      
133400         MOVE I36-ORAD-KVBEART    TO W001D3-KVBEART                       
133500         MOVE I36-ORAD-KDKVBRYT   TO W001D3-KDKVBRYT                      
133600         MOVE I36-ORAD-KDDSP      TO W001D3-KDDSP                         
133700         MOVE I36-ORAD-TITPO      TO W001D3-TITPO                         
133800         MOVE I36-ORAD-FLSLATT    TO W001D3-FLSLATT                       
133900         MOVE I36-ORAD-FLDIRLEV   TO W001D3-FLDIRLEV                      
134000         MOVE I36-ORAD-IDTRANSLOP TO W001D3-IDTRANSLOP                    
134100         MOVE I36-ORAD-KDFEL      TO W001D3-KDFEL                         
134200                                     WS-KDFEL                             
134300         IF WS-KDFEL = 1                                                  
134400             MOVE SPACE TO FELTEXT1B                                      
134500             MOVE SPACE TO FELTEXT1C                                      
134600         ELSE                                                             
134700            IF WS-KDFEL = 2                                               
134800               MOVE I36-ORAD-REKSIFFR-RAETT TO FELTEXT2B                  
134900            END-IF                                                        
135000         END-IF                                                           
135100         SET IX-FEL TO WS-KDFEL                                           
135200         MOVE FELTEXT(IX-FEL) TO W001D3-FELTEXT                           
135300         MOVE W001D3-DETALJ TO W001-RAD                                   
135400         PERFORM S05-SKRIV-W46055-001                                     
135500         PERFORM S03-LAES-W46089                                          
135600     END-PERFORM                                                          
135700     .                                                                    
135800     EJECT                                                                
135900                                                                          
136000 EC-FLYTTA-RHC-TILL-FELLISTA SECTION.                                     
136100     SKIP2                                                                
136200     MOVE I36-OHASH-IDPTYP     TO W001D4-IDPTYP                           
136300     MOVE I36-OHASH-IDDISTR    TO W001D4-IDDISTR                          
136400     MOVE I36-OHASH-IDKUNDNR   TO W001D4-IDKUNDNR                         
136500     MOVE I36-OHASH-IDORDNR    TO W001D4-IDORDNR                          
136600     MOVE I36-OHASH-SUHASH     TO W001D4-SUHASH                           
136700     MOVE I36-OHASH-IDTRANSLOP TO W001D4-IDTRANSLOP                       
136800     MOVE I36-OHASH-KDFEL      TO W001D4-KDFEL                            
136900                                 WS-KDFEL                                 
137000     PERFORM S05B-SKRIV-W46055-001-RUBRIK                                 
137100     IF WS-KDFEL = 1                                                      
137200         MOVE ' = '                  TO FELTEXT1B                         
137300         MOVE I36-OHASH-SUHASH-RAETT TO WS-SUHASH-RAETT                   
137400         MOVE WS-SUHASH-RAETT        TO FELTEXT1C                         
137500     END-IF                                                               
137600     SET IX-FEL TO WS-KDFEL                                               
137700     MOVE FELTEXT(IX-FEL) TO W001D4-FELTEXT                               
137800     MOVE W001D4-DETALJ   TO W001-RAD                                     
137900     PERFORM S05-SKRIV-W46055-001                                         
138000     PERFORM S03-LAES-W46089                                              
138100     PERFORM UNTIL NOT                                                    
138200       (W46089-EOF = NEJ AND                                              
138300             I36-OHUV-SORT-IDDISTR = WS-IDDISTR-1 AND                     
138400             I36-OHUV-SORT-TIFILDAT = WS-TIFILDAT AND                     
138500             I36-OHUV-SORT-TIHHMMSS = WS-TIHHMMSS AND                     
138600             I36-OHUV-SORT-IDDISTR = WS-IDDISTR-1 AND                     
138700             I36-OHUV-IDPTYP = 'RHC')                                     
138800         MOVE I36-OHASH-IDPTYP     TO W001D4-IDPTYP                       
138900         MOVE I36-OHASH-IDDISTR    TO W001D4-IDDISTR                      
139000         MOVE I36-OHASH-IDKUNDNR   TO W001D4-IDKUNDNR                     
139100         MOVE I36-OHASH-IDORDNR    TO W001D4-IDORDNR                      
139200         MOVE I36-OHASH-SUHASH     TO W001D4-SUHASH                       
139300         MOVE I36-OHASH-IDTRANSLOP TO W001D4-IDTRANSLOP                   
139400         MOVE I36-OHASH-KDFEL      TO W001D4-KDFEL                        
139500                                      WS-KDFEL                            
139600         IF WS-KDFEL = 1                                                  
139700             MOVE ' = '                  TO FELTEXT1B                     
139800             MOVE I36-OHASH-SUHASH-RAETT TO WS-SUHASH-RAETT               
139900             MOVE WS-SUHASH-RAETT        TO FELTEXT1C                     
140000         END-IF                                                           
140100         SET IX-FEL TO WS-KDFEL                                           
140200         MOVE FELTEXT(IX-FEL) TO W001D4-FELTEXT                           
140300         MOVE W001D4-DETALJ   TO W001-RAD                                 
140400         MOVE W001D4-DETALJ TO W001-RAD                                   
140500         PERFORM S05-SKRIV-W46055-001                                     
140600         PERFORM S03-LAES-W46089                                          
140700     END-PERFORM                                                          
140800     .                                                                    
140900     EJECT                                                                
141000                                                                          
141100 ED-SKRIV-RHD-POST SECTION.                                               
141200     SKIP2                                                                
141300     MOVE WS-IDKUNDNR TO W001D1-IDKUNDNR                                  
141400     MOVE SPACE TO W001D1-IDORDNR                                         
141500     MOVE WS-ANTAL-RADER TO W001D1-ANT-TRANS                              
141600     MOVE WS-ANTAL-RADER-OK TO W001D1-ANT-TRANS-OK                        
141700     MOVE KREDIT-TEXT TO W001D1-TEXT                                      
141800     MOVE W001D1-DETALJ TO W001-RAD                                       
141900     PERFORM S04-SKRIV-W46055-001                                         
142000     .                                                                    
142100     EJECT                                                                
142200                                                                          
142300 S01-LAES-W46088 SECTION.                                                 
142400     SKIP2                                                                
142500     READ W46088 INTO I08-AREA                                            
142600         AT END                                                           
142700             MOVE JA TO W46088-EOF                                        
142800     END-READ                                                             
142900                                                                          
143000     IF W46088-EOF = NEJ                                                  
143100         MOVE 'W46088'      TO POSTSUM-FDNAMN                             
143200         MOVE 'W46055D1'    TO POSTSUM-DDNAMN2                            
143300         MOVE I08-OHUV-IDPTYP TO POSTSUM-TRANSTYP                         
143400         CALL POSTSUM USING POSTSUM-PARM                                  
143500     END-IF                                                               
143600     .                                                                    
143700     EJECT                                                                
143800                                                                          
143900 S02-LAES-W46086 SECTION.                                                 
144000     SKIP2                                                                
144100     READ W46086 INTO I26-AREA                                            
144200         AT END                                                           
144300             MOVE JA TO W46086-EOF                                        
144400     END-READ                                                             
144500                                                                          
144600     IF W46086-EOF = NEJ                                                  
144700         MOVE 'W46086'   TO POSTSUM-FDNAMN                                
144800         MOVE 'W46055D2' TO POSTSUM-DDNAMN2                               
144900         MOVE I26-OHUV-IDPTYP TO POSTSUM-TRANSTYP                         
145000         CALL POSTSUM USING POSTSUM-PARM                                  
145100     END-IF                                                               
145200     .                                                                    
145300     EJECT                                                                
145400                                                                          
145500 S03-LAES-W46089 SECTION.                                                 
145600     SKIP2                                                                
145700     READ W46089 INTO I36-AREA                                            
145800         AT END                                                           
145900             MOVE JA TO W46089-EOF                                        
146000     END-READ                                                             
146100                                                                          
146200     IF W46089-EOF = NEJ                                                  
146300         MOVE 'W46089'   TO POSTSUM-FDNAMN                                
146400         MOVE 'W46055D3' TO POSTSUM-DDNAMN2                               
146500         MOVE I36-OHUV-IDPTYP TO POSTSUM-TRANSTYP                         
146600         CALL POSTSUM USING POSTSUM-PARM                                  
146700     END-IF                                                               
146800     .                                                                    
146900     EJECT                                                                
147000                                                                          
147100 S04-SKRIV-W46055-001 SECTION.                                            
147200     SKIP2                                                                
147300     IF W001-RADRAKNARE > W001-MAX-RADER-PER-SIDA                         
147400         PERFORM S04A-SKRIV-W46055-001-RUBRIK                             
147500     END-IF                                                               
147600*                                                                         
147700     WRITE W46055-001-RAD FROM W001-RAD                                   
147800*                                                                         
147900     ADD 1 TO W001-RADRAKNARE                                             
148000     MOVE SPACE TO W001-RAD                                               
148100     .                                                                    
148200     EJECT                                                                
148300                                                                          
148400 S04A-SKRIV-W46055-001-RUBRIK SECTION.                                    
148500     SKIP2                                                                
148600     ADD 1 TO W001-SIDRAKNARE                                             
148700     MOVE W001-SIDRAKNARE TO W001R1-SIDNR                                 
148800     WRITE W46055-001-RAD FROM W001R1-RUBRIK                              
148900*                                                                         
149000     WRITE W46055-001-RAD FROM W001R2-RUBRIK                              
149100*                                                                         
149200     WRITE W46055-001-RAD FROM W001R3-RUBRIK                              
149300*                                                                         
149400     WRITE W46055-001-RAD FROM W001R4-RUBRIK                              
149500*                                                                         
149600     MOVE WS-BEBETRAD-1 TO W001R5-BEBETRAD-1                              
149700     WRITE W46055-001-RAD FROM W001R5-RUBRIK                              
149800*                                                                         
149900     MOVE WS-BEBETRAD-2 TO W001R6-BEBETRAD-2                              
150000     WRITE W46055-001-RAD FROM W001R6-RUBRIK                              
150100*                                                                         
150200     WRITE W46055-001-RAD FROM W001R7-RUBRIK                              
150300*                                                                         
150400     WRITE W46055-001-RAD FROM W001R8-RUBRIK                              
150500*                                                                         
150600     MOVE 18 TO W001-RADRAKNARE                                           
150700     .                                                                    
150800     EJECT                                                                
150900                                                                          
151000 S05-SKRIV-W46055-001 SECTION.                                            
151100     SKIP2                                                                
151200     IF W001-RADRAKNARE > W001-MAX-RADER-PER-SIDA                         
151300         PERFORM S05A-SKRIV-W46055-001-RUBRIK                             
151400     END-IF                                                               
151500*                                                                         
151600     WRITE W46055-001-RAD FROM W001-RAD                                   
151700*                                                                         
151800     ADD 1 TO W001-RADRAKNARE                                             
151900     MOVE SPACE TO W001-RAD                                               
152000     .                                                                    
152100     EJECT                                                                
152200                                                                          
152300 S05A-SKRIV-W46055-001-RUBRIK SECTION.                                    
152400     SKIP2                                                                
152500     IF W001-RADRAKNARE > W001-MAX-RADER-PER-SIDA                         
152600         ADD 1 TO W001-SIDRAKNARE                                         
152700         MOVE W001-SIDRAKNARE TO W001R1-SIDNR                             
152800         WRITE W46055-001-RAD FROM W001R1-RUBRIK                          
152900*                                                                         
153000         WRITE W46055-001-RAD FROM W001R2-RUBRIK                          
153100*                                                                         
153200         WRITE W46055-001-RAD FROM W001R9-RUBRIK                          
153300*                                                                         
153400         WRITE W46055-001-RAD FROM W001R10-RUBRIK                         
153500*                                                                         
153600     END-IF                                                               
153700     EVALUATE I36-OHUV-IDPTYP                                             
153800                                                                          
153900       WHEN 'RHA'                                                         
154000               MOVE 'TRNO.  ERR  ERROR-TEXT'                              
154100                       TO W001R11-FEL                                     
154200               WRITE W46055-001-RAD FROM W001R11-RUBRIK                   
154300       WHEN 'RHB'                                                         
154400               MOVE 'TRNO.  ERR  ERROR-TEXT'                              
154500                              TO W001R12-FEL                              
154600               WRITE W46055-001-RAD FROM W001R12-RUBRIK                   
154700       WHEN 'RHC'                                                         
154800               MOVE 'TRNO.  ERR  ERROR-TEXT' TO W001R13-FEL               
154900               WRITE W46055-001-RAD FROM W001R13-RUBRIK                   
155000                                                                          
155100     END-EVALUATE                                                         
155200     MOVE 12 TO W001-RADRAKNARE                                           
155300     .                                                                    
155400     EJECT                                                                
155500                                                                          
155600 S05B-SKRIV-W46055-001-RUBRIK SECTION.                                    
155700     SKIP2                                                                
155800     IF (W001-RADRAKNARE + 5) < W001-MAX-RADER-PER-SIDA                   
155900        EVALUATE I36-OHUV-IDPTYP                                          
156000                                                                          
156100          WHEN 'RHA'                                                      
156200                   MOVE SPACE TO W001R11-FEL                              
156300                   WRITE W46055-001-RAD FROM W001R11-RUBRIK               
156400          WHEN 'RHB'                                                      
156500                   MOVE SPACE TO W001R12-FEL                              
156600                   WRITE W46055-001-RAD FROM W001R12-RUBRIK               
156700          WHEN 'RHC'                                                      
156800                   MOVE SPACE TO W001R13-FEL                              
156900                   WRITE W46055-001-RAD FROM W001R13-RUBRIK               
157000                                                                          
157100        END-EVALUATE                                                      
157200        ADD 3            TO W001-RADRAKNARE                               
157300     ELSE                                                                 
157400        MOVE 100 TO W001-RADRAKNARE                                       
157500        PERFORM S05A-SKRIV-W46055-001-RUBRIK                              
157600     END-IF                                                               
157700     .                                                                    
157800     EJECT                                                                
157900                                                                          
158000 S06-SKRIV-W46055-001-TOTAL SECTION.                                      
158100     SKIP2                                                                
158200     MOVE RHARAKNARE TO W001D6-RHARAKNARE                                 
158300     MOVE RHBRAKNARE TO W001D7-RHBRAKNARE                                 
158400     MOVE RHCRAKNARE TO W001D8-RHCRAKNARE                                 
158500     MOVE RHDRAKNARE TO W001D10-RHDRAKNARE                                
158600     MOVE RHERAKNARE TO W001D11-RHERAKNARE                                
158700     MOVE RHFRAKNARE TO W001D12-RHFRAKNARE                                
158800     ADD RHARAKNARE RHBRAKNARE RHCRAKNARE RHDRAKNARE                      
158900         RHERAKNARE RHFRAKNARE   GIVING RHTOTAL                           
159000     MOVE RHTOTAL TO W001T1-RHTOTAL                                       
159100     ADD 1 TO W001-SIDRAKNARE                                             
159200     MOVE W001-SIDRAKNARE TO W001R1-SIDNR                                 
159300     WRITE W46055-001-RAD FROM W001R1-RUBRIK                              
159400     WRITE W46055-001-RAD FROM W001R2-RUBRIK                              
159500     WRITE W46055-001-RAD FROM W001R14-RUBRIK                             
159600     WRITE W46055-001-RAD FROM W001R15-RUBRIK                             
159700     WRITE W46055-001-RAD FROM W001D5-DETALJ                              
159800     WRITE W46055-001-RAD FROM W001D6-DETALJ                              
159900     WRITE W46055-001-RAD FROM W001D7-DETALJ                              
160000     WRITE W46055-001-RAD FROM W001D8-DETALJ                              
160100     WRITE W46055-001-RAD FROM W001D10-DETALJ                             
160200     WRITE W46055-001-RAD FROM W001D11-DETALJ                             
160300     WRITE W46055-001-RAD FROM W001D12-DETALJ                             
160400     WRITE W46055-001-RAD FROM W001D9-DETALJ                              
160500     WRITE W46055-001-RAD FROM W001T1-TOTAL                               
160600     .                                                                    
160700     EJECT                                                                
160800                                                                          
160900 S07-LAES-W4608S SECTION.                                                 
161000     SKIP2                                                                
161100     READ W4608S INTO I1S-AREA                                            
161200         AT END                                                           
161300             MOVE JA TO W4608S-EOF                                        
161400     END-READ                                                             
161500                                                                          
161600     IF W4608S-EOF = NEJ                                                  
161700        EVALUATE I1S-KRED-IDPTYP                                          
161800                                                                          
161900          WHEN 'RHD'                                                      
162000                   ADD 1 TO RHDRAKNARE                                    
162100          WHEN 'RHE'                                                      
162200                   ADD 1 TO RHERAKNARE                                    
162300          WHEN 'RHF'                                                      
162400                   ADD 1 TO RHFRAKNARE                                    
162500                                                                          
162600        END-EVALUATE                                                      
162700        MOVE 'W4608S'   TO POSTSUM-FDNAMN                                 
162800        MOVE 'W46055D4' TO POSTSUM-DDNAMN2                                
162900        MOVE I1S-KRED-IDPTYP TO POSTSUM-TRANSTYP                          
163000        CALL POSTSUM USING POSTSUM-PARM                                   
163100     END-IF                                                               
163200     .                                                                    
163300     EJECT                                                                
163400                                                                          
163500 S12-SORT-RELEASE SECTION.                                                
163600     SKIP2                                                                
163700     RELEASE SORT-POST FROM SORTWS-AREA                                   
163800     .                                                                    
163900     EJECT                                                                
164000                                                                          
164100 S13-SORT-RETURN SECTION.                                                 
164200     SKIP2                                                                
164300     RETURN SORTFIL INTO SORTWS-AREA                                      
164400         AT END                                                           
164500             MOVE JA TO SORTFIL-EOF                                       
164600     END-RETURN                                                           
164700     .                                                                    
164800     EJECT                                                                
164900                                                                          
165000 S21-BEHANDLA-ORDERHUVUD SECTION.                                         
165100     SKIP2                                                                
165200     PERFORM S13-SORT-RETURN                                              
165300     .                                                                    
165400     EJECT                                                                
165500                                                                          
165600 S22-BEHANDLA-ORDERRADER SECTION.                                         
165700     SKIP2                                                                
165800     ADD 1 TO WS-ANTAL-RADER                                              
165900     PERFORM S13-SORT-RETURN                                              
166000     PERFORM UNTIL NOT                                                    
166100       (SORTFIL-EOF = NEJ AND                                             
166200             SORTWS-OHUV-SORT-IDDISTR = WS-IDDISTR-1 AND                  
166300             SORTWS-OHUV-SORT-TIFILDAT = WS-TIFILDAT AND                  
166400             SORTWS-OHUV-SORT-TIHHMMSS = WS-TIHHMMSS AND                  
166500             SORTWS-OHUV-IDDISTR = WS-IDDISTR-2 AND                       
166600             SORTWS-OHUV-IDKUNDNR = WS-IDKUNDNR AND                       
166700             SORTWS-OHUV-IDORDNR = WS-IDORDNR AND                         
166800             SORTWS-OHUV-IDPTYP = 'RHB')                                  
166900         ADD 1 TO WS-ANTAL-RADER                                          
167000         PERFORM S13-SORT-RETURN                                          
167100     END-PERFORM                                                          
167200     .                                                                    
167300     EJECT                                                                
167400                                                                          
167500 S23-BEHANDLA-HASHTOTAL SECTION.                                          
167600     SKIP2                                                                
167700     PERFORM S13-SORT-RETURN                                              
167800     .                                                                    
167900     EJECT                                                                
168000                                                                          
168100 Z-FINIT SECTION.                                                         
168200     SKIP2                                                                
168300     CLOSE W46086                                                         
168400           W46089                                                         
168500           W46088                                                         
168600           W4608S                                                         
168700*- - - - - - - - - - - - - - - - - - SKRIV UT ANTAL LÄSTA OCH             
168800*                                    SKRIVNA POSTER                       
168900                                                                          
169000     MOVE 'S' TO POSTSUM-OPKOD                                            
169100     CALL POSTSUM USING POSTSUM-PARM                                      
169200     .                                                                    
169300     EJECT                                                                
169400                                                                          
169500 IMS-GET-WDB201 SECTION.                                                  
169600     STRING 'WLGMTA01(IDGMT   >=' W-IDGMT-X ')'                           
169700            DELIMITED BY SIZE INTO SSA1                                   
169800     MOVE '  GE' TO GODK-STATUSKODER                                      
169900     CALL CBLTDLI USING GU GMTA-PCB DLI-IO-AREA-WDB2 SSA1                 
170000     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
170100     PERFORM IMS-STATUSKONTROLL                                           
170200     .                                                                    
170300     SKIP2                                                                
170400 IMS-GET-WDB101 SECTION.                                                  
170500     STRING 'WLBETC01(WDB101KY =' W-WDB101KY-X ')'                        
170600            DELIMITED BY SIZE INTO SSA1                                   
170700     MOVE '  GE' TO GODK-STATUSKODER                                      
170800     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-AREA SSA1                      
170900     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
171000     PERFORM IMS-STATUSKONTROLL                                           
171100     .                                                                    
171200     SKIP2                                                                
171300 IMS-STATUSKONTROLL SECTION.                                              
171400     SET STATUS-IX TO 1                                                   
171500     SEARCH GODK-STATUS                                                   
171600          AT END CALL FELLOG                                              
171700     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                             
171800          CONTINUE                                                        
171900     .                                                                    
