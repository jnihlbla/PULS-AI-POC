000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4034P00.                                                
000300 AUTHOR.         CAMELIA OLGRENER.                                        
000400 DATE-WRITTEN.   16/06/28.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        PROGRAMMET ÄR ETT BAKGRUNDSMPP. STARTAS AV 4345.                 
001000*        SKAPAR FLAGGOR FÖR SAMLINGSKOLLIN.                               
001100*                                                                         
001200*    INDATA.                                                              
001300*        TRANSAKTION: W4T34PX                                             
001400*                                                                         
001500                                                                          
001600 ENVIRONMENT DIVISION.                                                    
001700                                                                          
001800 DATA DIVISION.                                                           
001900     EJECT                                                                
002000 WORKING-STORAGE SECTION.                                                 
002100 77  CURRENT-SECTION             PIC X(24)   VALUE SPACE.                 
002200                                                                          
002300*    -- CHECKED BY WY2000                                                 
002400 77  IDPGM                       PIC X(08)   VALUE 'W4034P00'.            
002500                                                                          
002600*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
002700 77  FELTEXT                     PIC X(80)        VALUE SPACE.            
002800                                                                          
002900 77  JA                          PIC X       VALUE 'J'.                   
003000 77  NEJ                         PIC X       VALUE 'N'.                   
003100 77  WS-DAGENS-DATUM             PIC 9(8)    VALUE ZERO.                  
003200 77  WS-IDPRODNR                 PIC S9(7)   VALUE ZERO COMP-3.           
003300 77  WS-IDDISTR                  PIC 9(4)    VALUE ZERO.                  
003400 77  WS-KVKOLLI                  PIC 9(3)    VALUE ZERO.                  
003500 77  WS-KOMMATECKEN              PIC X       VALUE ','.                   
003600 77  IX                          PIC S9(9)   VALUE +0   COMP SYNC.        
003700                                                                          
003800 77  ALLT-SW                     PIC X.                                   
003900     88  ALLT-OK                             VALUE 'J'.                   
004000     88  ALLT-FEL                            VALUE 'N'.                   
004100                                                                          
004200 01  WS-VKORDBTO                 PIC 9(6)V9 VALUE ZERO.                   
004300 01  FILLER REDEFINES WS-VKORDBTO.                                        
004400     03 WS-KILO                  PIC 9(6).                                
004500     03 WS-HEKTO                 PIC 9.                                   
004600                                                                          
004700                                                                          
004800 01  WS-VLORDBTO                 PIC 9(4)V9(3) VALUE ZERO.                
004900 01  FILLER REDEFINES WS-VLORDBTO.                                        
005000     03 WS-VLM3                  PIC 9(4).                                
005100     03 WS-VLCM3                 PIC 9(3).                                
005200                                                                          
005300     EJECT                                                                
005400 01  LISTVAL                     PIC X(8)    VALUE SPACE.                 
005500 01  WS-PRT-DUMMY                PIC X       VALUE SPACE.                 
005600                                                                          
005700*REGARDING WS-KDPRTVAL:                                                   
005800*                                                                         
005900*THERE ARE 5 PRINTER FORMATS                                              
006000*ITS IMPORTANT TO USE THE CORRECT 88 LEVEL WHEN ADDING                    
006100*A NEW PRINTER ALIAS TO THE 88 LEVELS.                                    
006200*                                                                         
006300 77  WS-KDPRTVAL                 PIC XX.                                  
006400*    -- BELOW BELOW PRINTING TO NOVA PRINTER                              
006500*    -- NOTE!                                                             
006600*    -- WHEN CHANGING THIS 88 LEVEL ALSO CHANGE IN W4033300               
006700     88 SKRIV-PA-NOVA-SKRIVARE        VALUE '" ' '* ' '- '                
006800                                            '? '                          
006810                                            '0 ' '1 ' '7 ' '10'           
006820                                            '12'                          
006900                                            '20' '21' '22' '23'           
006910                                            '27' '50'                     
007000                                            '31' '32' '35' '36'           
007001                                            'AB' 'AS' 'AZ'                
007010                                            'CC'                          
007100                                            'D ' 'DD'                     
007200                                            'E3' 'E4'                     
007210                                            'F ' 'FV'                     
007300                                            'G ' 'GG' 'H '                
007400                                            'J ' 'JK'                     
007600                                            'LA' 'LP' 'LL'                
007610                                            'NN' 'NJ' 'NL'                
007700                                            'P ' 'PV' 'PS'                
007800                                            'Q1' 'Q2' 'Q3'                
007900                                            'R ' 'R7'                     
007901                                            'T '                          
007910                                            'UY' 'UI' 'UJ'                
008000                                            'VU' 'VV' 'WW'                
008100                                            'W3'                          
008200                                            'XA' 'X-' 'X '                
008300                                            'Y ' 'YH'                     
008500                                            'ZZ'.                         
008510*                                                                         
008600     EJECT                                                                
008700*****************************************************************         
008800*NOVA     AREA MED STYRTECKEN FÖR NOVA TERMO SKRIVARE.          *         
008900*         ANV. FÖR ATT SKRIVA KOLLIFLAGGA I A5 FORMAT I CDC     *         
009000*         NOVA-LBL = CASE LABEL SIZE NOVA FORMAT                *         
009100*****************************************************************         
009200 01  FILLER           PIC X(24)  VALUE 'KOLLI-FLNOVA  TERMO'.             
009300*    STYRTECKEN ENLIGT MANUAL: IMAJE/NOVA THERMAL PRINTER                 
009400*                              LABELPOINT                                 
009500 01  CASE-LABEL-THERMO-NOVA-A6.                                           
009600   03  NOVA-LBL-RAD  PIC X(132)  VALUE SPACE.                             
009700                                                                          
009800   03  NOVA-LBL-STYR-COBRA.                                               
009900     05  FILLER      PIC X(16) VALUE '&&??%%P%P=207,30'.                  
010000     05  FILLER      PIC X(19) VALUE '=1,0=5,8=24,0=31,96'.               
010100     05  FILLER      PIC X(21) VALUE '=32,8=33,0=34,1=45,87'.             
010200     05  FILLER      PIC X(12) VALUE '=63,13=136,0'.                      
010300     05  FILLER      PIC X(22) VALUE '=207,12=207,10%&&??000'.            
010400                                                                          
010500   03  NOVA-LBL-STYR-01.                                                  
010600     05  FILLER      PIC X(3)  VALUE '!CÅ'.                               
010700                                                                          
010800   03  NOVA-LBL-STYR-91.                                                  
010900     05  FILLER      PIC X(3)  VALUE '!PÅ'.                               
011000                                                                          
011100   03  NOVA-LBL-STYR-42.                                                  
011200     05  FILLER      PIC X(6)  VALUE '!Y42 0'.                            
011300                                                                          
011400*RUB-TRANSPORT NR                                                         
011500   03  NOVA-LBL-RUB-TRANSPORT-NR.                                         
011600     05  FILLER      PIC X(25) VALUE '!F T W  50 1900 L 1 1 3 '.          
011700     05  FILLER      PIC X(15) VALUE '"TRANSPORT NO"Å'.                   
011800*RUB-SAMKOLLI NR                                                          
011900   03  NOVA-LBL-RUB-SAMKOLLI-NR.                                          
012000     05  FILLER      PIC X(25) VALUE '!F T W  50 1500 L 1 1 3 '.          
012100     05  FILLER      PIC X(16) VALUE '"MIXED CASE NO"Å'.                  
012200                                                                          
012300   03  NOVA-LBL-RUB-FREIGHT-CODE.                                         
012400     05  FILLER      PIC X(25) VALUE '!F T W  50   50 R 1 1 3 '.          
012500     05  FILLER      PIC X(15) VALUE '"FREIGHT CODE"Å'.                   
012600                                                                          
012700*RUB-DISTRICT                                                             
012800   03  NOVA-LBL-RUB-DISTRICT.                                             
012900     05  FILLER      PIC X(25) VALUE '!F T W 400 1900  L 1 1 3 '.         
013000     05  FILLER      PIC X(11) VALUE '"DISTRICT"Å'.                       
013100                                                                          
013200*RUB-ANT KOLLI                                                            
013300   03  NOVA-LBL-RUB-ANT-KOLLI.                                            
013400     05  FILLER      PIC X(25) VALUE '!F T W 400 1050  L 1 1 3 '.         
013500     05  FILLER      PIC X(8)  VALUE '"CASES"Å'.                          
013600                                                                          
013700*RUB-WEIGHT KG                                                            
013800   03  NOVA-LBL-RUB-WEIGHT.                                               
013900     05  FILLER      PIC X(25) VALUE '!F T W  400   50 R 1 1 3 '.         
014000     05  FILLER      PIC X(12)  VALUE '"WEIGHT KG"Å'.                     
014100                                                                          
014200*RUB-IDDC                                                                 
014300   03  NOVA-LBL-RUB-IDDC.                                                 
014400     05  FILLER      PIC X(25) VALUE '!F T W 750 1900 L 1 1 3 '.          
014500     05  FILLER      PIC X(5)  VALUE '"DC"Å'.                             
014600                                                                          
014700*POSTNORD INFO?                                                           
014800   03  NOVA-LBL-RUB-TRANSPORT.                                            
014900     05  FILLER      PIC X(25) VALUE '!F T W  930 1950 L 1 1 3 '.         
015000     05  FILLER      PIC X(17) VALUE '"TRANSPORT INFO"Å'.                 
015100                                                                          
015200*DC WH ADDRESS                                                            
015300   03  NOVA-LBL-RUB-DC-WH-ADDRESS.                                        
015400     05  FILLER      PIC X(25) VALUE '!F T W 1150 1600 L 1 1 3 '.         
015500     05  FILLER      PIC X(16) VALUE '"DC WH ADDRESS"Å'.                  
015600                                                                          
015700   03  NOVA-LBL-RUB-CARRIER.                                              
015800     05  FILLER      PIC X(25) VALUE '!F T W 1270  850 L 1 1 3 '.         
015900     05  FILLER      PIC X(10) VALUE '"CARRIER"Å'.                        
016000                                                                          
016100*IDTRPTNR                                                                 
016200   03  NOVA-LBL-DATA-IDTRPTNR.                                            
016300     05  FILLER      PIC X(25) VALUE '!F T W  230 1900 L 3 3 6 '.         
016400     05  FILLER      PIC X(1)  VALUE '"'.                                 
016500     05  NOVA-LBL-IDTRPTNR   PIC Z(3) VALUE ZERO.                         
016600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
016700                                                                          
016800*IDKOLLI-SAMP                                                             
016900   03  NOVA-LBL-DATA-IDKOLLI-SAMP.                                        
017000*    05  FILLER      PIC X(25) VALUE '!F T W  230 1350 L 3 3 6 '.         
017100     05  FILLER      PIC X(25) VALUE '!F T W  230 1500 L 3 3 6 '.         
017200     05  FILLER      PIC X(1)  VALUE '"'.                                 
017300     05  NOVA-LBL-IDKOLLI-SAMP     PIC Z(5) VALUE ZERO.                   
017400     05  FILLER      PIC X(2)  VALUE '"Å'.                                
017500                                                                          
017600*KDFRAKT                                                                  
017700   03  NOVA-LBL-DATA-KDFRAKT.                                             
017800     05  FILLER      PIC X(25) VALUE '!F T W  230  50 R 3 3 6 '.          
017900     05  FILLER      PIC X(1)  VALUE '"'.                                 
018000     05  NOVA-LBL-KDFRAKT-1X   PIC XX VALUE SPACE.                        
018100     05  NOVA-LBL-KDFRAKT-1    REDEFINES                                  
018200         NOVA-LBL-KDFRAKT-1X   PIC ZZ.                                    
018300     05  NOVA-LBL-SLASH-1      PIC X  VALUE ' '.                          
018400     05  NOVA-LBL-KDFRAKT-2X   PIC XX VALUE SPACE.                        
018500     05  NOVA-LBL-KDFRAKT-2    REDEFINES                                  
018600         NOVA-LBL-KDFRAKT-2X   PIC ZZ.                                    
018700     05  NOVA-LBL-SLASH-2      PIC X  VALUE ' '.                          
018800     05  NOVA-LBL-KDFRAKT-3X   PIC XX VALUE SPACE.                        
018900     05  NOVA-LBL-KDFRAKT-3    REDEFINES                                  
019000         NOVA-LBL-KDFRAKT-3X   PIC ZZ.                                    
019100     05  FILLER      PIC X(2)  VALUE '"Å'.                                
019200                                                                          
019300*IDDISTR-SE                                                               
019400   03  NOVA-LBL-DATA-IDDISTR.                                             
019500     05  FILLER      PIC X(25) VALUE '!F T W  580 1900 L 3 3 6 '.         
019600     05  FILLER      PIC X(1)  VALUE '"'.                                 
019700     05  NOVA-LBL-IDDISTR PIC Z(4) VALUE ZERO.                            
019800     05  FILLER      PIC X(2)  VALUE '"Å'.                                
019900                                                                          
020000*KVKOLLI                                                                  
020100   03  NOVA-LBL-DATA-KVKOLLI.                                             
020200     05  FILLER      PIC X(25) VALUE '!F T W  580 1350 L 3 3 6 '.         
020300     05  FILLER      PIC X(1)  VALUE '"'.                                 
020400     05  NOVA-LBL-KVKOLLI PIC Z(4) VALUE ZERO.                            
020500     05  FILLER      PIC X(2)  VALUE '"Å'.                                
020600                                                                          
020700   03  NOVA-LBL-DATA-WEIGHT.                                              
020800     05  FILLER      PIC X(25) VALUE '!F T W  580  50 R 3 3 6 '.          
020900     05  FILLER      PIC X(1)  VALUE '"'.                                 
021000     05  NOVA-LBL-VKORDBTO PIC Z(5) VALUE ZERO.                           
021100     05  FILLER      PIC X(2)  VALUE '"Å'.                                
021200*                                                                         
021300   03  NOVA-LBL-DATA-KILO-HEKTO.                                          
021400     05  FILLER      PIC X(25) VALUE '!F T W  580  50 R 3 3 6 '.          
021500     05  FILLER      PIC X(1)  VALUE '"'.                                 
021600     05  NOVA-LBL-KILO    PIC Z(4)9  VALUE ZERO.                          
021700     05  NOVA-LBL-PUNKT   PIC X      VALUE '.'.                           
021800     05  NOVA-LBL-HEKTO   PIC 9      VALUE ZERO.                          
021900     05  FILLER      PIC X(2)  VALUE '"Å'.                                
022000                                                                          
022100*                                                                         
022200   03  NOVA-LBL-DATA-KILO-HEKTO-GB.                                       
022300     05  FILLER      PIC X(25) VALUE '!F T W  1580 20 R 3 3 6 '.          
022400     05  FILLER      PIC X(1)  VALUE '"'.                                 
022500     05  NOVA-LBL-KILO-GB    PIC Z(5)   VALUE ZERO.                       
022600     05  NOVA-LBL-PUNKT-GB   PIC X      VALUE '.'.                        
022700     05  NOVA-LBL-HEKTO-GB   PIC 9      VALUE ZERO.                       
022800     05  FILLER      PIC X(2)  VALUE '"Å'.                                
022900                                                                          
023000*IDDC                                                                     
023100   03  NOVA-LBL-DATA-IDDC.                                                
023200     05  FILLER      PIC X(25) VALUE '!F T W  930 1900 L 3 3 6 '.         
023300     05  FILLER      PIC X(1)  VALUE '"'.                                 
023400     05  NOVA-LBL-IDDC             PIC X(2) VALUE SPACE.                  
023500     05  FILLER      PIC X(2)  VALUE '"Å'.                                
023600                                                                          
023700*                                                                         
023800   03  NOVA-LBL-ADFLGEO.                                                  
023900     05  FILLER      PIC X(25) VALUE '!F T W 1300 1600 L 3 3 6 '.         
024000     05  FILLER      PIC X(1)  VALUE '"'.                                 
024100     05  NOVA-LBL-DATA-ADFLGEO        PIC X(3) VALUE ZERO.                
024200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
024300                                                                          
024400*                                                                         
024500   03  NOVA-LBL-ADFLOMR.                                                  
024600     05  FILLER      PIC X(25) VALUE '!F T W 1300 1100 L 3 3 6 '.         
024700     05  FILLER      PIC X(1)  VALUE '"'.                                 
024800     05  NOVA-LBL-DATA-ADFLOMR  PIC Z(3) VALUE ZERO.                      
024900     05  FILLER      PIC X(2)  VALUE '"Å'.                                
025000                                                                          
025100*                                                                         
025200   03  NOVA-LBL-ADRUTNIV.                                                 
025300     05  FILLER      PIC X(25) VALUE '!F T W 1300  600 L 3 3 6 '.         
025400     05  FILLER      PIC X(1)  VALUE '"'.                                 
025500     05  NOVA-LBL-DATA-ADRUTNIV  PIC Z(3) VALUE ZERO.                     
025600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
025700                                                                          
025800*                                                                         
025900   03  NOVA-LBL-BARCODE.                                                  
026000     05  FILLER    PIC X(30) VALUE '!F C W 1560 1600 L 140 3 12'.         
026100     05  FILLER                  PIC X(1)  VALUE '"'.                     
026200     05  NOVA-LBL-DISTR          PIC 9(4).                                
026300     05  NOVA-LBL-KUNDNR         PIC 9(6).                                
026400     05  NOVA-LBL-ORDNR          PIC 9(7).                                
026500     05  NOVA-LBL-KOLLI-SAMP     PIC 9(5).                                
026600     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
026700*                                                                         
026800   03  NOVA-LBL-BARCODE-POST.                                             
026900     05  FILLER    PIC X(30) VALUE '!F C W 1120 1600 L 140 3 12'.         
027000     05  FILLER                  PIC X(1)  VALUE '"'.                     
027100     05  NOVA-LBL-DISTR-POST     PIC 9(4).                                
027200     05  NOVA-LBL-KUNDNR-POST    PIC 9(6).                                
027300     05  NOVA-LBL-ORDNR-POST     PIC 9(7).                                
027400     05  NOVA-LBL-KOLLI-POST-SAMP PIC 9(5).                               
027500     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
027600                                                                          
027700*                                            1570                         
027800   03  NOVA-LBL-TEXT-BELOW-BARCODE.                                       
027900     05  FILLER      PIC X(30) VALUE '!F T W 1600 1600 L 1 1 3'.          
028000     05  FILLER                  PIC X(1)  VALUE '"'.                     
028100     05  NOVA-LBL-DIST           PIC 9(4).                                
028200     05  NOVA-LBL-KUNDN          PIC 9(6).                                
028300     05  NOVA-LBL-ORDN           PIC 9(7).                                
028400     05  NOVA-LBL-KOLI-SAMP      PIC 9(5).                                
028500     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
028600*POSTEN                                                                   
028700   03  NOVA-LBL-TXT-BLW-BARCODE-POST.                                     
028800     05  FILLER      PIC X(30) VALUE '!F T W 1620 1850 L 1 1 3'.          
028900     05  FILLER                  PIC X(1)  VALUE '"'.                     
029000     05  NOVA-LBL-DIST-POST      PIC 9(4).                                
029100     05  NOVA-LBL-KUNDN-POST     PIC 9(6).                                
029200     05  NOVA-LBL-ORDN-POST      PIC 9(7).                                
029300     05  NOVA-LBL-KOLI-POST      PIC 9(5).                                
029400     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
029500                                                                          
029600   03  NOVA-LBL-TEXT-SHIPPER-CDC.                                         
029700     05  FILLER      PIC X(30) VALUE '!F T W 1620 1300 L 1 1 3'.          
029800     05  FILLER                  PIC X(1)  VALUE '"'.                     
029900     05  FILLER      PIC X(20) VALUE 'SHIPPER: VOLVO CARS '.              
030000     05  FILLER      PIC X(18) VALUE 'CUSTOMER SERVICE, '.                
030100     05  FILLER      PIC X(21) VALUE 'SE-405 31 GOTHENBURG,'.             
030200     05  FILLER      PIC X(7)  VALUE ' SWEDEN'.                           
030300     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
030400                                                                          
030500   03  NOVA-LBL-TEXT-SHIPPER-CDC-POST.                                    
030600     05  FILLER      PIC X(30) VALUE '!F T W 1620 1300 L 1 1 3 '.         
030700     05  FILLER                  PIC X(1)  VALUE '"'.                     
030800     05  FILLER      PIC X(20) VALUE 'SHIPPER: VOLVO CARS '.              
030900     05  FILLER      PIC X(18) VALUE 'CUSTOMER SERVICE, '.                
031000     05  FILLER      PIC X(21) VALUE 'SE-405 31 GOTHENBURG,'.             
031100     05  FILLER      PIC X(7)  VALUE ' SWEDEN'.                           
031200     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
031300*POSTEN                                                                   
031400   03  NOVA-LBL-TEXT-PRODUKT.                                             
031500     05  FILLER      PIC X(25) VALUE '!F T W  950 990  R 1 1 3 '.         
031600     05  FILLER      PIC X(10)  VALUE '"PRODUKT"Å'.                       
031700                                                                          
031800   03  NOVA-LBL-TEXT-PRODUKT-1090.                                        
031900     05  FILLER      PIC X(25) VALUE '!F T W  950 990  R 1 1 3 '.         
032000     05  FILLER      PIC X(10)  VALUE '"PRODUKT"Å'.                       
032100                                                                          
032200   03  NOVA-LBL-TEXT-PRODUKT-DK.                                          
032300     05  FILLER      PIC X(25) VALUE '!F T W  950 990  R 1 1 3 '.         
032400     05  FILLER      PIC X(10)  VALUE '"PRODUKT"Å'.                       
032500                                                                          
032600   03  NOVA-LBL-TEXT-PRODUKT-NO.                                          
032700     05  FILLER      PIC X(25) VALUE '!F T W  950 990  R 1 1 3 '.         
032800     05  FILLER      PIC X(10)  VALUE '"PRODUKT"Å'.                       
032900                                                                          
033000   03  NOVA-LBL-8700-TEXT-PRODUKT.                                        
033100     05  FILLER      PIC X(25) VALUE '!F T W  950 999  R 1 1 3 '.         
033200     05  FILLER      PIC X(10)  VALUE '"PRODUKT"Å'.                       
033300                                                                          
033400   03  NOVA-LBL-TEXT-HIT.                                                 
033500     05  FILLER      PIC X(25) VALUE '!F T W 1050 990  R 1 1 3 '.         
033600     05  FILLER      PIC X(6)  VALUE '"HIT"Å'.                            
033700                                                                          
033800   03  NOVA-LBL-TEXT-HIT-1090.                                            
033900     05  FILLER      PIC X(25) VALUE '!F T W 1050 990  R 1 1 3 '.         
034000     05  FILLER      PIC X(6)  VALUE '"HIT"Å'.                            
034100                                                                          
034200   03  NOVA-LBL-REFILL-TEXT-HIT.                                          
034300     05  FILLER      PIC X(25) VALUE '!F T W 1050 990  R 1 1 3 '.         
034400     05  FILLER      PIC X(6)  VALUE '"HIT"Å'.                            
034500                                                                          
034600   03  NOVA-LBL-8700-TEXT-HIT.                                            
034700     05  FILLER      PIC X(25) VALUE '!F T W 1050 990  R 1 1 3 '.         
034800     05  FILLER      PIC X(6)  VALUE '"HIT"Å'.                            
034900                                                                          
035000   03  NOVA-LBL-TEXT-PAK.                                                 
035100     05  FILLER      PIC X(25) VALUE '!F T W 1050 990  R 1 1 3 '.         
035200     05  FILLER      PIC X(6)  VALUE '"PAK"Å'.                            
035300                                                                          
035400   03  NOVA-LBL-TEXT-PAK-1090.                                            
035500     05  FILLER      PIC X(25) VALUE '!F T W 1050 990  R 1 1 3 '.         
035600     05  FILLER      PIC X(6)  VALUE '"PAK"Å'.                            
035700                                                                          
035800   03  NOVA-LBL-REFILL-TEXT-PAK.                                          
035900     05  FILLER      PIC X(25) VALUE '!F T W 1050 990  R 1 1 3 '.         
036000     05  FILLER      PIC X(6)  VALUE '"PAK"Å'.                            
036100                                                                          
036200   03  NOVA-LBL-8700-TEXT-PAK.                                            
036300     05  FILLER      PIC X(25) VALUE '!F T W 1050 990  R 1 1 3 '.         
036400     05  FILLER      PIC X(6)  VALUE '"PAK"Å'.                            
036500                                                                          
036600   03  NOVA-LBL-TEXT-48.                                                  
036700     05  FILLER      PIC X(25) VALUE '!F T W 1000 990  R 1 1 3 '.         
036800     05  FILLER      PIC X(5)  VALUE '"48"Å'.                             
036900                                                                          
037000   03  NOVA-LBL-TEXT-49-1090.                                             
037100     05  FILLER      PIC X(25) VALUE '!F T W 1000 990  R 1 1 3 '.         
037200     05  FILLER      PIC X(5)  VALUE '"49"Å'.                             
037300                                                                          
037400   03  NOVA-LBL-8700-TEXT-48.                                             
037500     05  FILLER      PIC X(25) VALUE '!F T W 1000 990  R 1 1 3 '.         
037600     05  FILLER      PIC X(5)  VALUE '"48"Å'.                             
037700                                                                          
037800   03  NOVA-LBL-TEXT-69.                                                  
037900     05  FILLER      PIC X(25) VALUE '!F T W 1000 990  R 1 1 3 '.         
038000     05  FILLER      PIC X(5)  VALUE '"69"Å'.                             
038100                                                                          
038200   03  NOVA-LBL-TEXT-54-1090.                                             
038300     05  FILLER      PIC X(25) VALUE '!F T W 1000 990  R 1 1 3 '.         
038400     05  FILLER      PIC X(5)  VALUE '"54"Å'.                             
038500                                                                          
038600   03  NOVA-LBL-8700-TEXT-54.                                             
038700     05  FILLER      PIC X(25) VALUE '!F T W 1000 990  R 1 1 3 '.         
038800     05  FILLER      PIC X(5)  VALUE '"54"Å'.                             
038900*POSTEN                                                                   
039000   03  NOVA-LBL-SORTERINGSKOD-1090.                                       
039100     05  FILLER      PIC X(25) VALUE '!F T W 1550 320  L 4 3 3 '.         
039200     05  FILLER      PIC X(1)  VALUE '"'.                                 
039300     05  NOVA-LBL-ADFLGEO-1090-FI  PIC X(2)  VALUE 'FI'.                  
039400     05  NOVA-LBL-ADFLGEO-SORT-1090 PIC X(3) VALUE ZERO.                  
039500     05  FILLER      PIC X(2)  VALUE '"Å'.                                
039600                                                                          
039700   03  NOVA-LBL-SORTERINGSKOD-DK.                                         
039800     05  FILLER      PIC X(25) VALUE '!F T W 1550 320  L 4 3 3 '.         
039900     05  FILLER      PIC X(1)  VALUE '"'.                                 
040000     05  NOVA-LBL-ADFLGEO-DK       PIC X(2)  VALUE 'DK'.                  
040100     05  NOVA-LBL-ADFLGEO-SORT-DK  PIC X(3)  VALUE ZERO.                  
040200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
040300                                                                          
040400   03  NOVA-LBL-SORTERINGSKOD-NO.                                         
040500     05  FILLER      PIC X(25) VALUE '!F T W 1550 320  L 4 3 3 '.         
040600     05  FILLER      PIC X(1)  VALUE '"'.                                 
040700     05  NOVA-LBL-ADFLGEO-NO       PIC X(2)  VALUE 'NO'.                  
040800     05  NOVA-LBL-ADFLGEO-SORT-NO  PIC X(3)  VALUE ZERO.                  
040900     05  FILLER      PIC X(2)  VALUE '"Å'.                                
041000                                                                          
041100   03  NOVA-LBL-SORTERINGSKOD.                                            
041200     05  FILLER      PIC X(25) VALUE '!F T W 1550 320  L 4 3 3 '.         
041300     05  FILLER      PIC X(1)  VALUE '"'.                                 
041400     05  NOVA-LBL-ADFLGEO-SE   PIC X(2) VALUE 'SE'.                       
041500     05  NOVA-LBL-ADFLGEO-SORT PIC X(3) VALUE ZERO.                       
041600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
041700                                                                          
041800*POSTEN                                                                   
041900   03  NOVA-LBL-BARCODE-POSTEN.                                           
042000     05  FILLER    PIC X(30) VALUE '!F C W 1050  950 L 100 3 41'.         
042100     05  FILLER                  PIC X(1)  VALUE '"'.                     
042200     05  NOVA-LBL-POSTEN1        PIC X(13).                               
042300     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
042400                                                                          
042500   03  NOVA-LBL-BARCODE-POSTEN-1090.                                      
042600     05  FILLER    PIC X(30) VALUE '!F C W 1050  950 L 100 3 41'.         
042700     05  FILLER                  PIC X(1)  VALUE '"'.                     
042800     05  NOVA-LBL-POSTEN1-1090   PIC X(13).                               
042900     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
043000                                                                          
043100   03  NOVA-LBL-8700-BARCODE-POSTEN.                                      
043200     05  FILLER    PIC X(30) VALUE '!F C W 1050  950 L 100 3 41'.         
043300     05  FILLER                  PIC X(1)  VALUE '"'.                     
043400     05  NOVA-LBL-8700-POSTEN1   PIC X(13).                               
043500     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
043600                                                                          
043700   03  NOVA-LBL-TEXT-POSTEN.                                              
043800     05  FILLER    PIC X(30) VALUE '!F T W 1120  950 L 1 1 3'.            
043900     05  FILLER                  PIC X(1)  VALUE '"'.                     
044000     05  NOVA-LBL-POSTEN1-TEXT   PIC X(13).                               
044100     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
044200                                                                          
044300   03  NOVA-LBL-TEXT-POSTEN-1090.                                         
044400     05  FILLER    PIC X(30) VALUE '!F T W 1120  950 L 1 1 3'.            
044500     05  FILLER                  PIC X(1)  VALUE '"'.                     
044600     05  NOVA-LBL-POSTEN1-TEXT-1090 PIC X(13).                            
044700     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
044800                                                                          
044900   03  NOVA-LBL-8700-TEXT-POSTEN.                                         
045000     05  FILLER    PIC X(30) VALUE '!F T W  1120 950 L 1 1 3'.            
045100     05  FILLER                    PIC X(1)  VALUE '"'.                   
045200     05  NOVA-LBL-8700-POSTEN1-TEXT PIC X(13).                            
045300     05  FILLER                    PIC X(2)  VALUE '"Å'.                  
045400                                                                          
045500     EJECT                                                                
045600*****************************************************************         
045700*         AREA MED STYRTECKEN FÖR MARKPOINT TERMO SKRIVARE.     *         
045800*         ANV. FÖR ATT SKRIVA KOLLIFLAGGA I 7INCH FORMAT I CDC  *         
045900*         CL7INCH = CASE LABEL SIZE 7INCH                                 
046000*****************************************************************         
046100 01  FILLER           PIC X(24)  VALUE 'KOLLI-FL7INCH TERMO'.             
046200*    STYRTECKEN ENLIGT MANUAL: MARKPOINT THERMAL PRINTER                  
046300*                              LABELPOINT                                 
046400 01  CASE-LABEL-THERMO-7INCH.                                             
046500   03  CL7INCH-RAD   PIC X(132)  VALUE SPACE.                             
046600                                                                          
046700   03  CL7INCH-STYR-COBRA.                                                
046800     05  FILLER      PIC X(16) VALUE '&&??%%P%P=207,30'.                  
046900     05  FILLER      PIC X(19) VALUE '=1,0=5,8=24,0=31,96'.               
047000     05  FILLER      PIC X(21) VALUE '=32,8=33,0=34,1=45,87'.             
047100     05  FILLER      PIC X(12) VALUE '=63,13=136,0'.                      
047200     05  FILLER      PIC X(22) VALUE '=207,12=207,10%&&??000'.            
047300                                                                          
047400   03  CL7INCH-STYR-01.                                                   
047500     05  FILLER      PIC X(3)  VALUE '!CÅ'.                               
047600                                                                          
047700   03  CL7INCH-STYR-91.                                                   
047800     05  FILLER      PIC X(3)  VALUE '!PÅ'.                               
047900                                                                          
048000   03  CL7INCH-STYR-42.                                                   
048100     05  FILLER      PIC X(6)  VALUE '!Y42 0'.                            
048200                                                                          
048300*TRANSPORTNR                                                              
048400   03  CL7INCH-RUB-TRANSPORTNR.                                           
048500     05  FILLER      PIC X(25) VALUE '!F T N  100 250  L 1 1 3 '.         
048600     05  FILLER      PIC X(15) VALUE '"TRANSPORT NO"Å'.                   
048700*                                                                         
048800*MIXED-CASE                                                               
048900   03  CL7INCH-RUB-MIXED-CASE.                                            
049000     05  FILLER      PIC X(25) VALUE '!F T N  100 650 L 1 1 3 '.          
049100     05  FILLER      PIC X(16) VALUE '"MIXED CASE NO"Å'.                  
049200*                                                                         
049300   03  CL7INCH-RUB-FREIGHT-CODE.                                          
049400     05  FILLER      PIC X(25) VALUE '!F T N  100 2150 R 1 1 3 '.         
049500     05  FILLER      PIC X(15) VALUE '"FREIGHT CODE"Å'.                   
049600                                                                          
049700*RUB-DISTRICT                                                             
049800   03  CL7INCH-RUB-DISTRICT.                                              
049900     05  FILLER      PIC X(25) VALUE '!F T N  400 250  L 1 1 3 '.         
050000     05  FILLER      PIC X(11) VALUE '"DISTRICT"Å'.                       
050100*                                                                         
050200*RUB-ANT-KOLLI                                                            
050300   03  CL7INCH-RUB-CASES.                                                 
050400     05  FILLER      PIC X(25) VALUE '!F T N  400 1100 L 1 1 3 '.         
050500     05  FILLER      PIC X(8)  VALUE '"CASES"Å'.                          
050600*                                                                         
050700*RUB-WEIGHT KG                                                            
050800   03  CL7INCH-RUB-WEIGHT.                                                
050900     05  FILLER      PIC X(25) VALUE '!F T N  400 2150 R 1 1 3 '.         
051000     05  FILLER      PIC X(12)  VALUE '"WEIGHT KG"Å'.                     
051100*                                              Y    X                     
051200   03  CL7INCH-RUB-IDDC.                                                  
051300     05  FILLER      PIC X(25) VALUE '!F T N  700  250 L 1 1 3 '.         
051400     05  FILLER      PIC X(5)  VALUE '"DC"Å'.                             
051500*                                                                         
051600   03  CL7INCH-RUB-4-2-DC-WH-ADDRESS.                                     
051700     05  FILLER      PIC X(25) VALUE '!F T N 1080 1100 L 1 1 3 '.         
051800     05  FILLER      PIC X(16) VALUE '"DC WH ADDRESS"Å'.                  
051900                                                                          
052000*IDTRPTNR                                                                 
052100   03  CL7INCH-DATA-IDTRPTNR.                                             
052200     05  FILLER      PIC X(25) VALUE '!F T N  290  250 L 3 3 6 '.         
052300     05  FILLER      PIC X(1)  VALUE '"'.                                 
052400     05  CL7INCH-IDTRPTNR    PIC Z(3) VALUE ZERO.                         
052500     05  FILLER      PIC X(2)  VALUE '"Å'.                                
052600                                                                          
052700*IDKOLLI-SAMP                                                             
052800   03  CL7INCH-DATA-IDKOLLI-SAMP.                                         
052900     05  FILLER      PIC X(25) VALUE '!F T N  290  650 L 3 3 6 '.         
053000     05  FILLER      PIC X(1)  VALUE '"'.                                 
053100     05  CL7INCH-IDKOLLI-SAMP  PIC Z(5) VALUE ZERO.                       
053200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
053300                                                                          
053400*KDFRAKT                                                                  
053500   03  CL7INCH-DATA-KDFRAKT.                                              
053600     05  FILLER      PIC X(25) VALUE '!F T N  290 2150 R 3 3 6 '.         
053700     05  FILLER      PIC X(1)  VALUE '"'.                                 
053800     05  CL7INCH-KDFRAKT-1X       PIC XX VALUE SPACE.                     
053900     05  CL7INCH-KDFRAKT-1        REDEFINES                               
054000         CL7INCH-KDFRAKT-1X       PIC ZZ.                                 
054100     05  CL7INCH-SLASH-1          PIC X(1)  VALUE ' '.                    
054200     05  CL7INCH-KDFRAKT-2X       PIC XX VALUE SPACE.                     
054300     05  CL7INCH-KDFRAKT-2        REDEFINES                               
054400         CL7INCH-KDFRAKT-2X       PIC ZZ.                                 
054500     05  CL7INCH-SLASH-2          PIC X(1)  VALUE ' '.                    
054600     05  CL7INCH-KDFRAKT-3X       PIC ZZ VALUE SPACE.                     
054700     05  CL7INCH-KDFRAKT-3        REDEFINES                               
054800         CL7INCH-KDFRAKT-3X       PIC ZZ.                                 
054900     05  FILLER      PIC X(2)  VALUE '"Å'.                                
055000                                                                          
055100*IDDISTR-SE                                                               
055200   03  CL7INCH-DATA-IDDISTR.                                              
055300     05  FILLER      PIC X(25) VALUE '!F T N  590  250 L 3 3 6 '.         
055400     05  FILLER      PIC X(1)  VALUE '"'.                                 
055500     05  CL7INCH-IDDISTR PIC Z(4) VALUE ZERO.                             
055600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
055700*                                                                         
055800*KVKOLLI ANTAL                                                            
055900   03  CL7INCH-DATA-KVKOLLI.                                              
056000     05  FILLER      PIC X(25) VALUE '!F T N  590  700 L 3 3 6 '.         
056100     05  FILLER      PIC X(1)  VALUE '"'.                                 
056200     05  CL7INCH-KVKOLLI  PIC Z(5) VALUE ZERO.                            
056300     05  FILLER      PIC X(2)  VALUE '"Å'.                                
056400*                                                                         
056500   03  CL7INCH-DATA-WEIGHT.                                               
056600     05  FILLER      PIC X(25) VALUE '!F T N  590 2150 R 3 3 6 '.         
056700     05  FILLER      PIC X(1)  VALUE '"'.                                 
056800     05  CL7INCH-VKORDBTO PIC Z(5) VALUE ZERO.                            
056900     05  FILLER      PIC X(2)  VALUE '"Å'.                                
057000                                                                          
057100   03  CL7INCH-DATA-KILO-HEKTO.                                           
057200     05  FILLER      PIC X(25) VALUE '!F T N  590 2150 R 3 3 6 '.         
057300     05  FILLER      PIC X(1)  VALUE '"'.                                 
057400     05  CL7INCH-KILO     PIC Z(4)9  VALUE ZERO.                          
057500     05  CL7INCH-PUNKT    PIC X      VALUE '.'.                           
057600     05  CL7INCH-HEKTO    PIC 9      VALUE ZERO.                          
057700     05  FILLER      PIC X(2)  VALUE '"Å'.                                
057800                                                                          
057900*IDDC                                                                     
058000   03  CL7INCH-DATA-IDDC.                                                 
058100     05  FILLER      PIC X(25) VALUE '!F T N  890  250 L 3 3 6 '.         
058200     05  FILLER      PIC X(1)  VALUE '"'.                                 
058300     05  CL7INCH-IDDC PIC X(2) VALUE SPACE.                               
058400     05  FILLER      PIC X(2)  VALUE '"Å'.                                
058500*                                                                         
058600                                                                          
058700   03  CL7INCH-DATA-ADFLGEO.                                              
058800     05  FILLER      PIC X(25) VALUE '!F T N 1330 550  L 3 3 6 '.         
058900     05  FILLER      PIC X(1)  VALUE '"'.                                 
059000     05  CL7INCH-ADFLGEO PIC X(3) VALUE ZERO.                             
059100     05  FILLER      PIC X(2)  VALUE '"Å'.                                
059200*POSTEN                                        Y   X                      
059300   03  CL7INCH-ADFLGEO-POSTEN.                                            
059400     05  FILLER      PIC X(25) VALUE '!F T N 1330 900  L 3 3 6 '.         
059500     05  FILLER      PIC X(1)  VALUE '"'.                                 
059600     05  CL7INCH-ADFLGEO-POST    PIC X(3) VALUE ZERO.                     
059700     05  FILLER      PIC X(2)  VALUE '"Å'.                                
059800                                                                          
059900   03  CL7INCH-DATA-ADFLOMR.                                              
060000     05  FILLER      PIC X(25) VALUE '!F T N 1330 1050 L 3 3 6 '.         
060100     05  FILLER      PIC X(1)  VALUE '"'.                                 
060200     05  CL7INCH-ADFLOMR PIC Z(3) VALUE ZERO.                             
060300     05  FILLER      PIC X(2)  VALUE '"Å'.                                
060400*POSTEN                                                                   
060500   03  CL7INCH-ADFLOMR-POSTEN.                                            
060600     05  FILLER      PIC X(25) VALUE '!F T N 1330 1400 L 3 3 6 '.         
060700     05  FILLER      PIC X(1)  VALUE '"'.                                 
060800     05  CL7INCH-ADFLOMR-POST  PIC Z(2) VALUE ZERO.                       
060900     05  FILLER      PIC X(2)  VALUE '"Å'.                                
061000                                                                          
061100   03  CL7INCH-DATA-ADRUTNIV.                                             
061200     05  FILLER      PIC X(25) VALUE '!F T N 1330 1550 L 3 3 6 '.         
061300     05  FILLER      PIC X(1)  VALUE '"'.                                 
061400     05  CL7INCH-ADRUTNIV PIC Z(3) VALUE ZERO.                            
061500     05  FILLER      PIC X(2)  VALUE '"Å'.                                
061600*POSTEN                                        Y    X                     
061700   03  CL7INCH-ADRUTNIV-POSTEN.                                           
061800     05  FILLER      PIC X(25) VALUE '!F T N 1330 1900 L 3 3 6 '.         
061900     05  FILLER      PIC X(1)  VALUE '"'.                                 
062000     05  CL7INCH-ADRUTNIV-POST PIC Z(2) VALUE ZERO.                       
062100     05  FILLER      PIC X(2)  VALUE '"Å'.                                
062200*                                                                         
062300   03  CL7INCH-BARCODE.                                                   
062400     05  FILLER    PIC X(30) VALUE '!F C N 1580 550 L 140 3 12 '.         
062500     05  FILLER                  PIC X(1)  VALUE '"'.                     
062600     05  CL7INCH-DISTR           PIC 9(4).                                
062700     05  CL7INCH-KUNDNR          PIC 9(6).                                
062800     05  CL7INCH-ORDNR           PIC 9(7).                                
062900     05  CL7INCH-KOLLI-SAMP      PIC 9(5).                                
063000     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
063100                                                                          
063200   03  CL7INCH-BARCODE-POST.                                              
063300     05  FILLER    PIC X(30) VALUE '!F C N 1580 550 L 140 3 12 '.         
063400     05  FILLER                  PIC X(1)  VALUE '"'.                     
063500     05  CL7INCH-DISTR-POST      PIC 9(4).                                
063600     05  CL7INCH-KUNDNR-POST     PIC 9(6).                                
063700     05  CL7INCH-ORDNR-POST      PIC 9(7).                                
063800     05  CL7INCH-KOLLI-POST      PIC 9(5).                                
063900     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
064000                                                                          
064100*POSTEN                                                                   
064200   03  CL7INCH-TXT-BLW-BARCODE-POST.                                      
064300     05  FILLER      PIC X(30) VALUE '!F T N 1640 550 L 2 1 3 '.          
064400     05  FILLER                  PIC X(1)  VALUE '"'.                     
064500     05  CL7INCH-DIST-POST       PIC 9(4).                                
064600     05  CL7INCH-KUNDN-POST      PIC 9(6).                                
064700     05  CL7INCH-ORDN-POST       PIC 9(7).                                
064800     05  CL7INCH-KOLI-POST       PIC 9(5).                                
064900     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
065000                                                                          
065100   03  CL7INCH-TXT-BLW-BARCODE.                                           
065200     05  FILLER      PIC X(30) VALUE '!F T N 1640 550 L 2 1 3 '.          
065300     05  FILLER                  PIC X(1)  VALUE '"'.                     
065400     05  CL7INCH-DIST            PIC 9(4).                                
065500     05  CL7INCH-KUNDN           PIC 9(6).                                
065600     05  CL7INCH-ORDN            PIC 9(7).                                
065700     05  CL7INCH-KOLI-SAMP       PIC 9(5).                                
065800     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
065900                                                                          
066000   03  CL7INCH-LITEN-TEXT-SHIPPER-CDC.                                    
066100     05  FILLER      PIC X(30) VALUE '!F T N 1430 850 L 2 1 3 '.          
066200     05  FILLER                  PIC X(1)  VALUE '"'.                     
066300     05  FILLER      PIC X(20) VALUE 'SHIPPER: VOLVO CARS '.              
066400     05  FILLER      PIC X(18) VALUE 'CUSTOMER SERVICE, '.                
066500     05  FILLER      PIC X(21) VALUE 'SE-405 31 GOTHENBURG,'.             
066600     05  FILLER      PIC X(7)  VALUE ' SWEDEN'.                           
066700     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
066800                                                                          
066900   03  CL7INCH-TEXT-SHIPPER-CDC.                                          
067000     05  FILLER      PIC X(30) VALUE '!F T N 1690 350 L 2 1 3 '.          
067100     05  FILLER                  PIC X(1)  VALUE '"'.                     
067200     05  FILLER      PIC X(20) VALUE 'SHIPPER: VOLVO CARS '.              
067300     05  FILLER      PIC X(18) VALUE 'CUSTOMER SERVICE, '.                
067400     05  FILLER      PIC X(21) VALUE 'SE-405 31 GOTHENBURG,'.             
067500     05  FILLER      PIC X(7)  VALUE ' SWEDEN'.                           
067600     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
067700                                                                          
067800   03  CL7INCH-TEXT-SHIPPER-CDC-POST.                                     
067900     05  FILLER      PIC X(30) VALUE '!F T N 1470 800 L 2 1 3 '.          
068000     05  FILLER                  PIC X(1)  VALUE '"'.                     
068100     05  FILLER      PIC X(20) VALUE 'SHIPPER: VOLVO CARS '.              
068200     05  FILLER      PIC X(18) VALUE 'CUSTOMER SERVICE, '.                
068300     05  FILLER      PIC X(21) VALUE 'SE-405 31 GOTHENBURG,'.             
068400     05  FILLER      PIC X(7)  VALUE ' SWEDEN'.                           
068500     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
068600*POSTEN                                                                   
068700   03  CL7INCH-TEXT-PRODUKT.                                              
068800     05  FILLER      PIC X(25) VALUE '!F T N 1530 390  R 2 1 3 '.         
068900     05  FILLER      PIC X(10)  VALUE '"PRODUKT"Å'.                       
069000                                                                          
069100   03  CL7INCH-TEXT-PRODUKT-1090.                                         
069200     05  FILLER      PIC X(25) VALUE '!F T N 1030 390  R 2 1 3 '.         
069300     05  FILLER      PIC X(10)  VALUE '"PRODUKT"Å'.                       
069400                                                                          
069500   03  CL7INCH-8700-TEXT-PRODUKT.                                         
069600     05  FILLER      PIC X(25) VALUE '!F T N  890 400  R 2 1 3 '.         
069700     05  FILLER      PIC X(10)  VALUE '"PRODUKT"Å'.                       
069800                                                                          
069900   03  CL7INCH-TEXT-HIT.                                                  
070000     05  FILLER      PIC X(25) VALUE '!F T N 1630 350  R 2 1 3 '.         
070100     05  FILLER      PIC X(6)  VALUE '"HIT"Å'.                            
070200                                                                          
070300   03  CL7INCH-TEXT-HIT-1090.                                             
070400     05  FILLER      PIC X(25) VALUE '!F T N 1130 350  R 2 1 3 '.         
070500     05  FILLER      PIC X(6)  VALUE '"HIT"Å'.                            
070600                                                                          
070700   03  CL7INCH-8700-TEXT-HIT.                                             
070800     05  FILLER      PIC X(25) VALUE '!F T N  990 380  R 2 1 3 '.         
070900     05  FILLER      PIC X(6)  VALUE '"HIT"Å'.                            
071000                                                                          
071100   03  CL7INCH-TEXT-PAK.                                                  
071200     05  FILLER      PIC X(25) VALUE '!F T N 1630 350  R 2 1 3 '.         
071300     05  FILLER      PIC X(6)  VALUE '"PAK"Å'.                            
071400                                                                          
071500   03  CL7INCH-TEXT-PAK-1090.                                             
071600     05  FILLER      PIC X(25) VALUE '!F T N 1130 350  R 2 1 3 '.         
071700     05  FILLER      PIC X(6)  VALUE '"PAK"Å'.                            
071800                                                                          
071900   03  CL7INCH-REFILL-TEXT-PAK.                                           
072000     05  FILLER      PIC X(25) VALUE '!F T N  750 750  R 2 1 3 '.         
072100     05  FILLER      PIC X(6)  VALUE '"PAK"Å'.                            
072200                                                                          
072300   03  CL7INCH-8700-TEXT-PAK.                                             
072400     05  FILLER      PIC X(25) VALUE '!F T N  990 380  R 2 1 3 '.         
072500     05  FILLER      PIC X(6)  VALUE '"PAK"Å'.                            
072600                                                                          
072700   03  CL7INCH-TEXT-48.                                                   
072800     05  FILLER      PIC X(25) VALUE '!F T N 1580 340  R 2 1 3 '.         
072900     05  FILLER      PIC X(5)  VALUE '"48"Å'.                             
073000                                                                          
073100   03  CL7INCH-TEXT-48-EJ-SE.                                             
073200     05  FILLER      PIC X(25) VALUE '!F T N 1080 340  R 2 1 3 '.         
073300     05  FILLER      PIC X(5)  VALUE '"48"Å'.                             
073400                                                                          
073500   03  CL7INCH-TEXT-49-1090.                                              
073600     05  FILLER      PIC X(25) VALUE '!F T N 1080 340  R 2 1 3 '.         
073700     05  FILLER      PIC X(5)  VALUE '"49"Å'.                             
073800                                                                          
073900   03  CL7INCH-REFILL-TEXT-48.                                            
074000     05  FILLER      PIC X(25) VALUE '!F T N  700 750  R 2 1 3 '.         
074100     05  FILLER      PIC X(5)  VALUE '"48"Å'.                             
074200                                                                          
074300   03  CL7INCH-8700-TEXT-48.                                              
074400     05  FILLER      PIC X(25) VALUE '!F T N  940 380  R 2 1 3 '.         
074500     05  FILLER      PIC X(5)  VALUE '"48"Å'.                             
074600                                                                          
074700   03  CL7INCH-TEXT-69.                                                   
074800     05  FILLER      PIC X(25) VALUE '!F T N 1580 340  R 2 1 3 '.         
074900     05  FILLER      PIC X(5)  VALUE '"69"Å'.                             
075000                                                                          
075100   03  CL7INCH-TEXT-54-1090.                                              
075200     05  FILLER      PIC X(25) VALUE '!F T N 1180 340  R 2 1 3 '.         
075300     05  FILLER      PIC X(5)  VALUE '"54"Å'.                             
075400                                                                          
075500   03  CL7INCH-REFILL-TEXT-54.                                            
075600     05  FILLER      PIC X(25) VALUE '!F T N  700 750  R 2 1 3 '.         
075700     05  FILLER      PIC X(5)  VALUE '"54"Å'.                             
075800                                                                          
075900   03  CL7INCH-8700-TEXT-54.                                              
076000     05  FILLER      PIC X(25) VALUE '!F T N  940 380  R 2 1 3 '.         
076100     05  FILLER      PIC X(5)  VALUE '"54"Å'.                             
076200*POSTEN                                                                   
076300   03  CL7INCH-SORTERINGSKOD-1090.                                        
076400     05  FILLER      PIC X(25) VALUE '!F T N 1100 600  L 4 3 3 '.         
076500     05  FILLER      PIC X(1)  VALUE '"'.                                 
076600     05  CL7INCH-ADFLGEO-1090-FI   PIC X(2)  VALUE 'FI'.                  
076700     05  CL7INCH-ADFLGEO-SORT-1090 PIC X(3)  VALUE ZERO.                  
076800     05  FILLER      PIC X(2)  VALUE '"Å'.                                
076900                                                                          
077000   03  CL7INCH-SORTERINGSKOD-DK.                                          
077100     05  FILLER      PIC X(25) VALUE '!F T N 1100 600  L 4 3 3 '.         
077200     05  FILLER      PIC X(1)  VALUE '"'.                                 
077300     05  CL7INCH-ADFLGEO-DK        PIC X(2)  VALUE 'DK'.                  
077400     05  CL7INCH-ADFLGEO-SORT-DK   PIC X(3)  VALUE ZERO.                  
077500     05  FILLER      PIC X(2)  VALUE '"Å'.                                
077600                                                                          
077700   03  CL7INCH-SORTERINGSKOD-NO.                                          
077800     05  FILLER      PIC X(25) VALUE '!F T N 1100 600  L 4 3 3 '.         
077900     05  FILLER      PIC X(1)  VALUE '"'.                                 
078000     05  CL7INCH-ADFLGEO-NO        PIC X(2)  VALUE 'NO'.                  
078100     05  CL7INCH-ADFLGEO-SORT-NO   PIC X(3)  VALUE ZERO.                  
078200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
078300                                                                          
078400   03  CL7INCH-SORTERINGSKOD.                                             
078500     05  FILLER      PIC X(25) VALUE '!F T N 1600 600  L 4 3 3 '.         
078600     05  FILLER      PIC X(1)  VALUE '"'.                                 
078700     05  CL7INCH-ADFLGEO-SE    PIC X(2) VALUE 'SE'.                       
078800     05  CL7INCH-ADFLGEO-SORT  PIC X(3) VALUE ZERO.                       
078900     05  FILLER      PIC X(2)  VALUE '"Å'.                                
079000                                                                          
079100*POSTEN                                                                   
079200   03  CL7INCH-BARCODE-POSTEN.                                            
079300     05  FILLER    PIC X(30) VALUE '!F C N 1590 1050 L 100 3 41'.         
079400     05  FILLER                  PIC X(1)  VALUE '"'.                     
079500     05  CL7INCH-POSTEN1         PIC X(13).                               
079600     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
079700                                                                          
079800   03  CL7INCH-BARCODE-POSTEN-1090.                                       
079900     05  FILLER    PIC X(30) VALUE '!F C N 1090 1050 L 100 3 41'.         
080000     05  FILLER                  PIC X(1)  VALUE '"'.                     
080100     05  CL7INCH-POSTEN1-1090    PIC X(13).                               
080200     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
080300                                                                          
080400                                                                          
080500   03  CL7INCH-TEXT-POSTEN.                                               
080600     05  FILLER    PIC X(30) VALUE '!F T N 1640 1050 L 2 1 3'.            
080700     05  FILLER                  PIC X(1)  VALUE '"'.                     
080800     05  CL7INCH-POSTEN1-TEXT    PIC X(13).                               
080900     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
081000                                                                          
081100   03  CL7INCH-TEXT-POSTEN-1090.                                          
081200     05  FILLER    PIC X(30) VALUE '!F T N 1140 1050 L 2 1 3'.            
081300     05  FILLER                  PIC X(1)  VALUE '"'.                     
081400     05  CL7INCH-POSTEN1-TEXT-1090 PIC X(13).                             
081500     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
081600                                                                          
081700   03  CL7INCH-8700-TEXT-POSTEN.                                          
081800     05  FILLER    PIC X(30) VALUE '!F T N  990  800 L 2 1 3'.            
081900     05  FILLER                    PIC X(1)  VALUE '"'.                   
082000     05  CL7INCH-8700-POSTEN1-TEXT PIC X(13).                             
082100     05  FILLER                    PIC X(2)  VALUE '"Å'.                  
082200                                                                          
082300     EJECT                                                                
082400*****************************************************************         
082500*                                                                         
082600                                                                          
082700 01  FELKODER.                                                            
082800     03  FEL-IDKOLLI-SAMP        PIC 9(5)    VALUE ZERO.                  
082900     03  FEL-IDDISTR             PIC 9(4)    VALUE ZERO.                  
083000     03  FEL-KDPRTVAL            PIC X(2)    VALUE SPACE.                 
083100     03  FEL-IDTRANS             PIC X(4)    VALUE SPACE.                 
083200     03  FEL-IDDC                PIC X(2)    VALUE SPACE.                 
083300                                                                          
083400 01  RETURKODER.                                                          
083500     03  RKOD-ABEND-MED-DUMP     PIC S9(4) VALUE +1000 COMP SYNC.         
084200     EJECT                                                                
084300*01  FILLER -COPY W475WKNT                                                
084400     EJECT                                                                
084500*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
084600 01  GENERELLA-SUBPROGRAM.                                                
084700     03 W006PRS1                 PIC X(8)    VALUE 'W006PRS1'.            
084800     03 W006PRT                  PIC X(8)    VALUE 'W006PRT '.            
084900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
085000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
085100     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
085200     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
085300*                                                                         
085400     EJECT                                                                
085500 01  FILLER                      PIC X(16)  VALUE 'W006PRT  '.            
085600*   -COPY W006PRT                                                         
085700*                                                                         
085800 01  FILLER                      PIC X(16)   VALUE  'WORKAREA'.           
085900*01  -COPY WORKAREA                                                       
086000     EJECT                                                                
086100*01  -COPY W006PRAR                                                       
086200     EJECT                                                                
086300*                                                                         
086400 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
086500                                                                          
086600*01  MID -COPY W4I34P01                                                   
086700     EJECT                                                                
086800 01  FILLER                      PIC X(16)  VALUE 'MSG-AREA'.             
086900                                                                          
087000*01  -COPY WMSGAREA                                                       
087100     EJECT                                                                
087200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
087300*                                                                         
087400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
087500                                                                          
087600 01  NYCKLAR-TILL-DLI.                                                    
087700                                                                          
087800     03  W-WDE7ASEQ-X.                                                    
087900         05 W-IDDC               PIC X(2)    VALUE SPACE.                 
088000         05 W-IDKOLLI-SAMP       PIC S9(5)   VALUE ZERO  COMP-3.          
088100                                                                          
088200   03  W-KDSEGKEY-X.                                                      
088300     05  W-KDSEGKEY          PIC X(1)     VALUE '1'.                      
088400                                                                          
088500     EJECT                                                                
088600*    --- STATUS-KOD FRÅN IMS                                              
088700 01  STATUS-WS                   PIC XX.                                  
088800     88  SEGMENT-FINNS                       VALUE '  '.                  
088900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
089000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
089100     SKIP2                                                                
089200 01  GODK-STATUSKODER.                                                    
089300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
089400     SKIP3                                                                
089500 01  SSA1                        PIC X(128).                              
089600 01  SSA2                        PIC X(128).                              
089700 01  SSA3                        PIC X(64).                               
089800     EJECT                                                                
089900                                                                          
090000*    --- IMS FUNKTIONSKODER                                               
090100*01  -COPY W0003                                                          
090200     EJECT                                                                
090300*    ---  DLI INPUT-OUTPUT AREA                                           
090400 01  DLI-IO-WDE711.                                                       
090500*    03  -COPY WDE711                                                     
090600     EJECT                                                                
090700 LINKAGE SECTION.                                                         
090800*01  -COPY W0009   -PRE MSG-                                              
090900                                                                          
091000*01  -COPY W0009   -PRE ALT-                                              
091100     EJECT                                                                
091200*01  -COPY W0008   -PRE WDE7A-                                            
091300     05  FILLER                  PIC X.                                   
091400     EJECT                                                                
091500 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB                                
091600                           WDE7A-PCB.                                     
091700 MAIN SECTION.                                                            
091800     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB                                
091900                           WDE7A-PCB.                                     
092000                                                                          
092100     PERFORM IMS-GET-MSG                                                  
092200     IF SEGMENT-FINNS                                                     
092300       IF MSG-KDTRTYP = 'X'                                               
092400         PERFORM A-INIT                                                   
092500                                                                          
092600         PERFORM IMS-GU-WDE711-ASEQ                                       
092700                                                                          
092800         PERFORM B-SKRIV-ETIKETTER                                        
092900                                                                          
093000       END-IF                                                             
093100     END-IF                                                               
093200                                                                          
093300     MOVE ZERO TO RETURN-CODE                                             
093400     GOBACK                                                               
093500     .                                                                    
093600     EJECT                                                                
093700 A-INIT SECTION.                                                          
093800     MOVE 'A-INIT         '     TO CURRENT-SECTION                        
093900                                                                          
094000     MOVE JA  TO ALLT-SW                                                  
094100     MOVE +1                              TO IX                           
094200                                                                          
094300     MOVE MSG-INDATA-MINUS-1-TRANSKOD     TO MID-W4I34P01                 
094400                                                                          
094500     IF MID-KDPRTVAL = SPACE                                              
094600       MOVE MID-KDPRTVAL                  TO FEL-KDPRTVAL                 
094700       MOVE NEJ TO ALLT-SW                                                
094800     END-IF                                                               
094900                                                                          
095000     IF MID-IDDISTR NUMERIC                                               
095100       CONTINUE                                                           
095200     ELSE                                                                 
095300       MOVE MID-IDDISTR                   TO FEL-IDDISTR                  
095400       MOVE NEJ TO ALLT-SW                                                
095500     END-IF                                                               
095600                                                                          
095700     IF MID-IDKOLLI-SAMP NUMERIC                                          
095800       MOVE MID-IDKOLLI-SAMP              TO W-IDKOLLI-SAMP               
095900     ELSE                                                                 
096000       MOVE MID-IDKOLLI-SAMP              TO FEL-IDKOLLI-SAMP             
096100       MOVE NEJ TO ALLT-SW                                                
096200     END-IF                                                               
096300                                                                          
096400     IF MID-IDDC = SPACE                                                  
096500       MOVE MID-IDDC                      TO FEL-IDDC                     
096600       MOVE NEJ TO ALLT-SW                                                
096700     ELSE                                                                 
096800       MOVE MID-IDDC                      TO W-IDDC                       
096900     END-IF                                                               
097000                                                                          
097100     IF MSG-IDTRANS-1 NOT = '4345'                                        
097200        MOVE MSG-IDTRANS-1                TO FEL-IDTRANS                  
097300        MOVE NEJ TO ALLT-SW                                               
097400     END-IF                                                               
097500                                                                          
097600     IF NOT ALLT-OK                                                       
097700        MOVE '*** FEL PÅ INDATAT KOLLA PÅ FEL-  **'                       
097800                               TO FELTEXT                                 
097900        CALL ABEND USING RKOD-ABEND-MED-DUMP                              
098000     END-IF                                                               
098100                                                                          
098200     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-DAGENS-DATUM                  
098300     .                                                                    
098400     EJECT                                                                
098500 B-SKRIV-ETIKETTER SECTION.                                               
098600     MOVE 'B-SKRIV-ETIKETT'     TO CURRENT-SECTION                        
098700                                                                          
098800     MOVE '4KF11'                      TO LISTVAL (1:5)                   
098900                                                                          
099000     MOVE MID-KDPRTVAL                 TO LISTVAL (6:2)                   
099100     MOVE MID-KDPRTVAL                 TO WS-KDPRTVAL                     
099200                                                                          
099300     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-OPEN  LISTVAL                  
099400                         ALT-PCB WS-PRT-DUMMY WS-PRT-DUMMY                
099500                                                                          
099600     PERFORM BA-MOVE-PRINT-DATA                                           
099700                                                                          
099800     IF SKRIV-PA-NOVA-SKRIVARE                                            
099900       PERFORM BB-NOVA-CASE-LABEL                                         
100000     ELSE                                                                 
100100       PERFORM BC-MARKPOINT-A5-CASE-LABEL                                 
100200     END-IF                                                               
100300                                                                          
100400     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-CLOSE  LISTVAL                 
100500                         ALT-PCB WS-PRT-DUMMY WS-PRT-DUMMY                
100600                                                                          
100700     .                                                                    
100800     EJECT                                                                
100900 BA-MOVE-PRINT-DATA    SECTION.                                           
101000     MOVE 'BA-MOVE-PRINT- '     TO CURRENT-SECTION                        
101100                                                                          
101200     MOVE SKLI-IDTRPTNR                TO NOVA-LBL-IDTRPTNR               
101300                                          CL7INCH-IDTRPTNR                
101400                                                                          
101500     MOVE SKLI-IDKOLLI-SAMP            TO NOVA-LBL-IDKOLLI-SAMP           
101600                                          CL7INCH-IDKOLLI-SAMP            
101700                                                                          
101800     MOVE SKLI-IDDC                    TO NOVA-LBL-IDDC                   
101900                                          CL7INCH-IDDC                    
102000                                                                          
102100     MOVE SKLI-KVKOLLI-SAMP            TO NOVA-LBL-KVKOLLI                
102200                                          CL7INCH-KVKOLLI                 
102300                                                                          
102400     MOVE SKLI-VKKOLLIB-SAMP           TO WS-VKORDBTO                     
102500     IF  WS-KILO < 10                                                     
102600     AND (WS-KILO > ZERO OR WS-HEKTO > ZERO)                              
102700       MOVE WS-KILO      TO NOVA-LBL-KILO                                 
102800                            CL7INCH-KILO                                  
102900       MOVE WS-HEKTO     TO NOVA-LBL-HEKTO                                
103000                            CL7INCH-HEKTO                                 
103100     ELSE                                                                 
103200       MOVE WS-VKORDBTO  TO NOVA-LBL-VKORDBTO                             
103300                            CL7INCH-VKORDBTO                              
103400     END-IF                                                               
103500                                                                          
103600                                                                          
103700     IF SKLI-KDFRAKT(1) > ZERO                                            
103800       MOVE SKLI-KDFRAKT(1)            TO NOVA-LBL-KDFRAKT-3              
103900                                          CL7INCH-KDFRAKT-3               
104000     END-IF                                                               
104100     IF SKLI-KDFRAKT(2) > ZERO                                            
104200       MOVE '/'                        TO NOVA-LBL-SLASH-2                
104300                                          CL7INCH-SLASH-2                 
104400       MOVE SKLI-KDFRAKT(2)            TO NOVA-LBL-KDFRAKT-2              
104500                                          CL7INCH-KDFRAKT-2               
104600     END-IF                                                               
104700     IF SKLI-KDFRAKT(3) > ZERO                                            
104800       MOVE '/'                        TO NOVA-LBL-SLASH-1                
104900                                          CL7INCH-SLASH-1                 
105000       MOVE SKLI-KDFRAKT(3)            TO NOVA-LBL-KDFRAKT-1              
105100                                          CL7INCH-KDFRAKT-1               
105200     END-IF                                                               
105300                                                                          
105400     MOVE MID-IDDISTR                  TO NOVA-LBL-IDDISTR                
105500                                          CL7INCH-IDDISTR                 
105600                                                                          
105700*BARCODEN SKALL INNEHÅLLA ZERO I DISTRIKT, KUND OCH ORDERNR               
105800*PÅ FLAGGAN FÖR SAMLINGSKOLLIN!                                           
105900     MOVE ZERO                         TO NOVA-LBL-DISTR                  
106000                                          NOVA-LBL-DIST                   
106100                                          NOVA-LBL-KUNDNR                 
106200                                          NOVA-LBL-KUNDN                  
106300                                          NOVA-LBL-ORDNR                  
106400                                          NOVA-LBL-ORDN                   
106500                                          CL7INCH-DISTR                   
106600                                          CL7INCH-DIST                    
106700                                          CL7INCH-KUNDNR                  
106800                                          CL7INCH-KUNDN                   
106900                                          CL7INCH-ORDNR                   
107000                                          CL7INCH-ORDN                    
107100                                                                          
107200     MOVE SKLI-IDKOLLI-SAMP            TO NOVA-LBL-KOLLI-SAMP             
107300                                          NOVA-LBL-KOLI-SAMP              
107400                                          CL7INCH-KOLLI-SAMP              
107500                                          CL7INCH-KOLI-SAMP               
107600                                                                          
107700*POSTNORD DATA IN HÄR (GÄLLER INTE ÄN SÅ LÄNGE!)                          
107800                                                                          
107900     MOVE SKLI-ADFLGEO                 TO NOVA-LBL-DATA-ADFLGEO           
108000                                          CL7INCH-ADFLGEO                 
108100     MOVE SKLI-ADFLOMR                 TO NOVA-LBL-DATA-ADFLOMR           
108200                                          CL7INCH-ADFLOMR                 
108300     MOVE SKLI-ADRUTNIV                TO NOVA-LBL-DATA-ADRUTNIV          
108400                                          CL7INCH-ADRUTNIV                
108500     .                                                                    
108600     EJECT                                                                
108700                                                                          
108800 BB-NOVA-CASE-LABEL             SECTION.                                  
108900     MOVE 'BB-NOVA-CASE-LA'     TO CURRENT-SECTION                        
109000                                                                          
109100                                                                          
109200     MOVE SPACE       TO NOVA-LBL-RAD                                     
109300     MOVE NOVA-LBL-STYR-01 TO NOVA-LBL-RAD                                
109400     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
109500                         ALT-PCB PRT-NYSIDA-RAD1 NOVA-LBL-RAD             
109600                                                                          
109700     MOVE NOVA-LBL-STYR-01 TO NOVA-LBL-RAD                                
109800     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
109900                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
110000                                                                          
110100     MOVE NOVA-LBL-STYR-42 TO NOVA-LBL-RAD                                
110200     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
110300                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
110400                                                                          
110500     MOVE NOVA-LBL-RUB-TRANSPORT-NR   TO NOVA-LBL-RAD                     
110600     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
110700                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
110800                                                                          
110900     MOVE NOVA-LBL-RUB-SAMKOLLI-NR     TO NOVA-LBL-RAD                    
111000     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
111100                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
111200                                                                          
111300     MOVE NOVA-LBL-RUB-FREIGHT-CODE  TO NOVA-LBL-RAD                      
111400     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
111500                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
111600                                                                          
111700     MOVE NOVA-LBL-RUB-DISTRICT      TO NOVA-LBL-RAD                      
111800     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
111900                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
112000                                                                          
112100     MOVE NOVA-LBL-RUB-ANT-KOLLI     TO NOVA-LBL-RAD                      
112200     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
112300                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
112400                                                                          
112500     MOVE NOVA-LBL-RUB-WEIGHT       TO NOVA-LBL-RAD                       
112600     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
112700                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
112800                                                                          
112900     MOVE NOVA-LBL-RUB-IDDC         TO NOVA-LBL-RAD                       
113000     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
113100                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
113200                                                                          
113300     MOVE NOVA-LBL-DATA-IDTRPTNR       TO NOVA-LBL-RAD                    
113400     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
113500                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
113600                                                                          
113700     MOVE NOVA-LBL-DATA-IDKOLLI-SAMP     TO NOVA-LBL-RAD                  
113800     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
113900                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
114000*IDDC                                                                     
114100     MOVE NOVA-LBL-DATA-IDDC        TO NOVA-LBL-RAD                       
114200     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
114300                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
114400*KDFRAKT                                                                  
114500     MOVE NOVA-LBL-DATA-KDFRAKT     TO NOVA-LBL-RAD                       
114600     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
114700                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
114800                                                                          
114900     MOVE NOVA-LBL-DATA-IDDISTR        TO NOVA-LBL-RAD                    
115000     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
115100                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
115200                                                                          
115300     MOVE NOVA-LBL-DATA-KVKOLLI        TO NOVA-LBL-RAD                    
115400     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
115500                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
115600                                                                          
115700     IF    WS-KILO < 10                                                   
115800     AND (WS-KILO > ZERO OR WS-HEKTO > ZERO)                              
115900       MOVE NOVA-LBL-DATA-KILO-HEKTO TO NOVA-LBL-RAD                      
116000     ELSE                                                                 
116100       MOVE NOVA-LBL-DATA-WEIGHT TO NOVA-LBL-RAD                          
116200     END-IF                                                               
116300     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
116400                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
116500                                                                          
116600     MOVE NOVA-LBL-ADFLGEO              TO NOVA-LBL-RAD                   
116700     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
116800                     ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                     
116900                                                                          
117000     MOVE NOVA-LBL-ADFLOMR              TO NOVA-LBL-RAD                   
117100     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
117200                     ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                     
117300                                                                          
117400     MOVE NOVA-LBL-ADRUTNIV                TO  NOVA-LBL-RAD               
117500     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
117600                     ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                     
117700                                                                          
117800*BARCODE                                                                  
117900     MOVE NOVA-LBL-BARCODE           TO NOVA-LBL-RAD                      
118000     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
118100                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
118200                                                                          
118300*TXT-BLW-BARCODE                                                          
118400     MOVE NOVA-LBL-TEXT-BELOW-BARCODE         TO NOVA-LBL-RAD             
118500     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
118600                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
118700                                                                          
118800                                                                          
118900*EV SKALL *-MÄRKT DATA ANVÄNDAS PÅ SAMPKOLLIFLAGGAN                       
119000*POSTEN-A5-CL7                                                            
119100*    MOVE KOLLI-IDTRPTNR         TO TRP01-IDTRP                           
119200*    IF TRP01-TRP-MED-POSTEN                                              
119300*      MOVE NOVA-LBL-TEXT-PRODUKT TO NOVA-LBL-RAD                         
119400*      CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
119500*                          ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
119600*      IF WS-KDORDKL = '1'                                                
119700*48 HIT                                                                   
119800*        MOVE NOVA-LBL-TEXT-48     TO NOVA-LBL-RAD                        
119900*        CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
120000*                            ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD             
120100*        MOVE NOVA-LBL-TEXT-HIT    TO NOVA-LBL-RAD                        
120200*        CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
120300*                            ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD             
120400*      ELSE                                                               
120500*        IF (WS-KDORDKL = '3' OR '4')                                     
120600*69 PAK                                                                   
120700*          MOVE NOVA-LBL-TEXT-69     TO NOVA-LBL-RAD                      
120800*          CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL            
120900*                              ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD           
121000*          MOVE NOVA-LBL-TEXT-PAK    TO NOVA-LBL-RAD                      
121100*          CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL            
121200*                              ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD           
121300*        END-IF                                                           
121400*      END-IF                                                             
121500*                                                                         
121600*      MOVE NOVA-LBL-SORTERINGSKOD TO NOVA-LBL-RAD                        
121700*      CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
121800*                          ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
121900*                                                                         
122000*POSTENS KOLLINR                                                          
122100*      MOVE WS-IDKLIID   TO NOVA-LBL-POSTEN1                              
122200*                           NOVA-LBL-POSTEN1-TEXT                         
122300*                                                                         
122400*      MOVE NOVA-LBL-BARCODE-POSTEN TO NOVA-LBL-RAD                       
122500*      CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
122600*                          ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
122700*                                                                         
122800*      MOVE NOVA-LBL-TEXT-POSTEN TO NOVA-LBL-RAD                          
122900*      CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
123000*                          ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
123100*    END-IF                                                               
123200                                                                          
123300     MOVE NOVA-LBL-STYR-91 TO NOVA-LBL-RAD                                
123400     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
123500                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
123600     .                                                                    
123700     SKIP3                                                                
123800 BC-MARKPOINT-A5-CASE-LABEL     SECTION.                                  
123900     MOVE 'BC-MARKPOINT-A5'     TO CURRENT-SECTION                        
124000                                                                          
124100     MOVE SPACE       TO CL7INCH-RAD                                      
124200     MOVE CL7INCH-STYR-01 TO CL7INCH-RAD                                  
124300     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
124400                         ALT-PCB PRT-NYSIDA-RAD1 CL7INCH-RAD              
124500                                                                          
124600     MOVE CL7INCH-STYR-01 TO CL7INCH-RAD                                  
124700     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
124800                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
124900                                                                          
125000     MOVE CL7INCH-STYR-42 TO CL7INCH-RAD                                  
125100     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
125200                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
125300                                                                          
125400     MOVE CL7INCH-RUB-TRANSPORTNR TO CL7INCH-RAD                          
125500     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
125600                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
125700                                                                          
125800     MOVE CL7INCH-RUB-MIXED-CASE  TO CL7INCH-RAD                          
125900     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
126000                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
126100                                                                          
126200     MOVE CL7INCH-RUB-FREIGHT-CODE      TO CL7INCH-RAD                    
126300     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
126400                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
126500                                                                          
126600     MOVE CL7INCH-RUB-DISTRICT    TO CL7INCH-RAD                          
126700     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
126800                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
126900                                                                          
127000     MOVE CL7INCH-RUB-CASES       TO CL7INCH-RAD                          
127100     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
127200                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
127300                                                                          
127400     MOVE CL7INCH-RUB-WEIGHT        TO CL7INCH-RAD                        
127500     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
127600                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
127700                                                                          
127800     MOVE  CL7INCH-RUB-IDDC         TO CL7INCH-RAD                        
127900     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
128000                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
128100                                                                          
128200*POSTEN                                                                   
128300                                                                          
128400*IDTRPTNR                                                                 
128500     MOVE CL7INCH-DATA-IDTRPTNR         TO CL7INCH-RAD                    
128600     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
128700                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
128800                                                                          
128900     MOVE CL7INCH-DATA-IDKOLLI-SAMP TO CL7INCH-RAD                        
129000     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
129100                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
129200*IDDC                                                                     
129300     MOVE CL7INCH-DATA-IDDC             TO CL7INCH-RAD                    
129400     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
129500                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
129600*KDFRAKT                                                                  
129700     MOVE CL7INCH-DATA-KDFRAKT          TO CL7INCH-RAD                    
129800     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
129900                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
130000                                                                          
130100     MOVE CL7INCH-DATA-IDDISTR       TO CL7INCH-RAD                       
130200     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
130300                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
130400                                                                          
130500     MOVE CL7INCH-DATA-KVKOLLI      TO CL7INCH-RAD                        
130600     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
130700                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
130800                                                                          
130900     IF    WS-KILO < 10                                                   
131000     AND (WS-KILO > ZERO OR WS-HEKTO > ZERO)                              
131100       MOVE CL7INCH-DATA-KILO-HEKTO TO CL7INCH-RAD                        
131200     ELSE                                                                 
131300       MOVE CL7INCH-DATA-WEIGHT TO CL7INCH-RAD                            
131400     END-IF                                                               
131500     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
131600                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
131700                                                                          
131800     MOVE CL7INCH-DATA-ADFLGEO          TO CL7INCH-RAD                    
131900     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
132000                     ALT-PCB PRT-AFTER-1 CL7INCH-RAD                      
132100                                                                          
132200*                                                                         
132300     MOVE CL7INCH-DATA-ADFLOMR            TO CL7INCH-RAD                  
132400     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
132500                       ALT-PCB PRT-AFTER-1 CL7INCH-RAD                    
132600                                                                          
132700*                                                                         
132800     MOVE CL7INCH-DATA-ADRUTNIV            TO CL7INCH-RAD                 
132900     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
133000                     ALT-PCB PRT-AFTER-1 CL7INCH-RAD                      
133100                                                                          
133200*                                                                         
133300     MOVE CL7INCH-BARCODE TO CL7INCH-RAD                                  
133400     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
133500                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
133600                                                                          
133700*                                                                         
133800     MOVE CL7INCH-TXT-BLW-BARCODE    TO CL7INCH-RAD                       
133900     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
134000                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
134100                                                                          
134200*EV SKALL *-MÄRKT DATA ANVÄNDAS PÅ SAMPKOLLIFLAGGAN                       
134300*POSTEN                                                                   
134400*     MOVE CL7INCH-TEXT-SHIPPER-CDC-POST TO CL7INCH-RAD                   
134500*     MOVE CL7INCH-TEXT-SHIPPER-CDC TO CL7INCH-RAD                        
134600*     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                 
134700*                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                 
134800*                                                                         
134900*POSTEN-A5-CL7                                                            
135000*    MOVE KOLLI-IDTRPTNR         TO TRP01-IDTRP                           
135100*    IF TRP01-TRP-MED-POSTEN                                              
135200*      MOVE CL7INCH-TEXT-PRODUKT TO CL7INCH-RAD                           
135300*      CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
135400*                          ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
135500*      IF WS-KDORDKL = '1'                                                
135600*48 HIT                                                                   
135700*        MOVE CL7INCH-TEXT-48      TO CL7INCH-RAD                         
135800*        CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
135900*                            ALT-PCB PRT-AFTER-1 CL7INCH-RAD              
136000*        MOVE CL7INCH-TEXT-HIT     TO CL7INCH-RAD                         
136100*        CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
136200*                            ALT-PCB PRT-AFTER-1 CL7INCH-RAD              
136300*      ELSE                                                               
136400*        IF (WS-KDORDKL = '3' OR '4')                                     
136500*69 PAK                                                                   
136600*          MOVE CL7INCH-TEXT-69      TO CL7INCH-RAD                       
136700*          CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL            
136800*                              ALT-PCB PRT-AFTER-1 CL7INCH-RAD            
136900*          MOVE CL7INCH-TEXT-PAK     TO CL7INCH-RAD                       
137000*          CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL            
137100*                              ALT-PCB PRT-AFTER-1 CL7INCH-RAD            
137200*        END-IF                                                           
137300*      END-IF                                                             
137400*                                                                         
137500*      MOVE CL7INCH-BARCODE-POSTEN TO CL7INCH-RAD                         
137600*      CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
137700*                          ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
137800*                                                                         
137900**     MOVE CL7INCH-TEXT-POSTEN  TO CL7INCH-RAD                           
138000*      CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
138100*                          ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
138200*    END-IF                                                               
138300                                                                          
138400     MOVE CL7INCH-STYR-91 TO CL7INCH-RAD                                  
138500     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
138600                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
138700     .                                                                    
138800     SKIP3                                                                
138900                                                                          
139000* --- IMS SEKTIONER ---                                                   
139100                                                                          
139200 IMS-GET-MSG SECTION.                                                     
139300                                                                          
139400     MOVE '  QC' TO GODK-STATUSKODER                                      
139500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
139600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
139700     PERFORM IMS-STATUSKONTROLL                                           
139800     .                                                                    
139900 IMS-GU-WDE711-ASEQ  SECTION.                                             
140000                                                                          
140100     STRING 'WDE711  (WDE7ASEQ =' W-WDE7ASEQ-X ')'                        
140200          DELIMITED BY SIZE INTO SSA1                                     
140300                                                                          
140400                                                                          
140500     MOVE '  ' TO GODK-STATUSKODER                                        
140600     CALL CBLTDLI USING GU WDE7A-PCB DLI-IO-WDE711 SSA1                   
140700     MOVE WDE7A-STATUS-CODE TO STATUS-WS                                  
140800                                                                          
140900                                                                          
141000     PERFORM IMS-STATUSKONTROLL                                           
141100     .                                                                    
141200     EJECT                                                                
141300 IMS-STATUSKONTROLL SECTION.                                              
141400                                                                          
141500     SET STATUS-IX TO 1                                                   
141600     SEARCH GODK-STATUS                                                   
141700       AT END                                                             
141800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
141900           DELIMITED BY SIZE INTO FELTEXT                                 
142000         CALL FELLOG                                                      
142100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
142200         CONTINUE                                                         
142300     END-SEARCH                                                           
142400     .                                                                    
