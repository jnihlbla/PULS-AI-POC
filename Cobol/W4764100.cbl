000001 ID DIVISION.                                                             
000002 PROGRAM-ID.     W4764100.                                                
000003 AUTHOR.         CAMELIA OLGRENER.                                        
000004 DATE-WRITTEN.   03/03/18.                                                
000005 DATE-COMPILED.                                                           
000006                                                                          
000007*    FUNKTION:                                                            
000008*        SKAPAR FIL FÖR EDI-ÖVERFÖRING AV KOLLIDATA TILL DIV              
000009*        TRANSPORTÖRER, GEMOM AMTRIX OCH POSTEN IT.                       
000010*                                                                         
000011*                                                                         
000012*    ABENDKODER:                                                          
000013*        U0016 -  . . . .                                                 
000014*        U1000 -  . . . .                                                 
000015*                                                                         
000016*    E-TR:  9902865 21/10-10 NYTT URVAL FÖR EDI FIL TILL TRANSP.          
000017*    E-TR: 10142738 21/10-10 NYTT URVAL FÖR EDI FIL TILL TRANSP.          
000018*    E-TR: 10140564 20/09-11 NYTT URVAL TILL TRANSP (DDGS).               
000019*    E-TR: 10200987 26/06-13 NYTT URVAL TILL TRANSP (GALLIKER).           
000020*    E-TR: 10244338 04/11-14 NYTT URVAL TILL TRANSP (LAGERMAX).           
000021*    JIRA: 2614       /12-18 NYTT URVAL TILL TRANSP (DANX).               
000022*    JIRA: 2615       /02-19 NYTT URVAL TILL TRANSP (SCHENKER).           
000023*    JIRA: 2976       /02-19 NYTT URVAL TILL TRANSP (DHL).                
000024*    PBI : 1514931  06/09-19 REINST. NIGHT-PLUS-SCHENKER.                 
000025*    PBI : 1540086  16/10-19 LDC 1B TILL DK I URVAL TILL TRP.DANX.        
000026*    PBI : 1574986  10/12-19 NYTT URVAL TILL TRANSP (BCUBE).              
000027*    PBI : 1586994  09/01-20 NYTT URVAL TILL TRANSP (BCUBE).              
000028*    PBI : 1540109  25/02-20 NYTT URVAL TILL TRANSP (NEOVIA).             
000029*    STORY 1639474    /06-20 SEND CORRECT RECEIVER ADDRESS.               
000030*    STORY 1735484  28/09-20 NYTT URVAL DANX.                             
000031*                            BORTTAG AV DHL.                              
000032*    STORY 1834228  10/11-20 NYTT URVAL DANX.                             
000033*    STORY 1839234  13/11-20 NYTT URVAL DANX.                             
000034*    STORY 1906385  21/12-20 NYTT URVAL LAGERMAX.                         
000035*    STORY 1918243  22/12-20 NYTT URVAL DANX.                             
000036*    STORY 2171444  21/07-21 NY TRANSP. GEODIS                            
000037*    STORY 2806690  22/05-24 NY TRANSP. DHL.                              
000038*    STORY 2984996  22/09-28 NY TRANSP. DHL NORGE.                        
000039*    STORY 3412713  23/05-23 NEW SELECTION BCUBE.                         
000040*    STORY 3421774  23/08-17 NEW SELECTION NEOVIA, TRUCK&WHEEL            
000041*    STORY 4285328  17/02-25 NEW SELECTION LAGERMAX                       
000042*    STORY 4299083  25/02-25 NEW SELECTION DANX                           
000043*    STORY 4579778  24/10-25 NEW SELECTION DANX                           
000044*    STORY 4590609  30/10-25 GEODIS WILL NO LONGER HANDLE GOODS           
000045*                            FROM CDC.REMOVED CODE RELATED TO             
000046*                            DIST87-GEODIS,DIST87-GEODIS-1,               
000047*                            DIST87-GEODIS-17.                            
000048*                                                                         
000049     SKIP3                                                                
000050 ENVIRONMENT DIVISION.                                                    
000051     SKIP2                                                                
000052 INPUT-OUTPUT SECTION.                                                    
000053                                                                          
000054 FILE-CONTROL.                                                            
000055     SKIP2                                                                
000056*          --- SKEPPNINGSPOSTER                                           
000057     SELECT W47640                     ASSIGN TO W47641D1.                
000058     SKIP2                                                                
000059*          --- FIL FÖR ÖVERSÄTTNING TILL EDI-FORMAT                       
000060     SELECT W47641                     ASSIGN TO W47641D2.                
000061     SKIP2                                                                
000062     EJECT                                                                
000063 DATA DIVISION.                                                           
000064     SKIP2                                                                
000065 FILE SECTION.                                                            
000066     SKIP3                                                                
000067 FD  W47640                                                               
000068     RECORDING       V                                                    
000069     BLOCK CONTAINS  0.                                                   
000070                                                                          
000071 01  INPOST.                                                              
000072*    03  -COPY W476E121 -L.                                               
000073*                                                                         
000074     EJECT                                                                
000075 FD  W47641                                                               
000076     RECORDING       V                                                    
000077     BLOCK CONTAINS  0.                                                   
000078                                                                          
000079 01  UT-001-POST.                                                         
000080*    03  -COPY WEDI001G -PRE UT-                                          
000081                                                                          
000082 01  UT-UNB-POST.                                                         
000083*    03  -COPY WEDIUNBG -PRE UT-                                          
000084                                                                          
000085 01  UT-UNH-POST.                                                         
000086*    03  -COPY WEDIUNHG -PRE UT-.                                         
000087                                                                          
000088 01  UT-BGM-POST.                                                         
000089*    03  -COPY WEDIBGMG -PRE UT-.                                         
000090                                                                          
000091 01  UT-DTM-POST.                                                         
000092*    03  -COPY WEDIDTMG -PRE UT-.                                         
000093                                                                          
000094 01  UT-MOA-POST.                                                         
000095*    03  -COPY WEDIMOAG -PRE UT-.                                         
000096                                                                          
000097 01  UT-FTX-POST.                                                         
000098*    03  -COPY WEDIFTXG -PRE UT-.                                         
000099                                                                          
000100 01  UT-VIKT-CNT-POST.                                                    
000101*    03  -COPY WEDICNTG -PRE UT-VIKT- .                                   
000102                                                                          
000103 01  UT-KOLLI-CNT-POST.                                                   
000104*    03  -COPY WEDICNTG -PRE UT-KOLLI- .                                  
000105                                                                          
000106 01  UT-VOLYM-CNT-POST.                                                   
000107*    03  -COPY WEDICNTG -PRE UT-VOLYM- .                                  
000108                                                                          
000109 01  UT-TDT-POST.                                                         
000110*    03  -COPY WEDITDTG -PRE UT-.                                         
000111                                                                          
000112 01  UT-CNI-POST.                                                         
000113*    03  -COPY WEDICNIG -PRE UT-.                                         
000114                                                                          
000115 01  UT-LOC-POST.                                                         
000116*    03  -COPY WEDILOCG -PRE UT-.                                         
000117                                                                          
000118 01  UT-SEN-NAD-POST.                                                     
000119*    03  -COPY WEDINADG -PRE UT-SEN-.                                     
000120                                                                          
000121 01  UT-REC-NAD-POST.                                                     
000122*    03  -COPY WEDINADG -PRE UT-REC-.                                     
000123                                                                          
000124 01  UT-GID-POST.                                                         
000125*    03  -COPY WEDIGIDG -PRE UT-.                                         
000126                                                                          
000127 01  UT-HAN-POST.                                                         
000128*    03  -COPY WEDIHANG -PRE UT-.                                         
000129                                                                          
000130 01  UT-GID-MOA-POST.                                                     
000131*    03  -COPY WEDIMOAG -PRE UT-GID.                                      
000132                                                                          
000133 01  UT-GID-NAD-POST.                                                     
000134*    03  -COPY WEDINADG -PRE UT-GID-.                                     
000135                                                                          
000136 01  UT-MEA-VIKT-POST.                                                    
000137*    03  -COPY WEDIMEAG -PRE UT-VK-.                                      
000138                                                                          
000139 01  UT-MEA-VOLYM-POST.                                                   
000140*    03  -COPY WEDIMEAG -PRE UT-VOL-.                                     
000141                                                                          
000142 01  UT-DIM-POST.                                                         
000143*    03  -COPY WEDIDIMG -PRE UT-.                                         
000144                                                                          
000145 01  UT-GID-RFF-POST.                                                     
000146*    03  -COPY WEDIRFFG -PRE UT-GID-.                                     
000147                                                                          
000148 01  UT-003-POST.                                                         
000149*    03  -COPY WEDI003G -PRE UT-.                                         
000150                                                                          
000151     EJECT                                                                
000152 WORKING-STORAGE SECTION.                                                 
000153                                                                          
000154                                                                          
000155*    -- CHECKED BY WY2000                                                 
000156 77  IDPGM                       PIC X(8)    VALUE 'W4764100'.            
000157 77  JA                          PIC X       VALUE 'J'.                   
000158 77  NEJ                         PIC X       VALUE 'N'.                   
000159 77  PUNKT                       PIC X       VALUE '.'.                   
000160 77  WS-KOLLI-RAK                PIC 9(4)    VALUE ZERO.                  
000161 77  WS-CNI-RAK                  PIC 9(3)    VALUE ZERO.                  
000162 77  WS-IDDISTR                  PIC S9(5)   VALUE ZERO.                  
000163 77  WS-IDKUNDNR                 PIC S9(7)   VALUE ZERO.                  
000164 77  WS-IDSKEPPN                 PIC S9(7)   VALUE ZERO.                  
000165 77  WS-IDKONTO                  PIC 9(9)    VALUE ZERO.                  
000166 77  WS-IDPRODNR                 PIC S9(7)   VALUE ZERO.                  
000167                                                                          
000168 77  W47640-EOF-SW               PIC X       VALUE 'N'.                   
000169     88  W47640-EOF                          VALUE 'J'.                   
000170                                                                          
000171 77  SKRIVA-HAN-POST-SW          PIC X       VALUE 'J'.                   
000172     88  SKRIVA-HAN-POST                     VALUE 'J'.                   
000173                                                                          
000174 77  SKRIVA-TDT-POST-SW          PIC X       VALUE 'J'.                   
000175     88  SKRIVA-TDT-POST                     VALUE 'J'.                   
000176                                                                          
000177 77  UTSKRIFT-SW                 PIC X       VALUE 'J'.                   
000178     88  SKRIV-SKEPPNING                     VALUE 'J'.                   
000179                                                                          
000180     EJECT                                                                
000181 01  ARBETSFALT.                                                          
000182                                                                          
000183     03 WS-NUMBER-OF-SEGMENTS       PIC 9(06) VALUE ZERO.                 
000184     03 WS-MESSAGE-REF-NO           PIC 9(06) VALUE ZERO.                 
000185                                                                          
000186     03 IN-IDPTYP                   PIC X(03) VALUE SPACE.                
000187     03 EDI-IDPTYP                  PIC X(03) VALUE SPACE.                
000188                                                                          
000189     03 WS-ORDER-KLI-REF.                                                 
000190        05 WS-IDORDNR5              PIC 9(05) VALUE ZERO.                 
000191        05 WS-TECKEN                PIC X(01) VALUE '-'.                  
000192        05 WS-IDKOLLI               PIC 9(05) VALUE ZERO.                 
000193                                                                          
000194     03 WS-VKORDBTO                 PIC 9(6)V9 VALUE ZERO.                
000195     03 WS-VKORDBTO-ALFA REDEFINES WS-VKORDBTO.                           
000196        05 WS-VKORDBTO-W            PIC X(7).                             
000197                                                                          
000198     03 WS-VLORDBTO                 PIC 9(4)V9(3) VALUE ZERO.             
000199     03 WS-VLORDBTO-ALFA REDEFINES WS-VLORDBTO.                           
000200        05 WS-VLORDBTO-W            PIC X(7).                             
000201                                                                          
000202     03 WS-VKORDBTO-TOT             PIC 9(6)V9 VALUE ZERO.                
000203     03 WS-VKORDBTO-TOT-NUM REDEFINES WS-VKORDBTO-TOT.                    
000204        05 WS-VKORDBTO-TOT-W        PIC 9(7).                             
000205                                                                          
000206     03 WS-VLORDBTO-TOT             PIC 9(4)V9(3) VALUE ZERO.             
000207     03 WS-VLORDBTO-TOT-NUM REDEFINES WS-VLORDBTO-TOT.                    
000208        05 WS-VLORDBTO-TOT-W        PIC 9(7).                             
000209                                                                          
000210     03 WS-SUORDV                   PIC 9(9)V9(2) VALUE ZERO.             
000211     03 WS-SUORDV-NUM REDEFINES WS-SUORDV.                                
000212        05 WS-SUORDV-W              PIC 9(11).                            
000213                                                                          
000214     03 WS-CARRIER-ID.                                                    
000215        05 WS-IDTRPTNR              PIC 9(03) VALUE ZERO.                 
000216        05 WS-TECKEN                PIC X(01) VALUE ' '.                  
000217        05 WS-IDLBBET               PIC X(12) VALUE SPACE.                
000218                                                                          
000219 01  KONSTANTER.                                                          
000220                                                                          
000221     03 001-UPPGIFTER.                                                    
000222                                                                          
000223       05 WC-001-IDPTYP                 PIC X(03) VALUE '001'.            
000224       05 WC-001-LENGTH                 PIC 9(03) VALUE  073 .            
000225       05 WC-001-VOLVO-PARTS            PIC X(04) VALUE 'VPAR'.           
000226       05 WC-001-VOLVO-AMTRIX           PIC X(04) VALUE 'VAMP'.           
000227       05 WC-001-IFCSUM95               PIC X(08)                         
000228                                        VALUE 'IFCSUM95'.                 
000229                                                                          
000230                                                                          
000231     03 UNB-UPPGIFTER.                                                    
000232                                                                          
000233       05 WC-UNB-IDPTYP                 PIC X(03) VALUE 'UNB'.            
000234       05 WC-UNB-LENGTH                 PIC 9(03) VALUE  125 .            
000235       05 WC-UNB-UNOA                   PIC X(04) VALUE 'UNOA'.           
000236       05 WC-UNB-2                      PIC X(01) VALUE '1'.              
000237       05 WC-UNB-CDC-CAR-PARTS          PIC X(14)                         
000238                                        VALUE '01441         '.           
000239       05 WC-UNB-NIGHT-PLUS-SCHENKER    PIC X(14)                         
000240                                        VALUE '119782        '.           
000241       05 WC-UNB-LAGERMAX               PIC X(14)                         
000242                                        VALUE '190189        '.           
000243       05 WC-UNB-TRUCKWHEEL             PIC X(14)                         
000244                                        VALUE '358828        '.           
000245       05 WC-UNB-CAT                    PIC X(14)                         
000246                                        VALUE '373050        '.           
000247       05 WC-UNB-GALLIKER               PIC X(14)                         
000248                                        VALUE '101784        '.           
000249       05 WC-UNB-DANX                   PIC X(14)                         
000250                                        VALUE '401228        '.           
000251       05 WC-UNB-SCHENKER               PIC X(14)                         
000252                                        VALUE '381700        '.           
000253       05 WC-UNB-DHL                    PIC X(14)                         
000254                                        VALUE '27922         '.           
000255       05 WC-UNB-BCUBE                  PIC X(14)                         
000256                                        VALUE '32494         '.           
000257       05 WC-UNB-NEOVIA                 PIC X(14)                         
000258                                        VALUE '319323        '.           
000259       05 WC-UNB-GEODIS                 PIC X(14)                         
000260                                        VALUE '12482         '.           
000261                                                                          
000262     03 UNH-UPPGIFTER.                                                    
000263                                                                          
000264       05 WC-UNH-IDPTYP                 PIC X(03) VALUE 'UNH'.            
000265       05 WC-UNH-LENGTH                 PIC 9(03) VALUE  072 .            
000266       05 WC-UNH-TYP                    PIC X(06) VALUE 'IFCSUM'.         
000267       05 WC-UNH-VERSION-NO             PIC X(03) VALUE 'D  '.            
000268       05 WC-UNH-REL-NO                 PIC X(03) VALUE '95A'.            
000269       05 WC-UNH-AGENCY                 PIC X(02) VALUE 'UN'.             
000270                                                                          
000271                                                                          
000272     03 BGM-UPPGIFTER.                                                    
000273                                                                          
000274       05 WC-BGM-IDPTYP                 PIC X(03) VALUE 'BGM'.            
000275       05 WC-BGM-LENGTH                 PIC 9(03) VALUE  085 .            
000276       05 WC-BGM-CARGO-MANIFEST         PIC X(03) VALUE '785'.            
000277                                                                          
000278                                                                          
000279     03 DTM-UPPGIFTER.                                                    
000280                                                                          
000281       05 WC-DTM-IDPTYP                 PIC X(03) VALUE 'DTM'.            
000282       05 WC-DTM-LENGTH                 PIC 9(03) VALUE 041.              
000283       05 WC-DTM-QUAL                   PIC X(03) VALUE '137'.            
000284       05 WC-DTM-CCYYMMDDHHMM           PIC X(03) VALUE '203'.            
000285                                                                          
000286                                                                          
000287     03 MOA-UPPGIFTER.                                                    
000288                                                                          
000289       05 WC-MOA-IDPTYP                 PIC X(03) VALUE 'MOA'.            
000290       05 WC-MOA-LENGTH                 PIC 9(03) VALUE 030.              
000291       05 WC-MOA-INVOICE-AMOUNT         PIC X(03) VALUE '44 '.            
000292       05 WC-MOA-KOLLI-AMOUNT           PIC X(03) VALUE '40 '.            
000293       05 WC-MOA-SEK                    PIC X(17) VALUE 'SEK'.            
000294                                                                          
000295                                                                          
000296     03 FTX-UPPGIFTER.                                                    
000297                                                                          
000298       05 WC-FTX-IDPTYP                 PIC X(03) VALUE 'FTX'.            
000299       05 WC-FTX-LENGTH                 PIC 9(03) VALUE 368.              
000300       05 WC-FTX-4451-TEXT-SUB-QUAL     PIC X(03) VALUE 'COI'.            
000301       05 WC-FTX-STOCK                  PIC X(05) VALUE 'STOCK'.          
000302       05 WC-FTX-RUSH                   PIC X(05) VALUE 'RUSH '.          
000303       05 WC-FTX-LDC                    PIC X(05) VALUE 'LDC  '.          
000304                                                                          
000305                                                                          
000306     03 CNT-UPPGIFTER.                                                    
000307                                                                          
000308       05 WC-CNT-IDPTYP                 PIC X(03) VALUE 'CNT'.            
000309       05 WC-CNT-LENGTH                 PIC 9(03) VALUE  024.             
000310       05 WC-CNT-TOT-GROSS-WEIGHT       PIC X(03) VALUE '7  '.            
000311       05 WC-CNT-TOT-NUM-OF-PACK        PIC X(03) VALUE '11 '.            
000312       05 WC-CNT-TOT-VOLUME             PIC X(03) VALUE '15 '.            
000313       05 WC-CNT-KILOGRAM               PIC X(03) VALUE 'KGM'.            
000314       05 WC-CNT-PIECES                 PIC X(03) VALUE 'PCE'.            
000315       05 WC-CNT-CUBIC-METRE            PIC X(03) VALUE 'MTQ'.            
000316                                                                          
000317                                                                          
000318     03 TDT-UPPGIFTER.                                                    
000319                                                                          
000320       05 WC-TDT-IDPTYP                 PIC X(03) VALUE 'TDT'.            
000321       05 WC-TDT-LENGTH                 PIC 9(03) VALUE  205.             
000322       05 WC-TDT-MAIN-CARRIAGE-TRANSP   PIC X(03) VALUE '20 '.            
000323                                                                          
000324                                                                          
000325     03 CNI-UPPGIFTER.                                                    
000326                                                                          
000327       05 WC-CNI-IDPTYP                 PIC X(03) VALUE 'CNI'.            
000328       05 WC-CNI-LENGTH                 PIC 9(03) VALUE 084.              
000329                                                                          
000330                                                                          
000331     03 LOC-UPPGIFTER.                                                    
000332                                                                          
000333       05 WC-LOC-IDPTYP                 PIC X(03) VALUE 'LOC'.            
000334       05 WC-LOC-LENGTH                 PIC 9(03) VALUE  256.             
000335       05 WC-LOC-QUALIFIER              PIC X(03) VALUE '5  '.            
000336                                                                          
000337                                                                          
000338     03 NAD-UPPGIFTER.                                                    
000339                                                                          
000340       05 WC-NAD-IDPTYP                 PIC X(03) VALUE 'NAD'.            
000341       05 WC-NAD-LENGTH                 PIC 9(03) VALUE 558.              
000342       05 WC-NAD-DOCUMENT-SENDER        PIC X(03) VALUE 'CZ '.            
000343       05 WC-NAD-DOCUMENT-RECEIVER      PIC X(03) VALUE 'CN '.            
000344       05 WC-NAD-VOLVO-NAME             PIC X(09) VALUE                   
000345                                                  'VCCS     '.            
000346       05 WC-NAD-VOLVO-ZIP-CODE         PIC X(09) VALUE                   
000347                                                  '40531    '.            
000348       05 WC-NAD-VOLVO-TOWN             PIC X(10) VALUE                   
000349                                                  'GOTHENBURG'.           
000350       05 WC-NAD-VOLVO-COUNTRY          PIC X(03) VALUE 'SE '.            
000351                                                                          
000352                                                                          
000353     03 GID-UPPGIFTER.                                                    
000354                                                                          
000355       05 WC-GID-IDPTYP                 PIC X(03) VALUE 'GID'.            
000356       05 WC-GID-LENGTH                 PIC 9(03) VALUE 203.              
000357                                                                          
000358                                                                          
000359     03 HAN-UPPGIFTER.                                                    
000360                                                                          
000361       05 WC-HAN-IDPTYP                 PIC X(03) VALUE 'HAN'.            
000362       05 WC-HAN-LENGTH                 PIC 9(03) VALUE 89.               
000363                                                                          
000364                                                                          
000365     03 MEA-UPPGIFTER.                                                    
000366                                                                          
000367       05 WC-MEA-IDPTYP                 PIC X(03) VALUE 'MEA'.            
000368       05 WC-MEA-LENGTH                 PIC 9(03) VALUE 144.              
000369                                                                          
000370       05 WC-MEA-WEIGHT                 PIC X(03) VALUE 'WT '.            
000371       05 WC-MEA-GROSS-WEIGHT           PIC X(03) VALUE 'G  '.            
000372       05 WC-MEA-GROSS-VOLUME           PIC X(03) VALUE 'AAW'.            
000373       05 WC-MEA-KILOGRAM               PIC X(03) VALUE 'KGM'.            
000374       05 WC-MEA-VOLUME                 PIC X(03) VALUE 'VOL'.            
000375       05 WC-MEA-CUBIC-METRE            PIC X(03) VALUE 'MTQ'.            
000376                                                                          
000377                                                                          
000378     03 DIM-UPPGIFTER.                                                    
000379                                                                          
000380       05 WC-DIM-IDPTYP                 PIC X(03) VALUE 'DIM'.            
000381       05 WC-DIM-LENGTH                 PIC 9(03) VALUE 51.               
000382                                                                          
000383       05 WC-DIM-DIMENTION-QUAL         PIC X(03) VALUE '2  '.            
000384       05 WC-DIM-CENTIMETER             PIC X(03) VALUE 'CMT'.            
000385                                                                          
000386     03 GID-RFF-UPPGIFTER.                                                
000387                                                                          
000388       05 WC-GID-RFF-IDPTYP             PIC X(03) VALUE 'RFF'.            
000389       05 WC-GID-RFF-LENGTH             PIC 9(03) VALUE 079.              
000390       05 WC-GID-RFF-REF-QUAL           PIC X(03) VALUE 'CU '.            
000391                                                                          
000392     03 003-UPPGIFTER.                                                    
000393                                                                          
000394       05 WC-003-IDPTYP                 PIC X(03) VALUE '003'.            
000395       05 WC-003-LENGTH                 PIC 9(03) VALUE 73.               
000396                                                                          
000397 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
000398 01  FILLER REDEFINES DAGENS-DATUM.                                       
000399     03  DAGENS-DATUM-AAR        PIC 9(2).                                
000400     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
000401     03  DAGENS-DATUM-DAG        PIC 9(2).                                
000402     EJECT                                                                
000403                                                                          
000404 01  WS-CURRENT-DATE-TIME.                                                
000405     03 WS-CURRENT-DATE          PIC 9(8).                                
000406     03 WS-CURRENT-TIME          PIC 9(4).                                
000407                                                                          
000408     EJECT                                                                
000409*- - - - - - - - - - - - - -                                              
000410*      --- VALID IDDC CODES                                               
000411*                                                                         
000412*01    -COPY WWDC99                                                       
000413     EJECT                                                                
000414 01  TEST-IDDISTR       PIC 9(5)   COMP-3.                                
000415                                                                          
000416*01  FILLER -COPY WWDIST87    -RED TEST-IDDISTR.                          
000417     EJECT                                                                
000418 01  DYNAMISKA-SUBPROGRAM.                                                
000419*                                                                         
000420     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
000421     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000422     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000423     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
000424     SKIP2                                                                
000425*    --- PARAMETRAR TILL ABEND                                            
000426                                                                          
000427 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
000428 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
000429 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
000430     SKIP2                                                                
000431 01  FELTEXT.                                                             
000432     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
000433     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
000434     EJECT                                                                
000435*    --- PARAMETRAR TILL POSTSUM                                          
000436*                                                                         
000437*01  -COPY W0005   -PRE  POSTSUM-                                         
000438     EJECT                                                                
000439                                                                          
000440 01  IN-AREA-START               PIC X(24)   VALUE                        
000441                                 'IN-AREA-START  '.                       
000442     SKIP2                                                                
000443 01  IN-AREA.                                                             
000444     03  IN-DATA-AREA            PIC X(800).                              
000445                                                                          
000446*    03  E101-POST  -COPY W476E101 -PRE IN-  -RED IN-DATA-AREA            
000447                                                                          
000448*    03  E111-POST  -COPY W476E111 -PRE IN-  -RED IN-DATA-AREA            
000449                                                                          
000450*    03  E121-POST  -COPY W476E121 -PRE IN-  -RED IN-DATA-AREA            
000451                                                                          
000452     EJECT                                                                
000453 01  W-E101-SPAR-AREA            PIC X(24)   VALUE                        
000454                                 'E101-SPAR-AREA   '.                     
000455 01  W-SPAR-E101-AREA.                                                    
000456*    03  -COPY W476E101    -PRE SPAR-.                                    
000457     EJECT                                                                
000458 01  W-E111-SPAR-AREA            PIC X(24)   VALUE                        
000459                                 'E111-SPAR-AREA   '.                     
000460 01  W-SPAR-E111-AREA.                                                    
000461*    03  -COPY W476E111    -PRE SPAR-.                                    
000462     EJECT                                                                
000463 01  FILLER                      PIC X(24)   VALUE                        
000464                                 'UT-AREA-START  '.                       
000465                                                                          
000466 01  FILLER                      PIC X(24) VALUE '001-AREA'.              
000467 01  001-AREA.                                                            
000468*    03  -COPY WEDI001G                                                   
000469                                                                          
000470 01  FILLER                      PIC X(24) VALUE 'UNB-AREA'.              
000471 01  UNB-AREA.                                                            
000472*    03  -COPY WEDIUNBG.                                                  
000473                                                                          
000474 01  FILLER                      PIC X(24) VALUE 'UNH-AREA'.              
000475 01  UNH-AREA.                                                            
000476*    03  -COPY WEDIUNHG.                                                  
000477                                                                          
000478 01  FILLER                      PIC X(24) VALUE 'BGM-AREA'.              
000479 01  BGM-AREA.                                                            
000480*    03  -COPY WEDIBGMG.                                                  
000481                                                                          
000482 01  FILLER                      PIC X(24) VALUE 'DTM-AREA'.              
000483 01  DTM-AREA.                                                            
000484*    03  -COPY WEDIDTMG.                                                  
000485                                                                          
000486 01  FILLER                      PIC X(24) VALUE 'MOA-AREA'.              
000487 01  MOA-AREA.                                                            
000488*    03  -COPY WEDIMOAG.                                                  
000489                                                                          
000490 01  FILLER                      PIC X(24) VALUE 'FTX-AREA'.              
000491 01  FTX-AREA.                                                            
000492*    03  -COPY WEDIFTXG.                                                  
000493                                                                          
000494 01  FILLER                      PIC X(24) VALUE 'VIKT-CNT-AREA'.         
000495 01  VIKT-CNT-AREA.                                                       
000496*    03  -COPY WEDICNTG -PRE VIKT- .                                      
000497                                                                          
000498 01  FILLER                      PIC X(24) VALUE 'KOLLI-CNT-AREA'.        
000499 01  KOLLI-CNT-AREA.                                                      
000500*    03  -COPY WEDICNTG -PRE KOLLI- .                                     
000501                                                                          
000502 01  FILLER                      PIC X(24) VALUE 'VOLYM-CNT-AREA'.        
000503 01  VOLYM-CNT-AREA.                                                      
000504*    03  -COPY WEDICNTG -PRE VOLYM- .                                     
000505                                                                          
000506 01  FILLER                      PIC X(24) VALUE 'TDT-AREA'.              
000507 01  TDT-AREA.                                                            
000508*    03  -COPY WEDITDTG.                                                  
000509                                                                          
000510 01  FILLER                      PIC X(24) VALUE 'CNI-AREA'.              
000511 01  CNI-AREA.                                                            
000512*    03  -COPY WEDICNIG.                                                  
000513                                                                          
000514 01  FILLER                      PIC X(24) VALUE 'LOC-AREA'.              
000515 01  LOC-AREA.                                                            
000516*    03  -COPY WEDILOCG.                                                  
000517                                                                          
000518 01  FILLER                      PIC X(24) VALUE 'SEN-NAD-AREA'.          
000519 01  SEN-NAD-AREA.                                                        
000520*    03  -COPY WEDINADG -PRE SEN- .                                       
000521                                                                          
000522 01  FILLER                      PIC X(24) VALUE 'REC-NAD-AREA'.          
000523 01  REC-NAD-AREA.                                                        
000524*    03  -COPY WEDINADG -PRE REC- .                                       
000525                                                                          
000526 01  FILLER                      PIC X(24) VALUE 'GID-AREA'.              
000527 01  GID-AREA.                                                            
000528*    03  -COPY WEDIGIDG.                                                  
000529                                                                          
000530 01  FILLER                      PIC X(24) VALUE 'HAN-AREA'.              
000531 01  HAN-AREA.                                                            
000532*    03  -COPY WEDIHANG.                                                  
000533                                                                          
000534 01  FILLER                      PIC X(24) VALUE 'GID-MOA-AREA'.          
000535 01  GID-MOA-AREA.                                                        
000536*    03  -COPY WEDIMOAG -PRE GID-.                                        
000537                                                                          
000538 01  FILLER                      PIC X(24) VALUE 'GID-NAD-AREA'.          
000539 01  GID-NAD-AREA.                                                        
000540*    03  -COPY WEDINADG -PRE GID-.                                        
000541                                                                          
000542 01  FILLER                      PIC X(24) VALUE 'VIKT-MEA-AREA'.         
000543 01  VIKT-MEA-AREA.                                                       
000544*    03  -COPY WEDIMEAG -PRE VK-.                                         
000545                                                                          
000546 01  FILLER                      PIC X(24) VALUE 'VOLYM-MEA-AREA'.        
000547 01  VOLYM-MEA-AREA.                                                      
000548*    03  -COPY WEDIMEAG -PRE VOL-.                                        
000549                                                                          
000550 01  FILLER                      PIC X(24) VALUE 'DIM-AREA'.              
000551 01  DIM-AREA.                                                            
000552*    03  -COPY WEDIDIMG.                                                  
000553                                                                          
000554 01  FILLER                      PIC X(24) VALUE 'GID-RFF-AREA'.          
000555 01  GID-RFF-AREA.                                                        
000556*    03  -COPY WEDIRFFG.                                                  
000557                                                                          
000558 01  FILLER                      PIC X(24) VALUE '003-AREA'.              
000559 01  003-AREA.                                                            
000560*    03  -COPY WEDI003G.                                                  
000561     EJECT                                                                
000562 PROCEDURE DIVISION.                                                      
000563                                                                          
000564 MAIN SECTION.                                                            
000565                                                                          
000566     PERFORM A-INIT                                                       
000567                                                                          
000568     PERFORM S01-LAES-W47640                                              
000569                                                                          
000570     IF NOT W47640-EOF                                                    
000571       PERFORM S31-SKRIV-001-POST                                         
000572                                                                          
000573       PERFORM UNTIL W47640-EOF                                           
000574                                                                          
000575         IF IN-HUV-IDPTYP = 'E101'                                        
000576                                                                          
000577             MOVE IN-AREA        TO W-SPAR-E101-AREA                      
000578             MOVE IN-HUV-IDPTYP  TO IN-IDPTYP                             
000579             PERFORM B-NOLLSTALL-SKEPPNING                                
000580             PERFORM C-BEHANDLA-SKEPPN-INFO                               
000581             PERFORM S20-SKRIV-UNB-POST                                   
000582             PERFORM S21-SKRIV-SKEPPN-INFO                                
000583                                                                          
000584         ELSE                                                             
000585           IF IN-KND-IDPTYP = 'E111'                                      
000586                                                                          
000587             MOVE IN-AREA          TO W-SPAR-E111-AREA                    
000588             MOVE IN-KND-IDPTYP    TO IN-IDPTYP                           
000589             PERFORM D-NOLLSTALL-KOLLI                                    
000590                                                                          
000591             PERFORM E-BEHANDLA-KOLLI-GEN-INFO                            
000592                                                                          
000593           ELSE                                                           
000594             IF IN-KLI-IDPTYP = 'E121'                                    
000595                                                                          
000596               ADD +1              TO WS-KOLLI-RAK                        
000597               PERFORM F-BEHANDLA-KOLLI-INFO                              
000598               PERFORM S23-SKRIV-KOLLI-INFO                               
000599             END-IF                                                       
000600           END-IF                                                         
000601         END-IF                                                           
000602                                                                          
000603         PERFORM S01-LAES-W47640                                          
000604       END-PERFORM                                                        
000605                                                                          
000606                                                                          
000607       PERFORM S34-SKRIV-003-POST                                         
000608     END-IF                                                               
000609     PERFORM Z-FINIT                                                      
000610                                                                          
000611     MOVE ZERO TO RETURN-CODE                                             
000612     GOBACK                                                               
000613     .                                                                    
000614     EJECT                                                                
000615 A-INIT SECTION.                                                          
000616                                                                          
000617     OPEN INPUT  W47640                                                   
000618                                                                          
000619     OPEN OUTPUT W47641                                                   
000620                                                                          
000621     ACCEPT DAGENS-DATUM  FROM DATE                                       
000622     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
000623                                                                          
000624     MOVE ZERO  TO WS-KOLLI-RAK                                           
000625     .                                                                    
000626     EJECT                                                                
000627                                                                          
000628 B-NOLLSTALL-SKEPPNING SECTION.                                           
000629                                                                          
000630     COMPUTE  WS-MESSAGE-REF-NO =                                         
000631              WS-MESSAGE-REF-NO + 1                                       
000632     MOVE ZERO             TO WS-NUMBER-OF-SEGMENTS                       
000633                              WS-CNI-RAK                                  
000634                                                                          
000635     INITIALIZE            UNH-AREA                                       
000636                           BGM-AREA                                       
000637                           DTM-AREA                                       
000638                           MOA-AREA                                       
000639                           FTX-AREA                                       
000640                           VIKT-CNT-AREA                                  
000641                           KOLLI-CNT-AREA                                 
000642                           VOLYM-CNT-AREA                                 
000643                           TDT-AREA                                       
000644                           CNI-AREA                                       
000645                           LOC-AREA                                       
000646                           SEN-NAD-AREA                                   
000647                           REC-NAD-AREA                                   
000648                           GID-AREA                                       
000649                           HAN-AREA                                       
000650                           GID-MOA-AREA                                   
000651                           GID-NAD-AREA                                   
000652                           VIKT-MEA-AREA                                  
000653                           VOLYM-MEA-AREA                                 
000654                           DIM-AREA                                       
000655                           GID-RFF-AREA                                   
000656     .                                                                    
000657     EJECT                                                                
000658 C-BEHANDLA-SKEPPN-INFO SECTION.                                          
000659                                                                          
000660     PERFORM CA-SKAPA-UNH-POST                                            
000661     PERFORM CB-SKAPA-BGM-POST                                            
000662     PERFORM CC-SKAPA-DTM-POST                                            
000663     PERFORM CD-SKAPA-MOA-POST                                            
000664     PERFORM CE-SKAPA-FTX-POST                                            
000665     PERFORM CF-SKAPA-CNT-POST                                            
000666     PERFORM CG-SKAPA-TDT-POST                                            
000667     .                                                                    
000668     EJECT                                                                
000669 CA-SKAPA-UNH-POST SECTION.                                               
000670                                                                          
000671     MOVE WC-UNH-IDPTYP          TO UNH-IDPTYP                            
000672     MOVE WC-UNH-LENGTH          TO UNH-LENGTH                            
000673                                                                          
000674     MOVE WS-MESSAGE-REF-NO      TO                                       
000675                         UNH-0062-MESSAGE-REFERENCE                       
000676                                                                          
000677     MOVE WC-UNH-TYP             TO                                       
000678                         UNH-0065-MESSAGE-TYPE-ID                         
000679     MOVE WC-UNH-VERSION-NO      TO                                       
000680                         UNH-0052-MESSAGE-VERSION                         
000681     MOVE WC-UNH-REL-NO          TO                                       
000682                         UNH-0054-MESSAGE-RELEASE                         
000683     MOVE WC-UNH-AGENCY          TO                                       
000684                         UNH-0051-CONTROLING-AGENCY                       
000685     .                                                                    
000686     EJECT                                                                
000687 CB-SKAPA-BGM-POST SECTION.                                               
000688                                                                          
000689     MOVE WC-BGM-IDPTYP          TO BGM-IDPTYP                            
000690     MOVE WC-BGM-LENGTH          TO BGM-LENGTH                            
000691     MOVE WC-BGM-CARGO-MANIFEST  TO BGM-1001-DOCUMENT-NAME                
000692     MOVE IN-HUV-IDSHIPM         TO BGM-1004-DOCUMENT-NUMBER              
000693     .                                                                    
000694     EJECT                                                                
000695 CC-SKAPA-DTM-POST SECTION.                                               
000696                                                                          
000697     MOVE WC-DTM-IDPTYP          TO DTM-IDPTYP                            
000698     MOVE WC-DTM-LENGTH          TO DTM-LENGTH                            
000699     MOVE WC-DTM-QUAL            TO DTM-2005-DATE-TIME-PER-QUAL           
000700                                                                          
000701     MOVE FUNCTION CURRENT-DATE(1:8) TO WS-CURRENT-DATE                   
000702     MOVE FUNCTION CURRENT-DATE(9:4) TO WS-CURRENT-TIME                   
000703     MOVE WS-CURRENT-DATE-TIME       TO                                   
000704                            DTM-2380-DATE-TIME-PER                        
000705                                                                          
000706     MOVE WC-DTM-CCYYMMDDHHMM        TO                                   
000707                            DTM-2379-DATE-TIME-PER-FORM                   
000708     .                                                                    
000709     EJECT                                                                
000710 CD-SKAPA-MOA-POST SECTION.                                               
000711                                                                          
000712     MOVE WC-MOA-IDPTYP          TO MOA-IDPTYP                            
000713     MOVE WC-MOA-LENGTH          TO MOA-LENGTH                            
000714                                                                          
000715     MOVE WC-MOA-INVOICE-AMOUNT  TO MOA-5025-MON-AMOUNT-QUAL              
000716     MOVE IN-HUV-SUORDV          TO WS-SUORDV                             
000717     MOVE WS-SUORDV-W            TO MOA-5004-MONETARY-AMOUNT              
000718     MOVE IN-HUV-KDVALISO        TO MOA-6345-CURRENCY-CODED               
000719     .                                                                    
000720     EJECT                                                                
000721 CE-SKAPA-FTX-POST SECTION.                                               
000722                                                                          
000723     MOVE WC-FTX-IDPTYP          TO FTX-IDPTYP                            
000724     MOVE WC-FTX-LENGTH          TO FTX-LENGTH                            
000725                                                                          
000726     MOVE WC-FTX-4451-TEXT-SUB-QUAL                                       
000727                                 TO FTX-4451-TEXT-SUB-QUAL                
000728     IF IN-HUV-KDORDKL > +1                                               
000729       MOVE WC-FTX-STOCK         TO FTX-4440-FREE-TEXT                    
000730     ELSE                                                                 
000731       MOVE WC-FTX-RUSH          TO FTX-4440-FREE-TEXT                    
000732     END-IF                                                               
000733                                                                          
000734     MOVE IN-HUV-IDDISTR         TO TEST-IDDISTR                          
000735     MOVE IN-HUV-IDDC            TO WS-IDDC                               
000736                                                                          
000737*    IF IN-HUV-IDTRPTNR = 805 AND                                         
000738*       DIST87-VDH-1678       AND                                         
000739*       SDC-NL                                                            
000740*      MOVE WC-FTX-LDC           TO FTX-4440-FREE-TEXT                    
000741*    END-IF                                                               
000742     .                                                                    
000743     EJECT                                                                
000744 CF-SKAPA-CNT-POST SECTION.                                               
000745                                                                          
000746*    VIKT                                                                 
000747     MOVE WC-CNT-IDPTYP           TO VIKT-CNT-IDPTYP                      
000748     MOVE WC-CNT-LENGTH           TO VIKT-CNT-LENGTH                      
000749                                                                          
000750     MOVE WC-CNT-TOT-GROSS-WEIGHT TO VIKT-CNT-6069-CONTR-QUAL             
000751                                                                          
000752     IF IN-HUV-VKORDBTO > ZERO                                            
000753        MOVE IN-HUV-VKORDBTO      TO WS-VKORDBTO-TOT                      
000754        MOVE WS-VKORDBTO-TOT-W    TO                                      
000755                              VIKT-CNT-6066-CONTR-VALUE                   
000756     ELSE                                                                 
000757        MOVE 1                    TO WS-VKORDBTO-TOT                      
000758        MOVE WS-VKORDBTO-TOT-W    TO                                      
000759                              VIKT-CNT-6066-CONTR-VALUE                   
000760     END-IF                                                               
000761                                                                          
000762     MOVE WC-CNT-KILOGRAM         TO VIKT-CNT-6411-MEA-UNIT-Q             
000763                                                                          
000764*    ANTAL KOLLI                                                          
000765     MOVE WC-CNT-IDPTYP           TO KOLLI-CNT-IDPTYP                     
000766     MOVE WC-CNT-LENGTH           TO KOLLI-CNT-LENGTH                     
000767                                                                          
000768     MOVE WC-CNT-TOT-NUM-OF-PACK  TO KOLLI-CNT-6069-CONTR-QUAL            
000769                                                                          
000770     IF IN-HUV-KVKOLLI > ZERO                                             
000771       MOVE IN-HUV-KVKOLLI        TO KOLLI-CNT-6066-CONTR-VALUE           
000772     ELSE                                                                 
000773        MOVE ZERO                 TO KOLLI-CNT-6066-CONTR-VALUE           
000774     END-IF                                                               
000775                                                                          
000776     MOVE WC-CNT-PIECES           TO KOLLI-CNT-6411-MEA-UNIT-Q            
000777                                                                          
000778*    VOLYM                                                                
000779     MOVE WC-CNT-IDPTYP           TO VOLYM-CNT-IDPTYP                     
000780     MOVE WC-CNT-LENGTH           TO VOLYM-CNT-LENGTH                     
000781                                                                          
000782     MOVE WC-CNT-TOT-VOLUME       TO VOLYM-CNT-6069-CONTR-QUAL            
000783     IF IN-HUV-VLORDBTO > ZERO                                            
000784        MOVE IN-HUV-VLORDBTO      TO WS-VLORDBTO-TOT                      
000785        MOVE WS-VLORDBTO-TOT-W    TO                                      
000786                              VOLYM-CNT-6066-CONTR-VALUE                  
000787     ELSE                                                                 
000788        MOVE 1                    TO WS-VLORDBTO-TOT                      
000789        MOVE WS-VLORDBTO-TOT-W    TO                                      
000790                              VOLYM-CNT-6066-CONTR-VALUE                  
000791     END-IF                                                               
000792                                                                          
000793     MOVE WC-CNT-CUBIC-METRE      TO VOLYM-CNT-6411-MEA-UNIT-Q            
000794     .                                                                    
000795     EJECT                                                                
000796 CG-SKAPA-TDT-POST SECTION.                                               
000797                                                                          
000798     MOVE WC-TDT-IDPTYP          TO TDT-IDPTYP                            
000799     MOVE WC-TDT-LENGTH          TO TDT-LENGTH                            
000800     MOVE WC-TDT-MAIN-CARRIAGE-TRANSP                                     
000801                                 TO TDT-8051-TRANSP-STAGE-QUAL            
000802     IF SPAR-HUV-IDLBBET > SPACE                                          
000803       MOVE SPAR-HUV-IDTRPTNR    TO WS-IDTRPTNR                           
000804       MOVE SPAR-HUV-IDLBBET     TO WS-IDLBBET                            
000805       MOVE WS-CARRIER-ID        TO TDT-3127-CARRIER-ID                   
000806       MOVE JA                   TO SKRIVA-TDT-POST-SW                    
000807     ELSE                                                                 
000808       MOVE NEJ                  TO SKRIVA-TDT-POST-SW                    
000809     END-IF                                                               
000810     .                                                                    
000811     EJECT                                                                
000812 D-NOLLSTALL-KOLLI SECTION.                                               
000813                                                                          
000814     INITIALIZE            GID-AREA                                       
000815                           HAN-AREA                                       
000816                           GID-MOA-AREA                                   
000817                           GID-NAD-AREA                                   
000818                           VIKT-MEA-AREA                                  
000819                           VOLYM-MEA-AREA                                 
000820                           DIM-AREA                                       
000821                           GID-RFF-AREA                                   
000822     .                                                                    
000823     EJECT                                                                
000824 E-BEHANDLA-KOLLI-GEN-INFO SECTION.                                       
000825                                                                          
000826     PERFORM EA-SKAPA-CNI-POST                                            
000827     PERFORM EB-SKAPA-LOC-POST                                            
000828     PERFORM EC-SKAPA-SEN-NAD-POST                                        
000829     PERFORM ED-SKAPA-REC-NAD-POST                                        
000830                                                                          
000831     PERFORM S22-SKRIV-KOLLI-GEN-INFO                                     
000832                                                                          
000833     MOVE ZERO TO WS-KOLLI-RAK                                            
000834     .                                                                    
000835     EJECT                                                                
000836 EA-SKAPA-CNI-POST SECTION.                                               
000837                                                                          
000838     MOVE WC-CNI-IDPTYP          TO CNI-IDPTYP                            
000839     MOVE WC-CNI-LENGTH          TO CNI-LENGTH                            
000840                                                                          
000841     COMPUTE WS-CNI-RAK =  WS-CNI-RAK + 1                                 
000842                                                                          
000843     MOVE WS-CNI-RAK             TO CNI-1490-CONSOLID-ITEM-NO             
000844     .                                                                    
000845     EJECT                                                                
000846 EB-SKAPA-LOC-POST SECTION.                                               
000847                                                                          
000848     MOVE WC-LOC-IDPTYP          TO LOC-IDPTYP                            
000849     MOVE WC-LOC-LENGTH          TO LOC-LENGTH                            
000850                                                                          
000851     MOVE WC-LOC-QUALIFIER       TO LOC-3227-PLACE-LOC-QUAL               
000852                                                                          
000853     MOVE SPAR-HUV-IDDC          TO LOC-3225-LOC-ID                       
000854     .                                                                    
000855     EJECT                                                                
000856 EC-SKAPA-SEN-NAD-POST SECTION.                                           
000857                                                                          
000858     MOVE WC-NAD-IDPTYP          TO SEN-NAD-IDPTYP                        
000859     MOVE WC-NAD-LENGTH          TO SEN-NAD-LENGTH                        
000860                                                                          
000861     MOVE WC-NAD-DOCUMENT-SENDER TO SEN-NAD-3035-PARTY-QUAL               
000862     MOVE WC-NAD-VOLVO-NAME      TO                                       
000863                              SEN-NAD-3036-PARTY-NAME-1                   
000864     MOVE WC-NAD-VOLVO-ZIP-CODE  TO                                       
000865                              SEN-NAD-3251-POSTCODE-ID                    
000866     MOVE WC-NAD-VOLVO-TOWN      TO                                       
000867                              SEN-NAD-3164-CITY-NAME                      
000868     MOVE WC-NAD-VOLVO-COUNTRY   TO                                       
000869                              SEN-NAD-3207-COUNTRY-CODED                  
000870     .                                                                    
000871     EJECT                                                                
000872 ED-SKAPA-REC-NAD-POST SECTION.                                           
000873                                                                          
000874     MOVE WC-NAD-IDPTYP             TO REC-NAD-IDPTYP                     
000875     MOVE WC-NAD-LENGTH             TO REC-NAD-LENGTH                     
000876                                                                          
000877     MOVE WC-NAD-DOCUMENT-RECEIVER  TO REC-NAD-3035-PARTY-QUAL            
000878     MOVE SPAR-KND-IDDISTR          TO REC-NAD-3039-PARTY-ID              
000879                                                                          
000880     MOVE SPAR-KND-BEGMT            TO REC-NAD-3036-PARTY-NAME-1          
000881     MOVE SPAR-KND-ADGMT-GATA       TO REC-NAD-3042-STREET-PBOX-1         
000882     MOVE SPAR-KND-ADGMT-PADR       TO REC-NAD-3042-STREET-PBOX-2         
000883     MOVE SPAR-KND-ADGMT-LAND       TO REC-NAD-3042-STREET-PBOX-3         
000884     .                                                                    
000885     EJECT                                                                
000886 F-BEHANDLA-KOLLI-INFO SECTION.                                           
000887                                                                          
000888     PERFORM FA-SKAPA-GID-POST                                            
000889     PERFORM FB-SKAPA-HAN-POST                                            
000890     PERFORM FC-SKAPA-GID-MOA-POST                                        
000891     PERFORM FD-SKAPA-GID-NAD-POST                                        
000892     PERFORM FE-SKAPA-VIKT-MEA-POST                                       
000893     PERFORM FF-SKAPA-VOL-MEA-POST                                        
000894     PERFORM FG-SKAPA-DIM-POST                                            
000895     PERFORM FH-SKAPA-GID-RFF-POST                                        
000896     .                                                                    
000897     EJECT                                                                
000898 FA-SKAPA-GID-POST SECTION.                                               
000899                                                                          
000900     MOVE WC-GID-IDPTYP          TO GID-IDPTYP                            
000901     MOVE WC-GID-LENGTH          TO GID-LENGTH                            
000902                                                                          
000903     MOVE WS-KOLLI-RAK           TO GID-1496-GOODS-ITEM-NUMBER            
000904     .                                                                    
000905     EJECT                                                                
000906 FB-SKAPA-HAN-POST SECTION.                                               
000907                                                                          
000908     MOVE WC-HAN-IDPTYP          TO HAN-IDPTYP                            
000909     MOVE WC-HAN-LENGTH          TO HAN-LENGTH                            
000910                                                                          
000911     IF IN-KLI-KDFARLIG-KOLLI > +3                                        
000912       MOVE '1'                  TO HAN-7419-HAZ-MAT-CLASS                
000913       MOVE JA                   TO SKRIVA-HAN-POST-SW                    
000914     ELSE                                                                 
000915       MOVE NEJ                  TO SKRIVA-HAN-POST-SW                    
000916     END-IF                                                               
000917     .                                                                    
000918     EJECT                                                                
000919 FC-SKAPA-GID-MOA-POST SECTION.                                           
000920                                                                          
000921     MOVE WC-MOA-IDPTYP          TO MOA-IDPTYP                            
000922     MOVE WC-MOA-LENGTH          TO MOA-LENGTH                            
000923                                                                          
000924     MOVE WC-MOA-KOLLI-AMOUNT    TO                                       
000925                                 MOA-5025-MON-AMOUNT-QUAL                 
000926     MOVE IN-KLI-SUORDV-KOLLI    TO WS-SUORDV                             
000927     MOVE WS-SUORDV-W            TO MOA-5004-MONETARY-AMOUNT              
000928     MOVE IN-KLI-KDVALISO        TO MOA-6345-CURRENCY-CODED               
000929                                                                          
000930     .                                                                    
000931     EJECT                                                                
000932 FD-SKAPA-GID-NAD-POST SECTION.                                           
000933                                                                          
000934     MOVE WC-NAD-IDPTYP          TO GID-NAD-IDPTYP                        
000935     MOVE WC-NAD-LENGTH          TO GID-NAD-LENGTH                        
000936                                                                          
000937     MOVE WC-NAD-DOCUMENT-RECEIVER                                        
000938                                 TO GID-NAD-3035-PARTY-QUAL               
000939     MOVE IN-KLI-IDKUNDNR        TO GID-NAD-3039-PARTY-ID                 
000940                                                                          
000941     IF IN-KLI-ADGMT-GATA  = SPACE AND                                    
000942        IN-KLI-ADGMT-PADR  = SPACE AND                                    
000943        IN-KLI-ADGMT-LAND  = SPACE AND                                    
000944        IN-KLI-BEGMT-RAD1  = SPACE AND                                    
000945        IN-KLI-BEGMT-RAD2  = SPACE                                        
000946                                                                          
000947       MOVE SPAR-KND-BEGMT       TO GID-NAD-3036-PARTY-NAME-1             
000948       MOVE SPAR-KND-ADGMT-GATA  TO GID-NAD-3042-STREET-PBOX-1            
000949       MOVE SPAR-KND-ADGMT-PADR  TO GID-NAD-3042-STREET-PBOX-2            
000950       MOVE SPAR-KND-ADGMT-LAND  TO GID-NAD-3042-STREET-PBOX-3            
000951     ELSE                                                                 
000952       MOVE IN-KLI-BEGMT         TO GID-NAD-3036-PARTY-NAME-1             
000953       MOVE IN-KLI-ADGMT-GATA    TO GID-NAD-3042-STREET-PBOX-1            
000954       MOVE IN-KLI-ADGMT-PADR    TO GID-NAD-3042-STREET-PBOX-2            
000955       MOVE IN-KLI-ADGMT-LAND    TO GID-NAD-3042-STREET-PBOX-3            
000956     END-IF                                                               
000957                                                                          
000958     .                                                                    
000959     EJECT                                                                
000960 FE-SKAPA-VIKT-MEA-POST SECTION.                                          
000961                                                                          
000962     MOVE WC-MEA-IDPTYP          TO VK-MEA-IDPTYP                         
000963     MOVE WC-MEA-LENGTH          TO VK-MEA-LENGTH                         
000964                                                                          
000965     MOVE WC-MEA-WEIGHT          TO                                       
000966                                 VK-MEA-6311-MEASURE-QUAL                 
000967     MOVE WC-MEA-GROSS-WEIGHT    TO                                       
000968                                 VK-MEA-6313-MEASURE-DIM                  
000969     MOVE WC-MEA-KILOGRAM        TO                                       
000970                                 VK-MEA-6411-MEASURE-UNIT-Q               
000971     IF IN-KLI-VKORDBTO-KOLLI > ZERO                                      
000972       MOVE IN-KLI-VKORDBTO-KOLLI                                         
000973                                 TO WS-VKORDBTO                           
000974       MOVE WS-VKORDBTO-W        TO                                       
000975                                 VK-MEA-6314-MEASURE-VALUE                
000976     ELSE                                                                 
000977        MOVE 1                   TO WS-VKORDBTO                           
000978        MOVE WS-VKORDBTO-W       TO                                       
000979                                 VK-MEA-6314-MEASURE-VALUE                
000980     END-IF                                                               
000981     .                                                                    
000982     EJECT                                                                
000983 FF-SKAPA-VOL-MEA-POST SECTION.                                           
000984                                                                          
000985     MOVE WC-MEA-IDPTYP          TO VOL-MEA-IDPTYP                        
000986     MOVE WC-MEA-LENGTH          TO VOL-MEA-LENGTH                        
000987                                                                          
000988     MOVE WC-MEA-VOLUME          TO                                       
000989                                 VOL-MEA-6311-MEASURE-QUAL                
000990     MOVE WC-MEA-GROSS-VOLUME    TO                                       
000991                                 VOL-MEA-6313-MEASURE-DIM                 
000992     MOVE WC-MEA-CUBIC-METRE     TO                                       
000993                                 VOL-MEA-6411-MEASURE-UNIT-Q              
000994     IF IN-KLI-VLORDBTO-KOLLI > ZERO                                      
000995       MOVE IN-KLI-VLORDBTO-KOLLI                                         
000996                                 TO WS-VLORDBTO                           
000997       MOVE WS-VLORDBTO-W        TO                                       
000998                                 VOL-MEA-6314-MEASURE-VALUE               
000999     ELSE                                                                 
001000        MOVE 1                   TO WS-VLORDBTO                           
001001        MOVE WS-VLORDBTO-W       TO                                       
001002                                 VOL-MEA-6314-MEASURE-VALUE               
001003     END-IF                                                               
001004     .                                                                    
001005     EJECT                                                                
001006 FG-SKAPA-DIM-POST SECTION.                                               
001007                                                                          
001008     MOVE WC-DIM-IDPTYP          TO DIM-IDPTYP                            
001009     MOVE WC-DIM-LENGTH          TO DIM-LENGTH                            
001010                                                                          
001011     MOVE WC-DIM-DIMENTION-QUAL  TO                                       
001012                                 DIM-6145-DIMENSION-QUAL                  
001013     MOVE WC-DIM-CENTIMETER      TO                                       
001014                                 DIM-6411-MEASURE-UNIT-QUAL               
001015                                                                          
001016     IF IN-KLI-DIKOLLIL > ZERO                                            
001017       MOVE IN-KLI-DIKOLLIL      TO DIM-6168-LENGTH-DIMENSION             
001018     ELSE                                                                 
001019       MOVE 100                  TO DIM-6168-LENGTH-DIMENSION             
001020     END-IF                                                               
001021                                                                          
001022     IF IN-KLI-DIKOLLIB > ZERO                                            
001023       MOVE IN-KLI-DIKOLLIB      TO DIM-6140-WIDTH-DIMENSION              
001024     ELSE                                                                 
001025       MOVE 100                  TO DIM-6140-WIDTH-DIMENSION              
001026     END-IF                                                               
001027                                                                          
001028     IF IN-KLI-DIKOLLIH > ZERO                                            
001029       MOVE IN-KLI-DIKOLLIH      TO DIM-6008-HEIGHT-DIMENSION             
001030     ELSE                                                                 
001031       MOVE 100                  TO DIM-6008-HEIGHT-DIMENSION             
001032     END-IF                                                               
001033     .                                                                    
001034     EJECT                                                                
001035 FH-SKAPA-GID-RFF-POST SECTION.                                           
001036                                                                          
001037     MOVE WC-GID-RFF-IDPTYP      TO RFF-IDPTYP                            
001038     MOVE WC-GID-RFF-LENGTH      TO RFF-LENGTH                            
001039                                                                          
001040     MOVE WC-GID-RFF-REF-QUAL    TO RFF-1153-REFERENCE-QUAL               
001041                                                                          
001042     MOVE IN-KLI-IDORDNR5        TO WS-IDORDNR5                           
001043     MOVE IN-KLI-IDKOLLI         TO WS-IDKOLLI                            
001044     MOVE WS-ORDER-KLI-REF       TO RFF-1154-REFERENCE-NO                 
001045     .                                                                    
001046     EJECT                                                                
001047 S01-LAES-W47640  SECTION.                                                
001048                                                                          
001049     READ W47640 INTO IN-AREA                                             
001050     AT END                                                               
001051        MOVE HIGH-VALUE TO IN-AREA                                        
001052        SET W47640-EOF TO TRUE                                            
001053                                                                          
001054     NOT AT END                                                           
001055        MOVE 'W47640' TO POSTSUM-FDNAMN                                   
001056        MOVE 'W47641D1' TO POSTSUM-DDNAMN2                                
001057        MOVE IN-IDPTYP TO POSTSUM-TRANSTYP                                
001058        CALL POSTSUM USING POSTSUM-PARM                                   
001059     END-READ                                                             
001060     .                                                                    
001061     EJECT                                                                
001062 S11-POSTSUM-UTPOST SECTION.                                              
001063                                                                          
001064     MOVE EDI-IDPTYP TO POSTSUM-TRANSTYP                                  
001065     MOVE 'W47641' TO POSTSUM-FDNAMN                                      
001066     MOVE 'W47641D2' TO POSTSUM-DDNAMN2                                   
001067     CALL POSTSUM USING POSTSUM-PARM                                      
001068     .                                                                    
001069     EJECT                                                                
001070 S20-SKRIV-UNB-POST SECTION.                                              
001071                                                                          
001072     INITIALIZE UNB-AREA                                                  
001073                                                                          
001074     MOVE WC-UNB-IDPTYP          TO UNB-IDPTYP                            
001075     MOVE WC-UNB-LENGTH          TO UNB-LENGTH                            
001076                                                                          
001077     MOVE WC-UNB-UNOA            TO UNB-0001-SYNTAX-ID                    
001078     MOVE WC-UNB-2               TO UNB-0002-SYNTAX-VERSION-NO            
001079     MOVE WC-UNB-CDC-CAR-PARTS   TO UNB-0004-SENDER-ID                    
001080                                                                          
001081     MOVE SPAR-HUV-IDDC          TO WS-IDDC                               
001082     MOVE SPAR-HUV-IDDISTR       TO TEST-IDDISTR                          
001083                                                                          
001084     IF (CDC-SE AND DIST87-NIGHT-PLUS) OR                                 
001085        (DDC-SE AND DIST87-NIGHT-PLUS) OR                                 
001086        (DDC-BE AND DIST87-NIGHT-PLUS) OR                                 
001087        (DDC-DE AND DIST87-NIGHT-PLUS) OR                                 
001088        (SDC-NL AND DIST87-NIGHT-PLUS)                                    
001089       MOVE WC-UNB-NIGHT-PLUS-SCHENKER TO UNB-0010-RECIPIENT-ID           
001090     END-IF                                                               
001091*                                                                         
001092*                                                                         
001093     IF (CDC-SE    AND DIST87-LAGERMAX) OR                                
001094        (DDC-SE    AND DIST87-LAGERMAX) OR                                
001095        (DDC-BE    AND DIST87-LAGERMAX) OR                                
001096        (DDC-DE    AND DIST87-LAGERMAX) OR                                
001097        (DDC-FR    AND DIST87-LAGERMAX) OR                                
001098        (SDC-AT    AND DIST87-LAGERMAX) OR                                
001099        (SDC-NL    AND DIST87-LAGERMAX) OR                                
001100        (LDC-NL-3R AND DIST87-LAGERMAX) OR                                
001101        (CDC-SE    AND DIST87-LAGERMAX-LYNK) OR                           
001102        (DDC-SE    AND DIST87-LAGERMAX-LYNK) OR                           
001103        (DDC-BE    AND DIST87-LAGERMAX-LYNK) OR                           
001104        (DDC-DE    AND DIST87-LAGERMAX-LYNK) OR                           
001105        (SDC-AT    AND DIST87-LAGERMAX-LYNK) OR                           
001106        (SDC-NL    AND DIST87-LAGERMAX-LYNK)                              
001107       MOVE WC-UNB-LAGERMAX    TO UNB-0010-RECIPIENT-ID                   
001108     END-IF                                                               
001109*                                                                         
001110*                                                                         
001111*    IF (CDC-SE    AND DIST87-TRUCKWHEEL) OR                              
001112*       (DDC-SE    AND DIST87-TRUCKWHEEL) OR                              
001113*       (DDC-DE    AND DIST87-TRUCKWHEEL) OR                              
001114*       (SDC-NL    AND DIST87-TRUCKWHEEL) OR                              
001115*       (LDC-FR-3P AND DIST87-TRUCKWHEEL)                                 
001116*                                                                         
001117     IF DIST87-TRUCKWHEEL                                                 
001118       MOVE WC-UNB-TRUCKWHEEL  TO UNB-0010-RECIPIENT-ID                   
001119     END-IF                                                               
001120*                                                                         
001121*                                                                         
001122     IF (CDC-SE    AND DIST87-GALLIKER) OR                                
001123        (SDC-NL    AND DIST87-GALLIKER) OR                                
001124        (DDC-SE    AND DIST87-GALLIKER) OR                                
001125        (DDC-BE    AND DIST87-GALLIKER) OR                                
001126        (DDC-DE    AND DIST87-GALLIKER) OR                                
001127        (LDC-CH-3H AND DIST87-GALLIKER)                                   
001128       MOVE WC-UNB-GALLIKER    TO UNB-0010-RECIPIENT-ID                   
001129     END-IF                                                               
001130*                                                                         
001131*                                                                         
001132     IF (CDC-SE    AND DIST87-DANX)          OR                           
001133        (DDC-SE    AND DIST87-DANX)          OR                           
001134        (DDC-NO    AND DIST87-DANX)          OR                           
001135        (LDC-FI-3O AND DIST87-DANX)          OR                           
001136        (CDC-SE    AND DIST87-DANX-DK)       OR                           
001137        (DDC-SE    AND DIST87-DANX-DK)       OR                           
001138        (DDC-BE    AND DIST87-DANX-DK)       OR                           
001139        (DDC-DE    AND DIST87-DANX-DK)       OR                           
001140        (LDC-SE-1B AND DIST87-DANX-DK)       OR                           
001141        (CDC-SE    AND DIST87-DANX-SE)       OR                           
001142        (DDC-SE    AND DIST87-DANX-SE)       OR                           
001143        (LDC-SE-1C AND DIST87-DANX-SE)       OR                           
001144        (CDC-SE    AND DIST87-DANX-LYNK)     OR                           
001145        (DDC-SE    AND DIST87-DANX-LYNK)     OR                           
001146        (LDC-FI-3O AND DIST87-DANX-LYNK)     OR                           
001147        (DDC-SE    AND DIST87-DANX-INT)      OR                           
001148        (DDC-SE    AND DIST87-DANX-POLESTAR) OR                           
001149        (CDC-SE    AND DIST87-DANX-POLEN)    OR                           
001150        (LDC-PL-3S AND DIST87-DANX-POLEN)    OR                           
001151        (SDC-NL    AND DIST87-DANX-POLEN)    OR                           
001160        (DDC-SE    AND DIST87-DANX-POLEN)    OR                           
001170        (DDC-DE    AND DIST87-DANX-POLEN)                                 
001171       MOVE WC-UNB-DANX        TO UNB-0010-RECIPIENT-ID                   
001172     END-IF                                                               
001173*                                                                         
001174*                                                                         
001175     IF (CDC-SE    AND DIST87-SCHENKER) OR                                
001176        (DDC-SE    AND DIST87-SCHENKER)                                   
001177       MOVE WC-UNB-SCHENKER    TO UNB-0010-RECIPIENT-ID                   
001178     END-IF                                                               
001179*                                                                         
001180*                                                                         
001181     IF (CDC-SE    AND DIST87-BCUBE)      OR                              
001182        (DDC-SE    AND DIST87-BCUBE)      OR                              
001183        (DDC-FR    AND DIST87-BCUBE)      OR                              
001184        (DDC-DE    AND DIST87-BCUBE)      OR                              
001185        (DDC-BE    AND DIST87-BCUBE)      OR                              
001186        (SDC-IT    AND DIST87-BCUBE)      OR                              
001187        (SDC-NL    AND DIST87-BCUBE)      OR                              
001188        (LDC-IT-3D AND DIST87-BCUBE)      OR                              
001189        (LDC-IT-3F AND DIST87-BCUBE)      OR                              
001190        (CDC-SE    AND DIST87-BCUBE-PLUS) OR                              
001191        (SDC-NL    AND DIST87-BCUBE-PLUS) OR                              
001192        (DDC-SE    AND DIST87-BCUBE-PLUS) OR                              
001193        (DDC-FR    AND DIST87-BCUBE-PLUS) OR                              
001194        (DDC-DE    AND DIST87-BCUBE-PLUS) OR                              
001195        (LDC-IT-3F AND DIST87-BCUBE-PLUS)                                 
001196       MOVE WC-UNB-BCUBE       TO UNB-0010-RECIPIENT-ID                   
001197     END-IF                                                               
001198*                                                                         
001199*    IF (CDC-SE    AND DIST87-NEOVIA-ES)     OR                           
001200****    (CDC-SE    AND DIST87-NEOVIA-AFRIKA) OR                           
001201*       (DDC-SE    AND DIST87-NEOVIA-ES)     OR                           
001202****    (DDC-SE    AND DIST87-NEOVIA-AFRIKA) OR                           
001203*       (DDC-DE    AND DIST87-NEOVIA-ES)     OR                           
001204*       (DDC-FR    AND DIST87-NEOVIA-ES)     OR                           
001205*       (SDC-ES    AND DIST87-NEOVIA-ES)     OR                           
001206*       (SDC-NL    AND DIST87-NEOVIA-ES)     OR                           
001207*       (SDC-ES    AND DIST87-NEOVIA-AFRIKA)                              
001208*                                                                         
001209     IF DIST87-NEOVIA-ES                                                  
001210       MOVE WC-UNB-NEOVIA      TO UNB-0010-RECIPIENT-ID                   
001220     END-IF                                                               
001221*                                                                         
001222     IF SDC-ES     AND DIST87-NEOVIA-AFRIKA                               
001223       MOVE WC-UNB-NEOVIA      TO UNB-0010-RECIPIENT-ID                   
001224     END-IF                                                               
001225*                                                                         
001226     IF (LDC-SE-1A AND DIST87-DHL)    OR                                  
001227        (LDC-SE-1B AND DIST87-DHL)    OR                                  
001228        (LDC-SE-1D AND DIST87-DHL)    OR                                  
001229        (LDC-SE-1E AND DIST87-DHL)    OR                                  
001230        (CDC-SE    AND DIST87-DHL-NO) OR                                  
001240        (DDC-SE    AND DIST87-DHL-NO) OR                                  
001241        (DDC-NO    AND DIST87-DHL-NO) OR                                  
001242        (LDC-NO-3J AND DIST87-DHL-NO)                                     
001243       MOVE WC-UNB-DHL         TO UNB-0010-RECIPIENT-ID                   
001244     END-IF                                                               
001245*                                                                         
001246     MOVE FUNCTION CURRENT-DATE(3:8) TO UNB-0017-DATE                     
001247     MOVE FUNCTION CURRENT-DATE(9:4) TO UNB-0019-TIME                     
001248                                                                          
001249     WRITE UT-UNB-POST               FROM UNB-AREA                        
001250     MOVE UNB-AREA(1:3)              TO EDI-IDPTYP                        
001260     PERFORM S11-POSTSUM-UTPOST                                           
001261     .                                                                    
001262     EJECT                                                                
001263 S21-SKRIV-SKEPPN-INFO SECTION.                                           
001264                                                                          
001265     WRITE UT-UNH-POST                FROM UNH-AREA                       
001266     MOVE UNH-AREA (1:3)              TO EDI-IDPTYP                       
001267     PERFORM S11-POSTSUM-UTPOST                                           
001268                                                                          
001269     WRITE UT-BGM-POST                FROM BGM-AREA                       
001270     MOVE BGM-AREA(1:3)               TO EDI-IDPTYP                       
001271     PERFORM S11-POSTSUM-UTPOST                                           
001272                                                                          
001273     WRITE UT-DTM-POST                FROM DTM-AREA                       
001274     MOVE DTM-AREA(1:3)               TO EDI-IDPTYP                       
001275     PERFORM S11-POSTSUM-UTPOST                                           
001276                                                                          
001277     WRITE UT-MOA-POST                FROM MOA-AREA                       
001278     MOVE MOA-AREA(1:3)               TO EDI-IDPTYP                       
001279     PERFORM S11-POSTSUM-UTPOST                                           
001280                                                                          
001281     WRITE UT-FTX-POST                FROM FTX-AREA                       
001282     MOVE FTX-AREA(1:3)               TO EDI-IDPTYP                       
001283     PERFORM S11-POSTSUM-UTPOST                                           
001284                                                                          
001285     WRITE UT-VIKT-CNT-POST           FROM VIKT-CNT-AREA                  
001286     MOVE VIKT-CNT-AREA(1:3)          TO EDI-IDPTYP                       
001287     PERFORM S11-POSTSUM-UTPOST                                           
001288                                                                          
001289     WRITE UT-KOLLI-CNT-POST          FROM KOLLI-CNT-AREA                 
001290     MOVE KOLLI-CNT-AREA(1:3)         TO EDI-IDPTYP                       
001291     PERFORM S11-POSTSUM-UTPOST                                           
001292                                                                          
001293     WRITE UT-VOLYM-CNT-POST          FROM VOLYM-CNT-AREA                 
001294     MOVE VOLYM-CNT-AREA(1:3)         TO EDI-IDPTYP                       
001295     PERFORM S11-POSTSUM-UTPOST                                           
001296     .                                                                    
001297     EJECT                                                                
001298 S22-SKRIV-KOLLI-GEN-INFO SECTION.                                        
001299                                                                          
001300     IF SKRIVA-TDT-POST                                                   
001301       WRITE UT-TDT-POST             FROM TDT-AREA                        
001302       MOVE TDT-AREA(1:3)            TO EDI-IDPTYP                        
001303       PERFORM S11-POSTSUM-UTPOST                                         
001304                                                                          
001305       MOVE NEJ                      TO SKRIVA-TDT-POST-SW                
001306     END-IF                                                               
001307                                                                          
001308     WRITE UT-CNI-POST               FROM CNI-AREA                        
001309     MOVE CNI-AREA(1:3)              TO EDI-IDPTYP                        
001310     PERFORM S11-POSTSUM-UTPOST                                           
001311                                                                          
001312     WRITE UT-LOC-POST               FROM LOC-AREA                        
001313     MOVE LOC-AREA(1:3)              TO EDI-IDPTYP                        
001314     PERFORM S11-POSTSUM-UTPOST                                           
001315                                                                          
001316     WRITE UT-SEN-NAD-POST           FROM SEN-NAD-AREA                    
001317     MOVE SEN-NAD-AREA(1:3)          TO EDI-IDPTYP                        
001318     PERFORM S11-POSTSUM-UTPOST                                           
001319                                                                          
001320     WRITE UT-REC-NAD-POST           FROM REC-NAD-AREA                    
001321     MOVE REC-NAD-AREA(1:3)          TO EDI-IDPTYP                        
001322     PERFORM S11-POSTSUM-UTPOST                                           
001323     .                                                                    
001324     EJECT                                                                
001325 S23-SKRIV-KOLLI-INFO SECTION.                                            
001326                                                                          
001327     WRITE UT-GID-POST               FROM GID-AREA                        
001328     MOVE GID-AREA(1:3)              TO EDI-IDPTYP                        
001329     PERFORM S11-POSTSUM-UTPOST                                           
001330                                                                          
001331     IF SKRIVA-HAN-POST                                                   
001332       WRITE UT-HAN-POST             FROM HAN-AREA                        
001333       MOVE HAN-AREA(1:3)            TO EDI-IDPTYP                        
001334       PERFORM S11-POSTSUM-UTPOST                                         
001335     END-IF                                                               
001336                                                                          
001337     WRITE UT-GID-MOA-POST           FROM MOA-AREA                        
001338     MOVE MOA-AREA(1:3)              TO EDI-IDPTYP                        
001339     PERFORM S11-POSTSUM-UTPOST                                           
001340                                                                          
001341     WRITE UT-GID-NAD-POST           FROM GID-NAD-AREA                    
001342     MOVE GID-NAD-AREA(1:3)          TO EDI-IDPTYP                        
001343     PERFORM S11-POSTSUM-UTPOST                                           
001344                                                                          
001345     WRITE UT-MEA-VIKT-POST          FROM VIKT-MEA-AREA                   
001346     MOVE VIKT-MEA-AREA(1:3)         TO EDI-IDPTYP                        
001347     PERFORM S11-POSTSUM-UTPOST                                           
001348                                                                          
001349     WRITE UT-MEA-VOLYM-POST         FROM VOLYM-MEA-AREA                  
001350     MOVE VOLYM-MEA-AREA(1:3)        TO EDI-IDPTYP                        
001351     PERFORM S11-POSTSUM-UTPOST                                           
001352                                                                          
001353     WRITE UT-DIM-POST               FROM DIM-AREA                        
001354     MOVE DIM-AREA(1:3)              TO EDI-IDPTYP                        
001355     PERFORM S11-POSTSUM-UTPOST                                           
001356                                                                          
001357     WRITE UT-GID-RFF-POST           FROM GID-RFF-AREA                    
001358     MOVE GID-RFF-AREA(1:3)          TO EDI-IDPTYP                        
001359     PERFORM S11-POSTSUM-UTPOST                                           
001360     .                                                                    
001361     EJECT                                                                
001362 S31-SKRIV-001-POST SECTION.                                              
001363                                                                          
001364     INITIALIZE 001-AREA                                                  
001365                                                                          
001366     MOVE WC-001-IDPTYP          TO 001-IDPTYP                            
001367     MOVE WC-001-LENGTH          TO 001-LENGTH                            
001368                                                                          
001369     MOVE WC-001-VOLVO-PARTS     TO 001-SNODE-SEN-COMMON-NODE             
001370     MOVE WC-001-VOLVO-AMTRIX    TO 001-RNODE-REC-COMMON-NODE             
001371     MOVE WC-001-IFCSUM95        TO 001-VFILE-VIRTUAL-FILE-NAME           
001372                                                                          
001373     MOVE FUNCTION CURRENT-DATE(3:8) TO                                   
001374                                 001-VFDATE-VIR-FILE-DATE                 
001375     MOVE FUNCTION CURRENT-DATE(9:4) TO WS-CURRENT-TIME                   
001376                                 001-VFTIME-VIR-FILE-TIME                 
001377                                                                          
001378     WRITE UT-001-POST               FROM 001-AREA                        
001379     MOVE 001-AREA(1:3)              TO EDI-IDPTYP                        
001380     PERFORM S11-POSTSUM-UTPOST                                           
001381     .                                                                    
001382     EJECT                                                                
001383 S34-SKRIV-003-POST SECTION.                                              
001384                                                                          
001385     INITIALIZE 003-AREA                                                  
001386                                                                          
001387     MOVE WC-003-IDPTYP          TO 003-IDPTYP                            
001388     MOVE WC-003-LENGTH          TO 003-LENGTH                            
001389                                                                          
001390     WRITE UT-003-POST           FROM 003-AREA                            
001391     MOVE 003-AREA(1:3)          TO EDI-IDPTYP                            
001392     PERFORM S11-POSTSUM-UTPOST                                           
001393     .                                                                    
001394     EJECT                                                                
001395 Z-FINIT SECTION.                                                         
001396     CLOSE W47640                                                         
001397           W47641                                                         
001398     SKIP2                                                                
001399     MOVE 'S' TO POSTSUM-OPKOD                                            
001400     CALL POSTSUM USING POSTSUM-PARM                                      
001401     .                                                                    
001402     EJECT                                                                
