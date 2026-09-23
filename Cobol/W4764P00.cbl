000001 ID DIVISION.                                                             
000002 PROGRAM-ID.     W4764P00.                                                
000003 AUTHOR.         CAMELIA OLGRENER.                                        
000004 DATE-WRITTEN.   OKT. 2020.                                               
000005 DATE-COMPILED.                                                           
000006                                                                          
000007*    FUNKTION:                                                            
000008*        SKAPAR FIL FÖR EDI-ÖVERFÖRING AV TRPINFO-PACKING SPEC.           
000009*        TILL DIVERSE TRANSPORTÖRER.                                      
000010*                                                                         
000011*                                                                         
000012*    ABENDKODER:                                                          
000013*        U0016 -  . . . .                                                 
000014*        U1000 -  . . . .                                                 
000015*                                                                         
000016*                                                                         
000017*    STORY 3421774  23/08-17 NEW SELECTION NEOVIA, TRUCK&WHEEL            
000018*                                                                         
000019     SKIP3                                                                
000020 ENVIRONMENT DIVISION.                                                    
000021     SKIP2                                                                
000030 INPUT-OUTPUT SECTION.                                                    
000031                                                                          
000032 FILE-CONTROL.                                                            
000033     SKIP2                                                                
000034*          --- SKEPPNINGSPOSTER                                           
000035     SELECT W4764O                     ASSIGN TO W4764PD1.                
000036     SKIP2                                                                
000037*          --- FIL FÖR ÖVERSÄTTNING TILL EDI-FORMAT                       
000038     SELECT W4764P                     ASSIGN TO W4764PD2.                
000039     SKIP2                                                                
000040     EJECT                                                                
000041 DATA DIVISION.                                                           
000042     SKIP2                                                                
000043 FILE SECTION.                                                            
000044     SKIP3                                                                
000045 FD  W4764O                                                               
000046     RECORDING       V                                                    
000047     BLOCK CONTAINS  0.                                                   
000048                                                                          
000049 01  INPOST.                                                              
000050*    03  -COPY W4764O21 -L.                                               
000051*                                                                         
000052     EJECT                                                                
000053 FD  W4764P                                                               
000054     RECORDING       V                                                    
000055     BLOCK CONTAINS  0.                                                   
000056                                                                          
000057 01  UT-001-POST.                                                         
000058*    03  -COPY WEDI001G -PRE UT-                                          
000059                                                                          
000060 01  UT-UNB-POST.                                                         
000061*    03  -COPY WEDIUNBA -PRE UT-                                          
000062                                                                          
000063 01  UT-UNH-POST.                                                         
000064*    03  -COPY WEDIUNHA -PRE UT-.                                         
000065                                                                          
000066 01  UT-BGM-POST.                                                         
000067*    03  -COPY WEDIBGMA -PRE UT-.                                         
000068                                                                          
000069 01  UT-DTM-POST.                                                         
000070*    03  -COPY WEDIDTMA -PRE UT-.                                         
000080                                                                          
000081 01  UT-CU-RFF-POST.                                                      
000082*    03  -COPY WEDIRFFA -PRE UT-CU-.                                      
000083                                                                          
000084 01  UT-SEN-NAD-POST.                                                     
000085*    03  -COPY WEDINADA -PRE UT-SEN-.                                     
000086                                                                          
000087 01  UT-LOC-POST.                                                         
000088*    03  -COPY WEDILOCA -PRE UT-.                                         
000089                                                                          
000090 01  UT-TDT-POST.                                                         
000091*    03  -COPY WEDITDTA -PRE UT-.                                         
000092                                                                          
000093 01  UT-EQD-POST.                                                         
000094*    03  -COPY WEDIEQDA -PRE UT-.                                         
000095                                                                          
000096 01  UT-CPS-POST.                                                         
000097*    03  -COPY WEDICPSA -PRE UT-.                                         
000098                                                                          
000099 01  UT-PAC-POST.                                                         
000100*    03  -COPY WEDIPACA -PRE UT-.                                         
000110                                                                          
000111 01  UT-VKB-MEA-POST.                                                     
000112*    03  -COPY WEDIMEAA -PRE UT-VKB- .                                    
000113                                                                          
000114 01  UT-VKN-MEA-POST.                                                     
000115*    03  -COPY WEDIMEAA -PRE UT-VKN- .                                    
000116                                                                          
000117 01  UT-VLB-MEA-POST.                                                     
000118*    03  -COPY WEDIMEAA -PRE UT-VLB- .                                    
000119                                                                          
000120 01  UT-LIN-POST.                                                         
000121*    03  -COPY WEDILINA -PRE UT-.                                         
000122                                                                          
000123 01  UT-IMD-POST.                                                         
000124*    03  -COPY WEDIIMDA -PRE UT-.                                         
000125                                                                          
000126 01  UT-QTY-POST.                                                         
000127*    03  -COPY WEDIQTYA -PRE UT-.                                         
000128                                                                          
000129 01  UT-REC-NAD-POST.                                                     
000130*    03  -COPY WEDINADA -PRE UT-REC-.                                     
000131                                                                          
000132 01  UT-FTX-POST.                                                         
000133*    03  -COPY WEDIFTXA -PRE UT-.                                         
000134                                                                          
000135 01  UT-ON-RFF-POST.                                                      
000136*    03  -COPY WEDIRFFA -PRE UT-ON-.                                      
000137                                                                          
000138 01  UT-KLI-RFF-POST.                                                     
000139*    03  -COPY WEDIRFFA -PRE UT-KLI-.                                     
000140                                                                          
000141 01  UT-CR-RFF-POST.                                                      
000142*    03  -COPY WEDIRFFA -PRE UT-CR-.                                      
000143                                                                          
000144 01  UT-003-POST.                                                         
000145*    03  -COPY WEDI003G -PRE UT-.                                         
000146                                                                          
000147     EJECT                                                                
000148 WORKING-STORAGE SECTION.                                                 
000149                                                                          
000150                                                                          
000160*    -- CHECKED BY WY2000                                                 
000170 77  IDPGM                       PIC X(8)    VALUE 'W4764P00'.            
000171 77  JA                          PIC X       VALUE 'J'.                   
000172 77  NEJ                         PIC X       VALUE 'N'.                   
000173 77  PUNKT                       PIC X       VALUE '.'.                   
000174 77  WS-IDART-RAK                PIC 9(4)    VALUE ZERO.                  
000175 77  WS-CPS-RAK                  PIC 9(3)    VALUE ZERO.                  
000176 77  WS-IDDISTR                  PIC S9(5)   VALUE ZERO.                  
000177 77  WS-IDKUNDNR                 PIC S9(7)   VALUE ZERO.                  
000178 77  WS-IDSKEPPN                 PIC S9(7)   VALUE ZERO.                  
000179 77  WS-IDKONTO                  PIC 9(9)    VALUE ZERO.                  
000180 77  WS-IDPRODNR                 PIC S9(7)   VALUE ZERO.                  
000181                                                                          
000182 77  W4764O-EOF-SW               PIC X       VALUE 'N'.                   
000183     88  W4764O-EOF                          VALUE 'J'.                   
000184                                                                          
000185 77  SKRIVA-HAN-POST-SW          PIC X       VALUE 'J'.                   
000186     88  SKRIVA-HAN-POST                     VALUE 'J'.                   
000187                                                                          
000188 77  SKRIVA-TDT-POST-SW          PIC X       VALUE 'J'.                   
000189     88  SKRIVA-TDT-POST                     VALUE 'J'.                   
000190                                                                          
000191 77  UTSKRIFT-SW                 PIC X       VALUE 'J'.                   
000192     88  SKRIV-SKEPPNING                     VALUE 'J'.                   
000193                                                                          
000194     EJECT                                                                
000195 01  ARBETSFALT.                                                          
000196                                                                          
000197     03 WS-NUMBER-OF-SEGMENTS       PIC 9(06) VALUE ZERO.                 
000198     03 WS-MESSAGE-REF-NO           PIC 9(06) VALUE ZERO.                 
000199                                                                          
000200     03 IN-IDPTYP                   PIC X(03) VALUE SPACE.                
000201     03 EDI-IDPTYP                  PIC X(03) VALUE SPACE.                
000202                                                                          
000203     03 WS-ORDER-KLI-REF.                                                 
000204        05 WS-IDORDNR5              PIC 9(05) VALUE ZERO.                 
000205        05 WS-TECKEN                PIC X(01) VALUE '-'.                  
000206        05 WS-IDKOLLI               PIC 9(05) VALUE ZERO.                 
000207                                                                          
000208     03 WS-VKORDBTO                 PIC 9(6)V9 VALUE ZERO.                
000209     03 WS-VKORDBTO-ALFA REDEFINES WS-VKORDBTO.                           
000210        05 WS-VKORDBTO-W            PIC X(7).                             
000220                                                                          
000221     03 WS-VLORDBTO                 PIC 9(4)V9(3) VALUE ZERO.             
000222     03 WS-VLORDBTO-ALFA REDEFINES WS-VLORDBTO.                           
000223        05 WS-VLORDBTO-W            PIC X(7).                             
000224                                                                          
000225     03 WS-VKARTNTO                 PIC 9(6)V9(3) VALUE ZERO.             
000226     03 WS-VKARTNTO-ALFA REDEFINES WS-VKARTNTO.                           
000227        05 WS-VKARTNTO-W              PIC X(9).                           
000228                                                                          
000229     03 WS-VLORDBTO-TOT             PIC 9(4)V9(3) VALUE ZERO.             
000230     03 WS-VLORDBTO-TOT-NUM REDEFINES WS-VLORDBTO-TOT.                    
000231        05 WS-VLORDBTO-TOT-W        PIC 9(7).                             
000232                                                                          
000233     03 WS-VKORDBTO-TOT             PIC 9(6)V9 VALUE ZERO.                
000234     03 WS-VKORDBTO-TOT-NUM REDEFINES WS-VKORDBTO-TOT.                    
000235        05 WS-VKORDBTO-TOT-W        PIC 9(7).                             
000236                                                                          
000237     03 WS-SUORDV                   PIC 9(9)V9(2) VALUE ZERO.             
000238     03 WS-SUORDV-NUM REDEFINES WS-SUORDV.                                
000239        05 WS-SUORDV-W              PIC 9(11).                            
000240                                                                          
000241     03 WS-CARRIER-ID.                                                    
000242        05 WS-IDTRPTNR              PIC 9(03) VALUE ZERO.                 
000243        05 WS-TECKEN                PIC X(01) VALUE ' '.                  
000244        05 WS-IDLBBET               PIC X(12) VALUE SPACE.                
000245                                                                          
000246     03 WS-ADPOST-PNRORT.                                                 
000247     05 WS-ADPOSTNR                 PIC X(10) VALUE SPACE.                
000248        05 WS-ADCITY                PIC X(25) VALUE SPACE.                
000249                                                                          
000250 01  KONSTANTER.                                                          
000251                                                                          
000252     03 001-UPPGIFTER.                                                    
000253                                                                          
000254       05 WC-001-IDPTYP                 PIC X(03) VALUE '001'.            
000255       05 WC-001-LENGTH                 PIC 9(03) VALUE  073 .            
000256       05 WC-001-VOLVO-PARTS            PIC X(04) VALUE 'VPAR'.           
000257       05 WC-001-VOLVO-AMTRIX           PIC X(04) VALUE 'VAMP'.           
000258       05 WC-001-DESADV                 PIC X(06)                         
000259                                        VALUE 'DESADV'.                   
000260                                                                          
000261                                                                          
000262     03 UNB-UPPGIFTER.                                                    
000263                                                                          
000264       05 WC-UNB-IDPTYP                 PIC X(03) VALUE 'UNB'.            
000265       05 WC-UNB-LENGTH                 PIC 9(03) VALUE  125.             
000266       05 WC-UNB-UNOA                   PIC X(04) VALUE 'UNOA'.           
000267       05 WC-UNB-2                      PIC X(01) VALUE '2'.              
000268       05 WC-UNB-CDC-CAR-PARTS          PIC X(14)                         
000269                                        VALUE '01441         '.           
000270       05 WC-UNB-BCUBE                  PIC X(14)                         
000271                                        VALUE '32494         '.           
000272       05 WC-UNB-TRUCKWHEEL             PIC X(14)                         
000273                                        VALUE '358828        '.           
000274       05 WC-UNB-NEOVIA                 PIC X(14)                         
000275                                        VALUE '319323        '.           
000276                                                                          
000277     03 UNH-UPPGIFTER.                                                    
000278                                                                          
000279       05 WC-UNH-IDPTYP                 PIC X(03) VALUE 'UNH'.            
000280       05 WC-UNH-LENGTH                 PIC 9(03) VALUE  072.             
000281       05 WC-UNH-TYP                    PIC X(06) VALUE 'DESADV'.         
000282       05 WC-UNH-VERSION-NO             PIC X(03) VALUE 'D  '.            
000283       05 WC-UNH-REL-NO                 PIC X(03) VALUE '07A'.            
000284       05 WC-UNH-AGENCY                 PIC X(02) VALUE 'UN'.             
000285       05 WC-UNH-GBS-NO                 PIC X(06) VALUE 'GBSL12'.         
000286                                                                          
000287                                                                          
000288     03 BGM-UPPGIFTER.                                                    
000289                                                                          
000290       05 WC-BGM-IDPTYP                 PIC X(03) VALUE 'BGM'.            
000291       05 WC-BGM-LENGTH                 PIC 9(03) VALUE  038.             
000292       05 WC-BGM-CARGO-MANIFEST         PIC X(03) VALUE '351'.            
000293                                                                          
000294                                                                          
000295     03 DTM-UPPGIFTER.                                                    
000296                                                                          
000297       05 WC-DTM-IDPTYP                 PIC X(03) VALUE 'DTM'.            
000298       05 WC-DTM-LENGTH                 PIC 9(03) VALUE  105.             
000299       05 WC-DTM-QUAL                   PIC X(03) VALUE '137'.            
000300       05 WC-DTM-CCYYMMDDHHMM           PIC X(03) VALUE '203'.            
000301                                                                          
000302                                                                          
000303     03 RFF-UPPGIFTER.                                                    
000304                                                                          
000305       05 WC-RFF-IDPTYP                 PIC X(03) VALUE 'RFF'.            
000306       05 WC-RFF-LENGTH                 PIC 9(03) VALUE  073.             
000307       05 WC-RFF-CU-QUAL                PIC X(03) VALUE 'CU '.            
000308       05 WC-RFF-ON-QUAL                PIC X(03) VALUE 'ON '.            
000309       05 WC-RFF-KLI-QUAL               PIC X(03) VALUE 'AAT'.            
000310       05 WC-RFF-CR-QUAL                PIC X(03) VALUE 'CR '.            
000311                                                                          
000312                                                                          
000313     03 NAD-UPPGIFTER.                                                    
000314                                                                          
000315       05 WC-NAD-IDPTYP                 PIC X(03) VALUE 'NAD'.            
000316       05 WC-NAD-LENGTH                 PIC 9(03) VALUE  198.             
000317       05 WC-NAD-DOCUMENT-SEN           PIC X(03) VALUE 'BY '.            
000318       05 WC-NAD-DOCUMENT-REC           PIC X(03) VALUE 'CN '.            
000319*                                                                         
000320       05 WC-NAD-VOLVO-NAME-IT          PIC X(23) VALUE                   
000321                                        'VOLVO CAR ITALIA S.P.A.'.        
000322       05 WC-NAD-VOLVO-POST-A-IT        PIC X(21) VALUE                   
000323                                        'VIA ENRICO MATTEI, 66'.          
000324       05 WC-NAD-VOLVO-TOWN-IT          PIC X(08) VALUE                   
000325                                        'BOLOGNA'.                        
000326       05 WC-NAD-VOLVO-ZIP-CODE-IT      PIC X(05) VALUE                   
000327                                        '40138'.                          
000328       05 WC-NAD-VOLVO-COUNTRY-IT       PIC X(03) VALUE 'IT '.            
000329       05 WC-NAD-REC-COUNTRY-IT         PIC X(03) VALUE 'IT '.            
000330*                                                                         
000331       05 WC-NAD-VOLVO-NAME-ES          PIC X(19) VALUE                   
000332                                        'VOLVO CAR ESPANA SL'.            
000333       05 WC-NAD-VOLVO-POST-A-ES        PIC X(26) VALUE                   
000334                                     'C/ JOSE LAZARO GALDIANO, 6'.        
000335       05 WC-NAD-VOLVO-TOWN-ES          PIC X(07) VALUE                   
000336                                        'MADRID'.                         
000337       05 WC-NAD-VOLVO-ZIP-CODE-ES      PIC X(05) VALUE                   
000338                                        '28036'.                          
000339       05 WC-NAD-VOLVO-COUNTRY-ES       PIC X(03) VALUE 'ES '.            
000340       05 WC-NAD-REC-COUNTRY-ES         PIC X(03) VALUE 'ES '.            
000342*                                                                         
000343       05 WC-NAD-VOLVO-NAME-FR          PIC X(16) VALUE                   
000344                                        'VOLVO CAR FRANCE'.               
000345       05 WC-NAD-VOLVO-POST-A-FR        PIC X(23) VALUE                   
000346                                        '131-151, RUE DU 1ER MAI'.        
000347       05 WC-NAD-VOLVO-TOWN-FR          PIC X(15) VALUE                   
000348                                        'NANTERRE CEDEX'.                 
000349       05 WC-NAD-VOLVO-ZIP-CODE-FR      PIC X(05) VALUE                   
000350                                        '92737'.                          
000351       05 WC-NAD-VOLVO-COUNTRY-FR       PIC X(03) VALUE 'FR '.            
000352       05 WC-NAD-REC-COUNTRY-FR         PIC X(03) VALUE 'FR '.            
000353*                                                                         
000365                                                                          
000366     03 LOC-UPPGIFTER.                                                    
000367                                                                          
000368       05 WC-LOC-IDPTYP                 PIC X(03) VALUE 'LOC'.            
000369       05 WC-LOC-LENGTH                 PIC 9(03) VALUE  041.             
000370       05 WC-LOC-QUAL                   PIC X(03) VALUE '7  '.            
000371       05 WC-LOC-AGREEMENT              PIC X(03) VALUE 'ZZZ'.            
000372                                                                          
000373                                                                          
000374     03 TDT-UPPGIFTER.                                                    
000375                                                                          
000376       05 WC-TDT-IDPTYP                 PIC X(03) VALUE 'TDT'.            
000377       05 WC-TDT-LENGTH                 PIC 9(03) VALUE  037.             
000378       05 WC-TDT-QUAL                   PIC X(03) VALUE '25 '.            
000379                                                                          
000380                                                                          
000381     03 EQD-UPPGIFTER.                                                    
000382                                                                          
000383       05 WC-EQD-IDPTYP                 PIC X(03) VALUE 'EQD'.            
000384       05 WC-EQD-LENGTH                 PIC 9(03) VALUE  038.             
000385       05 WC-EQD-QUAL                   PIC X(03) VALUE 'AH '.            
000386                                                                          
000387                                                                          
000388     03 CPS-UPPGIFTER.                                                    
000389                                                                          
000390       05 WC-CPS-IDPTYP                 PIC X(03) VALUE 'CPS'.            
000391       05 WC-CPS-LENGTH                 PIC 9(03) VALUE  024.             
000392       05 WC-CPS-SHIP-LEVEL             PIC X(03) VALUE '5  '.            
000393                                                                          
000394                                                                          
000395     03 PAC-UPPGIFTER.                                                    
000396                                                                          
000397       05 WC-PAC-IDPTYP                 PIC X(03) VALUE 'PAC'.            
000398       05 WC-PAC-LENGTH                 PIC 9(03) VALUE  008.             
000399                                                                          
000400                                                                          
000401     03 MEA-UPPGIFTER.                                                    
000402                                                                          
000403       05 WC-MEA-IDPTYP                 PIC X(03) VALUE 'MEA'.            
000404       05 WC-MEA-LENGTH                 PIC 9(03) VALUE  032.             
000405       05 WC-MEA-QUAL                   PIC X(03) VALUE 'AAX'.            
000406       05 WC-MEA-VKB-GROSS              PIC X(03) VALUE 'AAD'.            
000407       05 WC-MEA-VKN-NETT               PIC X(03) VALUE 'AAC'.            
000408       05 WC-MEA-VLB-GROSS              PIC X(03) VALUE 'ABJ'.            
000409       05 WC-MEA-VKB-KG                 PIC X(03) VALUE 'KGM'.            
000410       05 WC-MEA-VKN-KG                 PIC X(03) VALUE 'KGM'.            
000411       05 WC-MEA-VLB-MTQ                PIC X(03) VALUE 'MTQ'.            
000412                                                                          
000413                                                                          
000414     03 LIN-UPPGIFTER.                                                    
000415                                                                          
000416       05 WC-LIN-IDPTYP                 PIC X(03) VALUE 'LIN'.            
000417       05 WC-LIN-LENGTH                 PIC 9(03) VALUE  076.             
000418       05 WC-LIN-QUAL                   PIC X(03) VALUE 'IN '.            
000419                                                                          
000420                                                                          
000421     03 IMD-UPPGIFTER.                                                    
000422                                                                          
000423       05 WC-IMD-IDPTYP                 PIC X(03) VALUE 'IMD'.            
000424       05 WC-IMD-LENGTH                 PIC 9(03) VALUE  259.             
000425       05 WC-IMD-QUAL                   PIC X(03) VALUE 'A  '.            
000426                                                                          
000427                                                                          
000428     03 QTY-UPPGIFTER.                                                    
000429                                                                          
000430       05 WC-QTY-IDPTYP                 PIC X(03) VALUE 'QTY'.            
000431       05 WC-QTY-LENGTH                 PIC 9(03) VALUE  046.             
000432       05 WC-QTY-QUAL                   PIC X(03) VALUE '12 '.            
000433                                                                          
000434                                                                          
000435     03 FTX-UPPGIFTER.                                                    
000436                                                                          
000437       05 WC-FTX-IDPTYP                 PIC X(03) VALUE 'FTX'.            
000438       05 WC-FTX-LENGTH                 PIC 9(04) VALUE  2609.            
000439       05 WC-FTX-QUAL                   PIC X(03) VALUE 'COI'.            
000440                                                                          
000441                                                                          
000442     03 003-UPPGIFTER.                                                    
000443                                                                          
000444       05 WC-003-IDPTYP                 PIC X(03) VALUE '003'.            
000445       05 WC-003-LENGTH                 PIC 9(03) VALUE  073.             
000446                                                                          
000447 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
000448 01  FILLER REDEFINES DAGENS-DATUM.                                       
000449     03  DAGENS-DATUM-AAR        PIC 9(2).                                
000450     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
000451     03  DAGENS-DATUM-DAG        PIC 9(2).                                
000452     EJECT                                                                
000453                                                                          
000454 01  WS-CURRENT-DATE-TIME.                                                
000455     03 WS-CURRENT-DATE          PIC 9(8).                                
000456     03 WS-CURRENT-TIME          PIC 9(4).                                
000457                                                                          
000458     EJECT                                                                
000459*- - - - - - - - - - - - - -                                              
000460*      --- VALID IDDC CODES                                               
000461*                                                                         
000462*01    -COPY WWDC99                                                       
000463     EJECT                                                                
000464 01  TEST-IDDISTR       PIC 9(5)   COMP-3.                                
000465                                                                          
000466*01  FILLER -COPY WWDIST87    -RED TEST-IDDISTR.                          
000467     EJECT                                                                
000468     EJECT                                                                
000469 01  DYNAMISKA-SUBPROGRAM.                                                
000470*                                                                         
000471     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
000472     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000473     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000474     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
000475     SKIP2                                                                
000476*    --- PARAMETRAR TILL ABEND                                            
000477                                                                          
000478 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
000479 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
000480 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
000481     SKIP2                                                                
000482 01  FELTEXT.                                                             
000483     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
000484     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
000485     EJECT                                                                
000486*    --- PARAMETRAR TILL POSTSUM                                          
000487*                                                                         
000488*01  -COPY W0005   -PRE  POSTSUM-                                         
000489     EJECT                                                                
000490                                                                          
000491 01  IN-AREA-START               PIC X(24)   VALUE                        
000492                                 'IN-AREA-START  '.                       
000493     SKIP2                                                                
000494 01  IN-AREA.                                                             
000495     03  IN-DATA-AREA            PIC X(800).                              
000496                                                                          
000497*    03  E101-POST  -COPY W4764O01 -PRE IN-  -RED IN-DATA-AREA            
000498                                                                          
000499*    03  E111-POST  -COPY W4764O11 -PRE IN-  -RED IN-DATA-AREA            
000500                                                                          
000501*    03  E121-POST  -COPY W4764O21 -PRE IN-  -RED IN-DATA-AREA            
000502                                                                          
000503*    03  E131-POST  -COPY W4764O31 -PRE IN-  -RED IN-DATA-AREA            
000504                                                                          
000505     EJECT                                                                
000506 01  W-E101-SPAR-AREA            PIC X(24)   VALUE                        
000507                                 'E101-SPAR-AREA   '.                     
000508 01  W-SPAR-E101-AREA.                                                    
000509*    03  -COPY W4764O01    -PRE SPAR-.                                    
000510     EJECT                                                                
000511 01  W-E111-SPAR-AREA            PIC X(24)   VALUE                        
000512                                 'E111-SPAR-AREA   '.                     
000513 01  W-SPAR-E111-AREA.                                                    
000514*    03  -COPY W4764O11    -PRE SPAR-.                                    
000515     EJECT                                                                
000516 01  FILLER                      PIC X(24)   VALUE                        
000517                                 'UT-AREA-START  '.                       
000518                                                                          
000519 01  FILLER                      PIC X(24) VALUE '001-AREA'.              
000520 01  001-AREA.                                                            
000521*    03  -COPY WEDI001A                                                   
000522                                                                          
000523 01  FILLER                      PIC X(24) VALUE 'UNB-AREA'.              
000524 01  UNB-AREA.                                                            
000525*    03  -COPY WEDIUNBA.                                                  
000526                                                                          
000527 01  FILLER                      PIC X(24) VALUE 'UNH-AREA'.              
000528 01  UNH-AREA.                                                            
000529*    03  -COPY WEDIUNHA.                                                  
000530                                                                          
000531 01  FILLER                      PIC X(24) VALUE 'BGM-AREA'.              
000532 01  BGM-AREA.                                                            
000533*    03  -COPY WEDIBGMA.                                                  
000534                                                                          
000535 01  FILLER                      PIC X(24) VALUE 'DTM-AREA'.              
000536 01  DTM-AREA.                                                            
000537*    03  -COPY WEDIDTMA.                                                  
000538                                                                          
000539 01  FILLER                      PIC X(24) VALUE 'CU-RFF-AREA'.           
000540 01  CU-RFF-AREA.                                                         
000541*    03  -COPY WEDIRFFA -PRE CU- .                                        
000542                                                                          
000543 01  FILLER                      PIC X(24) VALUE 'SEN-NAD-AREA'.          
000544 01  SEN-NAD-AREA.                                                        
000545*    03  -COPY WEDINADA -PRE SEN- .                                       
000546                                                                          
000547 01  FILLER                      PIC X(24) VALUE 'LOC-AREA'.              
000548 01  LOC-AREA.                                                            
000549*    03  -COPY WEDILOCA.                                                  
000550                                                                          
000551 01  FILLER                      PIC X(24) VALUE 'TDT-AREA'.              
000552 01  TDT-AREA.                                                            
000553*    03  -COPY WEDITDTA.                                                  
000554                                                                          
000555 01  FILLER                      PIC X(24) VALUE 'EQD-AREA'.              
000556 01  EQD-AREA.                                                            
000557*    03  -COPY WEDIEQDA.                                                  
000558                                                                          
000559 01  FILLER                      PIC X(24) VALUE 'CPS-AREA'.              
000560 01  CPS-AREA.                                                            
000561*    03  -COPY WEDICPSA.                                                  
000562                                                                          
000563 01  FILLER                      PIC X(24) VALUE 'PAC-AREA'.              
000564 01  PAC-AREA.                                                            
000565*    03  -COPY WEDIPACA.                                                  
000566                                                                          
000567 01  FILLER                      PIC X(24) VALUE 'VKB-MEA-AREA'.          
000568 01  VKB-MEA-AREA.                                                        
000569*    03  -COPY WEDIMEAA -PRE VKB- .                                       
000570                                                                          
000571 01  FILLER                      PIC X(24) VALUE 'VKN-MEA-AREA'.          
000572 01  VKN-MEA-AREA.                                                        
000573*    03  -COPY WEDIMEAA -PRE VKN- .                                       
000574                                                                          
000575 01  FILLER                      PIC X(24) VALUE 'VLB-MEA-AREA'.          
000576 01  VLB-MEA-AREA.                                                        
000577*    03  -COPY WEDIMEAA -PRE VLB-.                                        
000578                                                                          
000579 01  FILLER                      PIC X(24) VALUE 'LIN-AREA'.              
000580 01  LIN-AREA.                                                            
000581*    03  -COPY WEDILINA.                                                  
000582                                                                          
000583 01  FILLER                      PIC X(24) VALUE 'IMD-AREA'.              
000584 01  IMD-AREA.                                                            
000585*    03  -COPY WEDIIMDA.                                                  
000586                                                                          
000587 01  FILLER                      PIC X(24) VALUE 'QTY-AREA'.              
000588 01  QTY-AREA.                                                            
000589*    03  -COPY WEDIQTYA.                                                  
000590                                                                          
000591 01  FILLER                      PIC X(24) VALUE 'REC-NAD-AREA'.          
000592 01  REC-NAD-AREA.                                                        
000593*    03  -COPY WEDINADA -PRE REC- .                                       
000594                                                                          
000595 01  FILLER                      PIC X(24) VALUE 'FTX-AREA'.              
000596 01  FTX-AREA.                                                            
000597*    03  -COPY WEDIFTXA.                                                  
000598                                                                          
000599 01  FILLER                      PIC X(24) VALUE 'ON-RFF-AREA'.           
000600 01  ON-RFF-AREA.                                                         
000601*    03  -COPY WEDIRFFA -PRE ON- .                                        
000602                                                                          
000603 01  FILLER                      PIC X(24) VALUE 'KLI-RFF-AREA'.          
000604 01  KLI-RFF-AREA.                                                        
000605*    03  -COPY WEDIRFFG -PRE KLI- .                                       
000606                                                                          
000607 01  FILLER                      PIC X(24) VALUE 'CR-RFF-AREA'.           
000608 01  CR-RFF-AREA.                                                         
000609*    03  -COPY WEDIRFFA -PRE CR- .                                        
000610                                                                          
000611 01  FILLER                      PIC X(24) VALUE '003-AREA'.              
000612 01  003-AREA.                                                            
000613*    03  -COPY WEDI003G.                                                  
000614     EJECT                                                                
000615 PROCEDURE DIVISION.                                                      
000616                                                                          
000617 MAIN SECTION.                                                            
000618                                                                          
000619     PERFORM A-INIT                                                       
000620                                                                          
000621     PERFORM S01-LAES-W4764O                                              
000622                                                                          
000623     IF NOT W4764O-EOF                                                    
000624       PERFORM S31-SKRIV-001-POST                                         
000625                                                                          
000626       PERFORM UNTIL W4764O-EOF                                           
000627                                                                          
000628         IF IN-HUV-IDPTYP = 'E101'                                        
000629                                                                          
000630             MOVE IN-AREA        TO W-SPAR-E101-AREA                      
000631             MOVE IN-HUV-IDPTYP  TO IN-IDPTYP                             
000632             MOVE IN-HUV-IDDISTR TO TEST-IDDISTR                          
000633             PERFORM B-NOLLSTALL-SKEPPNING                                
000634             PERFORM C-BEHANDLA-SKEPPN-INFO                               
000635             PERFORM S20-SKRIV-UNB-POST                                   
000636             PERFORM S21-SKRIV-SKEPPN-INFO                                
000637                                                                          
000638         ELSE                                                             
000639           IF IN-KND-IDPTYP = 'E111'                                      
000640                                                                          
000641             MOVE IN-AREA          TO W-SPAR-E111-AREA                    
000642             MOVE IN-KND-IDPTYP    TO IN-IDPTYP                           
000643                                                                          
000644             PERFORM D-NOLLSTALL-KUND                                     
000645             ADD +1                TO WS-CPS-RAK                          
000646                                                                          
000647             PERFORM E-SKAPA-SKRIV-KUND-INFO                              
000648                                                                          
000649           ELSE                                                           
000650             IF IN-KLI-IDPTYP = 'E121'                                    
000651                                                                          
000652               PERFORM F-SPARA-KOLLI-INFO                                 
000653             ELSE                                                         
000654               IF IN-KLI-IDPTYP = 'E131'                                  
000655                                                                          
000656                 PERFORM G-SKAPA-SKRIV-RAD-INFO                           
000657               END-IF                                                     
000658             END-IF                                                       
000659           END-IF                                                         
000660         END-IF                                                           
000661                                                                          
000662         PERFORM S01-LAES-W4764O                                          
000663       END-PERFORM                                                        
000664                                                                          
000665                                                                          
000666       PERFORM S34-SKRIV-003-POST                                         
000667     END-IF                                                               
000668     PERFORM Z-FINIT                                                      
000669                                                                          
000670     MOVE ZERO TO RETURN-CODE                                             
000671     GOBACK                                                               
000672                                                                          
000673     .                                                                    
000674     EJECT                                                                
000675 A-INIT SECTION.                                                          
000676                                                                          
000677     OPEN INPUT  W4764O                                                   
000678                                                                          
000679     OPEN OUTPUT W4764P                                                   
000680                                                                          
000681     ACCEPT DAGENS-DATUM  FROM DATE                                       
000682     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
000683                                                                          
000684     MOVE ZERO  TO WS-IDART-RAK                                           
000685     .                                                                    
000686     EJECT                                                                
000687                                                                          
000688 B-NOLLSTALL-SKEPPNING SECTION.                                           
000689                                                                          
000690     COMPUTE  WS-MESSAGE-REF-NO =                                         
000691              WS-MESSAGE-REF-NO + 1                                       
000692     MOVE ZERO             TO WS-NUMBER-OF-SEGMENTS                       
000693                              WS-CPS-RAK                                  
000694                                                                          
000695     INITIALIZE            UNH-AREA                                       
000696                           BGM-AREA                                       
000697                           DTM-AREA                                       
000698                           CU-RFF-AREA                                    
000699                           SEN-NAD-AREA                                   
000700                           LOC-AREA                                       
000701                           TDT-AREA                                       
000702                           EQD-AREA                                       
000703                           CPS-AREA                                       
000704                           PAC-AREA                                       
000705                           VKB-MEA-AREA                                   
000706                           VKN-MEA-AREA                                   
000707                           VLB-MEA-AREA                                   
000708                           LIN-AREA                                       
000709                           IMD-AREA                                       
000710                           QTY-AREA                                       
000711                           REC-NAD-AREA                                   
000712                           FTX-AREA                                       
000713                           ON-RFF-AREA                                    
000714                           KLI-RFF-AREA                                   
000715                           CR-RFF-AREA                                    
000716     .                                                                    
000717     EJECT                                                                
000718 C-BEHANDLA-SKEPPN-INFO SECTION.                                          
000719                                                                          
000720     PERFORM CA-SKAPA-UNH-POST                                            
000721     PERFORM CB-SKAPA-BGM-POST                                            
000722     PERFORM CC-SKAPA-DTM-POST                                            
000723     PERFORM CD-SKAPA-CU-RFF-POST                                         
000724     PERFORM CE-SKAPA-SEN-NAD-POST                                        
000725     PERFORM CF-SKAPA-LOC-POST                                            
000726     PERFORM CG-SKAPA-TDT-POST                                            
000727     PERFORM CH-SKAPA-EQD-POST                                            
000728     .                                                                    
000729     EJECT                                                                
000730 CA-SKAPA-UNH-POST SECTION.                                               
000731                                                                          
000732     MOVE WC-UNH-IDPTYP          TO UNH-IDPTYP                            
000733     MOVE WC-UNH-LENGTH          TO UNH-LENGTH                            
000734                                                                          
000735     MOVE WS-MESSAGE-REF-NO      TO                                       
000736                         UNH-0062-MESSAGE-REFERENCE                       
000737                                                                          
000738     MOVE WC-UNH-TYP             TO                                       
000739                         UNH-0065-MESSAGE-TYPE-ID                         
000740     MOVE WC-UNH-VERSION-NO      TO                                       
000741                         UNH-0052-MESSAGE-VERSION                         
000742     MOVE WC-UNH-REL-NO          TO                                       
000743                         UNH-0054-MESSAGE-RELEASE                         
000744     MOVE WC-UNH-AGENCY          TO                                       
000745                         UNH-0051-CONTROLING-AGENCY                       
000746     MOVE WC-UNH-GBS-NO          TO                                       
000747                         UNH-0057-ASSOCIATION-CODE                        
000748     .                                                                    
000749     EJECT                                                                
000750 CB-SKAPA-BGM-POST SECTION.                                               
000751                                                                          
000752     MOVE WC-BGM-IDPTYP          TO BGM-IDPTYP                            
000753     MOVE WC-BGM-LENGTH          TO BGM-LENGTH                            
000754     MOVE WC-BGM-CARGO-MANIFEST  TO BGM-1001-DOCUMENT-NAME                
000755     MOVE IN-HUV-IDSHIPM         TO BGM-1004-DOCUMENT-NUMBER              
000756     .                                                                    
000757     EJECT                                                                
000758 CC-SKAPA-DTM-POST SECTION.                                               
000759                                                                          
000760     MOVE WC-DTM-IDPTYP          TO DTM-IDPTYP                            
000761     MOVE WC-DTM-LENGTH          TO DTM-LENGTH                            
000762     MOVE WC-DTM-QUAL            TO DTM-2005-DATE-TIME-PER-QUAL           
000763                                                                          
000764     MOVE FUNCTION CURRENT-DATE(1:8) TO WS-CURRENT-DATE                   
000765     MOVE FUNCTION CURRENT-DATE(9:4) TO WS-CURRENT-TIME                   
000766     MOVE WS-CURRENT-DATE-TIME       TO                                   
000767                            DTM-2380-DATE-TIME-PER                        
000768                                                                          
000769     MOVE WC-DTM-CCYYMMDDHHMM        TO                                   
000770                            DTM-2379-DATE-TIME-PER-FORM                   
000771     .                                                                    
000772     EJECT                                                                
000773 CD-SKAPA-CU-RFF-POST SECTION.                                            
000774                                                                          
000775     MOVE WC-RFF-IDPTYP          TO CU-RFF-IDPTYP                         
000776     MOVE WC-RFF-LENGTH          TO CU-RFF-LENGTH                         
000777                                                                          
000778     MOVE WC-RFF-CU-QUAL         TO CU-RFF-C506-REFERENCE                 
000779     MOVE IN-HUV-IDDISTR         TO CU-RFF-1154-REFERENCE-NO              
000780     .                                                                    
000781     EJECT                                                                
000782 CE-SKAPA-SEN-NAD-POST SECTION.                                           
000783                                                                          
000784     MOVE WC-NAD-IDPTYP            TO SEN-NAD-IDPTYP                      
000785     MOVE WC-NAD-LENGTH            TO SEN-NAD-LENGTH                      
000786                                                                          
000787     MOVE WC-NAD-DOCUMENT-SEN      TO SEN-NAD-3035-PARTY-QUAL             
000788*                                                                         
000789     IF DIST87-BCUBE             OR                                       
000790        DIST87-BCUBE-PLUS                                                 
000791                                                                          
000792       MOVE WC-NAD-VOLVO-NAME-IT   TO SEN-NAD-3124-COMP-NAME-1            
000793       MOVE WC-NAD-VOLVO-POST-A-IT TO SEN-NAD-3042-STREET-PBOX-1          
000794       MOVE WC-NAD-VOLVO-TOWN-IT   TO SEN-NAD-3164-CITY-NAME              
000795       MOVE WC-NAD-VOLVO-ZIP-CODE-IT                                      
000796                                   TO SEN-NAD-3251-POSTCODE-ID            
000797       MOVE WC-NAD-VOLVO-COUNTRY-IT                                       
000798                                   TO SEN-NAD-3207-COUNTRY-CODED          
000799     END-IF                                                               
000800*                                                                         
000801     IF DIST87-NEOVIA-ES         OR                                       
000802        DIST87-NEOVIA-AFRIKA                                              
000803                                                                          
000804       MOVE WC-NAD-VOLVO-NAME-ES   TO SEN-NAD-3124-COMP-NAME-1            
000805       MOVE WC-NAD-VOLVO-POST-A-ES TO SEN-NAD-3042-STREET-PBOX-1          
000806       MOVE WC-NAD-VOLVO-TOWN-ES   TO SEN-NAD-3164-CITY-NAME              
000807       MOVE WC-NAD-VOLVO-ZIP-CODE-ES                                      
000808                                   TO SEN-NAD-3251-POSTCODE-ID            
000809       MOVE WC-NAD-VOLVO-COUNTRY-ES                                       
000810                                   TO SEN-NAD-3207-COUNTRY-CODED          
000811     END-IF                                                               
000812*                                                                         
000813     IF DIST87-TRUCKWHEEL                                                 
000814       MOVE WC-NAD-VOLVO-NAME-FR   TO SEN-NAD-3124-COMP-NAME-1            
000815       MOVE WC-NAD-VOLVO-POST-A-FR TO SEN-NAD-3042-STREET-PBOX-1          
000816       MOVE WC-NAD-VOLVO-TOWN-FR   TO SEN-NAD-3164-CITY-NAME              
000817       MOVE WC-NAD-VOLVO-ZIP-CODE-FR                                      
000818                                   TO SEN-NAD-3251-POSTCODE-ID            
000819       MOVE WC-NAD-VOLVO-COUNTRY-FR                                       
000820                                   TO SEN-NAD-3207-COUNTRY-CODED          
000821     END-IF                                                               
000822     .                                                                    
000823     EJECT                                                                
000824 CF-SKAPA-LOC-POST SECTION.                                               
000825                                                                          
000826     MOVE WC-LOC-IDPTYP           TO LOC-IDPTYP                           
000827     MOVE WC-LOC-LENGTH           TO LOC-LENGTH                           
000828                                                                          
000829     MOVE WC-LOC-QUAL             TO LOC-3227-PLACE-DEL-QUAL              
000830                                                                          
000831     MOVE IN-HUV-IDDC             TO LOC-3225-DELIVERY-CENTER             
000832                                                                          
000833     MOVE WC-LOC-AGREEMENT        TO LOC-3055-AGREEMENT                   
000834     .                                                                    
000835     EJECT                                                                
000836 CG-SKAPA-TDT-POST SECTION.                                               
000837                                                                          
000840     MOVE WC-TDT-IDPTYP          TO TDT-IDPTYP                            
000850     MOVE WC-TDT-LENGTH          TO TDT-LENGTH                            
000851     MOVE WC-TDT-QUAL            TO TDT-8051-TRANSP-STAGE-QUAL            
000852     MOVE IN-HUV-IDTRPTNR        TO TDT-8213-ID-MEANS-OF-TRPT             
000853     .                                                                    
000854     EJECT                                                                
000855 CH-SKAPA-EQD-POST SECTION.                                               
000856                                                                          
000857     MOVE WC-EQD-IDPTYP          TO EQD-IDPTYP                            
000858     MOVE WC-EQD-LENGTH          TO EQD-LENGTH                            
000859                                                                          
000860     MOVE WC-EQD-QUAL            TO EQD-8053-TRANSP-STAGE-QUAL            
000870     MOVE IN-HUV-IDLBBET         TO EQD-8154-ID-MEANS-OF-TRPT             
000880     .                                                                    
000881     EJECT                                                                
000882 D-NOLLSTALL-KUND  SECTION.                                               
000883                                                                          
000884     INITIALIZE            CPS-AREA                                       
000885                           PAC-AREA                                       
000886                           VKB-MEA-AREA                                   
000887                           VKN-MEA-AREA                                   
000888                           VLB-MEA-AREA                                   
000889     .                                                                    
000890     EJECT                                                                
000891 E-SKAPA-SKRIV-KUND-INFO SECTION.                                         
000892                                                                          
000893     PERFORM EA-SKAPA-CPS-POST                                            
000894     PERFORM EB-SKAPA-PAC-POST                                            
000895     PERFORM EC-SKAPA-MEA-VKB-POST                                        
000896     PERFORM ED-SKAPA-MEA-VKN-POST                                        
000897     PERFORM EE-SKAPA-MEA-VLB-POST                                        
000898                                                                          
000899     PERFORM S22-SKRIV-KUND-INFO                                          
000900                                                                          
000901     MOVE ZERO TO WS-IDART-RAK                                            
000902     .                                                                    
000903     EJECT                                                                
000904 EA-SKAPA-CPS-POST SECTION.                                               
000905                                                                          
000906     MOVE WC-CPS-IDPTYP          TO CPS-IDPTYP                            
000907     MOVE WC-CPS-LENGTH          TO CPS-LENGTH                            
000908                                                                          
000909     MOVE WS-CPS-RAK             TO CPS-7164-HIERARCHIC-ID                
000910                                                                          
000911     MOVE WC-CPS-SHIP-LEVEL      TO CPS-7075-SHIP-LEVEL                   
000912     .                                                                    
000913     EJECT                                                                
000914 EB-SKAPA-PAC-POST SECTION.                                               
000915                                                                          
000916     MOVE WC-PAC-IDPTYP          TO PAC-IDPTYP                            
000917     MOVE WC-PAC-LENGTH          TO PAC-LENGTH                            
000918                                                                          
000919     MOVE IN-KND-KVKOLLI         TO PAC-7224-NUMBER-OF-PACK               
000920     .                                                                    
000921     EJECT                                                                
000922 EC-SKAPA-MEA-VKB-POST SECTION.                                           
000923                                                                          
000924     MOVE WC-MEA-IDPTYP          TO VKB-MEA-IDPTYP                        
000925     MOVE WC-MEA-LENGTH          TO VKB-MEA-LENGTH                        
000926                                                                          
000927     MOVE WC-MEA-VKB-GROSS       TO VKB-MEA-6311-MEASURE-QUAL             
000928     MOVE WC-MEA-VKB-KG          TO VKB-MEA-6411-MEASURE-UNIT-Q           
000929     MOVE IN-KND-VKORDBTO        TO WS-VKORDBTO                           
000930     MOVE WS-VKORDBTO-W          TO VKB-MEA-6314-MEASURE-VALUE            
000940     .                                                                    
000941     EJECT                                                                
000942 ED-SKAPA-MEA-VKN-POST SECTION.                                           
000943                                                                          
000944     MOVE WC-MEA-IDPTYP          TO VKN-MEA-IDPTYP                        
000945     MOVE WC-MEA-LENGTH          TO VKN-MEA-LENGTH                        
000946                                                                          
000947     MOVE WC-MEA-VKN-NETT        TO VKN-MEA-6311-MEASURE-QUAL             
000948     MOVE WC-MEA-VKN-KG          TO VKN-MEA-6411-MEASURE-UNIT-Q           
000949     MOVE IN-KND-VKARTNTO        TO WS-VKARTNTO                           
000950     MOVE WS-VKARTNTO-W          TO VKN-MEA-6314-MEASURE-VALUE            
000960     .                                                                    
000961     EJECT                                                                
000962 EE-SKAPA-MEA-VLB-POST SECTION.                                           
000963                                                                          
000964     MOVE WC-MEA-IDPTYP          TO VLB-MEA-IDPTYP                        
000965     MOVE WC-MEA-LENGTH          TO VLB-MEA-LENGTH                        
000966                                                                          
000967     MOVE WC-MEA-VLB-GROSS       TO VLB-MEA-6311-MEASURE-QUAL             
000968     MOVE WC-MEA-VLB-MTQ         TO VLB-MEA-6411-MEASURE-UNIT-Q           
000969     MOVE IN-KND-VLORDBTO        TO WS-VLORDBTO                           
000970     MOVE WS-VLORDBTO-W          TO VLB-MEA-6314-MEASURE-VALUE            
000971     .                                                                    
000972     EJECT                                                                
000973 F-SPARA-KOLLI-INFO SECTION.                                              
000974                                                                          
000975*    HÄR SPARAR JAG INFO SOM BEHÖVS PÅ RADERNA I H-..                     
000976                                                                          
000977                                                                          
000978     PERFORM FA-SPARA-REC-NAD-POST                                        
000979     PERFORM FB-SPARA-FTX-POST                                            
000980     PERFORM FC-SPARA-ON-RFF-POST                                         
000981     PERFORM FD-SPARA-KLI-RFF-POST                                        
000982     PERFORM FE-SPARA-CR-RFF-POST                                         
000983     .                                                                    
000984     EJECT                                                                
000985 FA-SPARA-REC-NAD-POST SECTION.                                           
000986                                                                          
000987     MOVE IN-KLI-IDKUNDNR        TO REC-NAD-3039-CONSIGNEE                
000988     MOVE IN-KLI-BEGMT-RAD1      TO REC-NAD-3124-COMP-NAME-1              
000989     MOVE IN-KLI-BEGMT-RAD2      TO REC-NAD-3124-COMP-NAME-2              
000990     MOVE IN-KLI-ADGMT-GATA      TO REC-NAD-3042-STREET-PBOX-1            
000991                                                                          
000992     MOVE IN-KLI-ADGMT-PADR      TO WS-ADPOST-PNRORT                      
000993     MOVE WS-ADPOSTNR            TO REC-NAD-3251-POSTCODE-ID              
000994     MOVE WS-ADCITY              TO REC-NAD-3164-CITY-NAME                
000995                                                                          
000996     IF DIST87-BCUBE             OR                                       
000997        DIST87-BCUBE-PLUS                                                 
000998                                                                          
000999       MOVE WC-NAD-REC-COUNTRY-IT TO REC-NAD-3207-COUNTRY-CODED           
001000     END-IF                                                               
001001*                                                                         
001002     IF DIST87-NEOVIA-ES         OR                                       
001003        DIST87-NEOVIA-AFRIKA                                              
001004                                                                          
001005       MOVE WC-NAD-REC-COUNTRY-ES TO REC-NAD-3207-COUNTRY-CODED           
001006     END-IF                                                               
001007*                                                                         
001008     IF DIST87-TRUCKWHEEL                                                 
001009                                                                          
001010       MOVE WC-NAD-REC-COUNTRY-FR TO REC-NAD-3207-COUNTRY-CODED           
001011     END-IF                                                               
001013     .                                                                    
001014     EJECT                                                                
001015 FB-SPARA-FTX-POST SECTION.                                               
001016                                                                          
001017     MOVE IN-KLI-KDORDKL         TO FTX-4440-FREE-TEXT                    
001018                                                                          
001019     .                                                                    
001020     EJECT                                                                
001021 FC-SPARA-ON-RFF-POST SECTION.                                            
001022                                                                          
001023     MOVE IN-KLI-IDORDNR5        TO ON-RFF-1154-REFERENCE-NO              
001024                                                                          
001025     .                                                                    
001026     EJECT                                                                
001027 FD-SPARA-KLI-RFF-POST SECTION.                                           
001028                                                                          
001029     MOVE IN-KLI-IDKOLLI         TO KLI-RFF-1154-REFERENCE-NO             
001030                                                                          
001031     .                                                                    
001032     EJECT                                                                
001033 FE-SPARA-CR-RFF-POST SECTION.                                            
001034                                                                          
001035     MOVE IN-KLI-BEKUNDRF        TO CR-RFF-1154-REFERENCE-NO              
001036                                                                          
001040     .                                                                    
001050     EJECT                                                                
001060 G-SKAPA-SKRIV-RAD-INFO SECTION.                                          
001070                                                                          
001080     PERFORM HA-SKAPA-LIN-POST                                            
001090     PERFORM HB-SKAPA-IMD-POST                                            
001100     PERFORM HC-SKAPA-QTY-POST                                            
001110     PERFORM HD-SKAPA-REC-NAD-POST                                        
001120     PERFORM HE-SKAPA-FTX-POST                                            
001130     PERFORM HF-SKAPA-ON-RFF-POST                                         
001140     PERFORM HG-SKAPA-KLI-RFF-POST                                        
001141     PERFORM HH-SKAPA-CR-RFF-POST                                         
001142                                                                          
001143     PERFORM S23-SKRIV-RAD-INFO                                           
001144     .                                                                    
001145     EJECT                                                                
001146 HA-SKAPA-LIN-POST SECTION.                                               
001147                                                                          
001148     MOVE WC-LIN-IDPTYP          TO LIN-IDPTYP                            
001149     MOVE WC-LIN-LENGTH          TO LIN-LENGTH                            
001150                                                                          
001151     ADD +1                      TO WS-IDART-RAK                          
001152     MOVE WS-IDART-RAK           TO LIN-1082-IDENTIFIER                   
001153     MOVE IN-RAD-IDARTNR         TO LIN-7140-PART-NO                      
001154     .                                                                    
001155     EJECT                                                                
001156 HB-SKAPA-IMD-POST SECTION.                                               
001157                                                                          
001158     MOVE WC-IMD-IDPTYP          TO IMD-IDPTYP                            
001159     MOVE WC-IMD-LENGTH          TO IMD-LENGTH                            
001160                                                                          
001161     MOVE WC-IMD-QUAL            TO IMD-7077-PART-DESCR                   
001162     MOVE IN-RAD-BEART           TO IMD-7008-PART-DESCRIPTION             
001163     .                                                                    
001164     EJECT                                                                
001165 HC-SKAPA-QTY-POST SECTION.                                               
001166                                                                          
001167     MOVE WC-QTY-IDPTYP          TO QTY-IDPTYP                            
001168     MOVE WC-QTY-LENGTH          TO QTY-LENGTH                            
001169                                                                          
001170     MOVE WC-QTY-QUAL            TO QTY-6063-QUANTITY-QUALIFIER           
001171     MOVE IN-RAD-KVLEVART        TO QTY-6060-QUANTITY                     
001172     .                                                                    
001173     EJECT                                                                
001174 HD-SKAPA-REC-NAD-POST SECTION.                                           
001175                                                                          
001176     MOVE WC-NAD-IDPTYP          TO REC-NAD-IDPTYP                        
001177     MOVE WC-NAD-LENGTH          TO REC-NAD-LENGTH                        
001178                                                                          
001179     MOVE WC-NAD-DOCUMENT-REC    TO REC-NAD-3035-PARTY-QUAL               
001180     .                                                                    
001181     EJECT                                                                
001182 HE-SKAPA-FTX-POST SECTION.                                               
001183                                                                          
001184     MOVE WC-FTX-IDPTYP          TO FTX-IDPTYP                            
001185     MOVE WC-FTX-LENGTH          TO FTX-LENGTH                            
001186                                                                          
001187     MOVE WC-FTX-QUAL            TO FTX-4451-TEXT-SUB-QUAL                
001188     .                                                                    
001189     EJECT                                                                
001190 HF-SKAPA-ON-RFF-POST SECTION.                                            
001191                                                                          
001192     MOVE WC-RFF-IDPTYP          TO ON-RFF-IDPTYP                         
001193     MOVE WC-RFF-LENGTH          TO ON-RFF-LENGTH                         
001194                                                                          
001195     MOVE WC-RFF-ON-QUAL         TO ON-RFF-1153-REFERENCE-QUAL            
001196     .                                                                    
001197     EJECT                                                                
001198 HG-SKAPA-KLI-RFF-POST SECTION.                                           
001199                                                                          
001200     MOVE WC-RFF-IDPTYP          TO KLI-RFF-IDPTYP                        
001201     MOVE WC-RFF-LENGTH          TO KLI-RFF-LENGTH                        
001202                                                                          
001203     MOVE WC-RFF-KLI-QUAL        TO KLI-RFF-1153-REFERENCE-QUAL           
001204     .                                                                    
001205     EJECT                                                                
001206 HH-SKAPA-CR-RFF-POST SECTION.                                            
001207                                                                          
001208     MOVE WC-RFF-IDPTYP          TO CR-RFF-IDPTYP                         
001209     MOVE WC-RFF-LENGTH          TO CR-RFF-LENGTH                         
001210                                                                          
001211     MOVE WC-RFF-CR-QUAL         TO CR-RFF-1153-REFERENCE-QUAL            
001212     .                                                                    
001213     EJECT                                                                
001214 S01-LAES-W4764O  SECTION.                                                
001215                                                                          
001216     READ W4764O INTO IN-AREA                                             
001217     AT END                                                               
001218        MOVE HIGH-VALUE TO IN-AREA                                        
001219        SET W4764O-EOF TO TRUE                                            
001220                                                                          
001221     NOT AT END                                                           
001222        MOVE 'W4764O' TO POSTSUM-FDNAMN                                   
001223        MOVE 'W4764PD1' TO POSTSUM-DDNAMN2                                
001224        MOVE IN-IDPTYP TO POSTSUM-TRANSTYP                                
001225        CALL POSTSUM USING POSTSUM-PARM                                   
001226     END-READ                                                             
001227     .                                                                    
001228     EJECT                                                                
001229 S11-POSTSUM-UTPOST SECTION.                                              
001230                                                                          
001231     MOVE EDI-IDPTYP TO POSTSUM-TRANSTYP                                  
001232     MOVE 'W4764P' TO POSTSUM-FDNAMN                                      
001233     MOVE 'W4764PD2' TO POSTSUM-DDNAMN2                                   
001234     CALL POSTSUM USING POSTSUM-PARM                                      
001235     .                                                                    
001236     EJECT                                                                
001237 S20-SKRIV-UNB-POST SECTION.                                              
001238                                                                          
001239     INITIALIZE UNB-AREA                                                  
001240                                                                          
001241     MOVE WC-UNB-IDPTYP          TO UNB-IDPTYP                            
001242     MOVE WC-UNB-LENGTH          TO UNB-LENGTH                            
001243                                                                          
001244     MOVE WC-UNB-UNOA            TO UNB-0001-SYNTAX-ID                    
001245     MOVE WC-UNB-2               TO UNB-0002-SYNTAX-VERSION-NO            
001246     MOVE WC-UNB-CDC-CAR-PARTS   TO UNB-0004-SENDER-ID                    
001247                                                                          
001248     IF DIST87-BCUBE             OR                                       
001249        DIST87-BCUBE-PLUS                                                 
001250                                                                          
001251       MOVE WC-UNB-BCUBE         TO UNB-0010-RECIPIENT-ID                 
001252     END-IF                                                               
001253                                                                          
001254     IF DIST87-TRUCKWHEEL                                                 
001257                                                                          
001258       MOVE WC-UNB-TRUCKWHEEL    TO UNB-0010-RECIPIENT-ID                 
001259     END-IF                                                               
001260                                                                          
001261     IF DIST87-NEOVIA-ES         OR                                       
001262        DIST87-NEOVIA-AFRIKA                                              
001263                                                                          
001264       MOVE WC-UNB-NEOVIA        TO UNB-0010-RECIPIENT-ID                 
001265     END-IF                                                               
001266*                                                                         
001267*                                                                         
001268     MOVE FUNCTION CURRENT-DATE(3:8) TO UNB-0017-DATE                     
001269     MOVE FUNCTION CURRENT-DATE(9:4) TO UNB-0019-TIME                     
001270                                                                          
001271     WRITE UT-UNB-POST               FROM UNB-AREA                        
001272     MOVE UNB-AREA(1:3)              TO EDI-IDPTYP                        
001273     PERFORM S11-POSTSUM-UTPOST                                           
001274     .                                                                    
001275     EJECT                                                                
001276 S21-SKRIV-SKEPPN-INFO SECTION.                                           
001277                                                                          
001278     WRITE UT-UNH-POST                FROM UNH-AREA                       
001279     MOVE UNH-AREA (1:3)              TO EDI-IDPTYP                       
001280     PERFORM S11-POSTSUM-UTPOST                                           
001281                                                                          
001282     WRITE UT-BGM-POST                FROM BGM-AREA                       
001283     MOVE BGM-AREA(1:3)               TO EDI-IDPTYP                       
001284     PERFORM S11-POSTSUM-UTPOST                                           
001285                                                                          
001286     WRITE UT-DTM-POST                FROM DTM-AREA                       
001287     MOVE DTM-AREA(1:3)               TO EDI-IDPTYP                       
001288     PERFORM S11-POSTSUM-UTPOST                                           
001289                                                                          
001290     WRITE UT-CU-RFF-POST             FROM CU-RFF-AREA                    
001291     MOVE CU-RFF-AREA(1:3)            TO EDI-IDPTYP                       
001292     PERFORM S11-POSTSUM-UTPOST                                           
001293                                                                          
001294     WRITE UT-SEN-NAD-POST            FROM SEN-NAD-AREA                   
001295     MOVE SEN-NAD-AREA(1:3)           TO EDI-IDPTYP                       
001296     PERFORM S11-POSTSUM-UTPOST                                           
001297                                                                          
001298     WRITE UT-LOC-POST                FROM LOC-AREA                       
001299     MOVE LOC-AREA(1:3)               TO EDI-IDPTYP                       
001300     PERFORM S11-POSTSUM-UTPOST                                           
001301                                                                          
001302     WRITE UT-TDT-POST                FROM TDT-AREA                       
001303     MOVE TDT-AREA(1:3)               TO EDI-IDPTYP                       
001304     PERFORM S11-POSTSUM-UTPOST                                           
001305                                                                          
001306     WRITE UT-EQD-POST                FROM EQD-AREA                       
001307     MOVE EQD-AREA(1:3)               TO EDI-IDPTYP                       
001308     PERFORM S11-POSTSUM-UTPOST                                           
001309     .                                                                    
001310     EJECT                                                                
001311 S22-SKRIV-KUND-INFO SECTION.                                             
001312                                                                          
001313     WRITE UT-CPS-POST               FROM CPS-AREA                        
001314     MOVE CPS-AREA(1:3)              TO EDI-IDPTYP                        
001315     PERFORM S11-POSTSUM-UTPOST                                           
001316                                                                          
001317     WRITE UT-PAC-POST               FROM PAC-AREA                        
001318     MOVE PAC-AREA(1:3)              TO EDI-IDPTYP                        
001319     PERFORM S11-POSTSUM-UTPOST                                           
001320                                                                          
001321     WRITE UT-VKB-MEA-POST           FROM VKB-MEA-AREA                    
001322     MOVE VKB-MEA-AREA(1:3)          TO EDI-IDPTYP                        
001323     PERFORM S11-POSTSUM-UTPOST                                           
001324                                                                          
001325     WRITE UT-VKN-MEA-POST           FROM VKN-MEA-AREA                    
001326     MOVE VKN-MEA-AREA(1:3)          TO EDI-IDPTYP                        
001327     PERFORM S11-POSTSUM-UTPOST                                           
001328                                                                          
001329     WRITE UT-VLB-MEA-POST           FROM VLB-MEA-AREA                    
001330     MOVE VLB-MEA-AREA(1:3)          TO EDI-IDPTYP                        
001331     PERFORM S11-POSTSUM-UTPOST                                           
001332     .                                                                    
001333     EJECT                                                                
001334 S23-SKRIV-RAD-INFO SECTION.                                              
001335                                                                          
001336     WRITE UT-LIN-POST               FROM LIN-AREA                        
001337     MOVE LIN-AREA(1:3)              TO EDI-IDPTYP                        
001338     PERFORM S11-POSTSUM-UTPOST                                           
001339                                                                          
001340     WRITE UT-IMD-POST               FROM IMD-AREA                        
001341     MOVE IMD-AREA(1:3)              TO EDI-IDPTYP                        
001342     PERFORM S11-POSTSUM-UTPOST                                           
001343                                                                          
001344     WRITE UT-QTY-POST               FROM QTY-AREA                        
001345     MOVE QTY-AREA(1:3)              TO EDI-IDPTYP                        
001346     PERFORM S11-POSTSUM-UTPOST                                           
001347                                                                          
001348     WRITE UT-REC-NAD-POST           FROM REC-NAD-AREA                    
001349     MOVE REC-NAD-AREA(1:3)          TO EDI-IDPTYP                        
001350     PERFORM S11-POSTSUM-UTPOST                                           
001351                                                                          
001352     WRITE UT-FTX-POST               FROM FTX-AREA                        
001353     MOVE FTX-AREA(1:3)              TO EDI-IDPTYP                        
001354     PERFORM S11-POSTSUM-UTPOST                                           
001355                                                                          
001356     WRITE UT-ON-RFF-POST            FROM ON-RFF-AREA                     
001357     MOVE ON-RFF-AREA(1:3)           TO EDI-IDPTYP                        
001358     PERFORM S11-POSTSUM-UTPOST                                           
001359                                                                          
001360     WRITE UT-KLI-RFF-POST           FROM KLI-RFF-AREA                    
001361     MOVE KLI-RFF-AREA(1:3)          TO EDI-IDPTYP                        
001362     PERFORM S11-POSTSUM-UTPOST                                           
001363                                                                          
001364     WRITE UT-CR-RFF-POST            FROM CR-RFF-AREA                     
001365     MOVE CR-RFF-AREA(1:3)           TO EDI-IDPTYP                        
001366     PERFORM S11-POSTSUM-UTPOST                                           
001367     .                                                                    
001368     EJECT                                                                
001369 S31-SKRIV-001-POST SECTION.                                              
001370                                                                          
001371     INITIALIZE 001-AREA                                                  
001372                                                                          
001373     MOVE WC-001-IDPTYP          TO 001-IDPTYP                            
001374     MOVE WC-001-LENGTH          TO 001-LENGTH                            
001375                                                                          
001376     MOVE WC-001-VOLVO-PARTS     TO 001-SNODE-SEN-COMMON-NODE             
001377     MOVE WC-001-VOLVO-AMTRIX    TO 001-RNODE-REC-COMMON-NODE             
001378     MOVE WC-001-DESADV          TO 001-VFILE-VIRTUAL-FILE-NAME           
001379                                                                          
001380     MOVE FUNCTION CURRENT-DATE(3:8) TO                                   
001381                                 001-VFDATE-VIR-FILE-DATE                 
001382     MOVE FUNCTION CURRENT-DATE(9:4) TO WS-CURRENT-TIME                   
001383                                 001-VFTIME-VIR-FILE-TIME                 
001384                                                                          
001385     WRITE UT-001-POST               FROM 001-AREA                        
001386     MOVE 001-AREA(1:3)              TO EDI-IDPTYP                        
001387     PERFORM S11-POSTSUM-UTPOST                                           
001388     .                                                                    
001389     EJECT                                                                
001390 S34-SKRIV-003-POST SECTION.                                              
001391                                                                          
001392     INITIALIZE 003-AREA                                                  
001393                                                                          
001394     MOVE WC-003-IDPTYP          TO 003-IDPTYP                            
001395     MOVE WC-003-LENGTH          TO 003-LENGTH                            
001396                                                                          
001397     WRITE UT-003-POST           FROM 003-AREA                            
001398     MOVE 003-AREA(1:3)          TO EDI-IDPTYP                            
001399     PERFORM S11-POSTSUM-UTPOST                                           
001400     .                                                                    
001401     EJECT                                                                
001402 Z-FINIT SECTION.                                                         
001403     CLOSE W4764O                                                         
001404           W4764P                                                         
001405     SKIP2                                                                
001406     MOVE 'S' TO POSTSUM-OPKOD                                            
001407     CALL POSTSUM USING POSTSUM-PARM                                      
001410     .                                                                    
001500     EJECT                                                                
