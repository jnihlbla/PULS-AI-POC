000001*** EDIT ALLOWED                                                          
000002*                            *************************************        
000003* >>> OBS! OBS! in some pgms the index refering to specific               
000004* >>> OBS! OBS! countries is hard coded!!                                 
000005* >>> OBS! OBS! if you add/remove/change in this table you                
000006* >>> OBS! OBS! have to verify all pgm using WNDCADRE and                 
000007* >>> OBS! OBS! check how 'ship-indx' is set !!                           
000008*                            *************************************        
000009*                            *** ANVÄNDS FÖR ATT HÄMTA                    
000010*                            *** ADRESS TILL RESPEKTIVE                   
000011*                            *** NDC VID UTSKRIFT AV                      
000012*                            *** LAGER-ADRESS OCH PROFORMAFAKT            
000013*                            *************************************        
000014                                                                          
000015 01  SHIPPER-TAB.                                                         
000016   03 SHIPPER-ADRESS-NDC41.                                               
000017***ANVÄND SHIP-INDX = 1                                                   
000018      05 FILLER                             PIC X(32)                     
000019         VALUE 'VOLVO CAR USA, LLC              '.                        
000020      05 FILLER                             PIC X(32)                     
000021         VALUE 'RUTHERFORD DIST.CENTER          '.                        
000022      05 FILLER                             PIC X(32)                     
000023         VALUE '300 HOWMEDICA WAY               '.                        
000024      05 FILLER                             PIC X(32)                     
000025         VALUE 'RUTHERFORD, NJ 07070            '.                        
000026      05 FILLER                             PIC X(32)                     
000027         VALUE 'USA                             '.                        
000028      05 FILLER                             PIC X(32)                     
000029         VALUE '201/935-2021                    '.                        
000030      05 FILLER                             PIC X(32)                     
000031         VALUE '201/935-1431                    '.                        
000032      05 FILLER                             PIC X(50)                     
000033      VALUE '                                                  '.         
000034   03 SHIPPER-ADRESS-NDC92.                                               
000035***ANVÄND SHIP-INDX = 2                                                   
000036      05 FILLER                             PIC X(32)                     
000037         VALUE 'VOLVO CAR USA, LLC              '.                        
000038      05 FILLER                             PIC X(32)                     
000039         VALUE 'HOLLAND BATTERY CENTER          '.                        
000040      05 FILLER                             PIC X(32)                     
000041         VALUE '581 OTTAWA AVE STE 100          '.                        
000042      05 FILLER                             PIC X(32)                     
000043         VALUE 'HOLLAND, MI 49423               '.                        
000044      05 FILLER                             PIC X(32)                     
000045         VALUE 'USA                             '.                        
000046      05 FILLER                             PIC X(32)                     
000047         VALUE '???/???-????                    '.                        
000048      05 FILLER                             PIC X(32)                     
000049         VALUE '???/???-????                    '.                        
000050      05 FILLER                             PIC X(50)                     
000051      VALUE '                                                  '.         
000052   03 SHIPPER-ADRESS-NDC43.                                               
000053***ANVÄND SHIP-INDX = 3                                                   
000054      05 FILLER                             PIC X(32)                     
000055         VALUE 'VOLVO CAR USA, LLC              '.                        
000056      05 FILLER                             PIC X(32)                     
000057         VALUE 'LOS ANGLES DISTRIB.CENTER       '.                        
000058      05 FILLER                             PIC X(32)                     
000059         VALUE '1851 S.CUCAMONGA AVENUE         '.                        
000060      05 FILLER                             PIC X(32)                     
000061         VALUE 'ONTARIO, CA  91761              '.                        
000062      05 FILLER                             PIC X(32)                     
000063         VALUE 'USA                             '.                        
000064      05 FILLER                             PIC X(32)                     
000065         VALUE '909/947-7650                    '.                        
000066      05 FILLER                             PIC X(32)                     
000067         VALUE '909/773-0099                    '.                        
000068      05 FILLER                             PIC X(50)                     
000069      VALUE '                                                  '.         
000070   03 SHIPPER-ADRESS-NDC44.                                               
000071***ANVÄND SHIP-INDX = 4                                                   
000072      05 FILLER                             PIC X(32)                     
000073         VALUE 'VOLVO CAR USA, LLC              '.                        
000074      05 FILLER                             PIC X(32)                     
000075         VALUE 'CEVA LOGISTICS                  '.                        
000076      05 FILLER                             PIC X(32)                     
000077         VALUE '3102 WEST VALLEY HWY            '.                        
000078      05 FILLER                             PIC X(32)                     
000079         VALUE 'AUBURN, WA 98001                '.                        
000080      05 FILLER                             PIC X(32)                     
000081         VALUE 'USA                             '.                        
000082      05 FILLER                             PIC X(32)                     
000083         VALUE '                                '.                        
000084      05 FILLER                             PIC X(32)                     
000085         VALUE '                                '.                        
000086      05 FILLER                             PIC X(50)                     
000087      VALUE '                                                  '.         
000088   03 SHIPPER-ADRESS-NDC45.                                               
000089***ANVÄND SHIP-INDX = 5                                                   
000090      05 FILLER                             PIC X(32)                     
000091         VALUE 'VOLVO CAR USA, LLC              '.                        
000092      05 FILLER                             PIC X(32)                     
000093         VALUE 'UNIPART LOGISTICS               '.                        
000094      05 FILLER                             PIC X(32)                     
000095         VALUE '2 GATEWAY COURT                 '.                        
000096      05 FILLER                             PIC X(32)                     
000097         VALUE 'BOLINGBROO IL 60440             '.                        
000098      05 FILLER                             PIC X(32)                     
000099         VALUE 'USA                             '.                        
000100      05 FILLER                             PIC X(32)                     
000101         VALUE '                                '.                        
000102      05 FILLER                             PIC X(32)                     
000103         VALUE '                                '.                        
000104      05 FILLER                             PIC X(50)                     
000105      VALUE '                                                  '.         
000106   03 SHIPPER-ADRESS-NDC46.                                               
000107***ANVÄND SHIP-INDX = 6                                                   
000108      05 FILLER                             PIC X(32)                     
000109         VALUE 'VOLVO CAR USA, LLC              '.                        
000110      05 FILLER                             PIC X(32)                     
000111         VALUE 'UNIPART LOGISTICS               '.                        
000112      05 FILLER                             PIC X(32)                     
000113         VALUE '13398 INTERNATIONAL PKWY        '.                        
000114      05 FILLER                             PIC X(32)                     
000115         VALUE 'JACKSONVILLE FL 32218           '.                        
000116      05 FILLER                             PIC X(32)                     
000117         VALUE 'USA                             '.                        
000118      05 FILLER                             PIC X(32)                     
000119         VALUE '                                '.                        
000120      05 FILLER                             PIC X(32)                     
000121         VALUE '                                '.                        
000122      05 FILLER                             PIC X(50)                     
000123      VALUE '                                                  '.         
000124   03 SHIPPER-ADRESS-NDC51.                                               
000125***ANVÄND SHIP-INDX = 7                                                   
000126      05 FILLER                             PIC X(32)                     
000127         VALUE 'NEOVIA LOGISTICS SERVICES CANADA'.                        
000128      05 FILLER                             PIC X(32)                     
000129         VALUE 'NEOVIA CANADA                   '.                        
000130      05 FILLER                             PIC X(32)                     
000131         VALUE '150 COURTNEYPARK DRIVE WEST     '.                        
000132      05 FILLER                             PIC X(32)                     
000133         VALUE 'MISSISAUGA, ONTARIO CAN L5W 1Y6 '.                        
000134      05 FILLER                             PIC X(32)                     
000135         VALUE 'CANADA L5W 1Y6                  '.                        
000136      05 FILLER                             PIC X(32)                     
000137         VALUE '                                '.                        
000138      05 FILLER                             PIC X(32)                     
000139         VALUE '                                '.                        
000140      05 FILLER                             PIC X(50)                     
000141      VALUE '                                                  '.         
000142   03 SHIPPER-ADRESS-NDC61.                                               
000143***ANVÄND SHIP-INDX = 8                                                   
000144      05 FILLER                             PIC X(32)                     
000145         VALUE '                                '.                        
000146      05 FILLER                             PIC X(32)                     
000147         VALUE 'VOLVO NDC NAGOYA.               '.                        
000148      05 FILLER                             PIC X(32)                     
000149         VALUE '1-5-10, HIGASHIHAMA,            '.                        
000150      05 FILLER                             PIC X(32)                     
000151         VALUE 'TOBISHIMA-MURA, AMA-GUN,        '.                        
000152      05 FILLER                             PIC X(32)                     
000153         VALUE 'AICHI, 490-1446, JAPAN          '.                        
000154      05 FILLER                             PIC X(32)                     
000155         VALUE '+81(5675)5-2879                 '.                        
000156      05 FILLER                             PIC X(32)                     
000157         VALUE '+81(5675)5-2336                 '.                        
000158      05 FILLER                             PIC X(50)                     
000159      VALUE '                                                  '.         
000160   03 SHIPPER-ADRESS-NDC62.                                               
000161***ANVÄND SHIP-INDX = 9                                                   
000162      05 FILLER                             PIC X(32)                     
000163         VALUE 'VOLVO AUSTRALIA PTY. LTD.       '.                        
000164      05 FILLER                             PIC X(32)                     
000165         VALUE 'MINTO DIST.CENTER.              '.                        
000166      05 FILLER                             PIC X(32)                     
000167         VALUE 'LOT 12. AIRDS ROAD              '.                        
000168      05 FILLER                             PIC X(32)                     
000169         VALUE 'MINTO  NSW  2566                '.                        
000170      05 FILLER                             PIC X(32)                     
000171         VALUE 'AUSTRALIA                       '.                        
000172      05 FILLER                             PIC X(32)                     
000173         VALUE '+61-2-9827-3100                 '.                        
000174      05 FILLER                             PIC X(32)                     
000175         VALUE '+61-2-9827-3143                 '.                        
000176      05 FILLER                             PIC X(50)                     
000177      VALUE '                                                  '.         
000178   03 SHIPPER-ADRESS-NDC67.                                               
000179***ANVÄND SHIP-INDX = 10                                                  
000180      05 FILLER                             PIC X(32)                     
000181         VALUE 'VOLVO AUTO INDIA PRIVATE LIMITED'.                        
000182      05 FILLER                             PIC X(32)                     
000183         VALUE '405-B, 1st Floor, Tower A       '.                        
000184      05 FILLER                             PIC X(32)                     
000185         VALUE 'DLF Cyber Park, Sector 20       '.                        
000186      05 FILLER                             PIC X(32)                     
000187         VALUE 'UDYOG VIHAR, Phase-III, GURUGRAM'.                        
000188      05 FILLER                             PIC X(32)                     
000189         VALUE 'HARYANA  INDIA  122016          '.                        
000190      05 FILLER                             PIC X(32)                     
000191         VALUE '+91 124 4600450                 '.                        
000192      05 FILLER                             PIC X(32)                     
000193         VALUE '                                '.                        
000194      05 FILLER                             PIC X(50)                     
000195      VALUE 'PARTSINDIA@VOLVOCARS.COM                          '.         
000196   03 SHIPPER-ADRESS-NDC52.                                               
000197***ANVÄND SHIP-INDX = 11                                                  
000198      05 FILLER                             PIC X(32)                     
000199         VALUE 'VOLVO BRASIL ??                 '.                        
000200      05 FILLER                             PIC X(32)                     
000201         VALUE 'TO BE ADDED ??                  '.                        
000202      05 FILLER                             PIC X(32)                     
000203         VALUE 'TO BE ADDED ??                  '.                        
000204      05 FILLER                             PIC X(32)                     
000205         VALUE 'TO BE ADDED ??                  '.                        
000206      05 FILLER                             PIC X(32)                     
000207         VALUE '                                '.                        
000208      05 FILLER                             PIC X(32)                     
000209         VALUE '                                '.                        
000210      05 FILLER                             PIC X(32)                     
000211         VALUE '                                '.                        
000212      05 FILLER                             PIC X(50)                     
000213      VALUE '??????????@VOLVOCARS.COM                          '.         
000214   03 SHIPPER-ADRESS-NDC53.                                               
000215***ANVÄND SHIP-INDX = 12                                                  
000216      05 FILLER                             PIC X(32)                     
000217         VALUE 'VOLVO MEXICO ??                 '.                        
000218      05 FILLER                             PIC X(32)                     
000219         VALUE 'TO BE ADDED ??                  '.                        
000220      05 FILLER                             PIC X(32)                     
000230         VALUE 'TO BE ADDED ??                  '.                        
000240      05 FILLER                             PIC X(32)                     
000250         VALUE 'TO BE ADDED ??                  '.                        
000260      05 FILLER                             PIC X(32)                     
000270         VALUE '                                '.                        
000280      05 FILLER                             PIC X(32)                     
000281         VALUE '                                '.                        
000282      05 FILLER                             PIC X(32)                     
000283         VALUE '                                '.                        
000284      05 FILLER                             PIC X(50)                     
000285      VALUE '??????????@VOLVOCARS.COM                          '.         
000286   03 SHIPPER-ADRESS-NDC85.                                               
000287***ANVÄND SHIP-INDX = 13                                                  
000288      05 FILLER                             PIC X(32)                     
000289         VALUE 'VOLVO SOUTH AFRICA ??           '.                        
000290      05 FILLER                             PIC X(32)                     
000291         VALUE 'TO BE ADDED ??                  '.                        
000292      05 FILLER                             PIC X(32)                     
000293         VALUE 'TO BE ADDED ??                  '.                        
000294      05 FILLER                             PIC X(32)                     
000295         VALUE 'TO BE ADDED ??                  '.                        
000296      05 FILLER                             PIC X(32)                     
000297         VALUE '                                '.                        
000298      05 FILLER                             PIC X(32)                     
000299         VALUE '                                '.                        
000300      05 FILLER                             PIC X(32)                     
000301         VALUE '                                '.                        
000302      05 FILLER                             PIC X(50)                     
000303      VALUE '??????????@VOLVOCARS.COM                          '.         
000304   03 SHIPPER-ADRESS-NDC86.                                               
000305***ANVÄND SHIP-INDX = 14                                                  
000306      05 FILLER                             PIC X(32)                     
000307         VALUE 'VOLVO TURKEY ??                 '.                        
000308      05 FILLER                             PIC X(32)                     
000309         VALUE 'TO BE ADDED ??                  '.                        
000310      05 FILLER                             PIC X(32)                     
000311         VALUE 'TO BE ADDED ??                  '.                        
000312      05 FILLER                             PIC X(32)                     
000313         VALUE 'TO BE ADDED ??                  '.                        
000314      05 FILLER                             PIC X(32)                     
000315         VALUE '                                '.                        
000316      05 FILLER                             PIC X(32)                     
000317         VALUE '                                '.                        
000318      05 FILLER                             PIC X(32)                     
000319         VALUE '                                '.                        
000320      05 FILLER                             PIC X(50)                     
000321      VALUE '??????????@VOLVOCARS.COM                          '.         
000322   03 SHIPPER-ADRESS-NDC87.                                               
000323***ANVÄND SHIP-INDX = 15                                                  
000324      05 FILLER                             PIC X(32)                     
000325         VALUE 'VOLVO CAR RDC MIDDLE EAST FZE   '.                        
000326      05 FILLER                             PIC X(32)                     
000327         VALUE '                                '.                        
000328      05 FILLER                             PIC X(32)                     
000329         VALUE 'JAFZA,LOB 15,SECOND FLOOR,OFFICE'.                        
000330      05 FILLER                             PIC X(32)                     
000331         VALUE 'DUBAI, UAE                      '.                        
000332      05 FILLER                             PIC X(32)                     
000333         VALUE '                                '.                        
000334      05 FILLER                             PIC X(32)                     
000335         VALUE '                                '.                        
000336      05 FILLER                             PIC X(32)                     
000337         VALUE '                                '.                        
000338      05 FILLER                             PIC X(50)                     
000339      VALUE '??????????@VOLVOCARS.COM                          '.         
000340   03 SHIPPER-ADRESS-NDC47.                                               
000341***ANVÄND SHIP-INDX = 16                                                  
000342      05 FILLER                             PIC X(32)                     
000343         VALUE 'VOLVO CAR USA, LLC              '.                        
000344      05 FILLER                             PIC X(32)                     
000345         VALUE 'DALLAS DIST.CENTER              '.                        
000346      05 FILLER                             PIC X(32)                     
000347         VALUE '3434 MCPHERSON DRIVE            '.                        
000348      05 FILLER                             PIC X(32)                     
000349         VALUE 'NORTHLAKE, TX 76247             '.                        
000350      05 FILLER                             PIC X(32)                     
000351         VALUE 'USA                             '.                        
000352      05 FILLER                             PIC X(32)                     
000353         VALUE '                                '.                        
000354      05 FILLER                             PIC X(32)                     
000355         VALUE '                                '.                        
000356      05 FILLER                             PIC X(50)                     
000357      VALUE '                                                  '.         
000358                                                                          
000359 01  WS-SHIPPER-RECORD  REDEFINES SHIPPER-TAB.                            
000360   03  SHIPPER-NDC      OCCURS 16.                                        
000361      05 SHIPPER-COMPANY                    PIC X(32).                    
000362      05 SHIPPER-NAME                       PIC X(32).                    
000363      05 SHIPPER-STREET                     PIC X(32).                    
000364      05 SHIPPER-CITY                       PIC X(32).                    
000365      05 SHIPPER-COUNTRY                    PIC X(32).                    
000366      05 SHIPPER-TEL                        PIC X(32).                    
000367      05 SHIPPER-TELEFAX                    PIC X(32).                    
000368      05 SHIPPER-IDMAIL                     PIC X(50).                    
000369                                                                          
000370*END**************************************************************        
