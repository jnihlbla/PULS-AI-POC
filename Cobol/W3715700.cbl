000001 ID DIVISION.                                                             
000002                                                                          
000003 PROGRAM-ID.     W3715700.                                                
000004 AUTHOR.         INGVAR SKJELBRED.                                        
000005 DATE-WRITTEN.   97/09/10.                                                
000006 DATE-COMPILED.                                                           
000007                                                                          
000008*                                                                         
000009*    FUNKTION:                                                            
000010*        PROGRAMMET BERÄKNAR ANTAL ARBETSDAGAR I SNITT DET TAR ATT        
000011* GODKÄNNA EN RAPPORT UNDER EN PERIOD                                     
000012*                                                                         
000013*    ABENDKODER:                                                          
000014*        U0016 -  . . . .                                                 
000015*        U1000 -  . . . .                                                 
000016*                                                                         
000017*    CHANGE LOG:                                                          
000024*      YY/MM/DD - NAME            - CHANGE DESCRIPTION                    
000025*      ----------------------------------------------------------         
000026*      15/04/13 - REDDY RAHUL     - CHINA EXCHANGE PHASE 2.               
000027*                                   E'TRACKER 10252358                    
000028*                                   CONSOLIDATE WEB REPORTS AND           
000029*                                   ADD CHINA REPORTS.                    
000030*      16/09/30 - DATTA ARUP      - NDC SEATTLE.                          
000031*                                   E'TRACKER 10287310                    
000032*                                   CREATE WEB REPORTS FOR DC 44          
000033*                                                                         
000034     SKIP3                                                                
000035 ENVIRONMENT DIVISION.                                                    
000036     SKIP2                                                                
000037 INPUT-OUTPUT SECTION.                                                    
000038                                                                          
000039 FILE-CONTROL.                                                            
000040     SKIP2                                                                
000041*          --- UPPFÖLJNINGS MATERIAL TILL DE OLIKA PERIODLISTORNA         
000042     SELECT W3714E                     ASSIGN TO W37157D1.                
000043     SKIP2                                                                
000044*          --- PERIOD LISTA DC11                                          
000045     SELECT LISTA                      ASSIGN TO W37157D2.                
000046     SKIP2                                                                
000047*          --- PERIOD LISTA DC91                                          
000048     SELECT LISTB                      ASSIGN TO W37157D3.                
000049     SKIP2                                                                
000050*          --- PERIOD LISTA DC61                                          
000051     SELECT LISTF                      ASSIGN TO W37157D4.                
000052*          --- PERIOD LISTA WEB DC                                        
000053     SELECT LISTG                      ASSIGN TO W37157D5.                
000054     EJECT                                                                
000055 DATA DIVISION.                                                           
000056     SKIP3                                                                
000057 FILE SECTION.                                                            
000058     SKIP3                                                                
000059 FD  W3714E                                                               
000060     RECORDING       F                                                    
000061     BLOCK CONTAINS  0.                                                   
000062                                                                          
000063*01  -COPY W3714E      -L.                                                
000064     SKIP3                                                                
000065 FD  LISTA                                                                
000066     RECORDING       F                                                    
000067     BLOCK CONTAINS  0.                                                   
000068     SKIP2                                                                
000069 01  LISTAS                      PIC X(121).                              
000070     SKIP3                                                                
000071 FD  LISTB                                                                
000072     RECORDING       V                                                    
000073     BLOCK CONTAINS  0.                                                   
000074     SKIP2                                                                
000075 01  LISTBS                      PIC X(125).                              
000076     SKIP2                                                                
000077 FD  LISTF                                                                
000078     RECORDING       F                                                    
000079     BLOCK CONTAINS  0.                                                   
000080     SKIP2                                                                
000090 01  LISTFS                      PIC X(121).                              
000100     EJECT                                                                
000101 FD  LISTG                                                                
000102     RECORDING       V                                                    
000103     BLOCK CONTAINS  0.                                                   
000104     SKIP2                                                                
000105 01  LISTGS                      PIC X(125).                              
000106     EJECT                                                                
000107 WORKING-STORAGE SECTION.                                                 
000108                                                                          
000109                                                                          
000110*    -- CHECKED BY WY2000                                                 
000111 77  IDPGM                       PIC X(8)    VALUE 'W3715700'.            
000112 77   PROGRAM-NAMN           VALUE 'W3715700'                             
000113                                 PIC X(8).                                
000114 77  JA                          PIC X       VALUE 'N'.                   
000115 77  NEJ                         PIC X       VALUE 'J'.                   
000116                                                                          
000117 77  W3714E-EOF-SW               PIC X       VALUE 'N'.                   
000118     88  END-OF-W3714E                       VALUE 'J'.                   
000119                                                                          
000120 01  SISTA-POST-SKRIVEN-DC11     PIC X       VALUE 'N'.                   
000121                                                                          
000122 01  SISTA-POST-SKRIVEN-DC91     PIC X       VALUE 'N'.                   
000123                                                                          
000124 01  SISTA-POST-SKRIVEN-DC61     PIC X       VALUE 'N'.                   
000125                                                                          
000126 01  SISTA-POST-SKRIVEN          PIC X       VALUE 'N'.                   
000127                                                                          
000128 77  FORSTA-POST-DC11-SW         PIC X       VALUE 'J'.                   
000129     88  FORSTA-POSTEN-DC11                  VALUE 'J'.                   
000130     88  ANDRA-POSTEN-DC11                  VALUE 'N'.                    
000140                                                                          
000141 77  FORSTA-POST-DC91-SW         PIC X       VALUE 'J'.                   
000142     88  FORSTA-POSTEN-DC91                  VALUE 'J'.                   
000143     88  ANDRA-POSTEN-DC91                  VALUE 'N'.                    
000144                                                                          
000145 77  FORSTA-POST-DC61-SW         PIC X       VALUE 'J'.                   
000146     88  FORSTA-POSTEN-DC61                  VALUE 'J'.                   
000147     88  ANDRA-POSTEN-DC61                  VALUE 'N'.                    
000148                                                                          
000149 77  FORSTA-POST-SW              PIC X       VALUE 'J'.                   
000150     88  FORSTA-POSTEN                       VALUE 'J'.                   
000160     88  ANDRA-POSTEN                       VALUE 'N'.                    
000161                                                                          
000162 77  SPAR-KDMFUP                 PIC X(2)  VALUE SPACE.                   
000163 77  SPAR-IDDC                   PIC X(2)  VALUE SPACE.                   
000164                                                                          
000165 77  SPAR-IDDISTR-DC11           PIC S9(5) VALUE +0 COMP-3.               
000166 77  SPAR-IDDISTR-DC91           PIC S9(5) VALUE +0 COMP-3.               
000167 77  SPAR-IDDISTR-DC61           PIC S9(5) VALUE +0 COMP-3.               
000168 77  SPAR-IDDISTR                PIC S9(5) VALUE +0 COMP-3.               
000169                                                                          
000170 77  SPAR-IDKUNDNR-DC11          PIC S9(7) VALUE +0 COMP-3.               
000180 77  SPAR-IDKUNDNR-DC91          PIC S9(7) VALUE +0 COMP-3.               
000181 77  SPAR-IDKUNDNR-DC61          PIC S9(7) VALUE +0 COMP-3.               
000182 77  SPAR-IDKUNDNR               PIC S9(7) VALUE +0 COMP-3.               
000183                                                                          
000184 77  W-ANTALPOSTER-TOT-DC11      PIC S9(7)   VALUE +0 COMP-3.             
000185 77  W-ANTALPOSTER-TOT-DC91      PIC S9(7)   VALUE +0 COMP-3.             
000186 77  W-ANTALPOSTER-TOT-DC61      PIC S9(7)   VALUE +0 COMP-3.             
000187 77  W-ANTALPOSTER-DC11          PIC S9(4)   VALUE +0 COMP-3.             
000188 77  W-ANTALPOSTER-DC91          PIC S9(4)   VALUE +0 COMP-3.             
000189 77  W-ANTALPOSTER-DC61          PIC S9(4)   VALUE +0 COMP-3.             
000190 77  W-ANTALPOSTER               PIC S9(4)   VALUE +0 COMP-3.             
000200 77  W-ANTALSUMMA-DC11           PIC S9(7)V99 VALUE +0 COMP-3.            
000201 77  W-ANTALSUMMA-DC91           PIC S9(7)V99 VALUE +0 COMP-3.            
000202 77  W-ANTALSUMMA-DC61           PIC S9(7)V99 VALUE +0 COMP-3.            
000203 77  W-ANTALSUMMA                PIC S9(7)V99 VALUE +0 COMP-3.            
000204 77  W-KVARBDAG-DC11             PIC S9(4)   VALUE +0 COMP-3.             
000205 77  W-KVARBDAG-DC91             PIC S9(4)   VALUE +0 COMP-3.             
000206 77  W-KVARBDAG-DC61             PIC S9(4)   VALUE +0 COMP-3.             
000207 77  W-KVARBDAG                  PIC S9(4)   VALUE +0 COMP-3.             
000208     EJECT                                                                
000209 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
000210 01  FILLER REDEFINES DAGENS-DATUM.                                       
000211     03  DAGENS-DATUM-AAR        PIC 9(2).                                
000212     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
000213     03  DAGENS-DATUM-DAG        PIC 9(2).                                
000214                                                                          
000215 01  WS-DATUM.                                                            
000216     03  DATUM-SEKEL             PIC X(2).                                
000217     03  DATUM-AAR               PIC X(2).                                
000218     03  FILLER                  PIC X(1)    VALUE '-'.                   
000219     03  DATUM-MAANAD            PIC X(2).                                
000220     03  FILLER                  PIC X(1)    VALUE '-'.                   
000221     03  DATUM-DAG               PIC X(2).                                
000222                                                                          
000223                                                                          
000224 01  WS-DATUM-GB.                                                         
000225     03  DATUM-GB-DAG            PIC X(2).                                
000226     03  FILLER                  PIC X(1)    VALUE '-'.                   
000227     03  DATUM-GB-MAANAD         PIC X(2).                                
000228     03  FILLER                  PIC X(1)    VALUE '-'.                   
000229     03  DATUM-GB-SEKEL          PIC X(2).                                
000230     03  DATUM-GB-AAR            PIC X(2).                                
000231                                                                          
000232                                                                          
000233                                                                          
000234 01  WS-DATUM-US.                                                         
000235     03  DATUM-US-MAANAD         PIC X(2).                                
000236     03  FILLER                  PIC X(1)    VALUE '-'.                   
000237     03  DATUM-US-DAG            PIC X(2).                                
000238     03  FILLER                  PIC X(1)    VALUE '-'.                   
000239     03  DATUM-US-SEKEL          PIC X(2).                                
000240     03  DATUM-US-AAR            PIC X(2).                                
000241                                                                          
000242                                                                          
000243     EJECT                                                                
000244 01  DYNAMISKA-SUBPROGRAM.                                                
000245*                                                                         
000246     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
000247     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
000248     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
000249     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
000250     03  WL10WBDC                PIC X(8)    VALUE 'WL10WBDC'.            
000251     SKIP2                                                                
000252*- - - - - - - - - - - - - -  PARAMETRAR TILL DATKORT                     
000253 01  FILLER                      PIC X(16)   VALUE 'DATKORT'.             
000254 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
000255     SKIP2                                                                
000256*01  -COPY WDATKORT                                                       
000257     SKIP2                                                                
000258*- - - - - - - - - - - - - -  PARAMETRAR TILL WL10WBDC                    
000259*01  -COPY WL10WBDC                                                       
000260     SKIP2                                                                
000261*    --- PARAMETRAR TILL ABEND                                            
000262                                                                          
000263 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
000264 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
000265 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
000266     SKIP2                                                                
000267 01  FELTEXT.                                                             
000268     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
000269     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
000270     EJECT                                                                
000271*    --- PARAMETRAR TILL POSTSUM                                          
000272*                                                                         
000273*01  -COPY W0005   -PRE  POSTSUM-                                         
000274     EJECT                                                                
000275*    --- VALID IDDC CODES                                                 
000276*                                                                         
000277*01  -COPY WWDC99                                                         
000278     EJECT                                                                
000279*01  -COPY WDATAREA                                                       
000280     EJECT                                                                
000281 01  IN-AREA-START               PIC X(24)   VALUE                        
000282                                 'IN-AREA-START  '.                       
000283     SKIP2                                                                
000284                                                                          
000285*01  AREA -COPY W3714E     -PRE IN-                                       
000286     EJECT                                                                
000287 01  WUT-AREA-START             PIC X(24)   VALUE                         
000288                                 'UT-RAD-AREA-START'.                     
000289 01  UT-RAD-DC11                 PIC X(121)  VALUE SPACE.                 
000290 01  UT-RAD-DC91                 PIC X(121)  VALUE SPACE.                 
000300 01  UT-RAD-DC61                 PIC X(121)  VALUE SPACE.                 
000301 01  UT-RAD                      PIC X(121)  VALUE SPACE.                 
000302                                                                          
000303 01  W-HJLP-AREA-START          PIC X(24)   VALUE                         
000304                                 'W-HJALP-AREA-START  '.                  
000305     SKIP2                                                                
000306 01  W-HJALPAREOR.                                                        
000307*                                                                         
000308     03  W-SKIP                  PIC 9(3) COMP-3  VALUE 1.                
000309     03  W-ANTAL-RADER-DC11                                               
000310                                 PIC 9(3)    VALUE 999.                   
000311     03  W-ANTAL-RADER-DC91                                               
000312                                 PIC 9(3)    VALUE 999.                   
000313     03  W-ANTAL-RADER-DC61                                               
000314                                 PIC 9(3)    VALUE 999.                   
000315     03  W-MAX-RADER-PER-SIDA                                             
000316                                 PIC 9(3)    VALUE 42.                    
000317     03  W-MAX-POSITIONER-PER-RAD                                         
000318                                 PIC 9(3)    VALUE 120.                   
000319     03  W-SIDRAKNARE-DC11       PIC S9(5)   COMP-3 VALUE ZERO.           
000320                                                                          
000330     03  W-SIDRAKNARE-DC91       PIC S9(5)   COMP-3 VALUE ZERO.           
000331                                                                          
000332     03  W-SIDRAKNARE-DC61       PIC S9(5)   COMP-3 VALUE ZERO.           
000333                                                                          
000334     EJECT                                                                
000335*                                                                         
000336     EJECT                                                                
000337 01  W002-AREA-START             PIC X(24)   VALUE                        
000338                                 'W002-AREA-START  '.                     
000339     SKIP2                                                                
000340 01  W-RUBRIK1-DC11.                                                      
000341     03  FILLER                PIC X(1)   VALUE '1'.                      
000342     03  FILLER                PIC X(2)   VALUE SPACE.                    
000343     03  FILLER                PIC X(15)  VALUE 'W37157-011'.             
000344     03  FILLER                PIC X(33)                                  
000345                  VALUE ' VECKO RAPPORT BYTESTUPPFÖLJNING '.              
000346     03  FILLER                PIC X(34)                                  
000347                  VALUE 'TIDEN DET TAR FRÅN STATUS 2 TILL 3'.             
000348     03  FILLER                PIC X(7)   VALUE ' DATUM '.                
000349     03  W-DAGENS-DATUM-DC11   PIC X(10)  VALUE SPACE.                    
000350     03  FILLER                PIC X(5)   VALUE SPACE.                    
000351     03  FILLER                PIC X(5)  VALUE 'SIDA '.                   
000352     03  W-SID-DC11            PIC Z(4)9.                                 
000353     03  FILLER                PIC X(3)  VALUE SPACE.                     
000354     EJECT                                                                
000355*                                                                         
000356 01  W-RUBRIK1-DC91.                                                      
000357     03  FILLER                PIC X(10)  VALUE 'W37157-091'.             
000358     03  FILLER                PIC X(1)   VALUE ';'.                      
000359     03  FILLER                PIC X(33)                                  
000360                  VALUE 'AVERAGE TIME;IN DAYS FROM STATUS;'.              
000361     03  FILLER                PIC X(25)                                  
000362                  VALUE '2 TO STATUS 3 WEEKLY LIST'.                      
000363     03  FILLER                PIC X(1)   VALUE ';'.                      
000364     03  FILLER                PIC X(7)   VALUE 'DATE;'.                  
000365     03  W-DAGENS-DATUM-DC91   PIC X(10)  VALUE SPACE.                    
000366     03  FILLER                PIC X(1)   VALUE ';'.                      
000367     EJECT                                                                
000368*                                                                         
000369 01  W-RUBRIK1-DC61.                                                      
000370     03  FILLER                PIC X(1)   VALUE '1'.                      
000380     03  FILLER                PIC X(2)   VALUE SPACE.                    
000390     03  FILLER                PIC X(15)  VALUE 'W37157-061'.             
000400     03  FILLER                PIC X(33)                                  
000410                  VALUE 'AVERAGE TIME IN DAYS FROM STATUS '.              
000420     03  FILLER                PIC X(34)                                  
000421                  VALUE '2 TO STATUS 3 WEEKLY LIST         '.             
000422     03  FILLER                PIC X(6)   VALUE 'DATE'.                   
000423     03  W-DAGENS-DATUM-DC61   PIC X(10)  VALUE SPACE.                    
000424     03  FILLER                PIC X(5)   VALUE SPACE.                    
000425     03  FILLER                PIC X(5)  VALUE 'PAGE '.                   
000426     03  W-SID-DC61            PIC Z(4)9.                                 
000427     03  FILLER                PIC X(4)  VALUE SPACE.                     
000428     EJECT                                                                
000429*                                                                         
000430*                                                                         
000431 01  W-RUBRIK1.                                                           
000432     03  FILLER                PIC X(3)   VALUE SPACE.                    
000433     03  FILLER                PIC X(15)  VALUE 'W37157-001'.             
000434     03  FILLER                PIC X(33)                                  
000435                  VALUE 'AVERAGE TIME IN DAYS FROM STATUS '.              
000436     03  FILLER                PIC X(26)                                  
000437                  VALUE '2 TO STATUS 3 WEEKLY LIST '.                     
000438     03  W-KDMFUP              PIC X(5)   VALUE SPACE.                    
000439     03  FILLER                PIC X(3)   VALUE SPACE.                    
000440     03  FILLER                PIC X(6)   VALUE 'DATE'.                   
000441     03  W-DAGENS-DATUM        PIC X(10)  VALUE SPACE.                    
000442     03  FILLER                PIC X(5)   VALUE SPACE.                    
000443     03  FILLER                PIC X(5)   VALUE 'PAGE '.                  
000444     03  FILLER                PIC X(5)   VALUE '    1'.                  
000445     03  FILLER                PIC X(4)   VALUE SPACE.                    
000446     EJECT                                                                
000447*                                                                         
000448     SKIP2                                                                
000449 01  W-RUBRIK2-DC11.                                                      
000450     03  FILLER                PIC X(1)   VALUE '0'.                      
000451     03  FILLER                PIC X(2)   VALUE SPACE.                    
000452     03  FILLER                PIC X(15)  VALUE 'DISTRIKT  '.             
000453     03  FILLER                PIC X(15)  VALUE 'KUND '.                  
000454     03  FILLER                PIC X(15)  VALUE 'ANTAL '.                 
000455     03  FILLER                PIC X(5)   VALUE SPACE.                    
000456     03  FILLER                PIC X(35)  VALUE SPACE.                    
000457     03  FILLER                PIC X(32)  VALUE SPACE.                    
000458*                                                                         
000459 01  W-RUBRIK2-DC91.                                                      
000460     03  FILLER                PIC X(8)  VALUE 'DISTRICT'.                
000461     03  FILLER                PIC X(1)   VALUE ';'.                      
000462     03  FILLER                PIC X(8)  VALUE 'CUSTOMER'.                
000463     03  FILLER                PIC X(1)   VALUE ';'.                      
000464     03  FILLER                PIC X(38)  VALUE                           
000465                  'DAYS;(WORKING DAY S EXCL. WEEKENDDAYS)'.               
000466     03  FILLER                PIC X(1)   VALUE ';'.                      
000467     03  FILLER                PIC X(1)   VALUE ';'.                      
000468*                                                                         
000469 01  W-RUBRIK2-DC61.                                                      
000470     03  FILLER                PIC X(1)   VALUE '0'.                      
000480     03  FILLER                PIC X(2)   VALUE SPACE.                    
000490     03  FILLER                PIC X(15)  VALUE 'DISTRICT  '.             
000500     03  FILLER                PIC X(15)  VALUE 'CUSTOMER'.               
000510     03  FILLER                PIC X(04)  VALUE SPACE.                    
000511     03  FILLER                PIC X(38)  VALUE                           
000512               'DAYS (WORKING DAY S EXCL. WEEKENDDAYS)'.                  
000513     03  FILLER                PIC X(29)  VALUE SPACE.                    
000514*                                                                         
000515     EJECT                                                                
000516*                                                                         
000517 01  W-RUBRIK2.                                                           
000518     03  FILLER                PIC X(3)   VALUE SPACE.                    
000519     03  FILLER                PIC X(9)   VALUE 'DC'.                     
000520     03  FILLER                PIC X(15)  VALUE 'DISTRICT  '.             
000521     03  FILLER                PIC X(15)  VALUE 'CUSTOMER'.               
000522     03  FILLER                PIC X(04)  VALUE SPACE.                    
000523     03  FILLER                PIC X(38)  VALUE                           
000524               'DAYS (WORKING DAY S EXCL. WEEKENDDAYS)'.                  
000525     03  FILLER                PIC X(20)  VALUE SPACE.                    
000526*                                                                         
000527     EJECT                                                                
000528 01  W-DETALJ1-DC11.                                                      
000529     03  W-STYRTECKEN-DC11     PIC X(1)   VALUE '0'.                      
000530     03  FILLER                PIC X(2)   VALUE SPACE.                    
000531     03  FILLER                PIC X(3)   VALUE SPACE.                    
000532     03  W-IDDISTR-DC11        PIC Z(4)9  VALUE ZERO.                     
000533     03  FILLER                PIC X(4)   VALUE SPACE.                    
000534     03  W-IDKUNDNR-DC11       PIC Z(6)9  VALUE ZERO.                     
000535     03  FILLER                PIC X(6)   VALUE SPACE.                    
000536     03  WS-KVARBDAG-DC11      PIC Z(6)9.99 VALUE ZERO.                   
000537                                                                          
000538 01  W-DETALJ1-DC91.                                                      
000539     03  W-IDDISTR-DC91        PIC Z(4)9  VALUE ZERO.                     
000540     03  W-FILLER              PIC X(1)   VALUE ';'.                      
000541     03  W-IDKUNDNR-DC91       PIC Z(6)9  VALUE ZERO.                     
000542     03  W-FILLER              PIC X(1)   VALUE ';'.                      
000543     03  WS-KVARBDAG-DC91      PIC Z(6)9.99 VALUE ZERO.                   
000544     03  W-FILLER              PIC X(1)   VALUE ';'.                      
000545                                                                          
000546 01  W-DETALJ1-DC61.                                                      
000547     03  W-STYRTECKEN-DC61     PIC X(1)   VALUE '0'.                      
000548     03  FILLER                PIC X(2)   VALUE SPACE.                    
000549     03  FILLER                PIC X(3)   VALUE SPACE.                    
000550     03  W-IDDISTR-DC61        PIC Z(4)9  VALUE ZERO.                     
000560     03  FILLER                PIC X(8)   VALUE SPACE.                    
000570     03  W-IDKUNDNR-DC61       PIC Z(6)9  VALUE ZERO.                     
000580     03  FILLER                PIC X(5)   VALUE SPACE.                    
000581     03  WS-KVARBDAG-DC61      PIC Z(6)9.99 VALUE ZERO.                   
000582                                                                          
000583                                                                          
000584 01  W-DETALJ1.                                                           
000585     03  FILLER                PIC X(3)   VALUE SPACE.                    
000586     03  W-IDDC                PIC X(2)   VALUE SPACE.                    
000587     03  FILLER                PIC X(10)  VALUE SPACE.                    
000588     03  W-IDDISTR             PIC Z(4)9  VALUE ZERO.                     
000589     03  FILLER                PIC X(8)   VALUE SPACE.                    
000590     03  W-IDKUNDNR            PIC Z(6)9  VALUE ZERO.                     
000591     03  FILLER                PIC X(5)   VALUE SPACE.                    
000592     03  WS-KVARBDAG           PIC Z(6)9.99 VALUE ZERO.                   
000593                                                                          
000594 01  W-BLANKRAD.                                                          
000595     03  W-STYR                PIC X(1)   VALUE ' '.                      
000596     03  FILLER                PIC X(120) VALUE SPACE.                    
000597                                                                          
000598 01  DAP-CONTROL-REC1.                                                    
000599     03  FILLER                PIC X(15)  VALUE ' ¤DAPW37157-001'.        
000600 01  DAP-CONTROL-REC2.                                                    
000601     03  FILLER                PIC X(05)  VALUE ' ¤DAP'.                  
000602     03  DAP-CONTROL-KDMFUP    PIC X(02)  VALUE SPACE.                    
000603                                                                          
000604     EJECT                                                                
000605 PROCEDURE DIVISION.                                                      
000606 MAIN SECTION.                                                            
000607     SKIP2                                                                
000608                                                                          
000609     PERFORM A-INIT                                                       
000610     PERFORM S01-LAES-W3714E                                              
000611     PERFORM UNTIL END-OF-W3714E                                          
000612       PERFORM B-BEARBETA                                                 
000613       PERFORM S01-LAES-W3714E                                            
000614     END-PERFORM                                                          
000615                                                                          
000616     PERFORM C-KOLL-OM-SIST-POST                                          
000617                                                                          
000618     PERFORM Z-FINIT                                                      
000619                                                                          
000620     MOVE ZERO TO RETURN-CODE                                             
000621     GOBACK                                                               
000622     .                                                                    
000623     EJECT                                                                
000624 A-INIT SECTION.                                                          
000625                                                                          
000626     OPEN INPUT  W3714E                                                   
000627                                                                          
000628     OPEN OUTPUT LISTA                                                    
000629                 LISTB                                                    
000630                 LISTF                                                    
000640                 LISTG                                                    
000641     SKIP2                                                                
000642     ACCEPT DAGENS-DATUM  FROM DATE                                       
000643     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
000644*    DATUM-INFORMATION HÄMTAS FRÅN DATUMKORTET                            
000645                                                                          
000646     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
000647                                                                          
000648     MOVE   D-AAR            TO  DAGENS-DATUM-AAR                         
000649     MOVE   D-MAANAD         TO  DAGENS-DATUM-MAANAD                      
000650     MOVE   D-DAG            TO  DAGENS-DATUM-DAG                         
000651                                                                          
000652     IF DAGENS-DATUM-AAR > 50                                             
000653        MOVE 19              TO DATUM-SEKEL                               
000654                                DATUM-GB-SEKEL                            
000655                                DATUM-US-SEKEL                            
000656     ELSE                                                                 
000657        MOVE 20              TO DATUM-SEKEL                               
000658                                DATUM-GB-SEKEL                            
000659                                DATUM-US-SEKEL                            
000660     END-IF                                                               
000661                                                                          
000662     MOVE DAGENS-DATUM-AAR    TO DATUM-AAR                                
000663                                 DATUM-GB-AAR                             
000664                                 DATUM-US-AAR                             
000665     MOVE DAGENS-DATUM-MAANAD TO DATUM-MAANAD                             
000666                                 DATUM-GB-MAANAD                          
000667                                 DATUM-US-MAANAD                          
000668     MOVE DAGENS-DATUM-DAG    TO DATUM-DAG                                
000669                                 DATUM-GB-DAG                             
000670                                 DATUM-US-DAG                             
000671                                                                          
000672     MOVE WS-DATUM            TO W-DAGENS-DATUM-DC11                      
000673     MOVE WS-DATUM-GB         TO W-DAGENS-DATUM-DC91                      
000674     MOVE WS-DATUM-US         TO W-DAGENS-DATUM-DC61                      
000675                                 W-DAGENS-DATUM                           
000676     MOVE +1                  TO W-SIDRAKNARE-DC11                        
000677                                 W-SIDRAKNARE-DC91                        
000678                                 W-SIDRAKNARE-DC61                        
000679     MOVE 'J'                 TO SISTA-POST-SKRIVEN-DC11                  
000680     MOVE 'J'                 TO SISTA-POST-SKRIVEN-DC91                  
000690     MOVE 'J'                 TO SISTA-POST-SKRIVEN-DC61                  
000691     MOVE 'J'                 TO SISTA-POST-SKRIVEN                       
000692     .                                                                    
000693     EJECT                                                                
000694 B-BEARBETA SECTION.                                                      
000695                                                                          
000696     MOVE IN-IDDC        TO WS-IDDC                                       
000697                            WBDC-IDDC                                     
000698                                                                          
000699     CALL WL10WBDC          USING WBDC-AREA                               
000700                                                                          
000701     IF WBDC-FLWEBDC = 'J'                                                
000702       PERFORM BI-PROCESS-WEBDC                                           
000703     ELSE                                                                 
000704       PERFORM BH-KONTR-OM-FIRST-POST                                     
000705                                                                          
000706       EVALUATE TRUE                                                      
000707         WHEN CDC-SE                                                      
000708                                                                          
000709           IF SPAR-IDDISTR-DC11 NOT = IN-IDDISTR                          
000710              PERFORM BB-BERAKNA-ARBETDAGAR-DC11                          
000711              PERFORM BA-KONTROLLERA-SIDBRYTNING                          
000712              MOVE IN-IDDISTR  TO SPAR-IDDISTR-DC11                       
000713              MOVE IN-IDKUNDNR TO SPAR-IDKUNDNR-DC11                      
000714              PERFORM BBA-SUMMERA-ARBETDAGAR-DC11                         
000715           ELSE                                                           
000716             IF SPAR-IDKUNDNR-DC11 NOT = IN-IDKUNDNR                      
000717                PERFORM BB-BERAKNA-ARBETDAGAR-DC11                        
000718                PERFORM BA-KONTROLLERA-SIDBRYTNING                        
000719                MOVE IN-IDDISTR  TO SPAR-IDDISTR-DC11                     
000720                MOVE IN-IDKUNDNR TO SPAR-IDKUNDNR-DC11                    
000721                PERFORM BBA-SUMMERA-ARBETDAGAR-DC11                       
000722             ELSE                                                         
000723                PERFORM BBA-SUMMERA-ARBETDAGAR-DC11                       
000724             END-IF                                                       
000725           END-IF                                                         
000726                                                                          
000727         WHEN SDC-NL-ET                                                   
000728           IF SPAR-IDDISTR-DC91 NOT = IN-IDDISTR                          
000729              PERFORM BC-BERAKNA-ARBETDAGAR-DC91                          
000730              PERFORM BA-KONTROLLERA-SIDBRYTNING                          
000731              MOVE IN-IDDISTR  TO SPAR-IDDISTR-DC91                       
000732              MOVE IN-IDKUNDNR TO SPAR-IDKUNDNR-DC91                      
000733              PERFORM BCA-SUMMERA-ARBETDAGAR-DC91                         
000734           ELSE                                                           
000735             IF SPAR-IDKUNDNR-DC91 NOT = IN-IDKUNDNR                      
000736                PERFORM BC-BERAKNA-ARBETDAGAR-DC91                        
000737                PERFORM BA-KONTROLLERA-SIDBRYTNING                        
000738                MOVE IN-IDDISTR  TO SPAR-IDDISTR-DC91                     
000739                MOVE IN-IDKUNDNR TO SPAR-IDKUNDNR-DC91                    
000740                PERFORM BCA-SUMMERA-ARBETDAGAR-DC91                       
000741             ELSE                                                         
000742                PERFORM BCA-SUMMERA-ARBETDAGAR-DC91                       
000743             END-IF                                                       
000744           END-IF                                                         
000745         WHEN NDC-JP                                                      
000746           IF SPAR-IDDISTR-DC61 NOT = IN-IDDISTR                          
000747              PERFORM BG-BERAKNA-ARBETDAGAR-DC61                          
000748              PERFORM BA-KONTROLLERA-SIDBRYTNING                          
000749              MOVE IN-IDDISTR  TO SPAR-IDDISTR-DC61                       
000750              MOVE IN-IDKUNDNR TO SPAR-IDKUNDNR-DC61                      
000760              PERFORM BGA-SUMMERA-ARBETDAGAR-DC61                         
000770           ELSE                                                           
000780             IF SPAR-IDKUNDNR-DC61 NOT = IN-IDKUNDNR                      
000790                PERFORM BG-BERAKNA-ARBETDAGAR-DC61                        
000800                PERFORM BA-KONTROLLERA-SIDBRYTNING                        
000810                MOVE IN-IDDISTR  TO SPAR-IDDISTR-DC61                     
000811                MOVE IN-IDKUNDNR TO SPAR-IDKUNDNR-DC61                    
000812                PERFORM BGA-SUMMERA-ARBETDAGAR-DC61                       
000813             ELSE                                                         
000814                PERFORM BGA-SUMMERA-ARBETDAGAR-DC61                       
000815             END-IF                                                       
000816           END-IF                                                         
000817       END-EVALUATE                                                       
000818     END-IF                                                               
000819                                                                          
000820     .                                                                    
000821     EJECT                                                                
000822 BA-KONTROLLERA-SIDBRYTNING SECTION.                                      
000823                                                                          
000824     EVALUATE TRUE                                                        
000825         WHEN CDC-SE                                                      
000826            PERFORM BAA-KONTROLLERA-SIDBRYTNING                           
000827         WHEN SDC-NL-ET                                                   
000828            PERFORM BAB-KONTROLLERA-SIDBRYTNING                           
000829         WHEN NDC-JP                                                      
000830            PERFORM BAF-KONTROLLERA-SIDBRYTNING                           
000840     END-EVALUATE                                                         
000841                                                                          
000842     .                                                                    
000843     EJECT                                                                
000844 BAA-KONTROLLERA-SIDBRYTNING SECTION.                                     
000845                                                                          
000846     IF W-ANTAL-RADER-DC11 > W-MAX-RADER-PER-SIDA                         
000847        ADD +1 TO W-SIDRAKNARE-DC11                                       
000848        MOVE W-SIDRAKNARE-DC11 TO W-SID-DC11                              
000849        MOVE W-RUBRIK1-DC11  TO UT-RAD-DC11                               
000850        PERFORM S02-SKRIV-LISTA                                           
000851        MOVE W-RUBRIK2-DC11  TO UT-RAD-DC11                               
000852        PERFORM S02-SKRIV-LISTA                                           
000853        MOVE +4 TO W-ANTAL-RADER-DC11                                     
000854        MOVE '0'          TO W-STYRTECKEN-DC11                            
000855     ELSE                                                                 
000856        MOVE ' '          TO W-STYRTECKEN-DC11                            
000857     END-IF                                                               
000858     SKIP2                                                                
000859**** SKRIVER DEN ORDINARIE RADEN ****                                     
000860     MOVE SPAR-IDDISTR-DC11   TO W-IDDISTR-DC11                           
000861     MOVE SPAR-IDKUNDNR-DC11  TO W-IDKUNDNR-DC11                          
000862     MOVE W-DETALJ1-DC11      TO UT-RAD-DC11                              
000863     PERFORM S02-SKRIV-LISTA                                              
000864     SKIP2                                                                
000865     MOVE SPACE TO UT-RAD-DC11                                            
000866     ADD  +1 TO W-ANTAL-RADER-DC11                                        
000867     .                                                                    
000868     EJECT                                                                
000869 BAB-KONTROLLERA-SIDBRYTNING SECTION.                                     
000870*                                                                         
000880                                                                          
000881**** SKRIVER DEN ORDINARIE RADEN ****                                     
000882     MOVE SPAR-IDDISTR-DC91   TO W-IDDISTR-DC91                           
000883     MOVE SPAR-IDKUNDNR-DC91  TO W-IDKUNDNR-DC91                          
000884     MOVE W-DETALJ1-DC91      TO UT-RAD-DC91                              
000885     PERFORM S03-SKRIV-LISTB                                              
000886     SKIP2                                                                
000887     MOVE SPACE TO UT-RAD-DC91                                            
000888     ADD  +1 TO W-ANTAL-RADER-DC91                                        
000889     .                                                                    
000890     EJECT                                                                
000900 BAF-KONTROLLERA-SIDBRYTNING SECTION.                                     
000910                                                                          
000920     IF W-ANTAL-RADER-DC61 > W-MAX-RADER-PER-SIDA                         
000930        ADD +1 TO W-SIDRAKNARE-DC61                                       
000940        MOVE W-SIDRAKNARE-DC61 TO W-SID-DC61                              
000950        MOVE W-RUBRIK1-DC61  TO UT-RAD-DC61                               
000960        PERFORM S07-SKRIV-LISTF                                           
000970        MOVE W-RUBRIK2-DC61  TO UT-RAD-DC61                               
000980        PERFORM S07-SKRIV-LISTF                                           
000981        MOVE +4 TO W-ANTAL-RADER-DC61                                     
000982        MOVE '0'          TO W-STYRTECKEN-DC61                            
000983     ELSE                                                                 
000984        MOVE ' '          TO W-STYRTECKEN-DC61                            
000985     END-IF                                                               
000986     SKIP2                                                                
000987**** SKRIVER DEN ORDINARIE RADEN ****                                     
000988     MOVE SPAR-IDDISTR-DC61   TO W-IDDISTR-DC61                           
000989     MOVE SPAR-IDKUNDNR-DC61  TO W-IDKUNDNR-DC61                          
000990     MOVE W-DETALJ1-DC61      TO UT-RAD-DC61                              
000991     PERFORM S07-SKRIV-LISTF                                              
000992     SKIP2                                                                
000993     MOVE SPACE TO UT-RAD-DC61                                            
000994     ADD  +1 TO W-ANTAL-RADER-DC61                                        
000995     .                                                                    
000996     EJECT                                                                
000997 BB-BERAKNA-ARBETDAGAR-DC11 SECTION.                                      
000998                                                                          
000999     IF W-ANTALPOSTER-DC11 > ZERO                                         
001000        COMPUTE W-ANTALSUMMA-DC11 =                                       
001001                W-KVARBDAG-DC11 / W-ANTALPOSTER-DC11                      
001002     END-IF                                                               
001003     MOVE W-ANTALSUMMA-DC11   TO WS-KVARBDAG-DC11                         
001004***  NOLLSTÄLL RÄKNARE *****                                              
001005                                                                          
001006     MOVE 'J'                 TO SISTA-POST-SKRIVEN-DC11                  
001007     MOVE ZERO                TO W-ANTALSUMMA-DC11                        
001008     MOVE ZERO                TO  W-ANTALPOSTER-DC11                      
001009     MOVE ZERO                TO  W-KVARBDAG-DC11                         
001010                                                                          
001011     .                                                                    
001012     EJECT                                                                
001013 BBA-SUMMERA-ARBETDAGAR-DC11 SECTION.                                     
001014                                                                          
001015     ADD +1            TO W-ANTALPOSTER-DC11                              
001016     ADD +1            TO W-ANTALPOSTER-TOT-DC11                          
001017     ADD IN-KVARBDAG   TO W-KVARBDAG-DC11                                 
001018     MOVE 'N'                 TO SISTA-POST-SKRIVEN-DC11                  
001019                                                                          
001020                                                                          
001021     .                                                                    
001022     EJECT                                                                
001023 BC-BERAKNA-ARBETDAGAR-DC91 SECTION.                                      
001024                                                                          
001025***  BERÄKNAR SNITT ARBETSTIDEN EN BYTESRAPPORT TAR FRÅN ****             
001026***  STATUS 2  TILL 3 INOM EN PERIOD                     ****             
001027                                                                          
001028     IF W-ANTALPOSTER-DC91 > ZERO                                         
001029        COMPUTE W-ANTALSUMMA-DC91 =                                       
001030                W-KVARBDAG-DC91 / W-ANTALPOSTER-DC91                      
001031     END-IF                                                               
001032     MOVE W-ANTALSUMMA-DC91   TO WS-KVARBDAG-DC91                         
001033     MOVE 'J'                 TO SISTA-POST-SKRIVEN-DC91                  
001034                                                                          
001035***  NOLLSTÄLL RÄKNARE *****                                              
001036                                                                          
001037     MOVE ZERO                TO W-ANTALSUMMA-DC91                        
001038     MOVE ZERO                TO  W-ANTALPOSTER-DC91                      
001039     MOVE ZERO                TO  W-KVARBDAG-DC91                         
001040                                                                          
001041     .                                                                    
001042     EJECT                                                                
001043                                                                          
001044 BCA-SUMMERA-ARBETDAGAR-DC91 SECTION.                                     
001045                                                                          
001046     ADD +1            TO W-ANTALPOSTER-DC91                              
001047     ADD +1            TO W-ANTALPOSTER-TOT-DC91                          
001048     ADD IN-KVARBDAG   TO W-KVARBDAG-DC91                                 
001049     MOVE 'N'          TO SISTA-POST-SKRIVEN-DC91                         
001050                                                                          
001051                                                                          
001052     .                                                                    
001053     EJECT                                                                
001054 BG-BERAKNA-ARBETDAGAR-DC61 SECTION.                                      
001055                                                                          
001056     IF W-ANTALPOSTER-DC61 > ZERO                                         
001057        COMPUTE W-ANTALSUMMA-DC61 =                                       
001058                W-KVARBDAG-DC61 / W-ANTALPOSTER-DC61                      
001059     END-IF                                                               
001060     MOVE W-ANTALSUMMA-DC61   TO WS-KVARBDAG-DC61                         
001070     MOVE 'J'          TO SISTA-POST-SKRIVEN-DC61                         
001080***  NOLLSTÄLL RÄKNARE *****                                              
001090                                                                          
001100     MOVE ZERO                TO W-ANTALSUMMA-DC61                        
001110     MOVE ZERO                TO  W-ANTALPOSTER-DC61                      
001120     MOVE ZERO                TO  W-KVARBDAG-DC61                         
001130                                                                          
001140                                                                          
001141     .                                                                    
001142     EJECT                                                                
001143 BGA-SUMMERA-ARBETDAGAR-DC61 SECTION.                                     
001144                                                                          
001145     ADD +1            TO W-ANTALPOSTER-DC61                              
001146     ADD +1            TO W-ANTALPOSTER-TOT-DC61                          
001147     ADD IN-KVARBDAG   TO W-KVARBDAG-DC61                                 
001148     MOVE 'N'          TO SISTA-POST-SKRIVEN-DC61                         
001149                                                                          
001150     .                                                                    
001151     EJECT                                                                
001152 BH-KONTR-OM-FIRST-POST SECTION.                                          
001153                                                                          
001154     EVALUATE TRUE                                                        
001155         WHEN CDC-SE                                                      
001156           IF FORSTA-POSTEN-DC11                                          
001157              MOVE 'N'           TO FORSTA-POST-DC11-SW                   
001158              MOVE IN-IDDISTR    TO SPAR-IDDISTR-DC11                     
001159              MOVE IN-IDKUNDNR   TO SPAR-IDKUNDNR-DC11                    
001160              MOVE W-SIDRAKNARE-DC11 TO W-SID-DC11                        
001161              MOVE W-RUBRIK1-DC11  TO UT-RAD-DC11                         
001162              PERFORM S02-SKRIV-LISTA                                     
001163              MOVE W-RUBRIK2-DC11  TO UT-RAD-DC11                         
001164              PERFORM S02-SKRIV-LISTA                                     
001165              MOVE +4 TO W-ANTAL-RADER-DC11                               
001166              MOVE W-BLANKRAD    TO UT-RAD-DC11                           
001167              PERFORM S02-SKRIV-LISTA                                     
001168           END-IF                                                         
001169         WHEN SDC-NL-ET                                                   
001170           IF FORSTA-POSTEN-DC91                                          
001171              MOVE 'N'           TO FORSTA-POST-DC91-SW                   
001172              MOVE IN-IDDISTR    TO SPAR-IDDISTR-DC91                     
001173              MOVE IN-IDKUNDNR   TO SPAR-IDKUNDNR-DC91                    
001174              MOVE W-RUBRIK1-DC91  TO UT-RAD-DC91                         
001175              PERFORM S03-SKRIV-LISTB                                     
001176              MOVE W-RUBRIK2-DC91  TO UT-RAD-DC91                         
001177              PERFORM S03-SKRIV-LISTB                                     
001178              MOVE +4 TO W-ANTAL-RADER-DC91                               
001179              MOVE W-BLANKRAD    TO UT-RAD-DC91                           
001180           END-IF                                                         
001190         WHEN NDC-JP                                                      
001200           IF FORSTA-POSTEN-DC61                                          
001210              MOVE 'N'           TO FORSTA-POST-DC61-SW                   
001220              MOVE IN-IDDISTR    TO SPAR-IDDISTR-DC61                     
001230              MOVE IN-IDKUNDNR   TO SPAR-IDKUNDNR-DC61                    
001231              MOVE W-SIDRAKNARE-DC61 TO W-SID-DC61                        
001232              MOVE W-RUBRIK1-DC61  TO UT-RAD-DC61                         
001233              PERFORM S07-SKRIV-LISTF                                     
001234              MOVE W-RUBRIK2-DC61  TO UT-RAD-DC61                         
001235              PERFORM S07-SKRIV-LISTF                                     
001236              MOVE +4 TO W-ANTAL-RADER-DC61                               
001237              MOVE W-BLANKRAD    TO UT-RAD-DC61                           
001238              PERFORM S07-SKRIV-LISTF                                     
001239           END-IF                                                         
001240     END-EVALUATE                                                         
001241     .                                                                    
001242     EJECT                                                                
001243                                                                          
001244 BI-PROCESS-WEBDC SECTION.                                                
001245                                                                          
001246     IF WBDC-KDMFUP = SPAR-KDMFUP                                         
001247       IF IN-IDDC = SPAR-IDDC                                             
001248         IF IN-IDDISTR = SPAR-IDDISTR AND                                 
001249            IN-IDKUNDNR = SPAR-IDKUNDNR                                   
001250           PERFORM BIC-SUM-WORKDAYS                                       
001251         ELSE                                                             
001252           PERFORM BID-WRITE-WORKDAYS                                     
001253           PERFORM BIC-SUM-WORKDAYS                                       
001254           MOVE IN-IDDISTR       TO SPAR-IDDISTR                          
001255           MOVE IN-IDKUNDNR      TO SPAR-IDKUNDNR                         
001256         END-IF                                                           
001257       ELSE                                                               
001258         PERFORM BID-WRITE-WORKDAYS                                       
001259         MOVE W-BLANKRAD         TO UT-RAD                                
001260         PERFORM S08-SKRIV-LISTG                                          
001261         PERFORM BIC-SUM-WORKDAYS                                         
001262         MOVE IN-IDDC            TO SPAR-IDDC                             
001263         MOVE IN-IDDISTR         TO SPAR-IDDISTR                          
001264         MOVE IN-IDKUNDNR        TO SPAR-IDKUNDNR                         
001265       END-IF                                                             
001266     ELSE                                                                 
001267       IF FORSTA-POSTEN                                                   
001268         SET ANDRA-POSTEN        TO TRUE                                  
001269       ELSE                                                               
001270         PERFORM BID-WRITE-WORKDAYS                                       
001271       END-IF                                                             
001272       PERFORM BIA-WRITE-DAP-RECORDS                                      
001273       PERFORM BIB-WRITE-HEADER                                           
001274       PERFORM BIC-SUM-WORKDAYS                                           
001275       MOVE WBDC-KDMFUP          TO SPAR-KDMFUP                           
001276       MOVE IN-IDDC              TO SPAR-IDDC                             
001277       MOVE IN-IDDISTR           TO SPAR-IDDISTR                          
001278       MOVE IN-IDKUNDNR          TO SPAR-IDKUNDNR                         
001279     END-IF                                                               
001280                                                                          
001281     .                                                                    
001282     EJECT                                                                
001283                                                                          
001284 BIA-WRITE-DAP-RECORDS SECTION.                                           
001285                                                                          
001286*    DAP CONTROL RECORDS                                                  
001287     MOVE DAP-CONTROL-REC1       TO UT-RAD                                
001288     PERFORM S08-SKRIV-LISTG                                              
001289                                                                          
001290     MOVE WBDC-KDMFUP            TO DAP-CONTROL-KDMFUP                    
001291     MOVE DAP-CONTROL-REC2       TO UT-RAD                                
001292     PERFORM S08-SKRIV-LISTG                                              
001293     .                                                                    
001294     EJECT                                                                
001295                                                                          
001296 BIB-WRITE-HEADER SECTION.                                                
001297                                                                          
001298*    DC HEADER                                                            
001299     MOVE WBDC-KDMFUP            TO W-KDMFUP                              
001300     MOVE W-RUBRIK1              TO UT-RAD                                
001301     PERFORM S08-SKRIV-LISTG                                              
001302     MOVE W-RUBRIK2              TO UT-RAD                                
001303     PERFORM S08-SKRIV-LISTG                                              
001304     MOVE W-BLANKRAD             TO UT-RAD                                
001305     PERFORM S08-SKRIV-LISTG                                              
001306     .                                                                    
001307     EJECT                                                                
001308                                                                          
001309 BIC-SUM-WORKDAYS SECTION.                                                
001310                                                                          
001311     ADD +1                      TO W-ANTALPOSTER                         
001312     ADD IN-KVARBDAG             TO W-KVARBDAG                            
001313     MOVE 'N'                    TO SISTA-POST-SKRIVEN                    
001314     .                                                                    
001315     EJECT                                                                
001316                                                                          
001317 BID-WRITE-WORKDAYS SECTION.                                              
001318                                                                          
001319     IF W-ANTALPOSTER > ZERO                                              
001320        COMPUTE W-ANTALSUMMA =                                            
001321                W-KVARBDAG / W-ANTALPOSTER                                
001322     END-IF                                                               
001323                                                                          
001324     MOVE W-ANTALSUMMA           TO WS-KVARBDAG                           
001325     MOVE SPAR-IDDC              TO W-IDDC                                
001326     MOVE SPAR-IDDISTR           TO W-IDDISTR                             
001327     MOVE SPAR-IDKUNDNR          TO W-IDKUNDNR                            
001328     MOVE W-DETALJ1              TO UT-RAD                                
001329     PERFORM S08-SKRIV-LISTG                                              
001330                                                                          
001331     MOVE 'J'                    TO SISTA-POST-SKRIVEN                    
001332     MOVE SPACE                  TO UT-RAD                                
001333     MOVE ZERO                   TO W-ANTALSUMMA                          
001334     MOVE ZERO                   TO W-ANTALPOSTER                         
001335     MOVE ZERO                   TO W-KVARBDAG                            
001336     .                                                                    
001337     EJECT                                                                
001338                                                                          
001339 C-KOLL-OM-SIST-POST SECTION.                                             
001340                                                                          
001341     IF SISTA-POST-SKRIVEN-DC11 = 'N'                                     
001342        PERFORM BB-BERAKNA-ARBETDAGAR-DC11                                
001343        PERFORM BAA-KONTROLLERA-SIDBRYTNING                               
001344     END-IF                                                               
001345                                                                          
001346     IF SISTA-POST-SKRIVEN-DC91 = 'N'                                     
001347        PERFORM BC-BERAKNA-ARBETDAGAR-DC91                                
001348        PERFORM BAB-KONTROLLERA-SIDBRYTNING                               
001349     END-IF                                                               
001350                                                                          
001351     IF SISTA-POST-SKRIVEN-DC61 = 'N'                                     
001360        PERFORM BG-BERAKNA-ARBETDAGAR-DC61                                
001370        PERFORM BAF-KONTROLLERA-SIDBRYTNING                               
001371     END-IF                                                               
001372                                                                          
001373     IF SISTA-POST-SKRIVEN = 'N'                                          
001374        PERFORM BID-WRITE-WORKDAYS                                        
001375     END-IF                                                               
001376                                                                          
001377     .                                                                    
001378     EJECT                                                                
001379 Z-FINIT SECTION.                                                         
001380     CLOSE W3714E                                                         
001381           LISTA                                                          
001382           LISTB                                                          
001383           LISTF                                                          
001384           LISTG                                                          
001385     SKIP2                                                                
001386     MOVE 'S' TO POSTSUM-OPKOD                                            
001387     CALL POSTSUM USING POSTSUM-PARM                                      
001388     .                                                                    
001389     EJECT                                                                
001390                                                                          
001391 S01-LAES-W3714E  SECTION.                                                
001392     READ W3714E INTO IN-AREA                                             
001393     AT END                                                               
001394        MOVE HIGH-VALUE TO IN-AREA                                        
001395        SET END-OF-W3714E TO TRUE                                         
001396                                                                          
001397     NOT AT END                                                           
001398        MOVE 'W3714E' TO POSTSUM-FDNAMN                                   
001399        MOVE 'W37157D1' TO POSTSUM-DDNAMN2                                
001400        MOVE 'IN  '    TO POSTSUM-TRANSTYP                                
001401        CALL POSTSUM USING POSTSUM-PARM                                   
001402     END-READ                                                             
001403     .                                                                    
001404     EJECT                                                                
001405                                                                          
001406 S02-SKRIV-LISTA  SECTION.                                                
001407                                                                          
001408     WRITE LISTAS FROM UT-RAD-DC11                                        
001409                                                                          
001410     .                                                                    
001411     EJECT                                                                
001412 S03-SKRIV-LISTB  SECTION.                                                
001413                                                                          
001414     WRITE LISTBS FROM UT-RAD-DC91                                        
001415                                                                          
001416     .                                                                    
001417     EJECT                                                                
001418                                                                          
001419 S07-SKRIV-LISTF  SECTION.                                                
001420                                                                          
001430     WRITE LISTFS FROM UT-RAD-DC61                                        
001440                                                                          
001450     .                                                                    
001451     EJECT                                                                
001452                                                                          
001453 S08-SKRIV-LISTG  SECTION.                                                
001454                                                                          
001455     WRITE LISTGS FROM UT-RAD                                             
001456                                                                          
001457     .                                                                    
001458     EJECT                                                                
001459                                                                          
001460 S99-ABEND SECTION.                                                       
001461                                                                          
001462     SKIP2                                                                
001463     MOVE 'S' TO POSTSUM-OPKOD                                            
001464     CALL POSTSUM USING POSTSUM-PARM                                      
001465     CALL ABEND USING RKOD-ABEND                                          
001466     .                                                                    
