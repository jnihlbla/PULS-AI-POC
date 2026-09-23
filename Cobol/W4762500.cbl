000001*********************************************                             
000002 ID DIVISION.                                                             
000003 PROGRAM-ID.     W4762500.                                                
000004 AUTHOR.         MOGREN STINA.                                            
000005 DATE-WRITTEN.   02/06/20.                                                
000006 DATE-COMPILED.                                                           
000007*                                                                         
000008*    FUNKTION:                                                            
000009*    **DDI  SKALL INSTALLERAS SENARE  (030202)                            
000010*                                                                         
000011*        PROGRAMMET STARTAS PÅ TID (SHIP-IT)                              
000012*        VIA SOP-RUTIN W476D1                                             
000013*        IDSHIPM ÄR PARAMETER IN                                          
000014*        SKAPA EN POST PER KOLLI PÅ ....                                  
000015*                     W47628 (TRANSPORT INFO, WRAPPING PROFORMA)          
000016*                     W47630 (TRANSPORT INFO, WRAPPING INFO VIPS)         
000017*                     W47635 (TRANSPORT INFO, BROKER AT)                  
000018*                     W47636 (TRANSPORT INFO, BROKER TO)                  
000019*                     W47637 (TRANSPORT INFO, BROKER JP)                  
000020*                     W47638 (TRANSPORT INFO, BROKER AU)                  
000021*                                                                         
000022*                                                                         
000023*        PROGRAMMET LÄSER      WDE1                                       
000024*                              WDE4                                       
000025*                              WDE7                                       
000026*                              WDK6                                       
000027*                              WDK6                                       
000028*                              WDB6                                       
000029*                              WDD3                                       
000030*                                                                         
000031*    ABENDKODER:                                                          
000032*        U0016 -  . . . .                                                 
000033*        U1000 -  . . . .                                                 
000034*                                                                         
000035* ETRACKER 1570221 EMBALLAGE INFO TO VTAB FEB. 2006                       
000036* ETRACKER 6044087 BORTTAG AV FIL W47632  2006/2007                       
000037*                                                                         
000038                                                                          
000039     SKIP3                                                                
000040 ENVIRONMENT DIVISION.                                                    
000041     SKIP2                                                                
000042 INPUT-OUTPUT SECTION.                                                    
000043                                                                          
000044 FILE-CONTROL.                                                            
000045     SKIP2                                                                
000046*          --- SHIPPING INFO FRÅN WDR7                                    
000047     SELECT W47624                     ASSIGN TO W47625D1.                
000048     EJECT                                                                
000049*          --- FIL MED POST/KOLLI , EMB.INFO PROFORMA                     
000050     SELECT W47628                     ASSIGN TO W47625D3.                
000051     EJECT                                                                
000052*          --- FIL MED POST/KOLLI , EMB.INFO VIPS                         
000053     SELECT W47630                     ASSIGN TO W47625D4.                
000054     EJECT                                                                
000055*          --- FIL MED POST/KOLLI/RAD , BROOKER INFO , USA AT             
000056     SELECT W47635                     ASSIGN TO W47625D7.                
000057     EJECT                                                                
000058*          --- FIL MED POST/KOLLI/RAD , BROOKER INFO , USA TO             
000059     SELECT W47636                     ASSIGN TO W47625D8.                
000060     EJECT                                                                
000061*          --- FIL MED POST/KOLLI/RAD , BROOKER INFO , JAPAN              
000062     SELECT W47637                     ASSIGN TO W47625D9.                
000063     EJECT                                                                
000064*          --- FIL MED POST/KOLLI/RAD , BROOKER INFO , AUSTRALIEN         
000065     SELECT W47638                     ASSIGN TO W47625DA.                
000066     EJECT                                                                
000067 DATA DIVISION.                                                           
000068     SKIP2                                                                
000069 FILE SECTION.                                                            
000070     SKIP3                                                                
000071 FD  W47624                                                               
000072     RECORDING       F                                                    
000073     BLOCK CONTAINS  0.                                                   
000074                                                                          
000075*01  POST -COPY W4762401  -PRE  IN-  -L.                                  
000076                                                                          
000077 FD  W47628                                                               
000078     RECORDING       F                                                    
000079     BLOCK CONTAINS  0.                                                   
000080                                                                          
000081*01  POST -COPY W47394    -PRE  UT3-  -L.                                 
000082     EJECT                                                                
000083 FD  W47630                                                               
000084     RECORDING       F                                                    
000085     BLOCK CONTAINS  0.                                                   
000086                                                                          
000087*01  POST -COPY W461014   -PRE  UT4-  -L.                                 
000088                                                                          
000089 FD  W47635                                                               
000090     RECORDING       V                                                    
000091     BLOCK CONTAINS  0.                                                   
000092                                                                          
000093 01  BR-POST-H.                                                           
000094*    03  FILLER   -COPY W47584H     -L.                                   
000095                                                                          
000096 01  BR-POST-O.                                                           
000097*    03  FILLER   -COPY W47584O     -L.                                   
000098                                                                          
000099 01  BR-POST-P.                                                           
000100*    03  FILLER   -COPY W47584P     -L.                                   
000101                                                                          
000102 01  BR-POST-D.                                                           
000103*    03  FILLER   -COPY W47584DU    -L.                                   
000104                                                                          
000105 01  BR-POST-T.                                                           
000106*    03  FILLER   -COPY W47584T     -L.                                   
000107                                                                          
000108 01  BR-POST-X.                                                           
000109*    03  FILLER   -COPY W4763501    -L.                                   
000110                                                                          
000111 FD  W47636                                                               
000112     RECORDING       V                                                    
000113     BLOCK CONTAINS  0.                                                   
000114                                                                          
000115 01  TO-POST-H.                                                           
000116*    03  FILLER   -COPY W47584H     -L.                                   
000117                                                                          
000118 01  TO-POST-O.                                                           
000119*    03  FILLER   -COPY W47584O     -L.                                   
000120                                                                          
000121 01  TO-POST-P.                                                           
000122*    03  FILLER   -COPY W47584P     -L.                                   
000123                                                                          
000124 01  TO-POST-D.                                                           
000125*    03  FILLER   -COPY W47584D     -L.                                   
000126                                                                          
000127 01  TO-POST-T.                                                           
000128*    03  FILLER   -COPY W47584T     -L.                                   
000129                                                                          
000130 01  TO-POST-X.                                                           
000131*    03  FILLER   -COPY W4763501    -L.                                   
000132                                                                          
000133 FD  W47637                                                               
000134     RECORDING       V                                                    
000135     BLOCK CONTAINS  0.                                                   
000136                                                                          
000137 01  JP-POST-H.                                                           
000138*    03  FILLER   -COPY W4756GH     -L.                                   
000139                                                                          
000140 01  JP-POST-D.                                                           
000141*    03  FILLER   -COPY W4756GD     -L.                                   
000142                                                                          
000143 01  JP-POST-X.                                                           
000144*    03  FILLER   -COPY W4763501    -L.                                   
000145                                                                          
000146 FD  W47638                                                               
000147     RECORDING       V                                                    
000148     BLOCK CONTAINS  0.                                                   
000149                                                                          
000150 01  AU-POST-H.                                                           
000151*    03  FILLER   -COPY W47584H     -L.                                   
000152                                                                          
000153 01  AU-POST-O.                                                           
000154*    03  FILLER   -COPY W47584O     -L.                                   
000155                                                                          
000156 01  AU-POST-P.                                                           
000157*    03  FILLER   -COPY W47584P     -L.                                   
000158                                                                          
000159 01  AU-POST-D.                                                           
000160*    03  FILLER   -COPY W47584D     -L.                                   
000161                                                                          
000162 01  AU-POST-T.                                                           
000163*    03  FILLER   -COPY W47584T     -L.                                   
000164                                                                          
000165 01  AU-POST-X.                                                           
000166*    03  FILLER   -COPY W4763501    -L.                                   
000167 WORKING-STORAGE SECTION.                                                 
000168                                                                          
000169 77  IDPGM                       PIC X(8)    VALUE 'W4762500'.            
000170 77  JA                          PIC X       VALUE 'J'.                   
000171 77  NEJ                         PIC X       VALUE 'N'.                   
000172 77  INDX                        PIC S9(5)   VALUE +0   COMP SYNC.        
000173 77  MAX-INDX                    PIC S9(5)   VALUE +999 COMP SYNC.        
000174                                                                          
000175*01  -COPY WWDCKONS                                                       
000176     SKIP2                                                                
000177*01  -COPY WWDC99                                                         
000178                                                                          
000179 77  W47624-EOF-SW               PIC X       VALUE 'N'.                   
000180     88  END-OF-W47624                       VALUE 'J'.                   
000181                                                                          
000182 77  SAMKOLLI-SW                 PIC X       VALUE 'N'.                   
000183     88  SAMKOLLI-FINNS                      VALUE 'J'.                   
000184                                                                          
000185 01  FILLER                      PIC X(16)   VALUE 'WS-SEKTION'.          
000186 01  WS-SEKTION                  PIC X(30)   VALUE SPACES.                
000187                                                                          
000188 01  WX-IDKUNDRF                 PIC X(10).                               
000189 01  WX-IDPRODNR                 PIC 9(07).                               
000190                                                                          
000191 77  W-TIAAMMDD                  PIC 9(6).                                
000192 77  W-TIKLOCK                   PIC 9(8).                                
000193                                                                          
000194 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
000195 01  FILLER REDEFINES DAGENS-DATUM.                                       
000196     03  DAGENS-DATUM-AAR        PIC 9(2).                                
000197     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
000198     03  DAGENS-DATUM-DAG        PIC 9(2).                                
000199     EJECT                                                                
000200 01  WS-DATUM                    PIC 9(7)    VALUE ZERO.                  
000201 01  FILLER                      REDEFINES WS-DATUM.                      
000202     03  FILLER                  PIC 9(1).                                
000203     03  WS-DATUM6               PIC 9(6).                                
000204                                                                          
000205 01  W-JFR-KEY.                                                           
000206     03  W-JFR-IDDISTR       PIC S9(5)   VALUE ZERO COMP-3.               
000207     03  W-JFR-IDKUNDNR      PIC S9(7)   VALUE ZERO COMP-3.               
000208                                                                          
000209 01  WS-DISTR-KND.                                                        
000210     03  WS-IDDISTR          PIC 9(5)   VALUE ZERO.                       
000220     03  WS-IDKUNDNR         PIC 9(7)   VALUE ZERO.                       
000230                                                                          
000240 01  TEST-IDDISTR                PIC 9(5)    COMP-3.                      
000250*01  FILLER  -COPY   WWDIST07   -RED TEST-IDDISTR.                        
000260     EJECT                                                                
000270*01  FILLER  -COPY   WWDIST35   -RED TEST-IDDISTR.                        
000280     EJECT                                                                
000290*01  FILLER  -COPY   WWDIST79   -RED TEST-IDDISTR.                        
000300     EJECT                                                                
000400*01  FILLER  -COPY   WWDIS103   -RED TEST-IDDISTR.                        
000410     EJECT                                                                
000411*01  FILLER  -COPY   WWDIS130   -RED TEST-IDDISTR.                        
000412                                                                          
000413 01  FILLER                       PIC X(16)  VALUE 'REFILLTABDC'.         
000414*   -COPY WWDIST57                                                        
000415     EJECT                                                                
000416 01  FILLER                       PIC X(16)  VALUE 'WWDIST04   '.         
000417*01  -COPY WWDIST04                                                       
000418     EJECT                                                                
000419                                                                          
000420 01  FILLER                      PIC X(16) VALUE 'KOLLI UPPDELN'.         
000421 01  W-KOLLI-KDKOLLI.                                                     
000422     03  W-KDKOLLI-1             PIC X(1).                                
000423     03  W-KDKOLLI-2             PIC X(1).                                
000424     03  W-KDKOLLI-3             PIC X(1).                                
000425     03  FILLER                  PIC X(5).                                
000426                                                                          
000427 01  FILLER                      PIC X(16) VALUE 'TAB-SAMKLI'.            
000428 01  SAMKLI-TABELL.                                                       
000429    03 SK-TABELL  OCCURS 1000.                                            
000430      05 TAB-IDKOLLI-SAMP        PIC S9(5)    COMP-3 VALUE ZERO.          
000431                                                                          
000432 77  W-BRO                       PIC S9(5)    COMP-3 VALUE ZERO.          
000433 77  W-BRP                       PIC S9(5)    COMP-3 VALUE ZERO.          
000434 77  W-BRD                       PIC S9(5)    COMP-3 VALUE ZERO.          
000435 77  W-BRO-SEQ                   PIC S9(5)    COMP-3 VALUE ZERO.          
000436 77  WS-BROKER-KVKOLLI           PIC S9(9)    COMP-3 VALUE ZERO.          
000437 77  WS-BROKER-VKORDBTO        PIC S9(6)V9(1) COMP-3 VALUE ZERO.          
000438 77  WS-BROKER-VKORDNTO        PIC S9(6)V9(1) COMP-3 VALUE ZERO.          
000439 77  WS-BROKER-VLORDBTO        PIC S9(4)V9(3) COMP-3 VALUE ZERO.          
000440 77  WS-BROKER-SUFKTBEL        PIC S9(9)V9(2) COMP-3 VALUE ZERO.          
000441 77  WS-IDDC-MOT               PIC X(2)       VALUE SPACES.               
000442 01  KONSTANTER.                                                          
000443     03  WS-BROKER-NAME-RU    PIC X(40)                                   
000444              VALUE 'Burghart                                '.           
000445     03  WS-BROKER-NAME-ATLA  PIC X(40)                                   
000446              VALUE 'Expeditors                              '.           
000447     03  WS-BROKER-NAME-TO    PIC X(40)                                   
000448              VALUE 'FedEx                                   '.           
000449     03  WS-BROKER-NAME-AU    PIC X(40)                                   
000450              VALUE 'Wilson                                  '.           
000451                                                                          
000452 01  W-ALFA-ARTIN                PIC X(30)   VALUE SPACE.                 
000453 01  W-ALFA-ARTUT                PIC X(30)   VALUE SPACE.                 
000454                                                                          
000455 01  W-KDSOFT                    PIC S9      COMP-3.                      
000456                                                                          
000457 01  FILLER                      PIC X(16) VALUE 'ARTNR 20 POS '.         
000458 01  WS-BROKER-ARTNR-TJUGO.                                               
000459     03  WS-BROKER-IDARTNR       PIC B(5)Z(14).                           
000460     03  WS-BROKER-REKSIFFR      PIC 9(1).                                
000461                                                                          
000462 01  TEST-ARTIKEL                PIC 9(9)    COMP-3.                      
000463*01  FILLER  -COPY WWART04       -RED  TEST-ARTIKEL.                      
000464                                                                          
000465 01  DYNAMISKA-SUBPROGRAM.                                                
000466*                                                                         
000467     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
000468     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000469     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000470     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
000471     03  W460DIS1                PIC X(8)    VALUE 'W460DIS1'.            
000472     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
000473     03  W009REDU                PIC X(8)    VALUE 'W009REDU'.            
000474     03  W335PRIS                PIC X(8)    VALUE 'W335PRIS'.            
000475     SKIP2                                                                
000476 01  PARM-AREA                   PIC X(200)  VALUE SPACE.                 
000477 01  FILLER                      REDEFINES PARM-AREA.                     
000478     03  WS-IDSHIPM              PIC 9(7).                                
000479                                                                          
000480*    --- PARAMETRAR TILL ABEND                                            
000481                                                                          
000482 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
000483 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
000484 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
000485     SKIP2                                                                
000486 01  FELTEXT.                                                             
000487     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
000488     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
000489     EJECT                                                                
000490 01  WS6-NYCKLAR.                                                         
000491     03  WS6-IDSHIPM             PIC  9(7)  VALUE ZERO.                   
000492     03  WS6-IDDISTR             PIC S9(5)  VALUE ZERO  COMP-3.           
000493     03  WS6-IDKUNDNR            PIC S9(7)  VALUE ZERO  COMP-3.           
000494     03  WS6-IDORDNR7            PIC S9(7)  VALUE ZERO  COMP-3.           
000495     03  WS6-IDPRODNR            PIC S9(7)  VALUE ZERO  COMP-3.           
000496     03  WS6-IDKOLLI             PIC S9(5)  VALUE ZERO  COMP-3.           
000497                                                                          
000498*    --- PARAMETRAR TILL SUBPROGRAM W460DIS1                              
000499*        KOLL OM DISTR ÄR NOAC-DISTR                                      
000500*01  -COPY  W460DIS1                                                      
000501                                                                          
000502*    --- PARAMETRAR TILL POSTSUM                                          
000503*                                                                         
000504*01  -COPY W0005   -PRE  POSTSUM-                                         
000505     EJECT                                                                
000506*01  -COPY WDATAREA                                                       
000507     EJECT                                                                
000508 01 FILLER                       PIC X(8)   VALUE  'W335PRIS'.            
000509*   -COPY W335PRIS                                                        
000510     EJECT                                                                
000511 01  IN-AREA-START              PIC X(16) VALUE 'IN-AREA-START '.         
000512                                                                          
000513 01  AREA  -COPY W4762401  -PRE IN-                                       
000514                                                                          
000515 01  UT3-AREA-START             PIC X(16) VALUE 'UT3-AREA-START'.         
000516     SKIP2                                                                
000517 01  EXP-EMB-UTAREA.                                                      
000518*    03  -COPY W47394                                                     
000519     EJECT                                                                
000520 01  UT4-AREA-START             PIC X(16) VALUE 'UT4-AREA-START'.         
000521     SKIP2                                                                
000522 01  EMB-UTAREA.                                                          
000523*    03  -COPY W461014     -PRE UT-                                       
000524     EJECT                                                                
000525 01  UTB-AREA-START             PIC X(16) VALUE 'UTB-AREA-START'.         
000526********************************************                              
000527*        UTAREA BROAKER                                                   
000528********************************************                              
000529 01  FILLER                      PIC X(16) VALUE 'UTAREA BRO'.            
000530 01  BR-UTAREA-CAN.                                                       
000531*    03  AREA -COPY W47584T     -PRE BRT-                                 
000532*    03  AREA -COPY W47584D     -PRE BRD-                                 
000533*    03  AREA -COPY W47584O     -PRE BRO-                                 
000534*    03  AREA -COPY W47584P     -PRE BRP-                                 
000535*    03  AREA -COPY W47584H     -PRE BRH-                                 
000536*    03  -COPY W4763501                                                   
000537     SKIP3                                                                
000538 01  FILLER                      PIC X(16) VALUE 'UTAREA BRO-USA'.        
000539 01  BR-UTAREA-USA.                                                       
000540*    03  AREA -COPY W47584T     -PRE BRTU-                                
000541*    03  AREA -COPY W47584DU    -PRE BRDU-                                
000542*    03  AREA -COPY W47584O     -PRE BROU-                                
000543*    03  AREA -COPY W47584P     -PRE BRPU-                                
000544*    03  AREA -COPY W47584H     -PRE BRHU-                                
000545*    03  -COPY W4763501         -PRE US-                                  
000546     SKIP3                                                                
000547 01  UTC-AREA-START             PIC X(16) VALUE 'UTC-AREA-START'.         
000548********************************************                              
000549*        UTAREA BROOKER JP                                                
000550********************************************                              
000551 01  FILLER                      PIC X(16) VALUE 'UTAREA BRJ'.            
000552 01  BRJ-UTAREA.                                                          
000553*    03  AREA -COPY W4756GD     -PRE BRJD-                                
000554*    03  AREA -COPY W4756GH     -PRE BRJH-                                
000555     SKIP3                                                                
000556                                                                          
000557*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
000558*                                                                         
000559     EJECT                                                                
000560 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000561     SKIP3                                                                
000562 01  NYCKLAR-TILL-DLI.                                                    
000563     03  W-IDSHIPM-X.                                                     
000564         05  W-IDSHIPM           PIC  9(7)   VALUE ZERO.                  
000565     03  W-WDE111KY-X.                                                    
000566         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
000567         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
000568     03  W-WDE121KY-X.                                                    
000569         05  W-IDPRODNR          PIC S9(7)   VALUE ZERO COMP-3.           
000570         05  W-IDKOLLI           PIC S9(5)   VALUE ZERO COMP-3.           
000571     03  W-IDPURAD-X.                                                     
000572         05  W-IDPURAD           PIC S9(5)   VALUE ZERO COMP-3.           
000573                                                                          
000574     03    W-WDE7ASEQ-X.                                                  
000575       05  W-E7A-IDDC            PIC  X(2)   VALUE SPACE.                 
000576       05  W-E7A-IDKOLLIS        PIC S9(5)   VALUE ZERO  COMP-3.          
000577                                                                          
000578     03  W-IDGMT-X.                                                       
000579         05  W-IDDISTR-B2        PIC S9(5)   VALUE ZERO COMP-3.           
000580         05  W-IDKUNDNR-B2       PIC S9(7)   VALUE ZERO COMP-3.           
000581                                                                          
000582     03  W-WDE4BSEQ-MIN-X.                                                
000583         05  W-IDPRODNR-MIN      PIC S9(7) VALUE ZERO COMP-3.             
000584         05  W-IDPURAD-MIN       PIC S9(5) VALUE ZERO COMP-3.             
000585                                                                          
000586     03  W-WDE4BSEQ-MAX-X.                                                
000587         05  W-IDPRODNR-MAX      PIC S9(7) VALUE ZERO COMP-3.             
000588         05  W-IDPURAD-MAX       PIC S9(5) VALUE ZERO COMP-3.             
000589                                                                          
000590     03  W-WDE4ESEQ-X.                                                    
000591         05  W-IDPRODNR-ESEQ     PIC S9(7)   VALUE ZERO COMP-3.           
000592                                                                          
000593     03   W-IDPRODNR-E6-X.                                                
000594         05  W-IDPRODNR-E6       PIC S9(7)   VALUE ZERO COMP-3.           
000595     03  W-IDKOLLI-E6-X.                                                  
000596         05  W-IDKOLLI-E6        PIC S9(5)   VALUE ZERO COMP-3.           
000597                                                                          
000598     03   W-IDARTNR-X.                                                    
000599         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
000600                                                                          
000601     03  W-IDSKYLT-X.                                                     
000602         05  W-IDSKYLT           PIC X(3)    VALUE 'S  '.                 
000603                                                                          
000604*    TILL DC-STYR.                                                        
000605     03 W-IDDC-B6-X.                                                      
000606        05 W-IDDC-B6             PIC X(2).                                
000607                                                                          
000608     SKIP2                                                                
000609*    --- STATUS-KOD FRÅN IMS                                              
000610 01  STATUS-WS-E1                PIC XX.                                  
000611 01  STATUS-WS                   PIC XX.                                  
000612     88  SEGMENT-FINNS                       VALUE '  '.                  
000613     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
000614     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
000615     88  SEGMENT-SLUT                        VALUE 'GB'.                  
000616     SKIP2                                                                
000617 01  GODK-STATUSKODER.                                                    
000618     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000619     SKIP3                                                                
000620 01  FILLER                      PIC X(16)   VALUE 'SSA1..'.              
000621 01  SSA1                        PIC X(64).                               
000622 01  SSA2                        PIC X(64).                               
000623 01  SSA3                        PIC X(64).                               
000624     EJECT                                                                
000625*    --- IMS FUNKTIONSKODER                                               
000626*01  -COPY W0003                                                          
000627     EJECT                                                                
000628*    ---  DLI INPUT-OUTPUT AREA                                           
000629 01  FILLER                PIC X(16) VALUE 'DLI-IO-WDE101'.               
000630 01  DLI-IO-WDE101.                                                       
000631*    03  -COPY WDE101 -PRE W-                                             
000632     EJECT                                                                
000633 01  FILLER                PIC X(16) VALUE 'DLI-IO-WDE111'.               
000634 01  DLI-IO-WDE111.                                                       
000635*    03  -COPY WDE111                                                     
000636     EJECT                                                                
000637 01  FILLER                PIC X(16) VALUE 'DLI-IO-WDE121'.               
000638 01  DLI-IO-WDE121.                                                       
000639*    03  -COPY WDE121                                                     
000640 01  FILLER                PIC X(16) VALUE 'DLI-IO-WDE122'.               
000641 01  DLI-IO-WDE122.                                                       
000642*    03  -COPY WDE122                                                     
000643 01  FILLER                PIC X(16) VALUE 'DLI-IO-WDE131'.               
000644 01  DLI-IO-WDE131.                                                       
000645*    03  -COPY WDE131                                                     
000646     EJECT                                                                
000647 01  FILLER                PIC X(16) VALUE 'DLI-IO-WDE711'.               
000648 01  DLI-IO-WDE711.                                                       
000649*    03  -COPY WDE711                                                     
000650     EJECT                                                                
000651 01  FILLER                PIC X(16) VALUE 'DLI-IO-WDE401'.               
000652 01  DLI-IO-WDE401.                                                       
000653*    03  -COPY WDE401                                                     
000654     EJECT                                                                
000655 01  FILLER                PIC X(16) VALUE 'DLI-IO-WDE411'.               
000656 01  DLI-IO-WDE411.                                                       
000657*    03  -COPY WDE411                                                     
000658     EJECT                                                                
000659 01  FILLER                PIC X(16) VALUE 'DLI-IO-WDK601'.               
000660 01  DLI-IO-WDK601.                                                       
000661*    03  -COPY WDK601                                                     
000662     EJECT                                                                
000663 01  FILLER                PIC X(16) VALUE 'DLI-IO-WDK611'.               
000664 01  DLI-IO-WDK611.                                                       
000665*    03  -COPY WDK611                                                     
000666     EJECT                                                                
000667 01  FILLER                PIC X(16) VALUE 'DLI-IO-WDB601'.               
000668 01  DLI-IO-WDB601.                                                       
000669*    03  -COPY WDB601                                                     
000670     EJECT                                                                
000671 01  FILLER                PIC X(16) VALUE 'DLI-IO-WDD311'.               
000672 01  DLI-IO-WDD311.                                                       
000673*    03  -COPY WDD311                                                     
000674     EJECT                                                                
000675 LINKAGE SECTION.                                                         
000676 01  IO-PCB                      PIC X.                                   
000677                                                                          
000678*01  -COPY W0008  -PRE WDE1-                                              
000679     05  FILLER                  PIC X.                                   
000680     EJECT                                                                
000681*01  -COPY W0008  -PRE WDE4-                                              
000682     05  FILLER                  PIC X.                                   
000683     EJECT                                                                
000684*01  -COPY W0008  -PRE WDE4E-                                             
000685     05  FILLER                  PIC X.                                   
000686     EJECT                                                                
000687*01  -COPY W0008  -PRE WDK6-                                              
000688     05  FILLER                  PIC X.                                   
000689     EJECT                                                                
000690*01  -COPY W0008  -PRE WDB6-                                              
000691     05  FILLER                  PIC X.                                   
000692     EJECT                                                                
000693*01  -COPY W0008  -PRE WDD3-                                              
000694     05  FILLER                  PIC X.                                   
000695     EJECT                                                                
000696*01  -COPY W0008  -PRE WDE7-                                              
000697     05  FILLER                  PIC X.                                   
000698     EJECT                                                                
000699                                                                          
000700 01  PRIS-ARTC-PCB               PIC X.                                   
000701 01  PRIS-WDK7-PCB               PIC X.                                   
000702 01  PRIS-GMTA-PCB               PIC X.                                   
000703 01  PRIS-BETA-PCB               PIC X.                                   
000704 01  PRIS-GPRIA-PCB              PIC X.                                   
000705 01  PRIS-GPRIB-PCB              PIC X.                                   
000706 01  PRIS-COST-WDK6-PCB          PIC X.                                   
000707 01  PRIS-COST-WDK7-PCB          PIC X.                                   
000708 01  PRIS-COST-WDF1-PCB          PIC X.                                   
000709 01  PRIS-COST-9305-PCB          PIC X.                                   
000710 01  PRIS-COST-WDK72-PCB         PIC X.                                   
000711 01  PRIS-COST-WDB6-PCB          PIC X.                                   
000712                                                                          
000713 PROCEDURE DIVISION  USING   IO-PCB                                       
000714                           WDE1-PCB                                       
000715                           WDE4-PCB                                       
000716                           WDE4E-PCB                                      
000717                           WDK6-PCB                                       
000718                           WDB6-PCB                                       
000719                           WDD3-PCB                                       
000720                           WDE7-PCB                                       
000721                           PRIS-ARTC-PCB                                  
000722                           PRIS-WDK7-PCB                                  
000723                           PRIS-GMTA-PCB                                  
000724                           PRIS-BETA-PCB                                  
000725                           PRIS-GPRIA-PCB                                 
000726                           PRIS-GPRIB-PCB                                 
000727                           PRIS-COST-WDK6-PCB                             
000728                           PRIS-COST-WDK7-PCB                             
000729                           PRIS-COST-WDF1-PCB                             
000730                           PRIS-COST-9305-PCB                             
000731                           PRIS-COST-WDK72-PCB                            
000732                           PRIS-COST-WDB6-PCB.                            
000733 MAIN SECTION.                                                            
000734     ENTRY 'DLITCBL' USING   IO-PCB                                       
000735                           WDE1-PCB                                       
000736                           WDE4-PCB                                       
000737                           WDE4E-PCB                                      
000738                           WDK6-PCB                                       
000739                           WDB6-PCB                                       
000740                           WDD3-PCB                                       
000741                           WDE7-PCB                                       
000742                           PRIS-ARTC-PCB                                  
000743                           PRIS-WDK7-PCB                                  
000744                           PRIS-GMTA-PCB                                  
000745                           PRIS-BETA-PCB                                  
000746                           PRIS-GPRIA-PCB                                 
000747                           PRIS-GPRIB-PCB                                 
000748                           PRIS-COST-WDK6-PCB                             
000749                           PRIS-COST-WDK7-PCB                             
000750                           PRIS-COST-WDF1-PCB                             
000751                           PRIS-COST-9305-PCB                             
000752                           PRIS-COST-WDK72-PCB                            
000753                           PRIS-COST-WDB6-PCB.                            
000754                                                                          
000755     PERFORM A-INIT                                                       
000756                                                                          
000757     PERFORM S11-LAS-INFIL                                                
000758     MOVE IN-SHIP-DATA    TO PARM-AREA                                    
000759     PERFORM UNTIL  END-OF-W47624                                         
000760                                                                          
000770                                                                          
000771       EVALUATE IN-SHIP-IDPTYP                                            
000772         WHEN 'EXP'                                                       
000773           PERFORM E-SKAPA-EXP-EMB-PROF                                   
000774         WHEN 'EMB'                                                       
000775           PERFORM F-SKAPA-EMB-POSTER                                     
000776         WHEN 'BRO'                                                       
000777           PERFORM I-SKAPA-BRO-POSTER-NA                                  
000778         WHEN 'BRJ'                                                       
000779           PERFORM J-SKAPA-BRO-POSTER-JP                                  
000780         WHEN 'BRA'                                                       
000781           PERFORM K-SKAPA-BRO-POSTER-AU                                  
000782       END-EVALUATE                                                       
000783                                                                          
000784       PERFORM S11-LAS-INFIL                                              
000785       MOVE IN-SHIP-DATA  TO PARM-AREA                                    
000786                                                                          
000787     END-PERFORM                                                          
000788                                                                          
000789     PERFORM Z-FINIT                                                      
000790                                                                          
000791     MOVE ZERO TO RETURN-CODE                                             
000792     GOBACK                                                               
000793     .                                                                    
000794     EJECT                                                                
000795 A-INIT SECTION.                                                          
000796     MOVE 'A-INIT'          TO WS-SEKTION                                 
000797                                                                          
000798     OPEN INPUT  W47624                                                   
000799     OPEN OUTPUT W47628                                                   
000800                 W47630                                                   
000801                 W47635                                                   
000802                 W47636                                                   
000803                 W47637                                                   
000804                 W47638                                                   
000805                                                                          
000806     ACCEPT DAGENS-DATUM    FROM DATE                                     
000807     ACCEPT W-TIAAMMDD      FROM DATE                                     
000808     ACCEPT  W-TIKLOCK      FROM TIME                                     
000809                                                                          
000810* NOLLSTÄLL SK-TABELL!                                                    
000811                                                                          
000812     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
000813     MOVE ZERO                 TO W-SHIP-IDSHIPM                          
000814     MOVE 'GB '                TO W-IDSKYLT                               
000815     .                                                                    
000816     EJECT                                                                
000817 E-SKAPA-EXP-EMB-PROF SECTION.                                            
000818     MOVE 'E-SKAPA-EXP-EMB'  TO WS-SEKTION                                
000819                                                                          
000820       MOVE WS-IDSHIPM           TO W-IDSHIPM                             
000821       PERFORM IMS-GU-WDE101                                              
000822       PERFORM IMS-GNP-WDE111                                             
000823       MOVE SGMT-IDDISTR       TO W-IDDISTR                               
000824       MOVE SGMT-IDKUNDNR      TO W-IDKUNDNR                              
000825       MOVE +1                 TO INDX                                    
000826                                                                          
000827       PERFORM UNTIL SEGMENT-SAKNAS                                       
000828                                                                          
000829         PERFORM IMS-GNP-WDE121                                           
000830         MOVE SKOLLI-IDPRODNR      TO W-IDPRODNR                          
000831         MOVE SKOLLI-IDKOLLI       TO W-IDKOLLI                           
000832                                                                          
000833         PERFORM UNTIL SEGMENT-SAKNAS                                     
000834           PERFORM IMS-GNP-WDE131                                         
000835*                       LÄS E131 ENDAST FÖRSTA RADEN                      
000836           PERFORM EA-SKAPA-POSTER                                        
000837           PERFORM IMS-GNP-WDE121                                         
000838           MOVE SKOLLI-IDPRODNR    TO W-IDPRODNR                          
000839           MOVE SKOLLI-IDKOLLI     TO W-IDKOLLI                           
000840                                                                          
000841         END-PERFORM                                                      
000842         PERFORM IMS-GNP-WDE111                                           
000843         MOVE SGMT-IDDISTR       TO W-IDDISTR                             
000844         MOVE SGMT-IDKUNDNR      TO W-IDKUNDNR                            
000845                                                                          
000846       END-PERFORM                                                        
000847     .                                                                    
000848     EJECT                                                                
000849 EA-SKAPA-POSTER  SECTION.                                                
000850     MOVE 'EA-SKAPA-POSTER'    TO WS-SEKTION                              
000851                                                                          
000852     IF W-SHIP-IDDC NOT = W-IDDC-B6                                       
000853       MOVE W-SHIP-IDDC TO W-IDDC-B6                                      
000854       PERFORM IMS-GU-WDB601                                              
000855     END-IF                                                               
000856                                                                          
000857     MOVE SKOLLI-KDKOLLI        TO W-KOLLI-KDKOLLI                        
000858     MOVE SKOLLI-IDDISTR        TO TEST-IDDISTR                           
000859                                   DIST04-IDDISTR-SE                      
000860                                   DIST04-IDDISTR-EXP                     
000870     IF SKOLLI-IDKOLLI-SAMP > ZERO                                        
000880       PERFORM S02-KOLLA-EMB-SAMKLI                                       
000881     END-IF                                                               
000882                                                                          
000883     PERFORM S03-KDSOFT                                                   
000884                                                                          
000885     MOVE W-IDDC-B6             TO WS-IDDC                                
000886     IF CDC-SE                                                            
000887       IF DIST04-EMB-SE                               AND                 
000888          W-KDSOFT    = +0                            AND                 
000889         (W-KDKOLLI-1 = 'L' OR 'K' OR 'F' OR 'G' OR 'H'                   
000890                         OR 'U' OR 'W' OR 'Y'                             
000891                         OR 'C' )                                         
000892                                                                          
000893         MOVE SKOLLI-IDDISTR    TO EXP-EMB-IDDISTR                        
000894                                   WS-IDDISTR                             
000895                                                                          
000896         MOVE SKOLLI-IDKUNDNR   TO EXP-EMB-IDKUNDNR                       
000897                                                                          
000898         MOVE SKOLLI-KDFAKTYP   TO EXP-EMB-KDFAKTYP                       
000899         MOVE W-SHIP-IDSHIPM    TO EXP-EMB-IDFAKT                         
000900         MOVE W-SHIP-IDDC       TO EXP-EMB-IDDC-SEND                      
000901         IF SAMKOLLI-FINNS                                                
000902           MOVE SKLI-KDKOLLI-SAMP                                         
000903                                TO EXP-EMB-KDKOLLI                        
000904         ELSE                                                             
000905           MOVE SKOLLI-KDKOLLI  TO EXP-EMB-KDKOLLI                        
000906         END-IF                                                           
000907         MOVE SKOLLI-IDKUNDRF   TO EXP-EMB-IDKUNDRF                       
000908                                                                          
000909         SEARCH ALL DIST04-DIST-DC                                        
000910            AT END                                                        
000911               MOVE SPACE       TO EXP-EMB-IDDC-REC                       
000912            WHEN DIST04-SOK-IDDISTR(DIST04-IX1) = WS-IDDISTR              
000913               MOVE DIST04-IDDC-REC (DIST04-IX1)                          
000914                                 TO EXP-EMB-IDDC-REC                      
000915         END-SEARCH                                                       
000916                                                                          
000917         IF EXP-EMB-IDDC-REC > SPACE                                      
000918           PERFORM S13-SKRIV-W47628                                       
000919         END-IF                                                           
000920       END-IF                                                             
000921     ELSE                                                                 
000922       IF DIST04-EMB-EXP                              AND                 
000923          W-KDSOFT    = +0                            AND                 
000924         (W-KDKOLLI-1 = 'L' OR 'K' OR 'F' OR 'G' OR 'H'                   
000925                         OR 'U' OR 'W' OR 'Y'                             
000926                         OR 'C' )                                         
000927                                                                          
000928         MOVE SKOLLI-IDDISTR    TO EXP-EMB-IDDISTR                        
000929                                   WS-IDDISTR                             
000930                                                                          
000931         MOVE SKOLLI-IDKUNDNR   TO EXP-EMB-IDKUNDNR                       
000932                                   WS-IDKUNDNR                            
000933                                                                          
000934         MOVE SKOLLI-KDFAKTYP   TO EXP-EMB-KDFAKTYP                       
000935         MOVE W-SHIP-IDSHIPM    TO EXP-EMB-IDFAKT                         
000936         MOVE W-SHIP-IDDC       TO EXP-EMB-IDDC-SEND                      
000937         IF SAMKOLLI-FINNS                                                
000938           MOVE SKLI-KDKOLLI-SAMP                                         
000939                                TO EXP-EMB-KDKOLLI                        
000940         ELSE                                                             
000941           MOVE SKOLLI-KDKOLLI  TO EXP-EMB-KDKOLLI                        
000942         END-IF                                                           
000943         MOVE SKOLLI-IDKUNDRF   TO EXP-EMB-IDKUNDRF                       
000944                                                                          
000945         SEARCH ALL DIST04-DIST-KUND-DC                                   
000946            AT END                                                        
000947               MOVE SPACE       TO EXP-EMB-IDDC-REC                       
000948            WHEN DIST04-SOK-DIST-KND(DIST04-IX2) = WS-DISTR-KND           
000949               MOVE DIST04-IDDC-REC2 (DIST04-IX2)                         
000950                                TO EXP-EMB-IDDC-REC                       
000951         END-SEARCH                                                       
000952                                                                          
000953         IF EXP-EMB-IDDC-REC > SPACE                                      
000954           PERFORM S13-SKRIV-W47628                                       
000955         END-IF                                                           
000956       END-IF                                                             
000957     END-IF                                                               
000958     .                                                                    
000959     EJECT                                                                
000960 F-SKAPA-EMB-POSTER SECTION.                                              
000970      MOVE 'F-SKAPA-EMB'   TO WS-SEKTION                                  
000980                                                                          
000990      MOVE WS-IDSHIPM           TO W-IDSHIPM                              
001000      PERFORM IMS-GU-WDE101                                               
001010      PERFORM IMS-GNP-WDE111                                              
001011      MOVE SGMT-IDDISTR       TO W-IDDISTR                                
001012      MOVE SGMT-IDKUNDNR      TO W-IDKUNDNR                               
001013                                                                          
001014      PERFORM UNTIL SEGMENT-SAKNAS                                        
001015                                                                          
001016        PERFORM IMS-GNP-WDE121                                            
001017        MOVE SKOLLI-IDPRODNR      TO W-IDPRODNR                           
001018        MOVE SKOLLI-IDKOLLI       TO W-IDKOLLI                            
001019        PERFORM UNTIL SEGMENT-SAKNAS                                      
001020          PERFORM IMS-GNP-WDE131                                          
001021*                       LÄS E131 ENDAST FÖRSTA RADEN                      
001022          PERFORM FA-SKAPA-POSTER                                         
001023          PERFORM IMS-GNP-WDE121                                          
001024          MOVE SKOLLI-IDPRODNR    TO W-IDPRODNR                           
001025          MOVE SKOLLI-IDKOLLI     TO W-IDKOLLI                            
001026                                                                          
001027        END-PERFORM                                                       
001028        PERFORM IMS-GNP-WDE111                                            
001029        MOVE SGMT-IDDISTR       TO W-IDDISTR                              
001030        MOVE SGMT-IDKUNDNR      TO W-IDKUNDNR                             
001031                                                                          
001032      END-PERFORM                                                         
001033     .                                                                    
001034     EJECT                                                                
001035                                                                          
001036 FA-SKAPA-POSTER  SECTION.                                                
001037     MOVE 'FA-SKAPA'    TO WS-SEKTION                                     
001038                                                                          
001039     MOVE SKOLLI-KDKOLLI         TO W-KOLLI-KDKOLLI                       
001040                                                                          
001041     MOVE SPACE                  TO UT-EMB-W461014                        
001042     MOVE SKOLLI-IDDISTR         TO TEST-IDDISTR DIS1-IDDISTR             
001043                                                                          
001044     IF W-SHIP-IDDC NOT = W-IDDC-B6                                       
001045       MOVE W-SHIP-IDDC TO W-IDDC-B6                                      
001046       PERFORM IMS-GU-WDB601                                              
001047     END-IF                                                               
001048                                                                          
001049     CALL W460DIS1 USING DIS1-W460DIS1                                    
001050                                                                          
001051     PERFORM S03-KDSOFT                                                   
001052                                                                          
001053     IF (DIS130-NOAC OR DIS1-KDSVAR = JA) AND                             
001054*        SKOLLI-IDKUNDNR = ZERO           AND                             
001055        (SKOLLI-KDFAKTYP = 'R' OR 'G')    AND                             
001056        W-KDSOFT         = +0                                             
001057       IF SGMT-FLCOD NOT = JA                                             
001058         MOVE NEJ TO SGMT-FLCOD                                           
001059       END-IF                                                             
001060                                                                          
001061       IF   SGMT-FLCOD = NEJ  OR                                          
001062            DCS-CDC           OR                                          
001063            DCS-IDDC = WC-SDC-NL                                          
001064                                                                          
001065         MOVE '014'               TO UT-EMB-IDPTYP                        
001066         MOVE SKOLLI-IDDISTR      TO UT-EMB-IDDISTR                       
001067         MOVE SKOLLI-IDKUNDNR     TO UT-EMB-IDKUNDNR                      
001068         MOVE SKOLLI-KDFAKTYP     TO UT-EMB-KDFAKTYP                      
001069         MOVE W-SHIP-IDSHIPM      TO UT-EMB-IDFAKT                        
001070         MOVE W-SHIP-IDDC         TO UT-EMB-IDDC                          
001071         MOVE W-SHIP-TISKEPPN     TO WS-DATUM                             
001072         MOVE WS-DATUM6           TO UT-EMB-TIFAKT                        
001073         MOVE SKOLLI-IDORDNR7     TO UT-EMB-IDORDNR                       
001074         MOVE SKOLLI-KDKOLLI      TO W-KOLLI-KDKOLLI                      
001075                                                                          
001076         IF W-KOLLI-KDKOLLI = 'FLEN    '                                  
001077           MOVE 'K'               TO UT-EMB-KDPALL                        
001078           MOVE  4                TO UT-EMB-KVKRAG                        
001079           MOVE  1                TO UT-EMB-KVLOCK                        
001080           MOVE  1                TO UT-EMB-KVPALL                        
001081           PERFORM S14-SKRIV-W47630                                       
001082                                                                          
001083           MOVE 'K'               TO UT-EMB-KDPALL                        
001084           MOVE  4                TO UT-EMB-KVKRAG                        
001085           MOVE  1                TO UT-EMB-KVLOCK                        
001086           MOVE  1                TO UT-EMB-KVPALL                        
001087           PERFORM S14-SKRIV-W47630                                       
001088                                                                          
001089           MOVE 'L'               TO UT-EMB-KDPALL                        
001090           MOVE  0                TO UT-EMB-KVKRAG                        
001091           MOVE  1                TO UT-EMB-KVLOCK                        
001092           MOVE  1                TO UT-EMB-KVPALL                        
001093           PERFORM S14-SKRIV-W47630                                       
001094         ELSE                                                             
001095           IF W-KDKOLLI-1 = 'L' OR 'l' OR                                 
001096                            'K' OR 'k' OR                                 
001097                            'F' OR 'f' OR                                 
001098                            'G' OR 'g' OR                                 
001099                            'H' OR 'h' OR                                 
001100                            'U' OR 'u' OR                                 
001101                            'W' OR 'w' OR                                 
001102                            'Y' OR 'y' OR                                 
001103                            'C' OR 'c'                                    
001104             EVALUATE TRUE                                                
001105               WHEN W-KDKOLLI-1 = 'L' OR 'l'                              
001106                 MOVE 'L'         TO UT-EMB-KDPALL                        
001107               WHEN W-KDKOLLI-1 = 'K' OR 'k'                              
001108                 MOVE 'K'         TO UT-EMB-KDPALL                        
001109               WHEN W-KDKOLLI-1 = 'F' OR 'f'                              
001110                 MOVE 'F'         TO UT-EMB-KDPALL                        
001111               WHEN W-KDKOLLI-1 = 'G' OR 'g'                              
001112                 MOVE 'G'         TO UT-EMB-KDPALL                        
001113               WHEN W-KDKOLLI-1 = 'H' OR 'h'                              
001114                 MOVE 'H'         TO UT-EMB-KDPALL                        
001115               WHEN W-KDKOLLI-1 = 'U' OR 'u'                              
001116                 MOVE 'U'         TO UT-EMB-KDPALL                        
001117               WHEN W-KDKOLLI-1 = 'W' OR 'w'                              
001118                 MOVE 'W'         TO UT-EMB-KDPALL                        
001119               WHEN W-KDKOLLI-1 = 'Y' OR 'y'                              
001120                 MOVE 'Y'         TO UT-EMB-KDPALL                        
001121               WHEN W-KDKOLLI-1 = 'C' OR 'c'                              
001122                 MOVE 'C'         TO UT-EMB-KDPALL                        
001123             END-EVALUATE                                                 
001124             IF  W-KDKOLLI-2 NUMERIC                                      
001125               MOVE W-KDKOLLI-2        TO UT-EMB-KVKRAG                   
001126               IF  W-KDKOLLI-3 = 'D'                                      
001127                 MOVE 2                TO UT-EMB-KVLOCK                   
001128               ELSE                                                       
001129                 MOVE 1                TO UT-EMB-KVLOCK                   
001130               END-IF                                                     
001131             ELSE                                                         
001132               MOVE 0                  TO UT-EMB-KVKRAG                   
001133               MOVE 0                  TO UT-EMB-KVLOCK                   
001134             END-IF                                                       
001135             MOVE 1                  TO UT-EMB-KVPALL                     
001136             PERFORM S14-SKRIV-W47630                                     
001137           END-IF                                                         
001138         END-IF                                                           
001139       END-IF                                                             
001140                                                                          
001141     END-IF                                                               
001142     .                                                                    
001143     EJECT                                                                
001144                                                                          
001145 I-SKAPA-BRO-POSTER-NA  SECTION.                                          
001146     MOVE 'I-SKAPA-BRO-POSTER-NA' TO WS-SEKTION                           
001147                                                                          
001148     PERFORM S01-LAES-GRUNDDATA                                           
001149     MOVE ZERO                   TO W-BRO-SEQ                             
001150                                    WS-BROKER-KVKOLLI                     
001151                                    WS-BROKER-VKORDBTO                    
001152                                    WS-BROKER-VKORDNTO                    
001153                                    WS-BROKER-VLORDBTO                    
001154                                    WS-BROKER-SUFKTBEL                    
001155                                    W-BRO                                 
001156                                    W-BRP                                 
001157                                    W-BRD                                 
001158                                    WX-IDPRODNR                           
001159                                                                          
001160     MOVE SPACE                  TO BRO-IDKUNDRF                          
001161                                    BROU-IDKUNDRF                         
001162     MOVE WS-IDSHIPM             TO W-IDSHIPM                             
001163     PERFORM IMS-GU-WDE101                                                
001164     PERFORM IMS-GNP-WDE111                                               
001165     PERFORM UNTIL SEGMENT-SAKNAS                                         
001166       MOVE SGMT-IDDISTR           TO W-IDDISTR                           
001167                                      TEST-IDDISTR                        
001168       MOVE SGMT-IDKUNDNR          TO W-IDKUNDNR                          
001169                                                                          
001170       PERFORM IA-SKAPA-HUVUD-BRH                                         
001171                                                                          
001172       PERFORM UNTIL SEGMENT-SAKNAS                                       
001173*        OR SGMT-IDDISTR NOT = W-IDDISTR                                  
001174         PERFORM IB-HAMTA-TILLAGG                                         
001175         PERFORM IMS-GNP-WDE121                                           
001176         MOVE SKOLLI-IDPRODNR      TO W-IDPRODNR                          
001177         MOVE SKOLLI-IDKOLLI       TO W-IDKOLLI                           
001178         PERFORM UNTIL SEGMENT-SAKNAS                                     
001179                                                                          
001180           PERFORM ID-SKAPA-ORDER-BRO                                     
001181           PERFORM IE-SKAPA-KOLLI-BRP                                     
001182           PERFORM IMS-GNP-WDE131                                         
001183           PERFORM UNTIL SEGMENT-SAKNAS                                   
001184                                                                          
001185             PERFORM IF-SKAPA-RAD-BRD                                     
001186             PERFORM IMS-GNP-WDE131                                       
001187           END-PERFORM                                                    
001188           PERFORM IMS-GNP-WDE121                                         
001189           MOVE SKOLLI-IDPRODNR    TO W-IDPRODNR                          
001190           MOVE SKOLLI-IDKOLLI     TO W-IDKOLLI                           
001191                                                                          
001192         END-PERFORM                                                      
001193         PERFORM IMS-GNP-WDE111                                           
001194         MOVE SGMT-IDDISTR         TO W-IDDISTR                           
001195         MOVE SGMT-IDKUNDNR        TO W-IDKUNDNR                          
001196                                                                          
001197       END-PERFORM                                                        
001198       PERFORM IC-SKAPA-TOTAL-BRT                                         
001199     END-PERFORM                                                          
001200     .                                                                    
001201     EJECT                                                                
001202 IA-SKAPA-HUVUD-BRH  SECTION.                                             
001203     MOVE 'IA-SKAPA-HUVUD-BRH'    TO WS-SEKTION                           
001204                                                                          
001205     IF W-SHIP-IDDC NOT = W-IDDC-B6                                       
001206       MOVE W-SHIP-IDDC TO W-IDDC-B6                                      
001207       PERFORM IMS-GU-WDB601                                              
001208     END-IF                                                               
001209                                                                          
001210     IF ((DCS-CDC OR DCS-DDC) AND (DIST07-USA-CUSTOMERS))                 
001211       MOVE WC-NDC-US-RU       TO WS-IDDC-MOT                             
001212     ELSE                                                                 
001213       IF ((DCS-CDC OR DCS-DDC) AND (DIST07-CAN-CUSTOMERS))               
001214         MOVE WC-NDC-CA        TO WS-IDDC-MOT                             
001215       ELSE                                                               
001216         SEARCH ALL DIST57-REFILL-DC                                      
001217          AT END                                                          
001218             MOVE 'EJ TRÄFF I REFILLTAB WWDIST57'                         
001219                              TO FELTEXT                                  
001220             CALL FELLOG                                                  
001221          WHEN DIST57-SOK-IDDISTR(DIST57-IX) = TEST-IDDISTR               
001222            MOVE DIST57-REFILL-TO-DC(DIST57-IX) TO WS-IDDC-MOT            
001223         END-SEARCH                                                       
001224       END-IF                                                             
001225     END-IF                                                               
001226     IF (DCS-DDC AND DCS-NORWAY)                                          
001227       MOVE 'NORWAY    '         TO BRHU-BECITY                           
001228                                    BRH-BECITY                            
001229     ELSE                                                                 
001230       IF (DCS-DDC AND DCS-BELGIUM)                                       
001231         MOVE 'BELGIUM   '       TO BRHU-BECITY                           
001232                                    BRH-BECITY                            
001233       ELSE                                                               
001234         IF (DCS-DDC AND DCS-GERMANY)                                     
001235           MOVE 'GERMANY   '    TO BRHU-BECITY                            
001236                                   BRH-BECITY                             
001237         ELSE                                                             
001238           MOVE 'GOTHENBURG'    TO BRHU-BECITY                            
001239                                   BRH-BECITY                             
001240           IF DIST35-NDCCN-NDCUS-REFILL                                   
001241             MOVE 'SHANGHAI  '  TO BRHU-BECITY                            
001242           END-IF                                                         
001243         END-IF                                                           
001244       END-IF                                                             
001245     END-IF                                                               
001246                                                                          
001247     IF WS-IDDC-MOT NOT = W-IDDC-B6                                       
001248       MOVE WS-IDDC-MOT TO W-IDDC-B6                                      
001249       PERFORM IMS-GU-WDB601                                              
001250     END-IF                                                               
001251                                                                          
001252     IF (DCS-NDC-NA AND DCS-USA) OR                                       
001253         DIST35-NDCCN-NDCUS-REFILL                                        
001254       MOVE 'H'                   TO BRHU-IDPTYP                          
001255       MOVE W-SHIP-IDSHIPM        TO BRHU-IDFAKT                          
001256       MOVE SGMT-IDDISTR          TO BRHU-IDDISTR                         
001257       MOVE SGMT-IDKUNDNR         TO BRHU-IDKUNDNR                        
001258       MOVE 20                    TO BRHU-DAFAKT(1:2)                     
001259       MOVE DAGENS-DATUM          TO BRHU-DAFAKT(3:6)                     
001260       MOVE 1                     TO BRHU-KDFAKT                          
001261                                                                          
001262       MOVE ZERO                  TO BRHU-KDFRAKT                         
001263       MOVE W-SHIP-IDLBBET        TO BRHU-BESLULEV                        
001264       MOVE W-SHIP-IDSHIPM        TO BRHU-IDSKEPPN                        
001265       MOVE 'M'                   TO BRHU-BEWEIGHT                        
001266       MOVE 'M'                   TO BRHU-BEVOLUME                        
001267       MOVE 'SEK'                 TO BRHU-KDVALISO-SEK                    
001268**     MOVE 'USD'                 TO BRHU-KDVALISO-UTL                    
001269       MOVE SGMT-KDVALISO         TO BRHU-KDVALISO-UTL                    
001270       MOVE SGMT-PRKURS           TO BRHU-PRKURS                          
001271       MOVE SPACE                 TO BRHU-IDBOKN                          
001272       MOVE ZERO                  TO BRHU-VKORDBTO-FAKT                   
001273                                     BRHU-VKORDNTO-FAKT                   
001274                                     BRHU-VLORDBTO-FAKT                   
001275                                     BRHU-SUORDV-FAKT                     
001276                                     BRHU-PREMBHNT                        
001277                                     BRHU-PRFRAKT                         
001278                                     BRHU-SUFKTBEL                        
001279                                     BRHU-SUFKTUTL                        
001280                                     BRHU-PRFOERS                         
001281     ELSE                                                                 
001282       MOVE 'H'                   TO BRH-IDPTYP                           
001283       MOVE W-SHIP-IDSHIPM        TO BRH-IDFAKT                           
001284       MOVE SGMT-IDDISTR          TO BRH-IDDISTR                          
001285       MOVE SGMT-IDKUNDNR         TO BRH-IDKUNDNR                         
001286       MOVE 20                    TO BRH-DAFAKT(1:2)                      
001287       MOVE DAGENS-DATUM          TO BRH-DAFAKT(3:6)                      
001288       MOVE 1                     TO BRH-KDFAKT                           
001289                                                                          
001290       MOVE ZERO                  TO BRH-KDFRAKT                          
001291       MOVE W-SHIP-IDLBBET        TO BRH-BESLULEV                         
001292       MOVE W-SHIP-IDSHIPM        TO BRH-IDSKEPPN                         
001293       MOVE 'M'                   TO BRH-BEWEIGHT                         
001294       MOVE 'M'                   TO BRH-BEVOLUME                         
001295       MOVE 'SEK'                 TO BRH-KDVALISO-SEK                     
001296**     MOVE 'USD'                 TO BRH-KDVALISO-UTL                     
001297       MOVE SGMT-KDVALISO         TO BRH-KDVALISO-UTL                     
001298       MOVE SGMT-PRKURS           TO BRH-PRKURS                           
001299       MOVE SPACE                 TO BRH-IDBOKN                           
001300       MOVE ZERO                  TO BRH-VKORDBTO-FAKT                    
001301                                     BRH-VKORDNTO-FAKT                    
001302                                     BRH-VLORDBTO-FAKT                    
001303                                     BRH-SUORDV-FAKT                      
001304                                     BRH-PREMBHNT                         
001305                                     BRH-PRFRAKT                          
001306                                     BRH-SUFKTBEL                         
001307                                     BRH-SUFKTUTL                         
001308                                     BRH-PRFOERS                          
001309     END-IF                                                               
001310     .                                                                    
001311     EJECT                                                                
001312 IB-HAMTA-TILLAGG  SECTION.                                               
001313     MOVE 'IB-HAMTA-TILLAGG'      TO WS-SEKTION                           
001314                                                                          
001315     PERFORM IMS-GNP-WDE122                                               
001316     IF SEGMENT-FINNS                                                     
001317       IF (DCS-NDC-NA AND DCS-USA) OR                                     
001318           DIST35-NDCCN-NDCUS-REFILL                                      
001319         ADD TILL-PREMBHNT        TO BRHU-PREMBHNT                        
001320         ADD TILL-PRFRAKT         TO BRHU-PRFRAKT                         
001321         ADD TILL-PRFOERS         TO BRHU-PRFOERS                         
001322         MOVE TILL-IDBOKN         TO BRHU-IDBOKN                          
001323         IF DIST35-NDCCN-NDCUS-REFILL                                     
001324           COMPUTE BRHU-PREMBHNT ROUNDED =                                
001325                   BRHU-PREMBHNT * SGMT-PRKURS                            
001326           COMPUTE BRHU-PRFRAKT  ROUNDED =                                
001327                   BRHU-PRFRAKT  * SGMT-PRKURS                            
001328           COMPUTE BRHU-PRFOERS  ROUNDED =                                
001329                   BRHU-PRFOERS  * SGMT-PRKURS                            
001330         END-IF                                                           
001331       ELSE                                                               
001332         ADD TILL-PREMBHNT        TO BRH-PREMBHNT                         
001333         ADD TILL-PRFRAKT         TO BRH-PRFRAKT                          
001334         ADD TILL-PRFOERS         TO BRH-PRFOERS                          
001335         MOVE TILL-IDBOKN         TO BRH-IDBOKN                           
001336       END-IF                                                             
001337     END-IF                                                               
001338     .                                                                    
001339     EJECT                                                                
001340 IC-SKAPA-TOTAL-BRT  SECTION.                                             
001341     MOVE 'IC-SKAPA-TOTAL-BRT'    TO WS-SEKTION                           
001342                                                                          
001343     IF WS-IDDC-MOT NOT = W-IDDC-B6                                       
001344       MOVE WS-IDDC-MOT TO W-IDDC-B6                                      
001345       PERFORM IMS-GU-WDB601                                              
001346     END-IF                                                               
001347                                                                          
001348     IF (DCS-NDC-NA AND DCS-USA) OR                                       
001349         DIST35-NDCCN-NDCUS-REFILL                                        
001350       MOVE 'T'                   TO BRTU-IDPTYP                          
001351*      MOVE W-SHIP-IDSHIPM        TO BRTU-IDFAKT                          
001352       MOVE SKOLLI-IDFAKT         TO BRTU-IDFAKT                          
001353       MOVE W-IDDISTR             TO BRTU-IDDISTR                         
001354       MOVE SGMT-IDKUNDNR         TO BRTU-IDKUNDNR                        
001355       MOVE WS-BROKER-KVKOLLI     TO BRTU-KVKOLLI                         
001356       MOVE WS-BROKER-VKORDNTO    TO BRTU-VKORDNTO-TOT-KOLLI              
001357       MOVE WS-BROKER-VKORDBTO    TO BRTU-VKORDBTO-TOT-KOLLI              
001358       MOVE WS-BROKER-VLORDBTO    TO BRTU-VLORDBTO-TOT-KOLLI              
001359                                                                          
001360       MOVE '4  '                 TO US-BR-IDPTYP                         
001361       MOVE ZERO                  TO US-BR-IDLOPNR                        
001362       MOVE WS-IDSHIPM            TO US-BR-IDSHIPM                        
001363       MOVE BRTU-AREA             TO US-BR-FILLER                         
001364                                                                          
001365*      WRITE BR-POST-T FROM BRT-AREA                                      
001366       WRITE BR-POST-X FROM US-BR-W4763501                                
001367                                                                          
001368       MOVE 'W47635'               TO POSTSUM-FDNAMN                      
001369       MOVE 'BR      '             TO POSTSUM-DDNAMN2                     
001370       MOVE 'BRT'                  TO POSTSUM-TRANSTYP                    
001371                                                                          
001372       CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                      
001373     ELSE                                                                 
001374       MOVE 'T'                   TO BRT-IDPTYP                           
001375*      MOVE W-SHIP-IDSHIPM        TO BRT-IDFAKT                           
001376       MOVE SKOLLI-IDFAKT         TO BRT-IDFAKT                           
001377       MOVE W-IDDISTR             TO BRT-IDDISTR                          
001378       MOVE SGMT-IDKUNDNR         TO BRT-IDKUNDNR                         
001379       MOVE WS-BROKER-KVKOLLI     TO BRT-KVKOLLI                          
001380       MOVE WS-BROKER-VKORDNTO    TO BRT-VKORDNTO-TOT-KOLLI               
001381       MOVE WS-BROKER-VKORDBTO    TO BRT-VKORDBTO-TOT-KOLLI               
001382       MOVE WS-BROKER-VLORDBTO    TO BRT-VLORDBTO-TOT-KOLLI               
001383                                                                          
001384       MOVE '4  '                 TO BR-IDPTYP                            
001385       MOVE ZERO                  TO BR-IDLOPNR                           
001386       MOVE WS-IDSHIPM            TO BR-IDSHIPM                           
001387       MOVE BRT-AREA              TO BR-FILLER                            
001388                                                                          
001389*      WRITE TO-POST-T FROM BRT-AREA                                      
001390       WRITE TO-POST-X FROM BR-W4763501                                   
001391                                                                          
001392       MOVE 'W47636'               TO POSTSUM-FDNAMN                      
001393       MOVE 'BR      '             TO POSTSUM-DDNAMN2                     
001394       MOVE 'BRT'                  TO POSTSUM-TRANSTYP                    
001395                                                                          
001396       CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                      
001397     END-IF                                                               
001398                                                                          
001399     PERFORM ICA-SKRIV-POST-H                                             
001400     MOVE ZERO                     TO WS-BROKER-VKORDNTO                  
001401                                      WS-BROKER-VKORDBTO                  
001402                                      WS-BROKER-VLORDBTO                  
001403                                      WS-BROKER-KVKOLLI                   
001404     .                                                                    
001405     EJECT                                                                
001406 ICA-SKRIV-POST-H  SECTION.                                               
001407     MOVE 'ICA-SKRIV-POST-H'      TO WS-SEKTION                           
001408                                                                          
001409     IF WS-IDDC-MOT NOT = DCS-IDDC                                        
001410        MOVE WS-IDDC-MOT          TO W-IDDC-B6                            
001411        PERFORM IMS-GU-WDB601                                             
001412     END-IF                                                               
001413                                                                          
001414     IF (DCS-NDC-NA AND DCS-USA) OR                                       
001415         DIST35-NDCCN-NDCUS-REFILL                                        
001416       MOVE WS-BROKER-NAME-ATLA   TO BRHU-BEBROKER                        
001417                                                                          
001418       COMPUTE WS-BROKER-SUFKTBEL = WS-BROKER-SUFKTBEL +                  
001419               BRHU-PREMBHNT +                                            
001420               BRHU-PRFRAKT +                                             
001421               BRHU-PRFOERS                                               
001422       MOVE WS-BROKER-SUFKTBEL    TO BRHU-SUFKTBEL                        
001423                                                                          
001424                                                                          
001425       MOVE SKOLLI-KDFRAKT        TO BRHU-KDFRAKT                         
001426       MOVE WS-BROKER-VKORDNTO    TO BRHU-VKORDNTO-FAKT                   
001427       MOVE WS-BROKER-VKORDBTO    TO BRHU-VKORDBTO-FAKT                   
001428       MOVE WS-BROKER-VLORDBTO    TO BRHU-VLORDBTO-FAKT                   
001429       MOVE SKOLLI-KDVALISO       TO BRHU-KDVALISO-SEK                    
001430                                                                          
001431*          fix CO, 1/10 '15.                                              
001432*          fix pga att det blir SPACE i SKOLLI-KDVALISO (ibland)          
001433*          och det får inte vara blankt på filen till brokern!            
001434*          nedan gäller bara så länge usa/ca fakt.i sek!!!                
001435*          kolla detta igen om ändringar i WWDIST79-dealer-price          
001436       IF BRHU-KDVALISO-SEK = SPACE                                       
001437         MOVE 'SEK'               TO BRHU-KDVALISO-SEK                    
001438       END-IF                                                             
001439                                                                          
001440*             FAKTURERAD VALUTA                                           
001441       IF DIST79-DEALER-PRICE AND                                         
001442         SGMT-KDVALISO = SKOLLI-KDVALISO                                  
001443         MOVE 1.0                 TO BRHU-PRKURS                          
001444       END-IF                                                             
001445       COMPUTE BRHU-SUFKTUTL ROUNDED = BRHU-SUFKTBEL / BRHU-PRKURS        
001446                                                                          
001447       MOVE '1  '                 TO US-BR-IDPTYP                         
001448       MOVE ZERO                  TO US-BR-IDLOPNR                        
001449       MOVE WS-IDSHIPM            TO US-BR-IDSHIPM                        
001450       MOVE BRHU-AREA             TO US-BR-FILLER                         
001451                                                                          
001452*      WRITE BR-POST-H FROM BRHU-AREA                                     
001453       WRITE BR-POST-X FROM US-BR-W4763501                                
001454                                                                          
001455       MOVE 'W47635'               TO POSTSUM-FDNAMN                      
001456       MOVE 'BR      '             TO POSTSUM-DDNAMN2                     
001457       MOVE 'BRH'                  TO POSTSUM-TRANSTYP                    
001458                                                                          
001459       CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                      
001460     ELSE                                                                 
001461       MOVE WS-BROKER-NAME-TO     TO BRH-BEBROKER                         
001462                                                                          
001463       COMPUTE WS-BROKER-SUFKTBEL = WS-BROKER-SUFKTBEL +                  
001464               BRH-PREMBHNT +                                             
001465               BRH-PRFRAKT +                                              
001466               BRH-PRFOERS                                                
001467       MOVE WS-BROKER-SUFKTBEL     TO BRH-SUFKTBEL                        
001468                                                                          
001469                                                                          
001470       MOVE SKOLLI-KDFRAKT        TO BRH-KDFRAKT                          
001471       MOVE WS-BROKER-VKORDNTO    TO BRH-VKORDNTO-FAKT                    
001472       MOVE WS-BROKER-VKORDBTO    TO BRH-VKORDBTO-FAKT                    
001473       MOVE WS-BROKER-VLORDBTO    TO BRH-VLORDBTO-FAKT                    
001474       MOVE SKOLLI-KDVALISO       TO BRH-KDVALISO-SEK                     
001475                                                                          
001476*          fix CO, 1/10 '15.                                              
001477*          fix pga att det blir SPACE i SKOLLI-KDVALISO (ibland)          
001478*          och det får inte vara blankt på filen till brokern!            
001479*          nedan gäller bara så länge usa/ca fakt.i sek!!!                
001480*          kolla detta igen om ändringar i WWDIST79-dealer-price          
001481       IF BRH-KDVALISO-SEK = SPACE                                        
001482         MOVE 'SEK'               TO BRH-KDVALISO-SEK                     
001483       END-IF                                                             
001484                                                                          
001485*             FAKTURERAD VALUTA                                           
001486       IF DIST79-DEALER-PRICE AND                                         
001487         SGMT-KDVALISO = SKOLLI-KDVALISO                                  
001488         MOVE 1.0                 TO BRH-PRKURS                           
001489       END-IF                                                             
001490       COMPUTE BRH-SUFKTUTL ROUNDED = BRH-SUFKTBEL / BRH-PRKURS           
001491                                                                          
001492       MOVE '1  '                 TO BR-IDPTYP                            
001493       MOVE ZERO                  TO BR-IDLOPNR                           
001494       MOVE WS-IDSHIPM            TO BR-IDSHIPM                           
001495       MOVE BRH-AREA              TO BR-FILLER                            
001496                                                                          
001497*      WRITE TO-POST-H FROM BRH-AREA                                      
001498       WRITE TO-POST-X FROM BR-W4763501                                   
001499                                                                          
001500       MOVE 'W47636'               TO POSTSUM-FDNAMN                      
001501       MOVE 'BR      '             TO POSTSUM-DDNAMN2                     
001502       MOVE 'BRH'                  TO POSTSUM-TRANSTYP                    
001503                                                                          
001504       CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                      
001505     END-IF                                                               
001506     .                                                                    
001507     EJECT                                                                
001508                                                                          
001509 ID-SKAPA-ORDER-BRO SECTION.                                              
001510     MOVE 'ID-SKAPA-ORDER-BRO'      TO WS-SEKTION                         
001511                                                                          
001512     IF WS-IDDC-MOT NOT = W-IDDC-B6                                       
001513       MOVE WS-IDDC-MOT TO W-IDDC-B6                                      
001514       PERFORM IMS-GU-WDB601                                              
001515     END-IF                                                               
001516                                                                          
001517     IF (DCS-NDC-NA AND DCS-USA) OR                                       
001518         DIST35-NDCCN-NDCUS-REFILL                                        
001519       MOVE BROU-IDKUNDRF           TO WX-IDKUNDRF                        
001520       MOVE BROU-IDPRODNR           TO WX-IDPRODNR                        
001521     ELSE                                                                 
001522       MOVE BRO-IDKUNDRF            TO WX-IDKUNDRF                        
001523       MOVE BRO-IDPRODNR            TO WX-IDPRODNR                        
001524     END-IF                                                               
001525                                                                          
001526     IF SKOLLI-IDKUNDRF NOT = WX-IDKUNDRF                                 
001527       IF (DCS-NDC-NA AND DCS-USA) OR                                     
001528           DIST35-NDCCN-NDCUS-REFILL                                      
001529         MOVE 'O'                   TO BROU-IDPTYP                        
001530*        MOVE W-SHIP-IDSHIPM        TO BROU-IDFAKT                        
001531         MOVE SKOLLI-IDFAKT         TO BROU-IDFAKT                        
001532                                       BRHU-IDFAKT                        
001533         MOVE SGMT-IDDISTR          TO BROU-IDDISTR                       
001534         MOVE SGMT-IDKUNDNR         TO BROU-IDKUNDNR                      
001535         MOVE SKOLLI-IDPRODNR       TO BROU-IDPRODNR                      
001536         MOVE SKOLLI-IDKUNDRF       TO BROU-IDKUNDRF                      
001537         MOVE SKOLLI-TIORDREG       TO WS-DATUM                           
001538         MOVE WS-DATUM6             TO BROU-DAORDER(3:6)                  
001539         MOVE 20                    TO BROU-DAORDER(1:2)                  
001540                                                                          
001541         MOVE '2  '                 TO US-BR-IDPTYP                       
001542         ADD 1                      TO W-BRO                              
001543*        MOVE W-BRO                 TO US-BR-IDLOPNR                      
001544         MOVE ZERO                  TO US-BR-IDLOPNR                      
001545         MOVE WS-IDSHIPM            TO US-BR-IDSHIPM                      
001546         MOVE BROU-AREA             TO US-BR-FILLER                       
001547         MOVE SKOLLI-IDKUNDRF(3:5)  TO US-BR-FILLER(151:5)                
001548         MOVE SPACE                 TO US-BR-FILLER(156:5)                
001549                                                                          
001550*        WRITE BR-POST-O FROM BROU-AREA                                   
001551         WRITE BR-POST-X FROM US-BR-W4763501                              
001552                                                                          
001553         MOVE 'W47635'               TO POSTSUM-FDNAMN                    
001554         MOVE 'BR      '             TO POSTSUM-DDNAMN2                   
001555         MOVE 'BRO'                  TO POSTSUM-TRANSTYP                  
001556                                                                          
001557         CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                    
001558       ELSE                                                               
001559         MOVE 'O'                   TO BRO-IDPTYP                         
001560*        MOVE W-SHIP-IDSHIPM        TO BRO-IDFAKT                         
001561         MOVE SKOLLI-IDFAKT         TO BRO-IDFAKT                         
001562                                       BRH-IDFAKT                         
001563         MOVE SGMT-IDDISTR          TO BRO-IDDISTR                        
001564         MOVE SGMT-IDKUNDNR         TO BRO-IDKUNDNR                       
001565         MOVE SKOLLI-IDPRODNR       TO BRO-IDPRODNR                       
001566         MOVE SKOLLI-IDKUNDRF       TO BRO-IDKUNDRF                       
001567         MOVE SKOLLI-TIORDREG       TO WS-DATUM                           
001568         MOVE WS-DATUM6             TO BRO-DAORDER(3:6)                   
001569         MOVE 20                    TO BRO-DAORDER(1:2)                   
001570                                                                          
001571         MOVE '2  '                 TO BR-IDPTYP                          
001572         ADD 1                      TO W-BRO                              
001573*        MOVE W-BRO                 TO BR-IDLOPNR                         
001574         MOVE ZERO                  TO BR-IDLOPNR                         
001575         MOVE WS-IDSHIPM            TO BR-IDSHIPM                         
001576         MOVE BRO-AREA              TO BR-FILLER                          
001577         MOVE SKOLLI-IDKUNDRF(3:5)  TO BR-FILLER(151:5)                   
001578         MOVE SPACE                 TO BR-FILLER(156:5)                   
001579                                                                          
001580*        WRITE TO-POST-O FROM BRO-AREA                                    
001581         WRITE TO-POST-X FROM BR-W4763501                                 
001582                                                                          
001583         MOVE 'W47636'               TO POSTSUM-FDNAMN                    
001584         MOVE 'BR      '             TO POSTSUM-DDNAMN2                   
001585         MOVE 'BRO'                  TO POSTSUM-TRANSTYP                  
001586                                                                          
001587         CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                    
001588       END-IF                                                             
001589     ELSE                                                                 
001590       IF SKOLLI-IDPRODNR NOT = WX-IDPRODNR                               
001591         IF (DCS-NDC-NA AND DCS-USA) OR                                   
001592             DIST35-NDCCN-NDCUS-REFILL                                    
001593           MOVE 'O'                 TO BROU-IDPTYP                        
001594*          MOVE W-SHIP-IDSHIPM      TO BROU-IDFAKT                        
001595           MOVE SKOLLI-IDFAKT       TO BROU-IDFAKT                        
001596                                       BRHU-IDFAKT                        
001597           MOVE SGMT-IDDISTR        TO BROU-IDDISTR                       
001598           MOVE SGMT-IDKUNDNR       TO BROU-IDKUNDNR                      
001599           MOVE SKOLLI-IDPRODNR     TO BROU-IDPRODNR                      
001600           MOVE SKOLLI-IDKUNDRF     TO BROU-IDKUNDRF                      
001601           MOVE SKOLLI-TIORDREG     TO WS-DATUM                           
001602           MOVE WS-DATUM6           TO BROU-DAORDER(3:6)                  
001603           MOVE 20                  TO BROU-DAORDER(1:2)                  
001604                                                                          
001605           MOVE '2  '               TO US-BR-IDPTYP                       
001606           ADD 1                    TO W-BRO                              
001607*          MOVE W-BRO               TO US-BR-IDLOPNR                      
001608           MOVE ZERO                TO US-BR-IDLOPNR                      
001609           MOVE WS-IDSHIPM          TO US-BR-IDSHIPM                      
001610           MOVE BROU-AREA           TO US-BR-FILLER                       
001611           MOVE SKOLLI-IDKUNDRF(3:5) TO US-BR-FILLER(151:5)               
001612           MOVE SPACE               TO US-BR-FILLER(156:5)                
001613                                                                          
001614*          WRITE BR-POST-O FROM BROU-AREA                                 
001615           WRITE BR-POST-X FROM US-BR-W4763501                            
001616                                                                          
001617           MOVE 'W47635'             TO POSTSUM-FDNAMN                    
001618           MOVE 'BR      '           TO POSTSUM-DDNAMN2                   
001619           MOVE 'BRO'                TO POSTSUM-TRANSTYP                  
001620                                                                          
001621           CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                  
001622         ELSE                                                             
001623           MOVE 'O'                 TO BRO-IDPTYP                         
001624*          MOVE W-SHIP-IDSHIPM      TO BRO-IDFAKT                         
001625           MOVE SKOLLI-IDFAKT       TO BRO-IDFAKT                         
001626                                       BRH-IDFAKT                         
001627           MOVE SGMT-IDDISTR        TO BRO-IDDISTR                        
001628           MOVE SGMT-IDKUNDNR       TO BRO-IDKUNDNR                       
001629           MOVE SKOLLI-IDPRODNR     TO BRO-IDPRODNR                       
001630           MOVE SKOLLI-IDKUNDRF     TO BRO-IDKUNDRF                       
001631           MOVE SKOLLI-TIORDREG     TO WS-DATUM                           
001632           MOVE WS-DATUM6           TO BRO-DAORDER(3:6)                   
001633           MOVE 20                  TO BRO-DAORDER(1:2)                   
001634                                                                          
001635           MOVE '2  '               TO BR-IDPTYP                          
001636           ADD 1                    TO W-BRO                              
001637*          MOVE W-BRO               TO BR-IDLOPNR                         
001638           MOVE ZERO                TO BR-IDLOPNR                         
001639           MOVE WS-IDSHIPM          TO BR-IDSHIPM                         
001640           MOVE BRO-AREA            TO BR-FILLER                          
001641           MOVE SKOLLI-IDKUNDRF(3:5) TO BR-FILLER(151:5)                  
001642           MOVE SPACE               TO BR-FILLER(156:5)                   
001643                                                                          
001644*          WRITE TO-POST-O FROM BRO-AREA                                  
001645           WRITE TO-POST-X FROM BR-W4763501                               
001646                                                                          
001647           MOVE 'W47636'             TO POSTSUM-FDNAMN                    
001648           MOVE 'BR      '           TO POSTSUM-DDNAMN2                   
001649           MOVE 'BRO'                TO POSTSUM-TRANSTYP                  
001650                                                                          
001651           CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                  
001652         END-IF                                                           
001653       END-IF                                                             
001654     END-IF                                                               
001655                                                                          
001656     .                                                                    
001657     EJECT                                                                
001658 IE-SKAPA-KOLLI-BRP SECTION.                                              
001659     MOVE 'IE-SKAPA-KOLLI-BRP'    TO WS-SEKTION                           
001660                                                                          
001661     IF WS-IDDC-MOT NOT = W-IDDC-B6                                       
001662       MOVE WS-IDDC-MOT TO W-IDDC-B6                                      
001663       PERFORM IMS-GU-WDB601                                              
001664     END-IF                                                               
001665                                                                          
001666     IF (DCS-NDC-NA AND DCS-USA) OR                                       
001667         DIST35-NDCCN-NDCUS-REFILL                                        
001668       MOVE 'P'                   TO BRPU-IDPTYP                          
001669*      MOVE W-SHIP-IDSHIPM        TO BRPU-IDFAKT                          
001670       MOVE SKOLLI-IDFAKT         TO BRPU-IDFAKT                          
001671       MOVE SGMT-IDDISTR          TO BRPU-IDDISTR                         
001672       MOVE SGMT-IDKUNDNR         TO BRPU-IDKUNDNR                        
001673       MOVE SKOLLI-IDPRODNR       TO BRPU-IDPRODNR                        
001674       MOVE SKOLLI-IDKOLLI        TO BRPU-IDKOLLI                         
001675       EVALUATE TRUE                                                      
001676         WHEN SKOLLI-KDEMBTYP = 1                                         
001677           MOVE 'CASE'            TO BRPU-BEEMBTYP                        
001678         WHEN SKOLLI-KDEMBTYP = 2                                         
001679           MOVE 'PARCEL'          TO BRPU-BEEMBTYP                        
001680         WHEN SKOLLI-KDEMBTYP = 3                                         
001681           MOVE 'BUNDLE'          TO BRPU-BEEMBTYP                        
001682         WHEN SKOLLI-KDEMBTYP = 4                                         
001683           MOVE 'CRATE'           TO BRPU-BEEMBTYP                        
001684         WHEN SKOLLI-KDEMBTYP = 5                                         
001685           MOVE 'PIECE'           TO BRPU-BEEMBTYP                        
001686         WHEN SKOLLI-KDEMBTYP = 6                                         
001687           MOVE 'CASE'            TO BRPU-BEEMBTYP                        
001688         WHEN SKOLLI-KDEMBTYP = 7                                         
001689           MOVE 'PALLET'          TO BRPU-BEEMBTYP                        
001690         WHEN OTHER                                                       
001691           MOVE 'PALLET'          TO BRPU-BEEMBTYP                        
001692       END-EVALUATE                                                       
001693       MOVE SKOLLI-DIKOLLIL       TO BRPU-DIKOLLIL                        
001694       MOVE SKOLLI-DIKOLLIB       TO BRPU-DIKOLLIB                        
001695       MOVE SKOLLI-DIKOLLIH       TO BRPU-DIKOLLIH                        
001696       MOVE SKOLLI-VKORDBTO-KOLLI TO BRPU-VKORDBTO-KOLLI                  
001697       MOVE SKOLLI-VKORDNTO-KOLLI TO BRPU-VKORDNTO-KOLLI                  
001698       MOVE SKOLLI-VLORDBTO-KOLLI TO BRPU-VLORDBTO-KOLLI                  
001699       MOVE SKOLLI-SUORDV         TO BRPU-SUORDV-KOLLI                    
001700       IF DIST79-DEALER-PRICE                                             
001701         MOVE SKOLLI-SUORDV-LOC   TO BRPU-SUORDV-KOLLI                    
001702       END-IF                                                             
001703                                                                          
001704       MOVE '3  '                 TO US-BR-IDPTYP                         
001705       ADD 1                      TO W-BRP                                
001706*      MOVE W-BRP                 TO US-BR-IDLOPNR                        
001707       MOVE ZERO                  TO US-BR-IDLOPNR                        
001708       MOVE WS-IDSHIPM            TO US-BR-IDSHIPM                        
001709       MOVE BRPU-AREA             TO US-BR-FILLER                         
001710       MOVE SKOLLI-IDKUNDRF(3:5)  TO US-BR-FILLER(151:5)                  
001711       MOVE SKOLLI-IDKOLLI        TO US-BR-FILLER(156:5)                  
001712                                                                          
001713*      WRITE BR-POST-P FROM BRPU-AREA                                     
001714       WRITE BR-POST-X FROM US-BR-W4763501                                
001715                                                                          
001716       MOVE 'W47635'               TO POSTSUM-FDNAMN                      
001717       MOVE 'BR      '             TO POSTSUM-DDNAMN2                     
001718       MOVE 'BRP'                  TO POSTSUM-TRANSTYP                    
001719                                                                          
001720       CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                      
001721     ELSE                                                                 
001722       MOVE 'P'                   TO BRP-IDPTYP                           
001723*      MOVE W-SHIP-IDSHIPM        TO BRP-IDFAKT                           
001724       MOVE SKOLLI-IDFAKT         TO BRP-IDFAKT                           
001725       MOVE SGMT-IDDISTR          TO BRP-IDDISTR                          
001726       MOVE SGMT-IDKUNDNR         TO BRP-IDKUNDNR                         
001727       MOVE SKOLLI-IDPRODNR       TO BRP-IDPRODNR                         
001728       MOVE SKOLLI-IDKOLLI        TO BRP-IDKOLLI                          
001729       EVALUATE TRUE                                                      
001730         WHEN SKOLLI-KDEMBTYP = 1                                         
001731           MOVE 'CASE'            TO BRP-BEEMBTYP                         
001732         WHEN SKOLLI-KDEMBTYP = 2                                         
001733           MOVE 'PARCEL'          TO BRP-BEEMBTYP                         
001734         WHEN SKOLLI-KDEMBTYP = 3                                         
001735           MOVE 'BUNDLE'          TO BRP-BEEMBTYP                         
001736         WHEN SKOLLI-KDEMBTYP = 4                                         
001737           MOVE 'CRATE'           TO BRP-BEEMBTYP                         
001738         WHEN SKOLLI-KDEMBTYP = 5                                         
001739           MOVE 'PIECE'           TO BRP-BEEMBTYP                         
001740         WHEN SKOLLI-KDEMBTYP = 6                                         
001741           MOVE 'CASE'            TO BRP-BEEMBTYP                         
001742         WHEN SKOLLI-KDEMBTYP = 7                                         
001743           MOVE 'PALLET'          TO BRP-BEEMBTYP                         
001744         WHEN OTHER                                                       
001745           MOVE 'PALLET'          TO BRP-BEEMBTYP                         
001746       END-EVALUATE                                                       
001747       MOVE SKOLLI-DIKOLLIL       TO BRP-DIKOLLIL                         
001748       MOVE SKOLLI-DIKOLLIB       TO BRP-DIKOLLIB                         
001749       MOVE SKOLLI-DIKOLLIH       TO BRP-DIKOLLIH                         
001750       MOVE SKOLLI-VKORDBTO-KOLLI TO BRP-VKORDBTO-KOLLI                   
001751       MOVE SKOLLI-VKORDNTO-KOLLI TO BRP-VKORDNTO-KOLLI                   
001752       MOVE SKOLLI-VLORDBTO-KOLLI TO BRP-VLORDBTO-KOLLI                   
001753       MOVE SKOLLI-SUORDV         TO BRP-SUORDV-KOLLI                     
001754       IF DIST79-DEALER-PRICE                                             
001755         MOVE SKOLLI-SUORDV-LOC   TO BRP-SUORDV-KOLLI                     
001756       END-IF                                                             
001757                                                                          
001758       MOVE '3  '                 TO BR-IDPTYP                            
001759       ADD 1                      TO W-BRP                                
001760*      MOVE W-BRP                 TO BR-IDLOPNR                           
001761       MOVE ZERO                  TO BR-IDLOPNR                           
001762       MOVE WS-IDSHIPM            TO BR-IDSHIPM                           
001763       MOVE BRP-AREA              TO BR-FILLER                            
001764       MOVE SKOLLI-IDKUNDRF(3:5)  TO BR-FILLER(151:5)                     
001765       MOVE SKOLLI-IDKOLLI        TO BR-FILLER(156:5)                     
001766                                                                          
001767*      WRITE TO-POST-P FROM BRP-AREA                                      
001768       WRITE TO-POST-X FROM BR-W4763501                                   
001769                                                                          
001770       MOVE 'W47636'               TO POSTSUM-FDNAMN                      
001771       MOVE 'BR      '             TO POSTSUM-DDNAMN2                     
001772       MOVE 'BRP'                  TO POSTSUM-TRANSTYP                    
001773                                                                          
001774       CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                      
001775     END-IF                                                               
001776                                                                          
001777     ADD 1                      TO WS-BROKER-KVKOLLI                      
001778     ADD SKOLLI-VKORDBTO-KOLLI  TO WS-BROKER-VKORDBTO                     
001779     ADD SKOLLI-VKORDNTO-KOLLI  TO WS-BROKER-VKORDNTO                     
001780     ADD SKOLLI-VLORDBTO-KOLLI  TO WS-BROKER-VLORDBTO                     
001781     IF DIST79-DEALER-PRICE                                               
001782       IF (DCS-NDC-NA AND DCS-USA)                                        
001783         ADD SKOLLI-SUORDV-LOC  TO BRHU-SUORDV-FAKT                       
001784       ELSE                                                               
001785         ADD SKOLLI-SUORDV-LOC  TO BRH-SUORDV-FAKT                        
001786       END-IF                                                             
001787     ELSE                                                                 
001788       IF (DCS-NDC-NA AND DCS-USA) OR                                     
001789           DIST35-NDCCN-NDCUS-REFILL                                      
001790         ADD SKOLLI-SUORDV      TO BRHU-SUORDV-FAKT                       
001791       ELSE                                                               
001792         ADD SKOLLI-SUORDV      TO BRH-SUORDV-FAKT                        
001793       END-IF                                                             
001794     END-IF                                                               
001795                                                                          
001796     .                                                                    
001797     EJECT                                                                
001798 IF-SKAPA-RAD-BRD  SECTION.                                               
001799     MOVE 'IF-SKAPA-RAD-BRD'      TO WS-SEKTION                           
001800                                                                          
001801     IF WS-IDDC-MOT NOT = W-IDDC-B6                                       
001802       MOVE WS-IDDC-MOT TO W-IDDC-B6                                      
001803       PERFORM IMS-GU-WDB601                                              
001804     END-IF                                                               
001805                                                                          
001806     IF (DCS-NDC-NA AND DCS-USA) OR                                       
001807         DIST35-NDCCN-NDCUS-REFILL                                        
001808       MOVE 'D'                   TO BRDU-IDPTYP                          
001809*      MOVE W-SHIP-IDSHIPM        TO BRDU-IDFAKT                          
001810       MOVE SKOLLI-IDFAKT         TO BRDU-IDFAKT                          
001811       MOVE SGMT-IDDISTR          TO BRDU-IDDISTR                         
001812       MOVE SGMT-IDKUNDNR         TO BRDU-IDKUNDNR                        
001813       MOVE SKOLLI-IDPRODNR       TO BRDU-IDPRODNR                        
001814       MOVE SRAD-IDARTNR          TO BRDU-IDARTNR                         
001815                                       WS-BROKER-IDARTNR                  
001816       MOVE SRAD-KVLEVART         TO BRDU-KVLEVART                        
001817       MOVE SRAD-PRARTNTO         TO BRDU-PRARTNTO                        
001818       IF DIST79-DEALER-PRICE                                             
001819         MOVE SRAD-PRARTNTO-LOC   TO BRDU-PRARTNTO                        
001820       END-IF                                                             
001821       COMPUTE BRDU-VKARTNTO = SRAD-VKARTNTO * 1000                       
001822       MOVE SRAD-KDARTURS         TO BRDU-KDARTURS                        
001823       MOVE SRAD-IDLEVNR-ART      TO BRDU-IDLEVNR-ART                     
001824       ADD +1                     TO W-BRO-SEQ                            
001825       MOVE W-BRO-SEQ             TO BRDU-KVSEQ                           
001826       IF SRAD-IDARTNR NOT = W-IDARTNR                                    
001827         MOVE SRAD-IDARTNR        TO W-IDARTNR                            
001828         PERFORM IMS-GU-WDK601                                            
001829       END-IF                                                             
001830*      MOVE ART-KDSORT            TO BRDU-KDSORT                          
001831       MOVE ZERO                  TO BRDU-KDSORT                          
001832       MOVE ART-REKSIFFR          TO WS-BROKER-REKSIFFR                   
001833       MOVE WS-BROKER-ARTNR-TJUGO TO BRDU-IDARTNR                         
001834                                                                          
001835       MOVE '5  '                 TO US-BR-IDPTYP                         
001836       ADD 1                      TO W-BRD                                
001837*      MOVE W-BRD                 TO US-BR-IDLOPNR                        
001838       MOVE ZERO                  TO US-BR-IDLOPNR                        
001839       MOVE BRDU-AREA             TO US-BR-FILLER                         
001840       MOVE SKOLLI-IDKUNDRF(3:5)  TO US-BR-FILLER(151:5)                  
001841       MOVE SRAD-IDARTNR          TO W-ALFA-ARTIN                         
001842       PERFORM S20-VANSTER                                                
001843       MOVE W-ALFA-ARTUT          TO US-BR-FILLER(156:10)                 
001844       MOVE WS-IDSHIPM            TO US-BR-IDSHIPM                        
001845                                                                          
001846*      WRITE BR-POST-D FROM BRDU-AREA                                     
001847       WRITE BR-POST-X FROM US-BR-W4763501                                
001848                                                                          
001849       MOVE 'W47635'               TO POSTSUM-FDNAMN                      
001850       MOVE 'BR      '             TO POSTSUM-DDNAMN2                     
001851       MOVE 'BRD'                  TO POSTSUM-TRANSTYP                    
001852                                                                          
001853       CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                      
001854     ELSE                                                                 
001855       MOVE 'D'                   TO BRD-IDPTYP                           
001856*      MOVE W-SHIP-IDSHIPM        TO BRD-IDFAKT                           
001857       MOVE SKOLLI-IDFAKT         TO BRD-IDFAKT                           
001858       MOVE SGMT-IDDISTR          TO BRD-IDDISTR                          
001859       MOVE SGMT-IDKUNDNR         TO BRD-IDKUNDNR                         
001860       MOVE SKOLLI-IDPRODNR       TO BRD-IDPRODNR                         
001861       MOVE SRAD-IDARTNR          TO BRD-IDARTNR                          
001862                                     WS-BROKER-IDARTNR                    
001863       MOVE SRAD-KVLEVART         TO BRD-KVLEVART                         
001864       MOVE SRAD-PRARTNTO         TO BRD-PRARTNTO                         
001865       IF DIST79-DEALER-PRICE                                             
001866         MOVE SRAD-PRARTNTO-LOC   TO BRD-PRARTNTO                         
001867       END-IF                                                             
001868       COMPUTE BRD-VKARTNTO = SRAD-VKARTNTO * 1000                        
001869       MOVE SRAD-KDARTURS         TO BRD-KDARTURS                         
001870       ADD +1                     TO W-BRO-SEQ                            
001871       MOVE W-BRO-SEQ             TO BRD-KVSEQ                            
001872       IF SRAD-IDARTNR NOT = W-IDARTNR                                    
001873         MOVE SRAD-IDARTNR        TO W-IDARTNR                            
001874         PERFORM IMS-GU-WDK601                                            
001875       END-IF                                                             
001876*      MOVE ART-KDSORT            TO BRD-KDSORT                           
001877       MOVE ZERO                  TO BRD-KDSORT                           
001878       MOVE ART-REKSIFFR          TO WS-BROKER-REKSIFFR                   
001879       MOVE WS-BROKER-ARTNR-TJUGO TO BRD-IDARTNR                          
001880                                                                          
001881       MOVE '5  '                 TO BR-IDPTYP                            
001882       ADD 1                      TO W-BRD                                
001883*      MOVE W-BRD                 TO BR-IDLOPNR                           
001884       MOVE ZERO                  TO BR-IDLOPNR                           
001885       MOVE BRD-AREA              TO BR-FILLER                            
001886       MOVE SKOLLI-IDKUNDRF(3:5)  TO BR-FILLER(151:5)                     
001887       MOVE SRAD-IDARTNR          TO W-ALFA-ARTIN                         
001888       PERFORM S20-VANSTER                                                
001889       MOVE W-ALFA-ARTUT          TO BR-FILLER(156:10)                    
001890       MOVE WS-IDSHIPM            TO BR-IDSHIPM                           
001891                                                                          
001892*      WRITE TO-POST-D FROM BRD-AREA                                      
001893       WRITE TO-POST-X FROM BR-W4763501                                   
001894                                                                          
001895       MOVE 'W47636'               TO POSTSUM-FDNAMN                      
001896       MOVE 'BR      '             TO POSTSUM-DDNAMN2                     
001897       MOVE 'BRD'                  TO POSTSUM-TRANSTYP                    
001898                                                                          
001899       CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                      
001900     END-IF                                                               
001901                                                                          
001902     IF DIST79-DEALER-PRICE                                               
001903       COMPUTE WS-BROKER-SUFKTBEL = WS-BROKER-SUFKTBEL +                  
001904               SRAD-PRARTNTO-LOC * SRAD-KVLEVART                          
001905     ELSE                                                                 
001906       COMPUTE WS-BROKER-SUFKTBEL = WS-BROKER-SUFKTBEL +                  
001907               SRAD-PRARTNTO * SRAD-KVLEVART                              
001908     END-IF                                                               
001909     .                                                                    
001910     EJECT                                                                
001911 J-SKAPA-BRO-POSTER-JP  SECTION.                                          
001912     MOVE 'J-SKAPA-BRO-POSTER-JP' TO WS-SEKTION                           
001913                                                                          
001914     PERFORM S01-LAES-GRUNDDATA                                           
001915     MOVE ZERO                   TO W-BRO-SEQ                             
001916                                    WS-BROKER-KVKOLLI                     
001917                                    WS-BROKER-VKORDBTO                    
001918                                    WS-BROKER-VKORDNTO                    
001919                                    WS-BROKER-VLORDBTO                    
001920                                    WS-BROKER-SUFKTBEL                    
001921                                    W-BRO                                 
001922                                    W-BRP                                 
001923                                    W-BRD                                 
001924                                                                          
001925     MOVE SPACE                  TO BRO-IDKUNDRF                          
001926     MOVE WS-IDSHIPM             TO W-IDSHIPM                             
001927     PERFORM IMS-GU-WDE101                                                
001928     PERFORM IMS-GNP-WDE111                                               
001929     PERFORM UNTIL SEGMENT-SAKNAS                                         
001930       MOVE SGMT-IDDISTR           TO W-IDDISTR                           
001931                                      TEST-IDDISTR                        
001932       MOVE SGMT-IDKUNDNR          TO W-IDKUNDNR                          
001933                                                                          
001934       PERFORM JA-SKAPA-HUVUD-BRH                                         
001935                                                                          
001936       PERFORM UNTIL SEGMENT-SAKNAS                                       
001937         PERFORM JB-HAMTA-TILLAGG                                         
001938         PERFORM IMS-GNP-WDE121                                           
001939         MOVE SKOLLI-IDPRODNR      TO W-IDPRODNR                          
001940         MOVE SKOLLI-IDKOLLI       TO W-IDKOLLI                           
001941         PERFORM UNTIL SEGMENT-SAKNAS                                     
001942                                                                          
001943           PERFORM IMS-GNP-WDE131                                         
001944           PERFORM UNTIL SEGMENT-SAKNAS                                   
001945                                                                          
001946             PERFORM JF-SKAPA-RAD-BRD                                     
001947             PERFORM IMS-GNP-WDE131                                       
001948           END-PERFORM                                                    
001949           PERFORM IMS-GNP-WDE121                                         
001950           MOVE SKOLLI-IDPRODNR    TO W-IDPRODNR                          
001951           MOVE SKOLLI-IDKOLLI     TO W-IDKOLLI                           
001952                                                                          
001953         END-PERFORM                                                      
001954         PERFORM IMS-GNP-WDE111                                           
001955         MOVE SGMT-IDDISTR         TO W-IDDISTR                           
001956         MOVE SGMT-IDKUNDNR        TO W-IDKUNDNR                          
001957                                                                          
001958       END-PERFORM                                                        
001959       PERFORM JC-SKRIV-POST-H                                            
001960     END-PERFORM                                                          
001961     .                                                                    
001962     EJECT                                                                
001963 JA-SKAPA-HUVUD-BRH  SECTION.                                             
001964     MOVE 'JA-SKAPA-HUVUD-BRH'    TO WS-SEKTION                           
001965                                                                          
001966     MOVE 'HDR'                   TO BRJH-BROKJPH-IDPTYP                  
001967*    MOVE W-SHIP-IDSHIPM          TO BRJH-BROKJPH-IDFAKT                  
001968     MOVE SKOLLI-IDFAKT           TO BRJH-BROKJPH-IDFAKT                  
001969     MOVE 20                      TO BRJH-BROKJPH-DAFAKT(1:2)             
001970     MOVE DAGENS-DATUM            TO BRJH-BROKJPH-DAFAKT(3:6)             
001971                                                                          
001972     MOVE W-SHIP-IDDC             TO BRJH-BROKJPH-IDDC                    
001973     MOVE ZERO                    TO BRJH-BROKJPH-KDFRAKT                 
001974     MOVE SGMT-KDVALISO           TO BRJH-BROKJPH-KDVALISO                
001975     MOVE ZERO                    TO BRJH-BROKJPH-SUORDV-FAKT             
001976                                     BRJH-BROKJPH-PREMBHNT                
001977                                     BRJH-BROKJPH-PRFRAKT                 
001978                                     BRJH-BROKJPH-SUFKTUTL                
001979                                     BRJH-BROKJPH-PRFOERS                 
001980     .                                                                    
001981     EJECT                                                                
001982 JB-HAMTA-TILLAGG  SECTION.                                               
001983     MOVE 'JB-HAMTA-TILLAGG'      TO WS-SEKTION                           
001984                                                                          
001985     PERFORM IMS-GNP-WDE122                                               
001986     IF SEGMENT-FINNS                                                     
001987       ADD TILL-PREMBHNT          TO BRJH-BROKJPH-PREMBHNT                
001988       ADD TILL-PRFRAKT           TO BRJH-BROKJPH-PRFRAKT                 
001989       ADD TILL-PRFOERS           TO BRJH-BROKJPH-PRFOERS                 
001990     END-IF                                                               
001991     .                                                                    
001992     EJECT                                                                
001993 JC-SKRIV-POST-H  SECTION.                                                
001994     MOVE 'JC-SKRIV-POST-H'         TO WS-SEKTION                         
001995                                                                          
001996     COMPUTE WS-BROKER-SUFKTBEL = WS-BROKER-SUFKTBEL +                    
001997             BRH-PREMBHNT +                                               
001998             BRH-PRFRAKT +                                                
001999             BRH-PRFOERS                                                  
002000                                                                          
002001     MOVE SKOLLI-KDFAKTYP         TO BRJH-BROKJPH-KDFAKTYP                
002002     MOVE SKOLLI-KDFRAKT          TO BRJH-BROKJPH-KDFRAKT                 
002003*           FAKTURERAD VALUTA                                             
002004     MOVE SGMT-PRKURS             TO BRJH-BROKJPH-PRKURS                  
002005     IF DIST79-DEALER-PRICE AND                                           
002006       SKOLLI-KDVALISO = 'JPY'                                            
002007       MOVE 1.0                   TO BRJH-BROKJPH-PRKURS                  
002008     END-IF                                                               
002009     MOVE WS-BROKER-SUFKTBEL      TO BRJH-BROKJPH-SUORDV-FAKT             
002010     COMPUTE BRJH-BROKJPH-SUFKTUTL ROUNDED = WS-BROKER-SUFKTBEL           
002011                                   / BRJH-BROKJPH-PRKURS                  
002012     MOVE '1  '                   TO BR-IDPTYP                            
002013     MOVE ZERO                    TO BR-IDLOPNR                           
002014     MOVE WS-IDSHIPM              TO BR-IDSHIPM                           
002015     MOVE BRJH-BROKJPH-W4756GH    TO BR-FILLER                            
002016                                                                          
002017     WRITE JP-POST-X FROM BR-W4763501                                     
002018                                                                          
002019     MOVE 'W47637'                TO POSTSUM-FDNAMN                       
002020     MOVE 'BR      '              TO POSTSUM-DDNAMN2                      
002021     MOVE 'BRJH'                  TO POSTSUM-TRANSTYP                     
002022                                                                          
002023     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
002024     .                                                                    
002025     EJECT                                                                
002026 JF-SKAPA-RAD-BRD  SECTION.                                               
002027     MOVE 'JF-SKAPA-RAD-BRD'      TO WS-SEKTION                           
002028                                                                          
002029     MOVE 'DET'                   TO BRJD-BROKJPD-IDPTYP                  
002030*    MOVE W-SHIP-IDSHIPM          TO BRJD-BROKJPD-IDFAKT                  
002031     MOVE SKOLLI-IDFAKT           TO BRJD-BROKJPD-IDFAKT                  
002032     MOVE SKOLLI-IDKUNDRF(1:7)    TO BRJD-BROKJPD-IDORDNR7                
002033     MOVE SRAD-IDARTNR            TO BRJD-BROKJPD-IDARTNR                 
002034                                     WS-BROKER-IDARTNR                    
002035     MOVE SKOLLI-IDKOLLI          TO BRJD-BROKJPD-IDKOLLI                 
002036     MOVE SRAD-KVLEVART           TO BRJD-BROKJPD-KVLEVART                
002037     MOVE SRAD-PRARTNTO           TO BRJD-BROKJPD-PRARTNTO                
002038     IF DIST79-DEALER-PRICE                                               
002039       MOVE SRAD-PRARTNTO-LOC     TO BRJD-BROKJPD-PRARTNTO                
002040     END-IF                                                               
002041     MOVE SRAD-VKARTNTO           TO BRJD-BROKJPD-VKARTNTO                
002042     MOVE SRAD-KDARTURS           TO BRJD-BROKJPD-KDARTURS                
002043     MOVE SRAD-IDSTATNR           TO BRJD-BROKJPD-IDSTATNR                
002044     ADD +1                       TO W-BRO-SEQ                            
002045**   MOVE W-BRO-SEQ               TO BRJD-KVSEQ                           
002046     IF SRAD-IDARTNR NOT = W-IDARTNR                                      
002047       MOVE SRAD-IDARTNR          TO W-IDARTNR                            
002048       PERFORM IMS-GU-WDK601                                              
002049       PERFORM IMS-GNP-WDK611                                             
002050     END-IF                                                               
002051     MOVE ART-KDSORT              TO BRJD-BROKJPD-KDSORT                  
002052*    MOVE ZERO                    TO BRJD-BROKJPD-KDSORT                  
002053     MOVE ART-REKSIFFR            TO WS-BROKER-REKSIFFR                   
002054**   MOVE WS-BROKER-ARTNR-TJUGO   TO BRJD-BROKJPD-IDARTNR                 
002055     COMPUTE BRJD-BROKJPD-VKLEV ROUNDED = SRAD-KVLEVART *                 
002056                                  SRAD-VKARTNTO                           
002057     PERFORM S21-HAMTA-PRARTBTO                                           
002058     IF PRIS-KDSVAR = SPACE AND PRIS-PRARTBTO-MARK > ZERO                 
002059       MOVE PRIS-PRARTBTO-MARK    TO BRJD-BROKJPD-PRARTBTO-EXP            
002060     ELSE                                                                 
002061       MOVE CLAG-PRARTBTO-EXP     TO BRJD-BROKJPD-PRARTBTO-EXP            
002062     END-IF                                                               
002063     PERFORM IMS-GU-WDD311                                                
002064     MOVE TEXT-BEART              TO BRJD-BROKJPD-BEART                   
002065                                                                          
002066     MOVE '5  '                   TO BR-IDPTYP                            
002067     ADD 1                        TO W-BRD                                
002068*    MOVE W-BRD                   TO BR-IDLOPNR                           
002069     MOVE ZERO                    TO BR-IDLOPNR                           
002070     MOVE WS-IDSHIPM              TO BR-IDSHIPM                           
002071     MOVE BRJD-BROKJPD-W4756GD    TO BR-FILLER                            
002072     MOVE SKOLLI-IDKUNDRF(3:5)    TO BR-FILLER(151:5)                     
002073     MOVE SRAD-IDARTNR            TO W-ALFA-ARTIN                         
002074     PERFORM S20-VANSTER                                                  
002075     MOVE W-ALFA-ARTUT            TO BR-FILLER(156:10)                    
002076                                                                          
002077     WRITE JP-POST-X FROM BR-W4763501                                     
002078                                                                          
002079     MOVE 'W47637'                TO POSTSUM-FDNAMN                       
002080     MOVE 'BR      '              TO POSTSUM-DDNAMN2                      
002081     MOVE 'BRJD'                  TO POSTSUM-TRANSTYP                     
002082                                                                          
002083     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
002084                                                                          
002085     IF DIST79-DEALER-PRICE                                               
002086       COMPUTE WS-BROKER-SUFKTBEL = WS-BROKER-SUFKTBEL +                  
002087               SRAD-PRARTNTO-LOC * SRAD-KVLEVART                          
002088     ELSE                                                                 
002089       COMPUTE WS-BROKER-SUFKTBEL = WS-BROKER-SUFKTBEL +                  
002090               SRAD-PRARTNTO * SRAD-KVLEVART                              
002091     END-IF                                                               
002092     .                                                                    
002093     EJECT                                                                
002094 K-SKAPA-BRO-POSTER-AU  SECTION.                                          
002095     MOVE 'K-SKAPA-BRO-POSTER-AU' TO WS-SEKTION                           
002096                                                                          
002097     PERFORM S01-LAES-GRUNDDATA                                           
002098     MOVE ZERO                   TO W-BRO-SEQ                             
002099                                    WS-BROKER-KVKOLLI                     
002100                                    WS-BROKER-VKORDBTO                    
002101                                    WS-BROKER-VKORDNTO                    
002102                                    WS-BROKER-VLORDBTO                    
002103                                    WS-BROKER-SUFKTBEL                    
002104                                    W-BRO                                 
002105                                    W-BRP                                 
002106                                    W-BRD                                 
002107                                                                          
002108     MOVE SPACE                  TO BRO-IDKUNDRF                          
002109     MOVE WS-IDSHIPM             TO W-IDSHIPM                             
002110     PERFORM IMS-GU-WDE101                                                
002111     PERFORM IMS-GNP-WDE111                                               
002112     PERFORM UNTIL SEGMENT-SAKNAS                                         
002113       MOVE SGMT-IDDISTR           TO W-IDDISTR                           
002114                                      TEST-IDDISTR                        
002115       MOVE SGMT-IDKUNDNR          TO W-IDKUNDNR                          
002116                                                                          
002117       PERFORM KA-SKAPA-HUVUD-BRH                                         
002118                                                                          
002119       PERFORM UNTIL SEGMENT-SAKNAS                                       
002120         PERFORM KB-HAMTA-TILLAGG                                         
002121         PERFORM IMS-GNP-WDE121                                           
002122         MOVE SKOLLI-IDPRODNR      TO W-IDPRODNR                          
002123         MOVE SKOLLI-IDKOLLI       TO W-IDKOLLI                           
002124         PERFORM UNTIL SEGMENT-SAKNAS                                     
002125                                                                          
002126           PERFORM KD-SKAPA-ORDER-BRO                                     
002127           PERFORM KE-SKAPA-KOLLI-BRP                                     
002128           PERFORM IMS-GNP-WDE131                                         
002129           PERFORM UNTIL SEGMENT-SAKNAS                                   
002130                                                                          
002131             PERFORM KF-SKAPA-RAD-BRD                                     
002132             PERFORM IMS-GNP-WDE131                                       
002133           END-PERFORM                                                    
002134           PERFORM IMS-GNP-WDE121                                         
002135           MOVE SKOLLI-IDPRODNR    TO W-IDPRODNR                          
002136           MOVE SKOLLI-IDKOLLI     TO W-IDKOLLI                           
002137                                                                          
002138         END-PERFORM                                                      
002139         PERFORM IMS-GNP-WDE111                                           
002140         MOVE SGMT-IDDISTR         TO W-IDDISTR                           
002141         MOVE SGMT-IDKUNDNR        TO W-IDKUNDNR                          
002142                                                                          
002143       END-PERFORM                                                        
002144       MOVE STATUS-WS              TO STATUS-WS-E1                        
002145       PERFORM KC-SKAPA-TOTAL-BRT                                         
002146       MOVE STATUS-WS-E1           TO STATUS-WS                           
002147     END-PERFORM                                                          
002148     .                                                                    
002149     EJECT                                                                
002150 KA-SKAPA-HUVUD-BRH  SECTION.                                             
002151     MOVE 'KA-SKAPA-HUVUD-BRH'    TO WS-SEKTION                           
002152                                                                          
002153     MOVE 'H'                     TO BRH-IDPTYP                           
002154*    MOVE W-SHIP-IDSHIPM          TO BRH-IDFAKT                           
002155     MOVE SKOLLI-IDFAKT           TO BRH-IDFAKT                           
002156     MOVE SGMT-IDDISTR            TO BRH-IDDISTR                          
002157*    MOVE SGMT-IDKUNDNR           TO BRH-IDKUNDNR                         
002158     MOVE ZERO                    TO BRH-IDKUNDNR                         
002159     MOVE 20                      TO BRH-DAFAKT(1:2)                      
002160     MOVE DAGENS-DATUM            TO BRH-DAFAKT(3:6)                      
002161     MOVE 1                       TO BRH-KDFAKT                           
002162*     IF KORD-FLOVRLEV = JA                                               
002163*       MOVE 2                     TO BRH-KDFAKT                          
002164*     END-IF                                                              
002165                                                                          
002166     MOVE WC-NDC-AU               TO WS-IDDC-MOT                          
002167     MOVE ZERO                    TO BRH-KDFRAKT                          
002168*    MOVE W-SHIP-IDLBBET          TO BRH-BESLULEV                         
002169     MOVE 'GOTHENBURG'            TO BRH-BECITY                           
002170     MOVE W-SHIP-IDSHIPM          TO BRH-IDSKEPPN                         
002171     MOVE 'M'                     TO BRH-BEWEIGHT                         
002172     MOVE 'M'                     TO BRH-BEVOLUME                         
002173     MOVE 'SEK'                   TO BRH-KDVALISO-SEK                     
002174*    MOVE 'AUD'                   TO BRH-KDVALISO-UTL                     
002175     MOVE SGMT-KDVALISO           TO BRH-KDVALISO-UTL                     
002176     MOVE SGMT-PRKURS             TO BRH-PRKURS                           
002177*      NATIONELL VALUTA                                                   
002178     MOVE SPACE                   TO BRH-IDBOKN                           
002179                                     BRH-BESLULEV                         
002180     MOVE ZERO                    TO BRH-VKORDBTO-FAKT                    
002181                                     BRH-VKORDNTO-FAKT                    
002182                                     BRH-VLORDBTO-FAKT                    
002183                                     BRH-SUORDV-FAKT                      
002184                                     BRH-PREMBHNT                         
002185                                     BRH-PRFRAKT                          
002186                                     BRH-SUFKTBEL                         
002187                                     BRH-SUFKTUTL                         
002188                                     BRH-PRFOERS                          
002189     .                                                                    
002190     EJECT                                                                
002191 KB-HAMTA-TILLAGG  SECTION.                                               
002192     MOVE 'KB-HAMTA-TILLAGG'      TO WS-SEKTION                           
002193                                                                          
002194     PERFORM IMS-GNP-WDE122                                               
002195     IF SEGMENT-FINNS                                                     
002196       ADD TILL-PREMBHNT          TO BRH-PREMBHNT                         
002197       ADD TILL-PRFRAKT           TO BRH-PRFRAKT                          
002198       ADD TILL-PRFOERS           TO BRH-PRFOERS                          
002199       MOVE TILL-IDBOKN           TO BRH-IDBOKN                           
002200       MOVE TILL-BESLULEV         TO BRH-BESLULEV                         
002201     END-IF                                                               
002202     .                                                                    
002203     EJECT                                                                
002204 KC-SKAPA-TOTAL-BRT  SECTION.                                             
002205     MOVE 'KC-SKAPA-TOTAL-BRT'    TO WS-SEKTION                           
002206                                                                          
002207     MOVE 'T'                     TO BRT-IDPTYP                           
002208*    MOVE W-SHIP-IDSHIPM          TO BRT-IDFAKT                           
002209     MOVE SKOLLI-IDFAKT           TO BRT-IDFAKT                           
002210     MOVE SGMT-IDDISTR            TO BRT-IDDISTR                          
002211*    MOVE SGMT-IDKUNDNR           TO BRT-IDKUNDNR                         
002212     MOVE ZERO                    TO BRT-IDKUNDNR                         
002213     MOVE WS-BROKER-KVKOLLI       TO BRT-KVKOLLI                          
002214     MOVE WS-BROKER-VKORDNTO      TO BRT-VKORDNTO-TOT-KOLLI               
002215     MOVE WS-BROKER-VKORDBTO      TO BRT-VKORDBTO-TOT-KOLLI               
002216     MOVE WS-BROKER-VLORDBTO      TO BRT-VLORDBTO-TOT-KOLLI               
002217                                                                          
002218     MOVE '4  '                   TO BR-IDPTYP                            
002219     MOVE ZERO                    TO BR-IDLOPNR                           
002220     MOVE WS-IDSHIPM              TO BR-IDSHIPM                           
002221     MOVE BRT-AREA                TO BR-FILLER                            
002222                                                                          
002223     WRITE AU-POST-X FROM BR-W4763501                                     
002224                                                                          
002225     MOVE 'W47638'               TO POSTSUM-FDNAMN                        
002226     MOVE 'BR      '             TO POSTSUM-DDNAMN2                       
002227     MOVE 'BRT'                  TO POSTSUM-TRANSTYP                      
002228                                                                          
002229     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
002230                                                                          
002231     PERFORM KCA-SKRIV-POST-H                                             
002232     .                                                                    
002233     EJECT                                                                
002234 KCA-SKRIV-POST-H  SECTION.                                               
002235     MOVE 'KCA-SKRIV-POST-H'         TO WS-SEKTION                        
002236                                                                          
002237     MOVE WS-BROKER-NAME-AU       TO BRH-BEBROKER                         
002238                                                                          
002239     COMPUTE WS-BROKER-SUFKTBEL = WS-BROKER-SUFKTBEL +                    
002240             BRH-PREMBHNT +                                               
002241             BRH-PRFRAKT +                                                
002242             BRH-PRFOERS                                                  
002243     MOVE WS-BROKER-SUFKTBEL       TO BRH-SUFKTBEL                        
002244                                                                          
002245                                                                          
002246     MOVE SKOLLI-KDFRAKT          TO BRH-KDFRAKT                          
002247     MOVE WS-BROKER-VKORDNTO      TO BRH-VKORDNTO-FAKT                    
002248     MOVE WS-BROKER-VKORDBTO      TO BRH-VKORDBTO-FAKT                    
002249     MOVE WS-BROKER-VLORDBTO      TO BRH-VLORDBTO-FAKT                    
002250     MOVE SKOLLI-KDVALISO         TO BRH-KDVALISO-SEK                     
002251     MOVE SKOLLI-IDPRODNR         TO W-IDPRODNR-ESEQ                      
002252                                                                          
002253     PERFORM  IMS-GU-WDE401-ESEQ                                          
002254     IF SEGMENT-FINNS                                                     
002255       IF KORD-FLOVRLEV = JA                                              
002256         MOVE 2                   TO BRH-KDFAKT                           
002257       END-IF                                                             
002258     END-IF                                                               
002259*           FAKTURERAD VALUTA                                             
002260     IF DIST79-DEALER-PRICE AND                                           
002261       SGMT-KDVALISO = SKOLLI-KDVALISO                                    
002262       MOVE 1.0                   TO BRH-PRKURS                           
002263     END-IF                                                               
002264     COMPUTE BRH-SUFKTUTL ROUNDED = BRH-SUFKTBEL / BRH-PRKURS             
002265                                                                          
002266     MOVE '1  '                   TO BR-IDPTYP                            
002267     MOVE ZERO                    TO BR-IDLOPNR                           
002268     MOVE WS-IDSHIPM              TO BR-IDSHIPM                           
002269     MOVE BRH-AREA                TO BR-FILLER                            
002270                                                                          
002271     WRITE AU-POST-X FROM BR-W4763501                                     
002272                                                                          
002273     MOVE 'W47638'               TO POSTSUM-FDNAMN                        
002274     MOVE 'BR      '             TO POSTSUM-DDNAMN2                       
002275     MOVE 'BRH'                  TO POSTSUM-TRANSTYP                      
002276                                                                          
002277     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
002278     .                                                                    
002279     EJECT                                                                
002280                                                                          
002281 KD-SKAPA-ORDER-BRO SECTION.                                              
002282     MOVE 'KD-SKAPA-ORDER-BRO'      TO WS-SEKTION                         
002283                                                                          
002284     MOVE BRO-IDKUNDRF              TO WX-IDKUNDRF                        
002285     IF SKOLLI-IDKUNDRF NOT = WX-IDKUNDRF                                 
002286       MOVE 'O'                     TO BRO-IDPTYP                         
002287*      MOVE W-SHIP-IDSHIPM          TO BRO-IDFAKT                         
002288       MOVE SKOLLI-IDFAKT           TO BRO-IDFAKT                         
002289       MOVE SGMT-IDDISTR            TO BRO-IDDISTR                        
002290       MOVE SGMT-IDKUNDNR           TO BRO-IDKUNDNR                       
002291       MOVE SKOLLI-IDPRODNR         TO BRO-IDPRODNR                       
002292       MOVE SKOLLI-IDKUNDRF(3:5)    TO BRO-IDKUNDRF                       
002293       MOVE SKOLLI-TIORDREG         TO BRO-DAORDER(3:6)                   
002294       MOVE 20                      TO BRO-DAORDER(1:2)                   
002295                                                                          
002296       MOVE '2  '                   TO BR-IDPTYP                          
002297       ADD 1                        TO W-BRO                              
002298*      MOVE W-BRO                   TO BR-IDLOPNR                         
002299       MOVE ZERO                    TO BR-IDLOPNR                         
002300       MOVE WS-IDSHIPM              TO BR-IDSHIPM                         
002301       MOVE BRO-AREA                TO BR-FILLER                          
002302       MOVE SKOLLI-IDKUNDRF(3:5)    TO BR-FILLER(151:5)                   
002303       MOVE SPACE                   TO BR-FILLER(156:5)                   
002304                                                                          
002305       WRITE AU-POST-X FROM BR-W4763501                                   
002306                                                                          
002307       MOVE 'W47638'               TO POSTSUM-FDNAMN                      
002308       MOVE 'BR      '             TO POSTSUM-DDNAMN2                     
002309       MOVE 'BRO'                  TO POSTSUM-TRANSTYP                    
002310                                                                          
002311       CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                      
002312     END-IF                                                               
002313                                                                          
002314     .                                                                    
002315     EJECT                                                                
002316 KE-SKAPA-KOLLI-BRP SECTION.                                              
002317     MOVE 'KE-SKAPA-KOLLI-BRP'    TO WS-SEKTION                           
002318                                                                          
002319     MOVE 'P'                     TO BRP-IDPTYP                           
002320*    MOVE W-SHIP-IDSHIPM          TO BRP-IDFAKT                           
002321     MOVE SKOLLI-IDFAKT           TO BRP-IDFAKT                           
002322     MOVE SGMT-IDDISTR            TO BRP-IDDISTR                          
002323     MOVE SGMT-IDKUNDNR           TO BRP-IDKUNDNR                         
002324     MOVE SKOLLI-IDPRODNR         TO BRP-IDPRODNR                         
002325     MOVE SKOLLI-IDKOLLI          TO BRP-IDKOLLI                          
002326     EVALUATE TRUE                                                        
002327       WHEN SKOLLI-KDEMBTYP = 1                                           
002328         MOVE 'CASE'              TO BRP-BEEMBTYP                         
002329       WHEN SKOLLI-KDEMBTYP = 2                                           
002330         MOVE 'PARCEL'            TO BRP-BEEMBTYP                         
002331       WHEN SKOLLI-KDEMBTYP = 3                                           
002332         MOVE 'BUNDLE'            TO BRP-BEEMBTYP                         
002333       WHEN SKOLLI-KDEMBTYP = 4                                           
002334         MOVE 'CRATE'             TO BRP-BEEMBTYP                         
002335       WHEN SKOLLI-KDEMBTYP = 5                                           
002336         MOVE 'PIECE'             TO BRP-BEEMBTYP                         
002337       WHEN SKOLLI-KDEMBTYP = 6                                           
002338         MOVE 'CASE'              TO BRP-BEEMBTYP                         
002339       WHEN SKOLLI-KDEMBTYP = 7                                           
002340         MOVE 'PALLET'            TO BRP-BEEMBTYP                         
002341       WHEN OTHER                                                         
002342         MOVE 'PALLET'            TO BRP-BEEMBTYP                         
002343     END-EVALUATE                                                         
002344     MOVE SKOLLI-DIKOLLIL         TO BRP-DIKOLLIL                         
002345     MOVE SKOLLI-DIKOLLIB         TO BRP-DIKOLLIB                         
002346     MOVE SKOLLI-DIKOLLIH         TO BRP-DIKOLLIH                         
002347     MOVE SKOLLI-VKORDBTO-KOLLI   TO BRP-VKORDBTO-KOLLI                   
002348     MOVE SKOLLI-VKORDNTO-KOLLI   TO BRP-VKORDNTO-KOLLI                   
002349     MOVE SKOLLI-VLORDBTO-KOLLI   TO BRP-VLORDBTO-KOLLI                   
002350     MOVE SKOLLI-SUORDV           TO BRP-SUORDV-KOLLI                     
002351     IF DIST79-DEALER-PRICE                                               
002352       MOVE SKOLLI-SUORDV-LOC     TO BRP-SUORDV-KOLLI                     
002353     END-IF                                                               
002354                                                                          
002355     MOVE '3  '                   TO BR-IDPTYP                            
002356     ADD 1                        TO W-BRP                                
002357*    MOVE W-BRP                   TO BR-IDLOPNR                           
002358     MOVE ZERO                    TO BR-IDLOPNR                           
002359     MOVE WS-IDSHIPM              TO BR-IDSHIPM                           
002360     MOVE BRP-AREA                TO BR-FILLER                            
002361     MOVE SKOLLI-IDKUNDRF(3:5)    TO BR-FILLER(151:5)                     
002362     MOVE SKOLLI-IDKOLLI          TO BR-FILLER(156:5)                     
002363                                                                          
002364     WRITE AU-POST-X FROM BR-W4763501                                     
002365                                                                          
002366     MOVE 'W47638'               TO POSTSUM-FDNAMN                        
002367     MOVE 'BR      '             TO POSTSUM-DDNAMN2                       
002368     MOVE 'BRP'                  TO POSTSUM-TRANSTYP                      
002369                                                                          
002370     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
002371                                                                          
002372     ADD 1                      TO WS-BROKER-KVKOLLI                      
002373     ADD SKOLLI-VKORDBTO-KOLLI  TO WS-BROKER-VKORDBTO                     
002374     ADD SKOLLI-VKORDNTO-KOLLI  TO WS-BROKER-VKORDNTO                     
002375     ADD SKOLLI-VLORDBTO-KOLLI  TO WS-BROKER-VLORDBTO                     
002376     IF DIST79-DEALER-PRICE                                               
002377       ADD SKOLLI-SUORDV-LOC    TO BRH-SUORDV-FAKT                        
002378     ELSE                                                                 
002379       ADD SKOLLI-SUORDV        TO BRH-SUORDV-FAKT                        
002380     END-IF                                                               
002381                                                                          
002382     .                                                                    
002383     EJECT                                                                
002384 KF-SKAPA-RAD-BRD  SECTION.                                               
002385     MOVE 'KF-SKAPA-RAD-BRD'      TO WS-SEKTION                           
002386                                                                          
002387     MOVE 'D'                     TO BRD-IDPTYP                           
002388*    MOVE W-SHIP-IDSHIPM          TO BRD-IDFAKT                           
002389     MOVE SKOLLI-IDFAKT           TO BRD-IDFAKT                           
002390     MOVE SGMT-IDDISTR            TO BRD-IDDISTR                          
002391     MOVE SGMT-IDKUNDNR           TO BRD-IDKUNDNR                         
002392     MOVE SKOLLI-IDPRODNR         TO BRD-IDPRODNR                         
002393     MOVE SRAD-IDARTNR            TO BRD-IDARTNR                          
002394                                     WS-BROKER-IDARTNR                    
002395     MOVE SRAD-KVLEVART           TO BRD-KVLEVART                         
002396     MOVE SRAD-PRARTNTO           TO BRD-PRARTNTO                         
002397     IF DIST79-DEALER-PRICE                                               
002398       MOVE SRAD-PRARTNTO-LOC     TO BRD-PRARTNTO                         
002399     END-IF                                                               
002400     COMPUTE BRD-VKARTNTO ROUNDED = SRAD-VKARTNTO * 1000                  
002401     MOVE SRAD-KDARTURS           TO BRD-KDARTURS                         
002402     ADD +1                       TO W-BRO-SEQ                            
002403     MOVE W-BRO-SEQ               TO BRD-KVSEQ                            
002404     IF SRAD-IDARTNR NOT = W-IDARTNR                                      
002405       MOVE SRAD-IDARTNR          TO W-IDARTNR                            
002406       PERFORM IMS-GU-WDK601                                              
002407     END-IF                                                               
002408*    MOVE ART-KDSORT              TO BRD-KDSORT                           
002409     MOVE ZERO                    TO BRD-KDSORT                           
002410     MOVE ART-REKSIFFR            TO WS-BROKER-REKSIFFR                   
002411     MOVE WS-BROKER-ARTNR-TJUGO   TO BRD-IDARTNR                          
002412                                                                          
002413     MOVE '5  '                   TO BR-IDPTYP                            
002414     ADD 1                        TO W-BRD                                
002415*    MOVE W-BRD                   TO BR-IDLOPNR                           
002416     MOVE ZERO                    TO BR-IDLOPNR                           
002417     MOVE WS-IDSHIPM              TO BR-IDSHIPM                           
002418     MOVE BRD-AREA                TO BR-FILLER                            
002419     MOVE SKOLLI-IDKUNDRF(3:5)    TO BR-FILLER(151:5)                     
002420     MOVE SRAD-IDARTNR            TO W-ALFA-ARTIN                         
002421     PERFORM S20-VANSTER                                                  
002422     MOVE W-ALFA-ARTUT            TO BR-FILLER(156:10)                    
002423                                                                          
002424     WRITE AU-POST-X FROM BR-W4763501                                     
002425                                                                          
002426     MOVE 'W47638'               TO POSTSUM-FDNAMN                        
002427     MOVE 'BR      '             TO POSTSUM-DDNAMN2                       
002428     MOVE 'BRAD'                 TO POSTSUM-TRANSTYP                      
002429                                                                          
002430     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
002431                                                                          
002432     IF DIST79-DEALER-PRICE                                               
002433       COMPUTE WS-BROKER-SUFKTBEL = WS-BROKER-SUFKTBEL +                  
002434               SRAD-PRARTNTO-LOC * SRAD-KVLEVART                          
002435     ELSE                                                                 
002436       COMPUTE WS-BROKER-SUFKTBEL = WS-BROKER-SUFKTBEL +                  
002437               SRAD-PRARTNTO * SRAD-KVLEVART                              
002438     END-IF                                                               
002439     .                                                                    
002440     EJECT                                                                
002441                                                                          
002442 Z-FINIT SECTION.                                                         
002443                                                                          
002444     CLOSE W47624                                                         
002445     CLOSE W47628                                                         
002446           W47630                                                         
002447           W47635                                                         
002448           W47636                                                         
002449           W47637                                                         
002450           W47638                                                         
002451     SKIP2                                                                
002452     MOVE 'S' TO POSTSUM-OPKOD                                            
002453     CALL POSTSUM USING POSTSUM-PARM                                      
002454     .                                                                    
002455     EJECT                                                                
002456 S01-LAES-GRUNDDATA SECTION.                                              
002457                                                                          
002458       MOVE WS-IDSHIPM           TO W-IDSHIPM                             
002459       PERFORM IMS-GU-WDE101                                              
002460       IF SEGMENT-FINNS                                                   
002461         PERFORM IMS-GNP-WDE111                                           
002462         MOVE SGMT-IDDISTR       TO W-IDDISTR                             
002463         MOVE SGMT-IDKUNDNR      TO W-IDKUNDNR                            
002464         IF SEGMENT-FINNS                                                 
002465           PERFORM IMS-GNP-WDE121                                         
002466         END-IF                                                           
002467       END-IF                                                             
002468       MOVE SKOLLI-IDPRODNR      TO W-IDPRODNR                            
002469       MOVE SKOLLI-IDKOLLI       TO W-IDKOLLI                             
002470     .                                                                    
002471     EJECT                                                                
002472 S02-KOLLA-EMB-SAMKLI SECTION.                                            
002473                                                                          
002474     MOVE NEJ                   TO SAMKOLLI-SW                            
002475     MOVE +1                    TO INDX                                   
002476     PERFORM UNTIL INDX > MAX-INDX                                        
002477       IF TAB-IDKOLLI-SAMP (INDX) = ZERO                                  
002478         MOVE SKOLLI-IDKOLLI-SAMP TO                                      
002479                             TAB-IDKOLLI-SAMP (INDX)                      
002480         MOVE W-SHIP-IDDC         TO W-E7A-IDDC                           
002481         MOVE SKOLLI-IDKOLLI-SAMP TO W-E7A-IDKOLLIS                       
002482         PERFORM IMS-GU-WDE711-ASEQ                                       
002483         IF SEGMENT-FINNS                                                 
002484           MOVE SKLI-KDKOLLI-SAMP TO W-KOLLI-KDKOLLI                      
002485           MOVE JA                TO SAMKOLLI-SW                          
002486         ELSE                                                             
002487           MOVE SPACE             TO W-KOLLI-KDKOLLI                      
002488         END-IF                                                           
002489         MOVE +1000               TO INDX                                 
002490       ELSE                                                               
002491         IF SKOLLI-IDKOLLI-SAMP =                                         
002492                                TAB-IDKOLLI-SAMP (INDX)                   
002493           MOVE +1000           TO INDX                                   
002494           MOVE SPACE           TO W-KOLLI-KDKOLLI                        
002495         ELSE                                                             
002496           ADD +1               TO INDX                                   
002497         END-IF                                                           
002498       END-IF                                                             
002499     END-PERFORM                                                          
002500     .                                                                    
002501     EJECT                                                                
002502 S03-KDSOFT SECTION.                                                      
002503     MOVE 'S03-KDSOFT'   TO WS-SEKTION                                    
002504                                                                          
002505     MOVE +0                     TO W-KDSOFT                              
002506     MOVE SKOLLI-IDPRODNR        TO W-IDPRODNR-MIN                        
002507                                    W-IDPRODNR-MAX                        
002508     MOVE SRAD-IDPURAD           TO W-IDPURAD-MIN                         
002509                                    W-IDPURAD-MAX                         
002510     PERFORM IMS-GU-WDE411-BSEQ                                           
002511     IF ORAD-IDBIL > SPACE AND   ORAD-IDSYSTEM   = 'VDI '                 
002512       MOVE +1                   TO W-KDSOFT                              
002513     ELSE                                                                 
002514       IF ORAD-IDSYSTEM = 'SOFT'                                          
002515         MOVE +2                 TO W-KDSOFT                              
002516       ELSE                                                               
002517         MOVE ORAD-IDARTNR       TO TEST-ARTIKEL                          
002518         IF ART04-SOFTWARE                                                
002519           MOVE +3               TO W-KDSOFT                              
002520         ELSE                                                             
002521           IF ORAD-IDSYSTEM = 'W371' OR 'W37A'                            
002522             MOVE +4             TO W-KDSOFT                              
002523           ELSE                                                           
002524             MOVE +0             TO W-KDSOFT                              
002525           END-IF                                                         
002526         END-IF                                                           
002527       END-IF                                                             
002528     END-IF                                                               
002529     .                                                                    
002530     EJECT                                                                
002531 S11-LAS-INFIL  SECTION.                                                  
002532     READ W47624 INTO IN-AREA                                             
002533      AT END                                                              
002534         MOVE HIGH-VALUE TO IN-AREA                                       
002535         SET END-OF-W47624 TO TRUE                                        
002536                                                                          
002537      NOT AT END                                                          
002538         MOVE 'W47624'       TO POSTSUM-FDNAMN                            
002539         MOVE 'W47625D1'     TO POSTSUM-DDNAMN2                           
002540         MOVE SPACE          TO POSTSUM-TRANSTYP                          
002541         CALL POSTSUM USING POSTSUM-PARM                                  
002542     END-READ                                                             
002543     .                                                                    
002544     EJECT                                                                
002545                                                                          
002546 S13-SKRIV-W47628 SECTION.                                                
002547                                                                          
002548     WRITE UT3-POST FROM EXP-EMB-UTAREA                                   
002549                                                                          
002550     MOVE SPACE           TO POSTSUM-TRANSTYP                             
002551     MOVE 'W47628'        TO POSTSUM-FDNAMN                               
002552     MOVE 'W47625D3'      TO POSTSUM-DDNAMN2                              
002553     CALL POSTSUM USING POSTSUM-PARM                                      
002554     .                                                                    
002555     EJECT                                                                
002556 S14-SKRIV-W47630 SECTION.                                                
002557                                                                          
002558     WRITE UT4-POST FROM  EMB-UTAREA                                      
002559                                                                          
002560     MOVE SPACE           TO POSTSUM-TRANSTYP                             
002561     MOVE 'W47630'        TO POSTSUM-FDNAMN                               
002562     MOVE 'W47625D4'      TO POSTSUM-DDNAMN2                              
002563     CALL POSTSUM USING POSTSUM-PARM                                      
002564     .                                                                    
002565     EJECT                                                                
002566 S20-VANSTER  SECTION.                                                    
002567                                                                          
002568     INSPECT W-ALFA-ARTIN REPLACING LEADING ZERO BY SPACE                 
002569     CALL W009REDU USING W-ALFA-ARTIN W-ALFA-ARTUT                        
002570     .                                                                    
002571     EJECT                                                                
002572 S21-HAMTA-PRARTBTO SECTION.                                              
002573                                                                          
002574                                                                          
002575     MOVE 'S21-HAMTA-PRARTBTO'  TO WS-SEKTION                             
002576                                                                          
002577     MOVE 1                    TO PRIS-KDCALL                             
002578     MOVE IDPGM                TO PRIS-IDPGM                              
002579     MOVE SRAD-IDARTNR         TO PRIS-IDARTNR                            
002580     MOVE SKOLLI-IDDISTR       TO PRIS-IDDISTR                            
002581     MOVE SKOLLI-IDKUNDNR      TO PRIS-IDKUNDNR                           
002582     MOVE SGMT-IDDC            TO PRIS-IDDC                               
002583     MOVE +1                   TO PRIS-KDORDKL                            
002584     MOVE +1                   TO PRIS-KVBEART                            
002585     MOVE NEJ                  TO PRIS-FLINVEST                           
002586                                                                          
002587     CALL W335PRIS USING PRIS-W335PRIS  PRIS-ARTC-PCB                     
002588                         PRIS-WDK7-PCB                                    
002589                         PRIS-GMTA-PCB  PRIS-BETA-PCB                     
002590                         PRIS-GPRIA-PCB PRIS-GPRIB-PCB                    
002591                         PRIS-COST-WDK6-PCB                               
002592                         PRIS-COST-WDK7-PCB                               
002593                         PRIS-COST-WDF1-PCB                               
002594                         PRIS-COST-9305-PCB                               
002595                         PRIS-COST-WDK72-PCB                              
002596                         PRIS-COST-WDB6-PCB                               
002597                                                                          
002598     IF PRIS-KDSVAR = '2'                                                 
002599        MOVE 'DIST/KUND SAKNAS NÄR W335PRIS LÄSER KUNDREG'                
002600                            TO FELTEXT                                    
002601        CALL ABEND USING RKOD-ABEND-MED-DUMP                              
002602     END-IF                                                               
002603                                                                          
002604     .                                                                    
002605     EJECT                                                                
002606                                                                          
002607* --- IMS SEKTIONER ---                                                   
002608                                                                          
002609     EJECT                                                                
002610 IMS-GU-WDE101 SECTION.                                                   
002611                                                                          
002612     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
002613          DELIMITED BY SIZE INTO SSA1                                     
002614     MOVE '    ' TO GODK-STATUSKODER                                      
002615     CALL CBLTDLI USING GU WDE1-PCB DLI-IO-WDE101 SSA1                    
002616     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
002617     PERFORM IMS-STATUSKONTROLL                                           
002618     .                                                                    
002619     EJECT                                                                
002620 IMS-GNP-WDE111 SECTION.                                                  
002621                                                                          
002622     MOVE 'WDE111'     TO SSA1                                            
002623     MOVE '  GE' TO GODK-STATUSKODER                                      
002624     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE111 SSA1                   
002625     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
002626     PERFORM IMS-STATUSKONTROLL                                           
002627     .                                                                    
002628     EJECT                                                                
002629 IMS-GNP-WDE121 SECTION.                                                  
002630                                                                          
002631     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
002632          DELIMITED BY SIZE INTO SSA1                                     
002633     MOVE 'WDE121'     TO SSA2                                            
002634     MOVE '  GE' TO GODK-STATUSKODER                                      
002635     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE121 SSA1 SSA2              
002636     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
002637     PERFORM IMS-STATUSKONTROLL                                           
002638     .                                                                    
002639     EJECT                                                                
002640 IMS-GNP-WDE122 SECTION.                                                  
002641                                                                          
002642     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
002643          DELIMITED BY SIZE INTO SSA1                                     
002644     MOVE 'WDE122  '          TO SSA2                                     
002645     MOVE '  GE' TO GODK-STATUSKODER                                      
002646     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE122 SSA1 SSA2              
002647     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
002648     PERFORM IMS-STATUSKONTROLL                                           
002649     .                                                                    
002650     EJECT                                                                
002651 IMS-GNP-WDE131 SECTION.                                                  
002652                                                                          
002653     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
002654          DELIMITED BY SIZE INTO SSA1                                     
002655     STRING 'WDE121  (WDE121KY =' W-WDE121KY-X ')'                        
002656          DELIMITED BY SIZE INTO SSA2                                     
002657     MOVE 'WDE131'       TO SSA3                                          
002658     MOVE '  GE' TO GODK-STATUSKODER                                      
002659     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE131 SSA1 SSA2 SSA3         
002660     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
002661     PERFORM IMS-STATUSKONTROLL                                           
002662     .                                                                    
002663     EJECT                                                                
002664 IMS-GU-WDE411-BSEQ  SECTION.                                             
002665     STRING 'WDE411  (WDE4BSEQ>=' W-WDE4BSEQ-MIN-X                        
002666                    '&WDE4BSEQ<=' W-WDE4BSEQ-MAX-X ') '                   
002667          DELIMITED BY SIZE INTO SSA1                                     
002668     MOVE '  GE' TO GODK-STATUSKODER                                      
002669     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-WDE411 SSA1                    
002670     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
002671     PERFORM IMS-STATUSKONTROLL                                           
002672     .                                                                    
002673     SKIP3                                                                
002674 IMS-GU-WDE401-ESEQ   SECTION.                                            
002675                                                                          
002676     STRING 'WDE401  (WDE4ESEQ =' W-WDE4ESEQ-X ')'                        
002677          DELIMITED BY SIZE INTO SSA1                                     
002678     MOVE '  GE' TO GODK-STATUSKODER                                      
002679     CALL CBLTDLI USING GU WDE4E-PCB DLI-IO-WDE401 SSA1                   
002680     MOVE WDE4E-STATUS-CODE TO STATUS-WS                                  
002681     PERFORM IMS-STATUSKONTROLL                                           
002682     .                                                                    
002683     SKIP3                                                                
002684 IMS-GU-WDK601  SECTION.                                                  
002685*    DISPLAY 'K601'                                                       
002686     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
002687          DELIMITED BY SIZE INTO SSA1                                     
002688     MOVE '  GE' TO GODK-STATUSKODER                                      
002689     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
002690     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
002691     PERFORM IMS-STATUSKONTROLL                                           
002692     .                                                                    
002693     EJECT                                                                
002694 IMS-GNP-WDK611 SECTION.                                                  
002695*    DISPLAY 'K611'                                                       
002696     MOVE 'WDK611'       TO SSA1                                          
002697     MOVE '  GE' TO GODK-STATUSKODER                                      
002698     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
002699     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
002700     PERFORM IMS-STATUSKONTROLL                                           
002701     .                                                                    
002702     EJECT                                                                
002703 IMS-GU-WDB601    SECTION.                                                
002704                                                                          
002705     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
002706          DELIMITED BY SIZE INTO SSA1                                     
002707     MOVE '  GE' TO GODK-STATUSKODER                                      
002708     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
002709     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
002710     PERFORM IMS-STATUSKONTROLL                                           
002711     IF SEGMENT-SAKNAS                                                    
002712         MOVE SPACE TO DCS-KDDC                                           
002713     END-IF                                                               
002714     .                                                                    
002715     EJECT                                                                
002716 IMS-GU-WDD311    SECTION.                                                
002717     MOVE 'IMS-GU-WDD311'  TO WS-SEKTION                                  
002718                                                                          
002719     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
002720            DELIMITED BY SIZE INTO SSA1                                   
002721     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
002722            DELIMITED BY SIZE INTO SSA2                                   
002723     MOVE '  GE' TO GODK-STATUSKODER                                      
002724     CALL CBLTDLI USING GU  WDD3-PCB DLI-IO-WDD311 SSA1 SSA2              
002725     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
002726     PERFORM IMS-STATUSKONTROLL                                           
002727     .                                                                    
002728     EJECT                                                                
002729 IMS-GU-WDE711-ASEQ  SECTION.                                             
002730     MOVE 'IMS-GU-WDE711-ASEQ '    TO WS-SEKTION                          
002731                                                                          
002732     STRING 'WDE711  (WDE7ASEQ =' W-WDE7ASEQ-X ')'                        
002733          DELIMITED BY SIZE INTO SSA1                                     
002734     MOVE '  GE' TO GODK-STATUSKODER                                      
002735     CALL CBLTDLI USING GU WDE7-PCB DLI-IO-WDE711 SSA1                    
002736     MOVE WDE7-STATUS-CODE TO STATUS-WS                                   
002737     PERFORM IMS-STATUSKONTROLL                                           
002738     .                                                                    
002739     EJECT                                                                
002740                                                                          
002741 IMS-STATUSKONTROLL SECTION.                                              
002742                                                                          
002743     SET STATUS-IX TO 1                                                   
002744     SEARCH GODK-STATUS                                                   
002745       AT END                                                             
002746         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
002747           DELIMITED BY SIZE INTO FELTEXT                                 
002748         CALL FELLOG                                                      
002749       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
002750         CONTINUE                                                         
002751     END-SEARCH                                                           
002760     .                                                                    
