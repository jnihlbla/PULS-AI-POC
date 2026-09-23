000001 ID DIVISION.                                                             
000002 PROGRAM-ID.     W4074500.                                                
000003 AUTHOR.         SUSANNE OLSSON.                                          
000004 DATE-WRITTEN.   99/12/10.                                                
000005 DATE-COMPILED.                                                           
000006                                                                          
000007**   FUNKTION:                                                            
000008*        PROGRAMMET ANVÄNDS FÖR ATT SKAPA EN PROFORMAFAKTURA OCH          
000009*        KOLLIFLAGGOR,FÖR ATT SKICKA MED GODSET VID RETUR AV RET-         
000010*        UR , SAMT SPARA INFORMATIONEN 1 ÅR.                              
000011*                                                                         
000012*        PROGRAMMET UPPDATERAR WDGX4102 (WDR4)                            
000013*        PROGRAMMET LÄSER      WDB2                                       
000014*        PROGRAMMET LÄSER      WDK6                                       
000015*                                                                         
000016*    ÄNDRAT: 2004-06  E-TRACKER SCR-ID 852989                             
000017*    ÄNDRAT: 2007-07  E-TRACKER SCR-ID 3025398                            
000018*    ÄNDRAT: 2008-04  E-TRACKER SCR-ID 880053                             
000019*                                                                         
000020*                                                                         
000021*    INDATA.                                                              
000022*        TRANSAKTION: W4T745                                              
000023*        MID:         W4I74501                                            
000024*                                                                         
000025*    UTDATA.                                                              
000026*        MOD:         W4O74501                                            
000027                                                                          
000028     SKIP3                                                                
000029 ENVIRONMENT DIVISION.                                                    
000030     EJECT                                                                
000031 DATA DIVISION.                                                           
000032 WORKING-STORAGE SECTION.                                                 
000033                                                                          
000034*    -- CHECKED BY WY2000                                                 
000035 77  IDPGM                       PIC X(08)   VALUE 'W4074500'.            
000036                                                                          
000037*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
000038 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
000039                                                                          
000040 77  JA                          PIC X       VALUE 'J'.                   
000041 77  NEJ                         PIC X       VALUE 'N'.                   
000042 77  WS-IDKOLLI                  PIC X(5)    VALUE ZERO.                  
000043 77  WS-VKORDBTO-KOLLI           PIC S9(6)V9(1) VALUE ZERO.               
000044 77  WS-MID-IDTRANS              PIC X(4)    VALUE '4749'.                
000045 77  WS-SPAR-IDKOLLI             PIC S9(5)   VALUE ZERO COMP-3.           
000046                                                                          
000052 77  LITEN-BOKSTAV               PIC X(30)                                
000053         VALUE 'abcdefghijklmnopqrstuvwxyzåäöü'.                          
000054 77  STOR-BOKSTAV                PIC X(30)                                
000055         VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZÅÄÖÜ'.                          
000056*    VÄRDET FÖR STORA TYSKA Y ÄR HEX 'FC'                                 
000057     EJECT                                                                
000058                                                                          
000059 77  TAB-INDX                    PIC 9(3)   VALUE ZERO.                   
000060                                                                          
000061 77  VISA-INFO-SW                PIC X       VALUE 'J'.                   
000062     88  VISA-INFO-OK                        VALUE 'J'.                   
000063     88  VISA-INFO-FEL                       VALUE 'N'.                   
000064                                                                          
000065 77  INPUT-SW                    PIC X       VALUE 'N'.                   
000066     88  INPUT-FINNS                         VALUE 'J'.                   
000067     88  INPUT-SAKNAS                        VALUE 'N'.                   
000068                                                                          
000069 77  KDCMD-SW                    PIC X       VALUE 'N'.                   
000070     88  KDCMD-FINNS                         VALUE 'J'.                   
000071     88  KDCMD-SAKNAS                        VALUE 'N'.                   
000072                                                                          
000073 77  NY-RAD-SW                   PIC X       VALUE 'N'.                   
000074     88  NY-RAD-FINNS                        VALUE 'J'.                   
000075     88  NY-RAD-SAKNAS                       VALUE 'N'.                   
000076                                                                          
000077 77  RAPPORT-SW                  PIC X       VALUE 'N'.                   
000078     88  NY-RAPPORT                          VALUE 'J'.                   
000079     88  GAMMAL-RAPPORT                      VALUE 'N'.                   
000080                                                                          
000081 77  VIKT-SAKNAS-SW              PIC X       VALUE 'N'.                   
000082     88  VIKT-SAKNAS                         VALUE 'J'.                   
000083                                                                          
000084 77  GODKAND-RAD-SW              PIC X       VALUE 'N'.                   
000085     88  GODKAND-RAD                         VALUE 'J'.                   
000086                                                                          
000087*    --- INDEX FÖR BLÄDDRINGSRADER                                        
000088 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
000089 77  MAX-INDX                    PIC S9(4)  VALUE +10   COMP SYNC.        
000090*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
000091                                                                          
000092 77  INDATA-SW                   PIC X       VALUE 'J'.                   
000093     88  INDATA-OK                           VALUE 'J'.                   
000094     88  INDATA-FEL                          VALUE 'N'.                   
000095                                                                          
000096 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
000097     88  NYCKLAR-OK                          VALUE 'J'.                   
000098     88  NYCKLAR-FEL                         VALUE 'N'.                   
000099                                                                          
000100 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
000101     88  EGEN-MID                            VALUE '4745'.                
000102     88  GODK-MID                            VALUE '4745'.                
000103     88  HELP-MID                            VALUE '0551'.                
000104                                                                          
000105     EJECT                                                                
000106******************************************************************        
000107 01  PRIS-TABELL.                                                         
000108     03  RADPRIS     OCCURS 10 TIMES.                                     
000109       05  WS-PRARTBTO             PIC S9(7)V9(2) VALUE ZERO.             
000110                                                                          
000111******************************************************************        
000112     EJECT                                                                
000113*      --- VALID IDDC CODES                                               
000114*                                                                         
000115*01    -COPY WWDC99                                                       
000116                                                                          
000121     EJECT                                                                
000122 01  TEST-IDDISTR                PIC 9(5)   VALUE ZERO COMP-3.            
000123*01  FILLER  -COPY WWDIST79      -RED TEST-IDDISTR.                       
000124*                                                                         
000125     EJECT                                                                
000126 01  BILD-HOPP-AREOR.                                                     
000127                                                                          
000128   03  FILLER            PIC X(16)   VALUE 'P-TO-P-AREA'.                 
000129 01  P-TO-P-T49.                                                          
000130*----TILL W40749                                                          
000131     03  P-TO-P-LL              PIC S9(4)   COMP SYNC.                    
000132*    03  P-TO-P-LL              PIC S9(4)   VALUE +117 COMP SYNC.         
000133     03  P-TO-P-Z1              PIC X(1)    VALUE LOW-VALUE.              
000134     03  P-TO-P-Z2              PIC X(1)    VALUE LOW-VALUE.              
000135     03  P-TO-P-KDTRANS         PIC X(7)    VALUE 'W4T749X'.              
000136     03  FILLER                 PIC X(1)    VALUE SPACE.                  
000137     03  P-TO-P-IDTRANS         PIC X(4)    VALUE '4745'.                 
000138     03  P-TO-P-KDMFSFOR        PIC X(1)    VALUE SPACE.                  
000139*    03  MID -COPY W4I74901 -PRE 4749-                                    
000140     EJECT                                                                
000141                                                                          
000142   03 FILLER             PIC X(16)   VALUE 'P-TO-P-AREA2'.                
000143 01  P-TO-P-T99.                                                          
000144*----TILL W40799                                                          
000145     03  P-TO-P2-LL             PIC S9(4)   COMP SYNC.                    
000146*    03  P-TO-P2-LL             PIC S9(4)   VALUE +117 COMP SYNC.         
000147     03  P-TO-P2-Z1             PIC X(1)    VALUE LOW-VALUE.              
000148     03  P-TO-P2-Z2             PIC X(1)    VALUE LOW-VALUE.              
000149     03  P-TO-P2-KDTRANS        PIC X(7)    VALUE 'W4T799X'.              
000150     03  FILLER                 PIC X(1)    VALUE SPACE.                  
000151     03  P-TO-P2-IDTRANS        PIC X(4)    VALUE '4745'.                 
000152     03  P-TO-P2-KDMFSFOR       PIC X(1)    VALUE SPACE.                  
000153*    03  MID -COPY W4I79901 -PRE 4799-                                    
000154     EJECT                                                                
000155                                                                          
000156*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
000157 01  GENERELLA-SUBPROGRAM.                                                
000158     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
000159     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
000160     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000161     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000162     03  W006PRT                 PIC X(8)    VALUE 'W006PRT'.             
000163     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
000164     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
000165     EJECT                                                                
000166*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
000167*01 -COPY WMEDAREA                                                        
000168     SKIP3                                                                
000169*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
000170*                                                                         
000171 01  FILLER                      PIC X(16)   VALUE 'WDATAREA'.            
000172     SKIP3                                                                
000173*01 -COPY WDATAREA                                                        
000174     EJECT                                                                
000175 01  MESSAGE-CODES.                                                       
000176     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
000177     03  INF-PRESS-PF4           PIC X(3)    VALUE '081'.                 
000178     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
000179     03  INF-PRINT-BEG           PIC X(3)    VALUE '118'.                 
000180     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
000181     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
000182     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
000183     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
000184     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
000185     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
000186     03  ERR-KEY-MISSING         PIC X(3)    VALUE '005'.                 
000187     03  ERR-FLERA-FUNKTIONER    PIC X(3)    VALUE '097'.                 
000188     03  ERR-ORDERN-EJ-KLAR      PIC X(3)    VALUE '791'.                 
000189     03  ERR-INGET-PRINTAT       PIC X(3)    VALUE '167'.                 
000190     03  ERR-KUND-SAKNAS         PIC X(3)    VALUE '063'.                 
000191     03  ERR-PART-MISS-ON-FILE   PIC X(3)    VALUE '769'.                 
000192     03  ERR-RAD-FINNS-REDAN     PIC X(3)    VALUE '245'.                 
000193     03  ERR-USER-NOT-AUTHORIZED PIC X(3)    VALUE '405'.                 
000194     03  ERR-UPPGIFTER-SAKNAS    PIC X(3)    VALUE '760'.                 
000195     03  ERR-KOLLI-SAKNAS        PIC X(3)    VALUE '758'.                 
000196     03  ERR-VIKT-SAKNAS         PIC X(3)    VALUE '792'.                 
000197     03  ERR-FAKTURA-KLAR        PIC X(3)    VALUE '281'.                 
000198     03  ERR-FEL-DISTRIKT        PIC X(3)    VALUE '747'.                 
000199     EJECT                                                                
000200*01  -COPY W006PRT                                                        
000201     EJECT                                                                
000202*01  -COPY WDECAREA                                                       
000203     EJECT                                                                
000204*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
000205*                                                                         
000206 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
000207     SKIP3                                                                
000208*01 -COPY WMSGINIT                                                        
000209     EJECT                                                                
000210*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
000211*                                                                         
000212 01  SPAR-AREA.                                                           
000213     03  SPAR-IDTRANS             PIC X(4)    VALUE '4745'.               
000214     03  SPAR-IDKUNDNR-ENTER      PIC S9(7) VALUE ZERO COMP-3.            
000215     03  SPAR-IDKUNDNR-NEXT       PIC S9(7) VALUE ZERO COMP-3.            
000216     03  SPAR-IDRAPP-ENTER        PIC X(10) VALUE SPACE.                  
000217     03  SPAR-IDRAPP-NEXT         PIC X(10) VALUE SPACE.                  
000218     03  SPAR-IDKOLLI-ENTER       PIC S9(5) VALUE ZERO COMP-3.            
000219     03  SPAR-IDKOLLI-NEXT        PIC S9(5) VALUE ZERO COMP-3.            
000220     03  SPAR-IDARTNR-ENTER       PIC S9(9) VALUE ZERO COMP-3.            
000221     03  SPAR-IDARTNR-NEXT        PIC S9(9) VALUE ZERO COMP-3.            
000222     03  SPAR-MID-IDKOLLI         PIC X(5)  VALUE SPACE.                  
000223     03  SPAR-IDAVS               PIC X(20) VALUE SPACE.                  
000224     03  SPAR-DARFSDAT            PIC 9(8)  VALUE ZERO.                   
000225     03  SPAR-KDKOLLI             PIC X(8)  VALUE SPACE.                  
000226     03  SPAR-VKORDBTO-KOLLI    PIC S9(6)V9(1) VALUE ZERO COMP-3.         
000227     03  SPAR-INPUTRADER OCCURS 10.                                       
000228       05  SPAR-IDKOLLI           PIC S9(5) VALUE ZERO  COMP-3.           
000229       05  SPAR-IDARTNR           PIC S9(9) VALUE ZERO  COMP-3.           
000230       05  SPAR-KVANTAL           PIC S9(7) VALUE ZERO  COMP-3.           
000231       05  SPAR-PRARTBTO        PIC S9(7)V9(2) VALUE ZERO  COMP-3.        
000232       05  SPAR-KDFEL             PIC X(3)  VALUE SPACE.                  
000233       05  SPAR-TENOTE            PIC X(40) VALUE SPACE.                  
000234     EJECT                                                                
000235*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
000236*                                                                         
000237 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
000238     SKIP3                                                                
000239*01  MID -COPY W4I74501                                                   
000240     EJECT                                                                
000241 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
000242     SKIP3                                                                
000243*01  -COPY WMSGAREA                                                       
000244     EJECT                                                                
000245     03  MOD REDEFINES MSG-AREA.                                          
000246*      05  -COPY W4O74501                                                 
000247     EJECT                                                                
000248 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
000249     SKIP3                                                                
000250*01  -COPY WMFSAREA                                                       
000251     EJECT                                                                
000252*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
000253*                                                                         
000254 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000255     SKIP3                                                                
000256 01  NYCKLAR-TILL-DLI.                                                    
000257                                                                          
000258     03  W-WDGX4101-X.                                                    
000259         05 W-4101-IDHTYP          PIC X(4)    VALUE '4101'.              
000260         05 W-4101-IDDC            PIC X(2)    VALUE SPACE.               
000261         05 W-4101-IDDISTR         PIC S9(5)   VALUE ZERO COMP-3.         
000262         05 W-4101-LOWVALUE        PIC X(21)   VALUE LOW-VALUE.           
000263                                                                          
000264     03  W-WDGX4102-X.                                                    
000265         05 W-4102-IDKUNDNR        PIC S9(7)   VALUE ZERO COMP-3.         
000266         05 W-4102-IDRAPP          PIC X(10)   VALUE SPACE.               
000267         05 W-4102-IDKOLLI         PIC S9(5)   VALUE ZERO COMP-3.         
000268         05 W-4102-IDARTNR         PIC S9(9)   VALUE ZERO COMP-3.         
000269                                                                          
000270     03  W-WDGX4102-MIN-X.                                                
000271         05 W-4102-IDKUNDNR-MIN    PIC S9(7)   VALUE ZERO COMP-3.         
000272         05 W-4102-IDRAPP-MIN      PIC X(10)   VALUE SPACE.               
000273         05 W-4102-IDKOLLI-MIN     PIC S9(5)   VALUE ZERO COMP-3.         
000274         05 W-4102-IDARTNR-MIN     PIC S9(9)   VALUE ZERO COMP-3.         
000275                                                                          
000276     03  W-WDGX4102-MAX-X.                                                
000277         05 W-4102-IDKUNDNR-MAX    PIC S9(7)   VALUE ZERO COMP-3.         
000278         05 W-4102-IDRAPP-MAX      PIC X(10)   VALUE SPACE.               
000279         05 W-4102-IDKOLLI-MAX     PIC S9(5) VALUE +99999 COMP-3.         
000280         05 W-4102-IDARTNR-MAX  PIC S9(9) VALUE +999999999 COMP-3.        
000281                                                                          
000282     03  W-IDGMT-X.                                                       
000283         05 W-IDDISTR-WDB2         PIC S9(5)   VALUE ZERO COMP-3.         
000284         05 W-IDKUNDNR-WDB2        PIC S9(7)   VALUE ZERO COMP-3.         
000285* TILL WDB101                                                             
000286     03  W-WDB101KY-X.                                                    
000287       05  W-WDB1-IDPARTNR       PIC X(9)  VALUE SPACE.                   
000288       05  W-WDB1-IDFTG          PIC 9(2)  VALUE ZERO.                    
000289                                                                          
000290                                                                          
000291     03  W-IDARTNR-X.                                                     
000292         05 W-IDARTNR              PIC S9(9)   VALUE ZERO COMP-3.         
000293     SKIP2                                                                
000294*    --- STATUS-KOD FRÅN IMS                                              
000295 01  STATUS-WS                   PIC XX.                                  
000296     88  SEGMENT-FINNS                       VALUE '  '.                  
000297     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
000298     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
000299     88  TRANSKOD-FEL                        VALUE 'A1'.                  
000300     88  SECURITY-FEL                        VALUE 'A4'.                  
000301     SKIP2                                                                
000302 01  GODK-STATUSKODER.                                                    
000303     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000304     SKIP3                                                                
000305 01  SSA1                        PIC X(96).                               
000306 01  SSA2                        PIC X(96).                               
000307     EJECT                                                                
000308*    --- IMS FUNKTIONSKODER                                               
000309*01  -COPY W0003                                                          
000310     EJECT                                                                
000311*    ---  DLI INPUT-OUTPUT AREA                                           
000312                                                                          
000313 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4101'.                    
000314 01  DLI-IO-WDGX4101.                                                     
000315*    03  -COPY WDGX4101                                                   
000316     EJECT                                                                
000317 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4102'.                    
000318 01  DLI-IO-WDGX4102.                                                     
000319*    03  -COPY WDGX4102                                                   
000320 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
000321 01  DLI-IO-WDB201.                                                       
000322*    03  -COPY WDB201                                                     
000323 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB101'.                      
000324 01  DLI-IO-WDB101.                                                       
000325*    03  -COPY WDB101                                                     
000326 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
000327 01  DLI-IO-WDK601.                                                       
000328*    03  -COPY WDK601                                                     
000329     EJECT                                                                
000330******************************************************************        
000331* TABELL FÖR ATT HÄMTA TEXT TILL MOTSVARANDE TEXTKOD                      
000332******************************************************************        
000333 01  TABELL.                                                              
000334     03  FILLER    PIC X(44)    VALUE                                     
000335                   '001                                         '.        
000336     03  FILLER    PIC X(44)    VALUE                                     
000337                   '002 Repig                                   '.        
000338     03  FILLER    PIC X(44)    VALUE                                     
000339                   '003 Använd                                  '.        
000340     03  FILLER    PIC X(44)    VALUE                                     
000341                   '004 Lackad                                  '.        
000342     03  FILLER    PIC X(44)    VALUE                                     
000343                   '005 Skadad p.g.a. dålig packning            '.        
000344     03  FILLER    PIC X(44)    VALUE                                     
000345                   '006 Rostig                                  '.        
000346     03  FILLER    PIC X(44)    VALUE                                     
000347                   '007 Smutsig                                 '.        
000348     03  FILLER    PIC X(44)    VALUE                                     
000349                   '008 Ej komplett                             '.        
000350     03  FILLER    PIC X(44)    VALUE                                     
000351                   '009 Bruten förpackning                      '.        
000352     03  FILLER    PIC X(44)    VALUE                                     
000353                   '010 Utgången artikel med nytt artikelnummer '.        
000354     03  FILLER    PIC X(44)    VALUE                                     
000355                   '011 Bucklig                                 '.        
000356     03  FILLER    PIC X(44)    VALUE                                     
000357                   '012 Fel artikel returnerad                  '.        
000358     03  FILLER    PIC X(44)    VALUE                                     
000359                   '013 Returtillstånd saknas                   '.        
000360     03  FILLER    PIC X(44)    VALUE                                     
000361                   '014 Returtillstånd annullerat               '.        
000362     03  FILLER    PIC X(44)    VALUE                                     
000363                   '015 För många i retur                       '.        
000364     03  FILLER    PIC X(44)    VALUE                                     
000365                   '016 Obrukbar förpackning orsakad av tejp    '.        
000366     03  FILLER    PIC X(44)    VALUE                                     
000367                   '017 Obrukbar förpack. orsakad av märkpenna  '.        
000368     03  FILLER    PIC X(44)    VALUE                                     
000369                   '018 Obrukbar säljförpackning                '.        
000370     03  FILLER    PIC X(44)    VALUE                                     
000371                   '019 Säljförpackning saknas                  '.        
000372     03  FILLER    PIC X(44)    VALUE                                     
000373                   '020 Bruten plombering                       '.        
000374     03  FILLER    PIC X(44)    VALUE                                     
000375                   '021 Transportskruvar borttagna              '.        
000376     03  FILLER    PIC X(44)    VALUE                                     
000377                   '022 Okänd Artikel                           '.        
000378     03  FILLER    PIC X(44)    VALUE                                     
000379                   '023 Fingeravtryck                           '.        
000380     03  FILLER    PIC X(44)    VALUE                                     
000381                   '024 Böjd                                    '.        
000382     03  FILLER    PIC X(44)    VALUE                                     
000383                   '025 Spräckt                                 '.        
000384     03  FILLER    PIC X(44)    VALUE                                     
000385                   '026 Destruerat/Skrotat_Krut/Batteri         '.        
000386     03  FILLER    PIC X(44)    VALUE                                     
000387                   '027 Destruerat/Skrotat_Farligt Gods         '.        
000388     03  FILLER    PIC X(44)    VALUE                                     
000389                   '028 Utgången Datum                          '.        
000390     03  FILLER    PIC X(44)    VALUE                                     
000391                   '029 LDC retur                               '.        
000392     03  FILLER    PIC X(44)    VALUE                                     
000393                   '030                                         '.        
000394     03  FILLER    PIC X(44)    VALUE                                     
000395                   '031                                         '.        
000396     03  FILLER    PIC X(44)    VALUE                                     
000397                   '032                                         '.        
000398     03  FILLER    PIC X(44)    VALUE                                     
000399                   '033                                         '.        
000400     03  FILLER    PIC X(44)    VALUE                                     
000401                   '034                                         '.        
000402     03  FILLER    PIC X(44)    VALUE                                     
000403                   '035                                         '.        
000404     03  FILLER    PIC X(44)    VALUE                                     
000405                   '036                                         '.        
000406     03  FILLER    PIC X(44)    VALUE                                     
000407                   '037                                         '.        
000408     03  FILLER    PIC X(44)    VALUE                                     
000409                   '038                                         '.        
000410     03  FILLER    PIC X(44)    VALUE                                     
000411                   '039                                         '.        
000412     03  FILLER    PIC X(44)    VALUE                                     
000413                   '040                                         '.        
000414     03  FILLER    PIC X(44)    VALUE                                     
000415                   '041                                         '.        
000416     03  FILLER    PIC X(44)    VALUE                                     
000417                   '042                                         '.        
000418     03  FILLER    PIC X(44)    VALUE                                     
000419                   '043                                         '.        
000420     03  FILLER    PIC X(44)    VALUE                                     
000421                   '044                                         '.        
000422     03  FILLER    PIC X(44)    VALUE                                     
000423                   '045                                         '.        
000424     03  FILLER    PIC X(44)    VALUE                                     
000425                   '046                                         '.        
000426     03  FILLER    PIC X(44)    VALUE                                     
000427                   '047                                         '.        
000428     03  FILLER    PIC X(44)    VALUE                                     
000429                   '048                                         '.        
000430     03  FILLER    PIC X(44)    VALUE                                     
000431                   '049                                         '.        
000432     03  FILLER    PIC X(44)    VALUE                                     
000433                   '050                                         '.        
000434     03  FILLER    PIC X(44)    VALUE                                     
000435                   '051                                         '.        
000436     03  FILLER    PIC X(44)    VALUE                                     
000437                   '052                                         '.        
000438     03  FILLER    PIC X(44)    VALUE                                     
000439                   '053                                         '.        
000440     03  FILLER    PIC X(44)    VALUE                                     
000441                   '054                                         '.        
000442     03  FILLER    PIC X(44)    VALUE                                     
000443                   '055                                         '.        
000444     03  FILLER    PIC X(44)    VALUE                                     
000445                   '056                                         '.        
000446     03  FILLER    PIC X(44)    VALUE                                     
000447                   '057                                         '.        
000448     03  FILLER    PIC X(44)    VALUE                                     
000449                   '058                                         '.        
000450     03  FILLER    PIC X(44)    VALUE                                     
000451                   '059                                         '.        
000452     03  FILLER    PIC X(44)    VALUE                                     
000453                   '060                                         '.        
000454     03  FILLER    PIC X(44)    VALUE                                     
000455                   '061                                         '.        
000456     03  FILLER    PIC X(44)    VALUE                                     
000457                   '062                                         '.        
000458     03  FILLER    PIC X(44)    VALUE                                     
000459                   '063                                         '.        
000460     03  FILLER    PIC X(44)    VALUE                                     
000461                   '064                                         '.        
000462     03  FILLER    PIC X(44)    VALUE                                     
000463                   '065                                         '.        
000464     03  FILLER    PIC X(44)    VALUE                                     
000465                   '066                                         '.        
000466     03  FILLER    PIC X(44)    VALUE                                     
000467                   '067                                         '.        
000468     03  FILLER    PIC X(44)    VALUE                                     
000469                   '068                                         '.        
000470     03  FILLER    PIC X(44)    VALUE                                     
000471                   '069                                         '.        
000472     03  FILLER    PIC X(44)    VALUE                                     
000473                   '070                                         '.        
000474     03  FILLER    PIC X(44)    VALUE                                     
000475                   '071                                         '.        
000476     03  FILLER    PIC X(44)    VALUE                                     
000477                   '072                                         '.        
000478     03  FILLER    PIC X(44)    VALUE                                     
000479                   '073                                         '.        
000480     03  FILLER    PIC X(44)    VALUE                                     
000481                   '074                                         '.        
000482     03  FILLER    PIC X(44)    VALUE                                     
000483                   '075                                         '.        
000484     03  FILLER    PIC X(44)    VALUE                                     
000485                   '076                                         '.        
000486     03  FILLER    PIC X(44)    VALUE                                     
000487                   '077                                         '.        
000488     03  FILLER    PIC X(44)    VALUE                                     
000489                   '078                                         '.        
000490     03  FILLER    PIC X(44)    VALUE                                     
000491                   '079                                         '.        
000492     03  FILLER    PIC X(44)    VALUE                                     
000493                   '080                                         '.        
000494     03  FILLER    PIC X(44)    VALUE                                     
000495                   '081                                         '.        
000496     03  FILLER    PIC X(44)    VALUE                                     
000497                   '082                                         '.        
000498     03  FILLER    PIC X(44)    VALUE                                     
000499                   '083                                         '.        
000500     03  FILLER    PIC X(44)    VALUE                                     
000501                   '084                                         '.        
000502     03  FILLER    PIC X(44)    VALUE                                     
000503                   '085                                         '.        
000504     03  FILLER    PIC X(44)    VALUE                                     
000505                   '086                                         '.        
000506     03  FILLER    PIC X(44)    VALUE                                     
000507                   '087                                         '.        
000508     03  FILLER    PIC X(44)    VALUE                                     
000509                   '088                                         '.        
000510     03  FILLER    PIC X(44)    VALUE                                     
000511                   '089                                         '.        
000512     03  FILLER    PIC X(44)    VALUE                                     
000513                   '090                                         '.        
000514     03  FILLER    PIC X(44)    VALUE                                     
000515                   '091                                         '.        
000516     03  FILLER    PIC X(44)    VALUE                                     
000517                   '092                                         '.        
000518     03  FILLER    PIC X(44)    VALUE                                     
000519                   '093                                         '.        
000520     03  FILLER    PIC X(44)    VALUE                                     
000521                   '094                                         '.        
000522     03  FILLER    PIC X(44)    VALUE                                     
000523                   '095                                         '.        
000524     03  FILLER    PIC X(44)    VALUE                                     
000525                   '096                                         '.        
000526     03  FILLER    PIC X(44)    VALUE                                     
000527                   '097                                         '.        
000528     03  FILLER    PIC X(44)    VALUE                                     
000529                   '098                                         '.        
000530     03  FILLER    PIC X(44)    VALUE                                     
000531                   '099                                         '.        
000532     03  FILLER    PIC X(44)    VALUE                                     
000533                   '100                                         '.        
000534     03  FILLER    PIC X(44)    VALUE                                     
000535                   '101                                         '.        
000536     03  FILLER    PIC X(44)    VALUE                                     
000537                   '102 Scratched                               '.        
000538     03  FILLER    PIC X(44)    VALUE                                     
000539                   '103 Used                                    '.        
000540     03  FILLER    PIC X(44)    VALUE                                     
000541                   '104 Painted                                 '.        
000542     03  FILLER    PIC X(44)    VALUE                                     
000543                   '105 Damage due to bad packing               '.        
000544     03  FILLER    PIC X(44)    VALUE                                     
000545                   '106 Rusty                                   '.        
000546     03  FILLER    PIC X(44)    VALUE                                     
000547                   '107 Dirty                                   '.        
000548     03  FILLER    PIC X(44)    VALUE                                     
000549                   '108 Incomplete                              '.        
000550     03  FILLER    PIC X(44)    VALUE                                     
000551                   '109 Broken packing                          '.        
000552     03  FILLER    PIC X(44)    VALUE                                     
000553                   '110 Old design with new part number         '.        
000554     03  FILLER    PIC X(44)    VALUE                                     
000555                   '111 Dented                                  '.        
000556     03  FILLER    PIC X(44)    VALUE                                     
000557                   '112 Wrong part returned                     '.        
000558     03  FILLER    PIC X(44)    VALUE                                     
000559                   '113 No return permission                    '.        
000560     03  FILLER    PIC X(44)    VALUE                                     
000561                   '114 Return permission cancelled             '.        
000562     03  FILLER    PIC X(44)    VALUE                                     
000563                   '115 To many in return                       '.        
000564     03  FILLER    PIC X(44)    VALUE                                     
000565                   '116 Box destroyed by tape                   '.        
000566     03  FILLER    PIC X(44)    VALUE                                     
000567                   '117 Box destroyed by marking pen            '.        
000568     03  FILLER    PIC X(44)    VALUE                                     
000569                   '118 Sales packing destroyed                 '.        
000570     03  FILLER    PIC X(44)    VALUE                                     
000571                   '119 Sales packing missing                   '.        
000572     03  FILLER    PIC X(44)    VALUE                                     
000573                   '120 Broken seal                             '.        
000574     03  FILLER    PIC X(44)    VALUE                                     
000575                   '121 Safety screws removed                   '.        
000576     03  FILLER    PIC X(44)    VALUE                                     
000577                   '122 Unknown Part                            '.        
000578     03  FILLER    PIC X(44)    VALUE                                     
000579                   '123 Fingerprints                            '.        
000580     03  FILLER    PIC X(44)    VALUE                                     
000581                   '124 Bent                                    '.        
000582     03  FILLER    PIC X(44)    VALUE                                     
000583                   '125 Cracked                                 '.        
000584     03  FILLER    PIC X(44)    VALUE                                     
000585                   '126 Destroyed/Scrapped_Explosive/Batteries  '.        
000586     03  FILLER    PIC X(44)    VALUE                                     
000587                   '127 Destryed/Scrapped_Dangerous Goods       '.        
000588     03  FILLER    PIC X(44)    VALUE                                     
000589                   '128 Date Expired                            '.        
000590     03  FILLER    PIC X(44)    VALUE                                     
000591                   '129 LDC return                              '.        
000592     03  FILLER    PIC X(44)    VALUE                                     
000593                   '130                                         '.        
000594     SKIP2                                                                
000595 01  FILLER REDEFINES TABELL.                                             
000596     03  KDFEL-TABELL  OCCURS 130.                                        
000597         05  TAB-KDFEL           PIC X(3).                                
000598         05  FILLER              PIC X(1).                                
000599         05  TAB-TENOTE          PIC X(40).                               
000600                                                                          
000601******************************************************************        
000602     EJECT                                                                
000603 LINKAGE SECTION.                                                         
000604*01  -COPY W0009   -PRE MSG-                                              
000605*01  -COPY W0009   -PRE W4749-                                            
000606*01  -COPY W0009   -PRE W4799-                                            
000607*01  -COPY W0008   -PRE USEA-                                             
000608     05  FILLER                  PIC X.                                   
000609                                                                          
000610*01  -COPY W0008  -PRE 4101-                                              
000611     05  FILLER                  PIC X.                                   
000612                                                                          
000613*01  -COPY W0008  -PRE WDB2-                                              
000614     05  FILLER                  PIC X.                                   
000615                                                                          
000616*01  -COPY W0008  -PRE WDB1-                                              
000617     05  FILLER                  PIC X.                                   
000618                                                                          
000619*01  -COPY W0008  -PRE WDK6-                                              
000620     05  FILLER                 PIC X.                                    
000621     EJECT                                                                
000622 PROCEDURE DIVISION  USING MSG-PCB W4749-PCB W4799-PCB USEA-PCB           
000623     4101-PCB WDB2-PCB WDB1-PCB WDK6-PCB.                                 
000624 MAIN SECTION.                                                            
000625     ENTRY 'DLITCBL' USING MSG-PCB W4749-PCB W4799-PCB USEA-PCB           
000626     4101-PCB WDB2-PCB WDB1-PCB WDK6-PCB.                                 
000627                                                                          
000628                                                                          
000629     PERFORM IMS-GET-MSG                                                  
000630     IF SEGMENT-FINNS                                                     
000631       PERFORM A-INIT                                                     
000632       PERFORM B-KOLLA-NYCKLAR                                            
000633       IF NYCKLAR-OK                                                      
000634         IF MFS-UPDATE OR MFS-PRINT                                       
000635           PERFORM G-KOLLA-INPUT                                          
000636           IF INDATA-OK                                                   
000637             PERFORM H-UPPDATERA                                          
000638           ELSE                                                           
000639             MOVE NEJ TO VISA-INFO-SW                                     
000640           END-IF                                                         
000641         ELSE                                                             
000642           IF MFS-FIRST                                                   
000643             PERFORM C-FOERSTA-SIDA                                       
000644           ELSE                                                           
000645             IF MFS-NEXT                                                  
000646               PERFORM D-NAESTA-SIDA                                      
000647             ELSE                                                         
000648               PERFORM E-SAMMA-SIDA                                       
000649             END-IF                                                       
000650           END-IF                                                         
000651         END-IF                                                           
000652         IF MFS-UPDATE OR MFS-PRINT                                       
000653           CONTINUE                                                       
000654         ELSE                                                             
000655           IF VISA-INFO-OK                                                
000656             PERFORM F-LAES-VISA-INFO                                     
000657           END-IF                                                         
000658         END-IF                                                           
000659       END-IF                                                             
000660       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O74501 + 4                      
000661       PERFORM IMS-INSERT-MSG                                             
000662     END-IF                                                               
000663                                                                          
000664     MOVE ZERO TO RETURN-CODE                                             
000665     GOBACK                                                               
000666     .                                                                    
000667     EJECT                                                                
000668 A-INIT SECTION.                                                          
000669                                                                          
000670     IF MSG-DUBBLA-TRANSKODER                                             
000671       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I74501                 
000672       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
000673       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
000674     ELSE                                                                 
000675       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I74501                  
000676       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
000677       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
000678     END-IF                                                               
000679                                                                          
000680     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
000681     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
000682     MOVE MFS-IDTRANS TO W-IDTRANS                                        
000683                                                                          
000684     MOVE LOW-VALUE TO MSG-AREA                                           
000685     MOVE 'W4O745N1' TO MFS-IDMOD                                         
000686     MOVE '4745' TO MOD-IDTRANS                                           
000687     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
000688                                                                          
000689     MOVE SPACE                       TO MED-IDMFSINF                     
000690     MOVE SPACE                       TO MED-IDMFSFEL                     
000691                                                                          
000692     IF EGEN-MID OR HELP-MID                                              
000693       CONTINUE                                                           
000694     ELSE                                                                 
000695       MOVE SPACE TO MFS-KDTRTYP                                          
000696       MOVE '7' TO MFS-IDPFK                                              
000697     END-IF                                                               
000698                                                                          
000699     .                                                                    
000700     EJECT                                                                
000701 B-KOLLA-NYCKLAR SECTION.                                                 
000702                                                                          
000703     MOVE ALL '+'           TO MSGI-WMSGINIT                              
000704     MOVE '001'             TO MSGI-KDCALL                                
000705     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
000706     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
000707     MOVE '4745'            TO MSGI-IDTRANS                               
000708     IF EGEN-MID                                                          
000709       MOVE MID-IDDISTR-IN      TO MSGI-IDDISTR                           
000710       MOVE MID-IDKUNDNR-IN     TO MSGI-IDKUNDNR                          
000711       MOVE MID-IDRAPP-IN       TO MSGI-IDRAPP                            
000712       MOVE MID-IDKOLLI-IN      TO MSGI-IDKOLLI                           
000713                                   WS-IDKOLLI                             
000714       MOVE MID-IDDC-IN         TO MSGI-IDDC                              
000715     END-IF                                                               
000716     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
000717     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
000718                                                                          
000719     MOVE 'GB'              TO MED-IDSKYLT                                
000720     MOVE '2'               TO MFS-KDMFSFOR                               
000721                                                                          
000722     MOVE JA TO NYCKLAR-SW                                                
000723                                                                          
000724                                                                          
000725*    -- KONTROLL AV IDDISTR                                               
000726     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
000727                                                                          
000728     IF MID-IDDISTR-IN NOT = ALL '+'                                      
000729       MOVE '7'         TO MFS-IDPFK                                      
000730       MOVE SPACE       TO MFS-KDTRTYP                                    
000731     END-IF                                                               
000732     INSPECT MSGI-IDDISTR REPLACING LEADING SPACE BY ZERO                 
000733     IF MSGI-IDDISTR NUMERIC                                              
000734       MOVE MSGI-IDDISTR TO W-4101-IDDISTR                                
000735                            W-IDDISTR-WDB2                                
000736                            TEST-IDDISTR                                  
000737     ELSE                                                                 
000738       MOVE NEJ TO NYCKLAR-SW                                             
000739     END-IF                                                               
000740                                                                          
000741*    -- KONTROLL AV IDKUNDNR                                              
000742     MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR-IN                              
000743                                                                          
000744     IF MID-IDKUNDNR-IN NOT = ALL '+'                                     
000745       MOVE '7'         TO MFS-IDPFK                                      
000746       MOVE SPACE       TO MFS-KDTRTYP                                    
000747     END-IF                                                               
000748     INSPECT MSGI-IDKUNDNR REPLACING LEADING SPACE BY ZERO                
000749     IF MSGI-IDKUNDNR NUMERIC                                             
000750       MOVE MSGI-IDKUNDNR TO W-4102-IDKUNDNR                              
000751                             W-4102-IDKUNDNR-MIN                          
000752                             W-4102-IDKUNDNR-MAX                          
000753                             W-IDKUNDNR-WDB2                              
000754     ELSE                                                                 
000755       MOVE NEJ TO NYCKLAR-SW                                             
000756     END-IF                                                               
000757                                                                          
000758*    -- KONTROLL AV IDRAPP                                                
000759     MOVE MFS-RENSA-FAELT TO MOD-IDRAPP-IN                                
000760                                                                          
000761     IF MID-IDRAPP-IN NOT = ALL '+'                                       
000762       MOVE '7'         TO MFS-IDPFK                                      
000763       MOVE SPACE       TO MFS-KDTRTYP                                    
000764     END-IF                                                               
000765     MOVE MSGI-IDRAPP TO W-4102-IDRAPP                                    
000766                         W-4102-IDRAPP-MIN                                
000767                         W-4102-IDRAPP-MAX                                
000768                                                                          
000769*    -- KONTROLL AV IDKOLLI                                               
000770     MOVE MFS-RENSA-FAELT TO MOD-IDKOLLI-IN                               
000771                                                                          
000772     IF MID-IDKOLLI-IN NOT = ALL '+'                                      
000773       MOVE '7'         TO MFS-IDPFK                                      
000774       MOVE SPACE       TO MFS-KDTRTYP                                    
000775       INSPECT MSGI-IDKOLLI REPLACING LEADING SPACE BY ZERO               
000776       IF MSGI-IDKOLLI NUMERIC                                            
000777         MOVE MSGI-IDKOLLI TO W-4102-IDKOLLI                              
000778                              W-4102-IDKOLLI-MIN                          
000779                              W-4102-IDKOLLI-MAX                          
000780       ELSE                                                               
000781         MOVE NEJ TO NYCKLAR-SW                                           
000782       END-IF                                                             
000783     END-IF                                                               
000784                                                                          
000785*    -- KONTROLL AV IDDC                                                  
000786     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
000787                                                                          
000788     IF MID-IDDC-IN NOT = ALL '+'                                         
000789       MOVE '7'           TO MFS-IDPFK                                    
000790       MOVE SPACE         TO MFS-KDTRTYP                                  
000791       MOVE MID-IDDC-IN   TO MSGI-IDDC                                    
000792     END-IF                                                               
000793     INSPECT MSGI-IDDC REPLACING LEADING SPACE BY ZERO                    
000794     MOVE MSGI-IDDC       TO W-4101-IDDC                                  
000795                                                                          
000801     IF GODK-MID OR NYCKLAR-OK                                            
000802       MOVE MSGI-IDDISTR        TO MOD-IDDISTR-UT                         
000803       INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE             
000804       MOVE MSGI-IDKUNDNR       TO MOD-IDKUNDNR-UT                        
000805       INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE            
000806       MOVE MSGI-IDRAPP         TO MOD-IDRAPP-UT                          
000807                                                                          
000808       IF MFS-FIRST                                                       
000809         IF  MID-IDKOLLI-IN = ALL '+'                                     
000810           MOVE SPACE             TO MOD-IDKOLLI-UT                       
000811         ELSE                                                             
000812           MOVE MSGI-IDKOLLI        TO MOD-IDKOLLI-UT                     
000813           INSPECT MOD-IDKOLLI-UT REPLACING LEADING ZERO BY SPACE         
000814         END-IF                                                           
000815       ELSE                                                               
000816         IF SPAR-MID-IDKOLLI = ALL '+'                                    
000817           MOVE SPACE             TO MOD-IDKOLLI-UT                       
000818         ELSE                                                             
000819           MOVE MSGI-IDKOLLI        TO MOD-IDKOLLI-UT                     
000820           INSPECT MOD-IDKOLLI-UT REPLACING LEADING ZERO BY SPACE         
000821         END-IF                                                           
000822       END-IF                                                             
000823                                                                          
000824       MOVE MSGI-IDDC           TO MOD-IDDC-UT                            
000825     ELSE                                                                 
000826       MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-UT                             
000827                               MOD-IDKUNDNR-UT                            
000828                               MOD-IDRAPP-UT                              
000829                               MOD-IDKOLLI-UT                             
000830                               MOD-IDDC-UT                                
000831     END-IF                                                               
000832                                                                          
000833     IF NYCKLAR-FEL                                                       
000834       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
000835       CALL WMEDKONV USING MED-WMEDAREA                                   
000836       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
000837       PERFORM MFS-RENSA-FAELT-IN                                         
000838       PERFORM MFS-RENSA-FAELT-UT                                         
000856     END-IF                                                               
000857                                                                          
000860     .                                                                    
000861     EJECT                                                                
000862 C-FOERSTA-SIDA SECTION.                                                  
000863                                                                          
000864     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
000865     CALL WMEDKONV USING MED-WMEDAREA                                     
000866     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
000867                                                                          
000868     PERFORM MFS-RENSA-FAELT-IN                                           
000869     .                                                                    
000870     EJECT                                                                
000871 D-NAESTA-SIDA SECTION.                                                   
000872                                                                          
000873     IF SPAR-IDTRANS = '4745'                                             
000874       MOVE SPAR-IDKUNDNR-NEXT TO W-4102-IDKUNDNR-MIN                     
000875                                  W-4102-IDKUNDNR-MAX                     
000876                                  W-4102-IDKUNDNR                         
000877       MOVE SPAR-IDRAPP-NEXT   TO W-4102-IDRAPP-MIN                       
000878                                  W-4102-IDRAPP-MAX                       
000879                                  W-4102-IDRAPP                           
000880       MOVE SPAR-IDKOLLI-NEXT  TO W-4102-IDKOLLI-MIN                      
000881                                  W-4102-IDKOLLI                          
000882       MOVE SPAR-IDARTNR-NEXT  TO W-4102-IDARTNR-MIN                      
000883                                  W-4102-IDARTNR                          
000884       MOVE SPAR-MID-IDKOLLI   TO WS-IDKOLLI                              
000885                                                                          
000886       PERFORM MFS-RENSA-FAELT-IN                                         
000887     ELSE                                                                 
000888       PERFORM MFS-RENSA-FAELT-IN                                         
000889     END-IF                                                               
000890     .                                                                    
000891     EJECT                                                                
000892 E-SAMMA-SIDA SECTION.                                                    
000893                                                                          
000894     IF EGEN-MID OR HELP-MID                                              
000895       IF SPAR-IDTRANS = '4745' OR '0551'                                 
000896         MOVE SPAR-IDKUNDNR-ENTER TO W-4102-IDKUNDNR-MIN                  
000897                                     W-4102-IDKUNDNR-MAX                  
000898                                     W-4102-IDKUNDNR                      
000899         MOVE SPAR-IDRAPP-ENTER   TO W-4102-IDRAPP-MIN                    
000900                                     W-4102-IDRAPP-MAX                    
000901                                     W-4102-IDRAPP                        
000902         MOVE SPAR-IDKOLLI-ENTER  TO W-4102-IDKOLLI-MIN                   
000903                                     W-4102-IDKOLLI                       
000904         MOVE SPAR-IDARTNR-ENTER  TO W-4102-IDARTNR-MIN                   
000905                                     W-4102-IDARTNR                       
000906         MOVE SPAR-MID-IDKOLLI    TO WS-IDKOLLI                           
000907       END-IF                                                             
000908                                                                          
000909       IF MID-INPUT = ALL '+'                                             
000910         PERFORM MFS-RENSA-FAELT-IN                                       
000911       ELSE                                                               
000912         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
000913         PERFORM EA-MID-INDATA-TILL-MOD                                   
000914         CALL WMEDKONV USING MED-WMEDAREA                                 
000915         MOVE MED-MFSINF TO MOD-TEMFSFEL                                  
000916       END-IF                                                             
000917                                                                          
000918     ELSE                                                                 
000919       PERFORM MFS-RENSA-FAELT-INPUT                                      
000920     END-IF                                                               
000921     .                                                                    
000922     EJECT                                                                
000923 EA-MID-INDATA-TILL-MOD SECTION.                                          
000924                                                                          
000925     IF MID-IDAVS-UPD = ALL '+'                                           
000926       MOVE MFS-RENSA-FAELT TO MOD-IDAVS-UPD                              
000927     ELSE                                                                 
000928       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDAVS-ATTR                       
000929       MOVE MFS-ROER-EJ-FAELT TO MOD-IDAVS-UPD                            
000930     END-IF                                                               
000931                                                                          
000932     IF MID-IDKOLLI-UPD = ALL '+'                                         
000933       MOVE MFS-RENSA-FAELT TO MOD-IDKOLLI-UPD                            
000934     ELSE                                                                 
000935       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDKOLLI-ATTR                     
000936       MOVE MFS-ROER-EJ-FAELT TO MOD-IDKOLLI-UPD                          
000937     END-IF                                                               
000938                                                                          
000939     IF MID-KDKOLLI-UPD = ALL '+'                                         
000940       MOVE MFS-RENSA-FAELT TO MOD-KDKOLLI-UPD                            
000941     ELSE                                                                 
000942       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDKOLLI-ATTR                     
000943       MOVE MFS-ROER-EJ-FAELT TO MOD-KDKOLLI-UPD                          
000944     END-IF                                                               
000945                                                                          
000946     IF MID-VKORDBTO-KOLLI-UPD = ALL '+'                                  
000947       MOVE MFS-RENSA-FAELT TO MOD-VKORDBTO-KOLLI-UPD                     
000948     ELSE                                                                 
000949       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-VKORDBTO-KOLLI-ATTR              
000950       MOVE MFS-ROER-EJ-FAELT TO MOD-VKORDBTO-KOLLI-UPD                   
000951     END-IF                                                               
000952                                                                          
000953     IF MID-FLTABORT = '+'                                                
000954       MOVE MFS-RENSA-FAELT TO MOD-FLTABORT                               
000955     ELSE                                                                 
000956       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLTABORT-ATTR                    
000957       MOVE MFS-ROER-EJ-FAELT TO MOD-FLTABORT                             
000958     END-IF                                                               
000959                                                                          
000960     IF MID-FLKLAR = '+'                                                  
000961       MOVE MFS-RENSA-FAELT TO MOD-FLKLAR                                 
000962     ELSE                                                                 
000963       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLKLAR-ATTR                      
000964       MOVE MFS-ROER-EJ-FAELT TO MOD-FLKLAR                               
000965     END-IF                                                               
000966                                                                          
000967     IF MID-KDPRTVAL = '+'                                                
000968       MOVE MFS-RENSA-FAELT TO MOD-KDPRTVAL                               
000969     ELSE                                                                 
000970       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDPRTVAL-ATTR                    
000971       MOVE MFS-ROER-EJ-FAELT TO MOD-KDPRTVAL                             
000972       MOVE INF-PRESS-PF4     TO MED-IDMFSINF                             
000973     END-IF                                                               
000974                                                                          
000975*- KOLLA ALLA RADER                                                       
000976                                                                          
000977     MOVE +1 TO INDX                                                      
000978*-TEST FÖR ATT EJ LÄSA OM FRÅN BÖRJAN NÄR INMATAT SIDA 2                  
000979     IF MID-IDARTNR (INDX) NOT =  ALL '+'                                 
000980       MOVE NEJ TO VISA-INFO-SW                                           
000981     END-IF                                                               
000982                                                                          
000983     PERFORM UNTIL INDX > MAX-INDX                                        
000984       IF MID-KDCMD (INDX)  =  '+'                                        
000985         MOVE MFS-RENSA-FAELT  TO MOD-KDCMD (INDX)                        
000986       ELSE                                                               
000987         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMD-ATTR (INDX)              
000988         MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD (INDX)                       
000989       END-IF                                                             
000990                                                                          
000991       IF MID-IDARTNR (INDX)  =  ALL '+'                                  
000992         MOVE MFS-RENSA-FAELT  TO MOD-IDARTNR (INDX)                      
000993       ELSE                                                               
000994         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDARTNR-ATTR (INDX)            
000995         MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR (INDX)                     
000996       END-IF                                                             
000997                                                                          
000998       IF MID-KVANTAL (INDX)  =  ALL '+'                                  
000999         MOVE MFS-RENSA-FAELT  TO MOD-KVANTAL (INDX)                      
001000       ELSE                                                               
001001         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVANTAL-ATTR (INDX)            
001002         MOVE MFS-ROER-EJ-FAELT TO MOD-KVANTAL (INDX)                     
001003       END-IF                                                             
001004                                                                          
001005       IF MID-PRARTBTO (INDX)  =  ALL '+'                                 
001006         MOVE MFS-RENSA-FAELT  TO MOD-PRARTBTO (INDX)                     
001007       ELSE                                                               
001008         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-PRARTBTO-ATTR (INDX)           
001009         MOVE MFS-ROER-EJ-FAELT TO MOD-PRARTBTO (INDX)                    
001010       END-IF                                                             
001011                                                                          
001012       IF MID-KDFEL (INDX)  =  ALL '+'                                    
001013         MOVE MFS-RENSA-FAELT  TO MOD-KDFEL (INDX)                        
001014       ELSE                                                               
001015         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDFEL-ATTR (INDX)              
001016         MOVE MFS-ROER-EJ-FAELT TO MOD-KDFEL (INDX)                       
001017       END-IF                                                             
001018                                                                          
001019       IF MID-TENOTE (INDX)  =  ALL '+'                                   
001020         MOVE MFS-RENSA-FAELT  TO MOD-TENOTE (INDX)                       
001021       ELSE                                                               
001022         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TENOTE-ATTR (INDX)             
001023         MOVE MFS-ROER-EJ-FAELT TO MOD-TENOTE (INDX)                      
001024       END-IF                                                             
001025                                                                          
001026       ADD +1 TO INDX                                                     
001027     END-PERFORM                                                          
001028                                                                          
001029     .                                                                    
001030     EJECT                                                                
001031 F-LAES-VISA-INFO SECTION.                                                
001032                                                                          
001033     IF DIST79-DEALER-PRICE                                               
001034       PERFORM S10-HAMTA-KDVALISO                                         
001035     ELSE                                                                 
001036       MOVE 'SEK'               TO MOD-KDVALISO                           
001037     END-IF                                                               
001038                                                                          
001039     PERFORM IMS-GU-4101-ROT                                              
001040                                                                          
001041     IF SEGMENT-SAKNAS                                                    
001042       IF MFS-FIRST                                                       
001043         MOVE ERR-KEY-MISSING TO MED-IDMFSFEL                             
001044         CALL WMEDKONV USING MED-WMEDAREA                                 
001045         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
001046         PERFORM MFS-RENSA-FAELT-UT                                       
001047       END-IF                                                             
001048     ELSE                                                                 
001049       IF WS-IDKOLLI = ALL '+'                                            
001050         PERFORM FA-LAES-ALLA-RADER                                       
001051       ELSE                                                               
001052         PERFORM FB-LAES-KOLLI-RADER                                      
001053       END-IF                                                             
001054                                                                          
001055       MOVE +1 TO INDX                                                    
001056       IF SEGMENT-FINNS                                                   
001057         MOVE 4102-IDKUNDNR TO SPAR-IDKUNDNR-ENTER                        
001058         MOVE 4102-IDRAPP   TO SPAR-IDRAPP-ENTER                          
001059         MOVE 4102-IDKOLLI  TO SPAR-IDKOLLI-ENTER                         
001060         MOVE 4102-IDARTNR  TO SPAR-IDARTNR-ENTER                         
001061         MOVE WS-IDKOLLI    TO SPAR-MID-IDKOLLI                           
001062       ELSE                                                               
001063         IF MFS-FIRST                                                     
001064           MOVE ERR-KEY-MISSING TO MED-IDMFSFEL                           
001065           CALL WMEDKONV USING MED-WMEDAREA                               
001066           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
001067           PERFORM MFS-RENSA-FAELT-UT                                     
001068           MOVE W-4102-IDKUNDNR-MIN   TO SPAR-IDKUNDNR-ENTER              
001069           MOVE W-4102-IDRAPP-MIN     TO SPAR-IDRAPP-ENTER                
001070           MOVE W-4102-IDKOLLI-MIN    TO SPAR-IDKOLLI-ENTER               
001071           MOVE W-4102-IDARTNR-MIN    TO SPAR-IDARTNR-ENTER               
001072           MOVE WS-IDKOLLI            TO SPAR-MID-IDKOLLI                 
001073         END-IF                                                           
001074         MOVE W-4102-IDKOLLI-MIN   TO SPAR-IDKOLLI-NEXT                   
001075         MOVE W-4102-IDARTNR-MIN   TO SPAR-IDARTNR-NEXT                   
001076       END-IF                                                             
001077                                                                          
001078       PERFORM UNTIL INDX > MAX-INDX                                      
001079         IF SEGMENT-FINNS                                                 
001080           MOVE 4102-IDKOLLI  TO MOD-IDKOLLI-RAD (INDX)                   
001081                                 SPAR-IDKOLLI (INDX)                      
001082                                 SPAR-IDKOLLI-NEXT                        
001083           MOVE 4102-IDARTNR  TO MOD-IDARTNR (INDX)                       
001084                                 SPAR-IDARTNR (INDX)                      
001085                                 SPAR-IDARTNR-NEXT                        
001086           MOVE 4102-KVANTAL  TO MOD-KVANTAL (INDX)                       
001087                                 SPAR-KVANTAL (INDX)                      
001088           MOVE 4102-PRARTBTO TO MOD-PRARTBTO (INDX)                      
001089                                 SPAR-PRARTBTO (INDX)                     
001090           MOVE 4102-KDFEL    TO MOD-KDFEL  (INDX)                        
001091                                 SPAR-KDFEL (INDX)                        
001092           MOVE 4102-TENOTE   TO MOD-TENOTE   (INDX)                      
001093                                 SPAR-TENOTE  (INDX)                      
001094           PERFORM MFS-STAENG-RAD-FAELT-UT                                
001095           PERFORM IMS-GNP-WDGX4102                                       
001096         ELSE                                                             
001097*TEST FÖR ATT EJ RENSA IFYLLD RAD VID ENTER IST. FÖR PF11                 
001098           IF MID-IDARTNR (INDX) = ALL '+' AND                            
001099              MID-KVANTAL (INDX) = ALL '+'                                
001100              PERFORM MFS-RENSA-RAD-FAELT-UT                              
001101           END-IF                                                         
001102           MOVE MFS-STAENG-FAELT-NOMOD  TO MOD-KDCMD-ATTR (INDX)          
001103           MOVE ZERO        TO   SPAR-IDKOLLI (INDX)                      
001104                                 SPAR-IDARTNR (INDX)                      
001105                                 SPAR-KVANTAL (INDX)                      
001106                                 SPAR-PRARTBTO (INDX)                     
001107           MOVE SPACE       TO   SPAR-KDFEL (INDX)                        
001108                                 SPAR-TENOTE  (INDX)                      
001109         END-IF                                                           
001110         ADD 1 TO INDX                                                    
001111       END-PERFORM                                                        
001112                                                                          
001113*-- OM DET FINNS ETT 11:E SEGMENT.                                        
001114                                                                          
001115       IF SEGMENT-FINNS                                                   
001116         MOVE 4102-IDKUNDNR TO SPAR-IDKUNDNR-NEXT                         
001117         MOVE 4102-IDRAPP   TO SPAR-IDRAPP-NEXT                           
001118         MOVE 4102-IDKOLLI  TO SPAR-IDKOLLI-NEXT                          
001119         MOVE 4102-IDARTNR  TO SPAR-IDARTNR-NEXT                          
001120         MOVE WS-IDKOLLI    TO SPAR-MID-IDKOLLI                           
001121         IF MED-IDMFSINF = SPACE                                          
001122           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
001123           CALL WMEDKONV USING MED-WMEDAREA                               
001124           MOVE MED-TEMFSINF TO MOD-TEMFSINF                              
001125         END-IF                                                           
001126       ELSE                                                               
001127         MOVE W-4102-IDKUNDNR-MIN   TO SPAR-IDKUNDNR-NEXT                 
001128         MOVE W-4102-IDRAPP-MIN     TO SPAR-IDRAPP-NEXT                   
001129         MOVE WS-IDKOLLI            TO SPAR-MID-IDKOLLI                   
001130         IF MED-IDMFSINF = SPACE                                          
001131           MOVE INF-LAST-PAGE  TO MED-IDMFSINF                            
001132           CALL WMEDKONV USING MED-WMEDAREA                               
001133           MOVE MED-MFSINF TO MOD-TEMFSINF                                
001134         END-IF                                                           
001135       END-IF                                                             
001136                                                                          
001137       MOVE '002'      TO MSGI-KDCALL                                     
001138       MOVE '4745'     TO SPAR-IDTRANS                                    
001139       MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                  
001140       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
001141     END-IF                                                               
001142     .                                                                    
001143     EJECT                                                                
001144 FA-LAES-ALLA-RADER SECTION.                                              
001145                                                                          
001146     PERFORM IMS-GNP-WDGX4102                                             
001147                                                                          
001148*-- KAN EJ VISA DESSA FÄLT OM EJ KOLLINR ANGETTS I NYCKELN.               
001149*-- GÄLLER ÄVEN SPAR-AREORNA.                                             
001150                                                                          
001151     MOVE ZERO                 TO MOD-IDKOLLI                             
001152     MOVE SPACE                TO MOD-KDKOLLI                             
001153                                  SPAR-KDKOLLI                            
001154     MOVE ZERO                 TO MOD-VKORDBTO-KOLLI                      
001155                                  SPAR-VKORDBTO-KOLLI                     
001156                                                                          
001157     IF SEGMENT-FINNS                                                     
001158        MOVE 4102-IDAVS        TO MOD-IDAVS                               
001159                                  SPAR-IDAVS                              
001160        MOVE 4102-DAREGDAT     TO MOD-DAREGDAT                            
001161        MOVE 4102-DARFSDAT     TO MOD-DARFSDAT                            
001162                                  SPAR-DARFSDAT                           
001163     ELSE                                                                 
001164        MOVE MFS-RENSA-FAELT   TO MOD-IDAVS                               
001165                                  MOD-DAREGDAT                            
001166                                  MOD-DARFSDAT                            
001167        MOVE SPACE             TO SPAR-IDAVS                              
001168                                  SPAR-KDKOLLI                            
001169        MOVE ZERO              TO SPAR-DARFSDAT                           
001170                                  SPAR-VKORDBTO-KOLLI                     
001171     END-IF                                                               
001172     .                                                                    
001173     EJECT                                                                
001174 FB-LAES-KOLLI-RADER SECTION.                                             
001175                                                                          
001176     IF MFS-NEXT OR MFS-ENTER                                             
001177       MOVE WS-IDKOLLI    TO W-4102-IDKOLLI-MAX                           
001178     END-IF                                                               
001179                                                                          
001180     PERFORM IMS-GNP-WDGX4102                                             
001181                                                                          
001182     IF SEGMENT-FINNS                                                     
001183        MOVE 4102-IDAVS        TO MOD-IDAVS                               
001184                                  SPAR-IDAVS                              
001185        MOVE 4102-DAREGDAT     TO MOD-DAREGDAT                            
001186        MOVE 4102-DARFSDAT     TO MOD-DARFSDAT                            
001187                                  SPAR-DARFSDAT                           
001188                                                                          
001189        MOVE 4102-IDKOLLI              TO MOD-IDKOLLI                     
001190        MOVE MFS-STAENG-FAELT-NOMOD    TO MOD-IDKOLLI-ATTR                
001191                                                                          
001192        MOVE 4102-KDKOLLI            TO SPAR-KDKOLLI                      
001193        IF 4102-KDKOLLI NOT = SPACE                                       
001194          MOVE 4102-KDKOLLI            TO MOD-KDKOLLI                     
001195        ELSE                                                              
001196          MOVE MFS-RENSA-FAELT         TO MOD-KDKOLLI                     
001197        END-IF                                                            
001198                                                                          
001199        MOVE 4102-VKORDBTO-KOLLI     TO SPAR-VKORDBTO-KOLLI               
001200        IF 4102-VKORDBTO-KOLLI > ZERO                                     
001201          MOVE 4102-VKORDBTO-KOLLI     TO MOD-VKORDBTO-KOLLI              
001202        ELSE                                                              
001203          MOVE MFS-RENSA-FAELT         TO MOD-VKORDBTO-KOLLI              
001204        END-IF                                                            
001205     ELSE                                                                 
001206        MOVE MFS-RENSA-FAELT   TO MOD-IDAVS                               
001207                                  MOD-DAREGDAT                            
001208                                  MOD-DARFSDAT                            
001209                                  MOD-IDKOLLI                             
001210                                  MOD-KDKOLLI                             
001211                                  MOD-VKORDBTO-KOLLI                      
001212        MOVE SPACE             TO SPAR-IDAVS                              
001213                                  SPAR-KDKOLLI                            
001214        MOVE ZERO              TO SPAR-DARFSDAT                           
001215                                  SPAR-VKORDBTO-KOLLI                     
001216     END-IF                                                               
001217     .                                                                    
001218     EJECT                                                                
001219 G-KOLLA-INPUT SECTION.                                                   
001220                                                                          
001221     MOVE MSGI-IDDC            TO WS-IDDC                                 
001222     IF CDC-SE OR NDC-JP OR NDC-AU OR SDC                                 
001223       PERFORM GA-KOLLA-INPUT-DATA                                        
001224     ELSE                                                                 
001225       MOVE ERR-USER-NOT-AUTHORIZED TO MED-IDMFSFEL                       
001226       CALL WMEDKONV USING MED-WMEDAREA                                   
001227       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
001228       PERFORM MFS-ROER-EJ-FAELT-IN                                       
001229       PERFORM MFS-ROER-EJ-FAELT-UT                                       
001230       MOVE NEJ TO INDATA-SW                                              
001231     END-IF                                                               
001232     .                                                                    
001233     EJECT                                                                
001234 GA-KOLLA-INPUT-DATA SECTION.                                             
001235                                                                          
001236     MOVE NEJ TO INPUT-SW                                                 
001237     MOVE NEJ TO KDCMD-SW                                                 
001238     MOVE JA  TO INDATA-SW                                                
001239                                                                          
001240     IF MID-INPUT = ALL '+'                                               
001241                                                                          
001242       IF MFS-UPDATE                                                      
001243         MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                        
001244       ELSE                                                               
001245         MOVE ERR-INGET-PRINTAT    TO MED-IDMFSFEL                        
001246       END-IF                                                             
001247       CALL WMEDKONV USING MED-WMEDAREA                                   
001248       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
001249       PERFORM MFS-ROER-EJ-FAELT-IN                                       
001250       PERFORM MFS-ROER-EJ-FAELT-UT                                       
001251       MOVE NEJ TO INDATA-SW                                              
001252     ELSE                                                                 
001253       PERFORM GB-KOLLA-INPUT-FAELT                                       
001254       PERFORM GC-KOLLA-INPUT-RADER                                       
001255       PERFORM GD-KOLLA-FLAGGOR                                           
001256                                                                          
001257       IF INDATA-FEL                                                      
001258         IF MED-IDMFSFEL  = SPACE                                         
001259           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
001260         END-IF                                                           
001261         CALL WMEDKONV USING MED-WMEDAREA                                 
001262         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
001263         PERFORM MFS-ROER-EJ-FAELT-UT                                     
001264         PERFORM MFS-ROER-EJ-FAELT-IN                                     
001265         MOVE NEJ TO INDATA-SW                                            
001266       ELSE                                                               
001267         IF MID-KDPRTVAL = '+'  AND MFS-PRINT                             
001268           MOVE INF-PRESS-PF11 TO MED-IDMFSINF                            
001269           CALL WMEDKONV USING MED-WMEDAREA                               
001270           MOVE MED-MFSINF TO MOD-TEMFSINF                                
001271           PERFORM MFS-ROER-EJ-FAELT-IN                                   
001272           PERFORM MFS-ROER-EJ-FAELT-UT                                   
001273           PERFORM MFS-LAES-IN-IGEN                                       
001274           MOVE NEJ TO INDATA-SW                                          
001275         END-IF                                                           
001276                                                                          
001277         IF MID-KDPRTVAL NOT = '+' AND MFS-UPDATE                         
001278           MOVE INF-PRESS-PF4 TO MED-IDMFSINF                             
001279           CALL WMEDKONV USING MED-WMEDAREA                               
001280           MOVE MED-MFSINF TO MOD-TEMFSINF                                
001281           MOVE MFS-ROER-EJ-FAELT     TO MOD-KDPRTVAL                     
001282           MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDPRTVAL-ATTR                
001283           MOVE NEJ TO INDATA-SW                                          
001284         END-IF                                                           
001285                                                                          
001286         IF INDATA-OK                                                     
001287           IF MID-FLTABORT = '+'  AND                                     
001288              MID-KDPRTVAL = '+'  AND                                     
001289              MID-FLKLAR   = '+'  AND                                     
001290              KDCMD-SAKNAS                                                
001291             PERFORM GE-KOLLA-OM-OK-UPPDATERA                             
001292           END-IF                                                         
001293                                                                          
001294           IF MID-FLKLAR NOT = ALL '+'                                    
001295             PERFORM GF-KOLLA-OM-VIKT-FINNS                               
001296           ELSE                                                           
001297*080428 E'TRACKER 880053 SKALL INTE VARA OK ATT ÄNDRA/LÄGGA TILL          
001298*OM RAPPORT REDAN KLAR OCH UTSKRIVEN.(HANLING FEE-RADER SKICKADE)         
001299             IF (MID-FLTABORT NOT = ALL '+')  OR                          
001300               KDCMD-FINNS OR                                             
001301               NY-RAD-FINNS                                               
001302               PERFORM GG-KOLLA-OM-KLAR-UTSKRIVEN                         
001303             END-IF                                                       
001304           END-IF                                                         
001305         END-IF                                                           
001306       END-IF                                                             
001307     END-IF                                                               
001308     .                                                                    
001309     EJECT                                                                
001310 GB-KOLLA-INPUT-FAELT SECTION.                                            
001311                                                                          
001312*- KOLLA INMATNINGSFÄLT EXKL RADER OCH FLAGGOR **************             
001313                                                                          
001314     IF MID-IDAVS-UPD NOT = ALL '+'                                       
001315       IF MID-FLTABORT = '+'  AND                                         
001316          MID-KDPRTVAL = '+'                                              
001317         MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDAVS-ATTR                      
001318       ELSE                                                               
001319         MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDAVS-ATTR                      
001320         MOVE ERR-FLERA-FUNKTIONER TO MED-IDMFSFEL                        
001321         MOVE NEJ TO INDATA-SW                                            
001322       END-IF                                                             
001323                                                                          
001324       MOVE JA TO INPUT-SW                                                
001325     END-IF                                                               
001326                                                                          
001327     IF MID-IDKOLLI-UPD NOT = ALL '+'                                     
001328       IF MID-IDKOLLI-UPD NOT NUMERIC                                     
001329         MOVE MFS-NUM-FAELT-FEL TO MOD-IDKOLLI-ATTR                       
001330         MOVE NEJ TO INDATA-SW                                            
001331       ELSE                                                               
001332         IF MID-IDKOLLI-UPD = ZERO                                        
001333           MOVE MFS-NUM-FAELT-FEL TO MOD-IDKOLLI-ATTR                     
001334           MOVE NEJ TO INDATA-SW                                          
001335         ELSE                                                             
001336           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKOLLI-ATTR                   
001337         END-IF                                                           
001338       END-IF                                                             
001339                                                                          
001340       MOVE JA TO INPUT-SW                                                
001341     ELSE                                                                 
001342       INSPECT MSGI-IDKOLLI REPLACING LEADING SPACE BY ZERO               
001343       IF MSGI-IDKOLLI NUMERIC                                            
001344         IF MSGI-IDKOLLI = ZERO                                           
001345           MOVE MFS-NUM-FAELT-FEL TO MOD-IDKOLLI-ATTR                     
001346*SO        MOVE ERR-KOLLI-SAKNAS  TO MED-IDMFSFEL                         
001347           MOVE '002'             TO MED-IDMFSFEL                         
001348           MOVE NEJ TO INDATA-SW                                          
001349         END-IF                                                           
001350       ELSE                                                               
001351         MOVE MFS-NUM-FAELT-FEL TO MOD-IDKOLLI-ATTR                       
001352*SO      MOVE ERR-KOLLI-SAKNAS  TO MED-IDMFSFEL                           
001353         MOVE '030'             TO MED-IDMFSFEL                           
001354         MOVE NEJ TO INDATA-SW                                            
001355       END-IF                                                             
001356     END-IF                                                               
001357                                                                          
001358     IF MID-KDKOLLI-UPD NOT = ALL '+'                                     
001359       MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDKOLLI-ATTR                      
001360       MOVE JA TO INPUT-SW                                                
001361     END-IF                                                               
001362                                                                          
001363     IF MID-VKORDBTO-KOLLI-UPD NOT = ALL '+'                              
001364       MOVE MID-VKORDBTO-KOLLI-UPD TO DEC-IDFRIDATA                       
001365       MOVE +6                     TO DEC-KVHELTAL                        
001366       MOVE +1                     TO DEC-KVDECIMAL                       
001367       CALL WDECEDIT USING DEC-WDECAREA                                   
001368       IF DEC-KDSVAR-FEL                                                  
001369         MOVE MFS-NUM-FAELT-FEL TO MOD-VKORDBTO-KOLLI-ATTR                
001370         MOVE NEJ TO INDATA-SW                                            
001371       ELSE                                                               
001372         MOVE DEC-IDEDITDATA TO WS-VKORDBTO-KOLLI                         
001373         MOVE MFS-NUM-FAELT-RAETT TO MOD-VKORDBTO-KOLLI-ATTR              
001374       END-IF                                                             
001375                                                                          
001376       MOVE JA TO INPUT-SW                                                
001377     END-IF                                                               
001378                                                                          
001379     .                                                                    
001380     EJECT                                                                
001381 GC-KOLLA-INPUT-RADER SECTION.                                            
001382                                                                          
001383*- KOLLA KANTKODER *******************************************            
001384                                                                          
001385     MOVE +1                           TO INDX                            
001386                                                                          
001387     PERFORM UNTIL INDX                 >  MAX-INDX                       
001388       IF MID-KDCMD(INDX)    NOT = ALL '+'                                
001389         IF MID-KDCMD-DELETE (INDX)                                       
001390           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-ATTR (INDX)             
001391           MOVE JA TO KDCMD-SW                                            
001392         ELSE                                                             
001393           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-ATTR (INDX)               
001394           MOVE NEJ TO INDATA-SW                                          
001395         END-IF                                                           
001396       END-IF                                                             
001397                                                                          
001398       ADD +1                          TO INDX                            
001399     END-PERFORM                                                          
001400                                                                          
001401                                                                          
001402*- KOLLA INMATNINGSRADER *************************************            
001403                                                                          
001404     MOVE +1                           TO INDX                            
001405                                                                          
001406     PERFORM UNTIL INDX                 >  MAX-INDX                       
001407       IF MID-IDARTNR(INDX)    NOT = ALL '+'                              
001408         IF MID-IDARTNR(INDX) NUMERIC                                     
001409           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDARTNR-ATTR (INDX)            
001410           MOVE JA TO INPUT-SW                                            
001411           MOVE JA TO NY-RAD-SW                                           
001412         ELSE                                                             
001413           MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-ATTR (INDX)              
001414           MOVE NEJ TO INDATA-SW                                          
001415         END-IF                                                           
001416       ELSE                                                               
001417         IF MID-KVANTAL(INDX) NOT = ALL '+'                               
001418           MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-ATTR (INDX)              
001419           MOVE NEJ TO INDATA-SW                                          
001420         END-IF                                                           
001421       END-IF                                                             
001422                                                                          
001423       IF MID-KVANTAL(INDX)    NOT = ALL '+'                              
001424         IF MID-KVANTAL(INDX) NUMERIC                                     
001425           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVANTAL-ATTR (INDX)            
001426           MOVE JA TO INPUT-SW                                            
001427         ELSE                                                             
001428           MOVE MFS-NUM-FAELT-FEL TO MOD-KVANTAL-ATTR (INDX)              
001429           MOVE NEJ TO INDATA-SW                                          
001430         END-IF                                                           
001431       ELSE                                                               
001432         IF MID-IDARTNR (INDX) NOT = ALL '+'                              
001433           MOVE MFS-NUM-FAELT-FEL TO MOD-KVANTAL-ATTR (INDX)              
001434           MOVE NEJ TO INDATA-SW                                          
001435         END-IF                                                           
001436       END-IF                                                             
001437                                                                          
001438       IF MID-PRARTBTO (INDX) NOT = ALL '+'                               
001439         MOVE MID-PRARTBTO (INDX)  TO DEC-IDFRIDATA                       
001440         MOVE +7                   TO DEC-KVHELTAL                        
001441         MOVE +2                   TO DEC-KVDECIMAL                       
001442         CALL WDECEDIT USING DEC-WDECAREA                                 
001443         IF DEC-KDSVAR-FEL                                                
001444           MOVE MFS-NUM-FAELT-FEL TO MOD-PRARTBTO-ATTR (INDX)             
001445           MOVE NEJ TO INDATA-SW                                          
001446         ELSE                                                             
001447           MOVE DEC-IDEDITDATA TO WS-PRARTBTO (INDX)                      
001448           MOVE MFS-NUM-FAELT-RAETT TO MOD-PRARTBTO-ATTR (INDX)           
001449         END-IF                                                           
001450                                                                          
001451         MOVE JA TO INPUT-SW                                              
001452       END-IF                                                             
001453                                                                          
001454       IF MID-KDFEL(INDX)      NOT = ALL '+'                              
001455         IF MID-KDFEL(INDX) NUMERIC                                       
001456           MOVE MFS-NUM-FAELT-RAETT TO MOD-KDFEL-ATTR (INDX)              
001457           MOVE JA TO INPUT-SW                                            
001458         ELSE                                                             
001459           MOVE MFS-NUM-FAELT-FEL TO MOD-KDFEL-ATTR (INDX)                
001460           MOVE NEJ TO INDATA-SW                                          
001461         END-IF                                                           
001462       END-IF                                                             
001463                                                                          
001464       IF MID-TENOTE(INDX)     NOT = ALL '+'                              
001465         MOVE MFS-ALFA-FAELT-RAETT TO MOD-TENOTE-ATTR (INDX)              
001466         MOVE JA TO INPUT-SW                                              
001467       END-IF                                                             
001468                                                                          
001469       ADD +1                          TO INDX                            
001470     END-PERFORM                                                          
001471                                                                          
001472     IF KDCMD-FINNS AND INPUT-FINNS                                       
001473       MOVE ERR-FLERA-FUNKTIONER TO MED-IDMFSFEL                          
001474       MOVE NEJ TO INDATA-SW                                              
001475     END-IF                                                               
001476                                                                          
001477     IF NY-RAD-FINNS                                                      
001478       IF MID-FLKLAR = ALL '+'                                            
001479         CONTINUE                                                         
001480       ELSE                                                               
001481         MOVE ERR-FLERA-FUNKTIONER TO MED-IDMFSFEL                        
001482         MOVE NEJ TO INDATA-SW                                            
001483       END-IF                                                             
001484     END-IF                                                               
001485                                                                          
001486*-- TESTAR SÅ ATT KOLLINR ÄR IFYLLT VID UPPDATERING AV RAD.               
001487*-- KOLLINR FÅR EJ VARA 0.                                                
001488     IF NY-RAD-FINNS               AND                                    
001489        MID-IDKOLLI-UPD = ALL '+'  AND                                    
001490           SPAR-MID-IDKOLLI = ALL '+'                                     
001491       MOVE ERR-KOLLI-SAKNAS TO MED-IDMFSFEL                              
001492       MOVE MFS-NUM-FAELT-FEL TO MOD-IDKOLLI-ATTR                         
001493       MOVE NEJ TO INDATA-SW                                              
001494     END-IF                                                               
001495     .                                                                    
001496     EJECT                                                                
001497 GD-KOLLA-FLAGGOR SECTION.                                                
001498                                                                          
001499     IF MID-FLTABORT NOT = '+'                                            
001500       IF MID-FLTABORT = 'J' OR 'Y'                                       
001501         IF MID-FLKLAR  = '+'  AND                                        
001502            MID-KDPRTVAL = '+' AND                                        
001503            KDCMD-SAKNAS       AND                                        
001504            INPUT-SAKNAS                                                  
001505                                                                          
001506           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLTABORT-ATTR                 
001507         ELSE                                                             
001508           MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLTABORT-ATTR                 
001509           MOVE ERR-FLERA-FUNKTIONER TO MED-IDMFSFEL                      
001510           MOVE NEJ TO INDATA-SW                                          
001511         END-IF                                                           
001512       ELSE                                                               
001513         MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLTABORT-ATTR                   
001514         MOVE NEJ TO INDATA-SW                                            
001515       END-IF                                                             
001516     END-IF                                                               
001517                                                                          
001518     IF MID-FLKLAR NOT = '+'                                              
001519       IF MID-FLKLAR = 'J' OR 'Y'                                         
001520         IF MID-FLTABORT  = '+'  AND                                      
001521            MID-KDPRTVAL = '+' AND                                        
001522            KDCMD-SAKNAS                                                  
001523                                                                          
001524           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLKLAR-ATTR                   
001525         ELSE                                                             
001526           MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLKLAR-ATTR                   
001527           MOVE ERR-FLERA-FUNKTIONER TO MED-IDMFSFEL                      
001528           MOVE NEJ TO INDATA-SW                                          
001529         END-IF                                                           
001530         IF SPAR-IDAVS = SPACE   AND                                      
001531             MID-IDAVS-UPD = ALL '+'                                      
001532           MOVE MFS-ALFA-FAELT-FEL     TO MOD-FLKLAR-ATTR                 
001533           IF MED-IDMFSFEL = SPACE                                        
001534             MOVE ERR-UPPGIFTER-SAKNAS TO MED-IDMFSFEL                    
001535           END-IF                                                         
001536           MOVE NEJ TO INDATA-SW                                          
001537         END-IF                                                           
001538       ELSE                                                               
001539         MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLKLAR-ATTR                     
001540         MOVE NEJ TO INDATA-SW                                            
001541       END-IF                                                             
001542     END-IF                                                               
001543                                                                          
001544     IF MID-KDPRTVAL NOT = '+'                                            
001545       IF MID-KDPRTVAL = 'F' OR 'L' OR 'P' OR 'T'                         
001546         IF MID-FLKLAR = '+' AND                                          
001547            MID-FLTABORT = '+' AND                                        
001548            KDCMD-SAKNAS       AND                                        
001549            INPUT-SAKNAS                                                  
001550                                                                          
001551           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDPRTVAL-ATTR                 
001552         ELSE                                                             
001553           MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDPRTVAL-ATTR                 
001554           MOVE ERR-FLERA-FUNKTIONER TO MED-IDMFSFEL                      
001555           MOVE NEJ TO INDATA-SW                                          
001556         END-IF                                                           
001557                                                                          
001558         IF SPAR-IDAVS = SPACE   AND                                      
001559            SPAR-DARFSDAT = ZERO                                          
001560           MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDPRTVAL-ATTR                 
001561           MOVE ERR-ORDERN-EJ-KLAR   TO MED-IDMFSFEL                      
001562           MOVE NEJ TO INDATA-SW                                          
001563         END-IF                                                           
001564       ELSE                                                               
001565         MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDPRTVAL-ATTR                   
001566         MOVE NEJ TO INDATA-SW                                            
001567       END-IF                                                             
001568     END-IF                                                               
001569                                                                          
001570     .                                                                    
001571     EJECT                                                                
001572 GE-KOLLA-OM-OK-UPPDATERA SECTION.                                        
001573                                                                          
001574* - LÄSER ARTIKELREGISTRET SÅ ATT ARTIKELN FINNS (WDK6)                   
001575                                                                          
001576     MOVE +1 TO INDX                                                      
001577     PERFORM UNTIL INDX > MAX-INDX                                        
001578       IF MID-IDARTNR (INDX) NOT = ALL '+'                                
001579         MOVE MID-IDARTNR (INDX) TO W-IDARTNR                             
001580         PERFORM IMS-GU-WDK601                                            
001581         IF SEGMENT-SAKNAS                                                
001582           MOVE ERR-PART-MISS-ON-FILE TO MED-IDMFSFEL                     
001583           MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-ATTR (INDX)              
001584           CALL WMEDKONV USING MED-WMEDAREA                               
001585           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
001586           PERFORM MFS-ROER-EJ-FAELT-IN                                   
001587           PERFORM MFS-ROER-EJ-FAELT-UT                                   
001588           MOVE NEJ TO INDATA-SW                                          
001589         ELSE                                                             
001590           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDARTNR-ATTR (INDX)            
001591         END-IF                                                           
001592       END-IF                                                             
001593       ADD +1 TO INDX                                                     
001594     END-PERFORM                                                          
001595                                                                          
001596* - LÄSER KUNDREGISTRET SÅ ATT KUNDEN FINNS (WDB2)                        
001597                                                                          
001598     PERFORM IMS-GU-WDB201                                                
001599                                                                          
001600     IF SEGMENT-FINNS                                                     
001601       CONTINUE                                                           
001602     ELSE                                                                 
001603       MOVE ERR-KUND-SAKNAS TO MED-IDMFSFEL                               
001604       CALL WMEDKONV USING MED-WMEDAREA                                   
001605       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
001606       PERFORM MFS-RENSA-FAELT-IN                                         
001607       PERFORM MFS-RENSA-FAELT-UT                                         
001608       MOVE NEJ TO INDATA-SW                                              
001609     END-IF                                                               
001610     .                                                                    
001611     EJECT                                                                
001612 GF-KOLLA-OM-VIKT-FINNS SECTION.                                          
001613                                                                          
001614     MOVE NEJ TO VIKT-SAKNAS-SW                                           
001615                                                                          
001616     PERFORM IMS-GU-4101-ROT                                              
001617                                                                          
001618     IF SEGMENT-FINNS                                                     
001619       PERFORM IMS-GNP-WDGX4102                                           
001620                                                                          
001621       IF SEGMENT-FINNS                                                   
001622         MOVE 4102-IDKOLLI    TO WS-SPAR-IDKOLLI                          
001623         IF 4102-VKORDBTO-KOLLI > ZERO                                    
001624                                                                          
001625           PERFORM UNTIL SEGMENT-SAKNAS                                   
001626             IF 4102-IDKOLLI = WS-SPAR-IDKOLLI                            
001627               CONTINUE                                                   
001628             ELSE                                                         
001629               MOVE 4102-IDKOLLI   TO WS-SPAR-IDKOLLI                     
001630               IF 4102-VKORDBTO-KOLLI = ZERO                              
001631                 MOVE JA TO VIKT-SAKNAS-SW                                
001632               END-IF                                                     
001633             END-IF                                                       
001634             PERFORM IMS-GNP-WDGX4102                                     
001635           END-PERFORM                                                    
001636         ELSE                                                             
001637           MOVE JA TO VIKT-SAKNAS-SW                                      
001638         END-IF                                                           
001639       END-IF                                                             
001640     END-IF                                                               
001641                                                                          
001642     IF VIKT-SAKNAS                                                       
001643       MOVE ERR-VIKT-SAKNAS TO MED-IDMFSFEL                               
001644       CALL WMEDKONV USING MED-WMEDAREA                                   
001645       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
001646       PERFORM MFS-RENSA-FAELT-IN                                         
001647       PERFORM MFS-RENSA-FAELT-UT                                         
001648       MOVE NEJ TO INDATA-SW                                              
001649     END-IF                                                               
001650     .                                                                    
001651     EJECT                                                                
001652 GG-KOLLA-OM-KLAR-UTSKRIVEN SECTION.                                      
001653                                                                          
001654     PERFORM IMS-GU-4101-ROT                                              
001655                                                                          
001656     IF SEGMENT-FINNS                                                     
001657       PERFORM IMS-GNP-WDGX4102                                           
001658       IF SEGMENT-FINNS                                                   
001659         IF 4102-DARFSDAT = ZERO                                          
001660           CONTINUE                                                       
001661         ELSE                                                             
001662           IF 4102-FLFAKT = JA                                            
001663             MOVE ERR-FAKTURA-KLAR TO MED-IDMFSFEL                        
001664             CALL WMEDKONV USING MED-WMEDAREA                             
001665             MOVE MED-MFSFEL TO MOD-TEMFSFEL                              
001666             PERFORM MFS-RENSA-FAELT-IN                                   
001667             PERFORM MFS-RENSA-FAELT-UT                                   
001668             MOVE NEJ TO INDATA-SW                                        
001669           END-IF                                                         
001670         END-IF                                                           
001671       END-IF                                                             
001672     END-IF                                                               
001673                                                                          
001674     .                                                                    
001675     EJECT                                                                
001676 H-UPPDATERA SECTION.                                                     
001677                                                                          
001678     IF MFS-PRINT                                                         
001679       EVALUATE MID-KDPRTVAL                                              
001680       WHEN 'P'                                                           
001681         PERFORM HA-STARTA-W40749-PRINT                                   
001682       WHEN 'F'                                                           
001683         PERFORM HF-STARTA-W40799-PRINT                                   
001684       WHEN 'L'                                                           
001685         PERFORM HF-STARTA-W40799-PRINT                                   
001686       WHEN 'T'                                                           
001687         PERFORM HA-STARTA-W40749-PRINT                                   
001688         PERFORM HF-STARTA-W40799-PRINT                                   
001689       END-EVALUATE                                                       
001690     ELSE                                                                 
001691       IF MID-FLTABORT NOT = '+'                                          
001692         PERFORM HB-DELETE-HEL-RAPPORT                                    
001693       ELSE                                                               
001694         IF KDCMD-FINNS                                                   
001695           PERFORM HC-DELETE-RAD                                          
001696         ELSE                                                             
001697           IF MID-FLKLAR NOT = '+'                                        
001698             PERFORM HD-UPPDAT-SKRIV-UT                                   
001699           ELSE                                                           
001700             PERFORM HE-UPPDATERA-RAPPORT                                 
001701           END-IF                                                         
001702         END-IF                                                           
001703       END-IF                                                             
001704     END-IF                                                               
001705     .                                                                    
001706     EJECT                                                                
001707 HA-STARTA-W40749-PRINT SECTION.                                          
001708                                                                          
001709     MOVE MFS-KDMFSFOR          TO P-TO-P-KDMFSFOR                        
001710                                                                          
001711     MOVE 'W40745'              TO 4749-MID-IDPGM                         
001712     MOVE MSGI-IDDISTR          TO 4749-MID-IDDISTR                       
001713     MOVE MSGI-IDKUNDNR         TO 4749-MID-IDKUNDNR                      
001714     MOVE MSGI-IDRAPP           TO 4749-MID-IDRAPP                        
001715     MOVE WS-IDKOLLI            TO 4749-MID-IDKOLLI                       
001716     MOVE MSGI-IDDC             TO 4749-MID-IDDC                          
001717                                                                          
001718     COMPUTE P-TO-P-LL = LENGTH OF 4749-MID-W4I74901 + 17                 
001719                                                                          
001720     PERFORM IMS-ISRT-MSG-ALT-4749                                        
001721                                                                          
001722     MOVE INF-PRINT-BEG TO MED-IDMFSINF                                   
001723     CALL WMEDKONV USING MED-WMEDAREA                                     
001724     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
001725*-VISAR BÅDE UPPDAT-GJORD OCH PRINT-BEGÄRD VID FLKLAR=JA                  
001726     PERFORM MFS-FORM-ATTR                                                
001727     PERFORM MFS-RENSA-FAELT-IN                                           
001728     .                                                                    
001729     EJECT                                                                
001730 HB-DELETE-HEL-RAPPORT SECTION.                                           
001731                                                                          
001732     PERFORM IMS-GU-4101-ROT                                              
001733     IF SEGMENT-SAKNAS                                                    
001734       MOVE ERR-KEY-MISSING TO MED-IDMFSFEL                               
001735       CALL WMEDKONV USING MED-WMEDAREA                                   
001736       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
001737       MOVE MFS-ALFA-FAELT-FEL TO MOD-FLTABORT-ATTR                       
001738       PERFORM MFS-RENSA-FAELT-UT                                         
001739     ELSE                                                                 
001740       PERFORM IMS-GHNP-WDGX4102                                          
001741       PERFORM UNTIL SEGMENT-SAKNAS                                       
001742         IF SEGMENT-FINNS                                                 
001743           PERFORM IMS-DLET-WDGX4102                                      
001744         END-IF                                                           
001745         PERFORM IMS-GHNP-WDGX4102                                        
001746       END-PERFORM                                                        
001747                                                                          
001748       PERFORM S01-INFO-UPDATE                                            
001749     END-IF                                                               
001750                                                                          
001751     .                                                                    
001752     EJECT                                                                
001753 HC-DELETE-RAD SECTION.                                                   
001754                                                                          
001755     PERFORM IMS-GU-4101-ROT                                              
001756                                                                          
001757     MOVE +1 TO INDX                                                      
001758     PERFORM UNTIL INDX > MAX-INDX                                        
001759                                                                          
001760       IF MID-KDCMD (INDX) NOT = '+'                                      
001761         MOVE SPAR-IDKOLLI (INDX)    TO W-4102-IDKOLLI                    
001762         MOVE SPAR-IDARTNR (INDX)    TO W-4102-IDARTNR                    
001763         PERFORM IMS-GHNP-4102                                            
001764         IF SEGMENT-FINNS                                                 
001765           PERFORM IMS-DLET-WDGX4102                                      
001766         END-IF                                                           
001767       END-IF                                                             
001768       ADD +1                          TO INDX                            
001769     END-PERFORM                                                          
001770                                                                          
001771     PERFORM S01-INFO-UPDATE                                              
001772     .                                                                    
001773     EJECT                                                                
001774 HD-UPPDAT-SKRIV-UT SECTION.                                              
001775                                                                          
001776     PERFORM IMS-GU-4101-ROT                                              
001777                                                                          
001778     IF SEGMENT-SAKNAS                                                    
001779       MOVE ERR-KEY-MISSING TO MED-IDMFSFEL                               
001780       CALL WMEDKONV USING MED-WMEDAREA                                   
001781       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
001782       MOVE MFS-ALFA-FAELT-FEL TO MOD-FLKLAR-ATTR                         
001783       PERFORM MFS-RENSA-FAELT-UT                                         
001784     ELSE                                                                 
001785       MOVE ZERO   TO W-4102-IDKOLLI-MIN                                  
001786       MOVE +99999 TO W-4102-IDKOLLI-MAX                                  
001787       PERFORM IMS-GHNP-WDGX4102                                          
001788       PERFORM UNTIL SEGMENT-SAKNAS                                       
001789         IF SEGMENT-FINNS                                                 
001790           IF MID-IDAVS-UPD NOT = ALL '+'                                 
001791             MOVE MID-IDAVS-UPD       TO 4102-IDAVS                       
001792           END-IF                                                         
001793*- HÄMTAR LOKALT DATUM I B-SEKTIONEN.                                     
001794           MOVE MSGI-TILOKDAT           TO 4102-DARFSDAT (3:6)            
001795           MOVE FUNCTION CURRENT-DATE (1:2)                               
001796                                        TO 4102-DARFSDAT (1:2)            
001797           IF CDC-SE OR SDC                                               
001798             MOVE NEJ                   TO 4102-FLFAKT                    
001799           END-IF                                                         
001800                                                                          
001801           PERFORM IMS-REPL-WDGX4102                                      
001802         END-IF                                                           
001803         PERFORM IMS-GHNP-WDGX4102                                        
001804       END-PERFORM                                                        
001805                                                                          
001806       PERFORM S01-INFO-UPDATE                                            
001807                                                                          
001808       MOVE ZERO                  TO 4749-MID-IDKOLLI                     
001809                                                                          
001810       PERFORM HA-STARTA-W40749-PRINT                                     
001811       PERFORM HF-STARTA-W40799-PRINT                                     
001812     END-IF                                                               
001813     .                                                                    
001814     EJECT                                                                
001815 HE-UPPDATERA-RAPPORT SECTION.                                            
001816                                                                          
001817     MOVE NEJ  TO RAPPORT-SW                                              
001818     MOVE NEJ  TO GODKAND-RAD-SW                                          
001819                                                                          
001820                                                                          
001821     PERFORM IMS-GU-4101-ROT                                              
001822                                                                          
001823     IF SEGMENT-FINNS                                                     
001824       PERFORM HEA-REPLACE-RADINFO                                        
001825                                                                          
001826       PERFORM HEB-INSERT-RAD                                             
001827     ELSE                                                                 
001828       MOVE '4101'             TO 4101-IDHTYP                             
001829       MOVE MSGI-IDDC          TO 4101-IDDC                               
001830       MOVE MSGI-IDDISTR       TO 4101-IDDISTR                            
001831       MOVE LOW-VALUE          TO 4101-LOWVALUE                           
001832                                                                          
001833       PERFORM IMS-ISRT-4101-ROT                                          
001834                                                                          
001835       MOVE JA  TO RAPPORT-SW                                             
001836                                                                          
001837       PERFORM HEB-INSERT-RAD                                             
001838     END-IF                                                               
001839     .                                                                    
001840     EJECT                                                                
001841 HEA-REPLACE-RADINFO SECTION.                                             
001842                                                                          
001843     IF MID-IDKOLLI-UPD = ALL '+'                                         
001844       MOVE MSGI-IDKOLLI     TO W-4102-IDKOLLI-MIN                        
001845                                W-4102-IDKOLLI-MAX                        
001846     ELSE                                                                 
001847       MOVE MID-IDKOLLI-UPD  TO W-4102-IDKOLLI-MIN                        
001848                                W-4102-IDKOLLI-MAX                        
001849     END-IF                                                               
001850                                                                          
001851     PERFORM IMS-GHNP-WDGX4102                                            
001852     IF SEGMENT-SAKNAS                                                    
001853       MOVE JA  TO RAPPORT-SW                                             
001854     END-IF                                                               
001855                                                                          
001856     IF MID-KDKOLLI-UPD = ALL '+'   AND                                   
001857        MID-VKORDBTO-KOLLI-UPD = ALL '+'                                  
001858       CONTINUE                                                           
001859     ELSE                                                                 
001860       PERFORM UNTIL SEGMENT-SAKNAS                                       
001861         IF SEGMENT-FINNS                                                 
001862                                                                          
001863           IF MID-KDKOLLI-UPD NOT = ALL '+'                               
001864             MOVE MID-KDKOLLI-UPD    TO 4102-KDKOLLI                      
001865           END-IF                                                         
001866                                                                          
001867           IF MID-VKORDBTO-KOLLI-UPD NOT = ALL '+'                        
001868             MOVE WS-VKORDBTO-KOLLI  TO 4102-VKORDBTO-KOLLI               
001869           END-IF                                                         
001870                                                                          
001871           PERFORM IMS-REPL-WDGX4102                                      
001872           MOVE JA TO GODKAND-RAD-SW                                      
001873         END-IF                                                           
001874         PERFORM IMS-GHNP-WDGX4102                                        
001875       END-PERFORM                                                        
001876     END-IF                                                               
001877                                                                          
001878                                                                          
001879*--- UPPDATERA ISSUER FÖR VARJE KOLLI SOM FINNS TIDIGARE. KAN             
001880*--- FYLLAS I VID VALFRITT TILLFÄLLE.                                     
001881                                                                          
001882     IF MID-IDAVS-UPD  = ALL '+'                                          
001883       CONTINUE                                                           
001884     ELSE                                                                 
001885       PERFORM IMS-GU-4101-ROT                                            
001886                                                                          
001887       MOVE ZERO   TO W-4102-IDKOLLI-MIN                                  
001888       MOVE +99999 TO W-4102-IDKOLLI-MAX                                  
001889       PERFORM IMS-GHNP-WDGX4102                                          
001890       PERFORM UNTIL SEGMENT-SAKNAS                                       
001891         IF SEGMENT-FINNS                                                 
001892                                                                          
001893           MOVE MID-IDAVS-UPD       TO 4102-IDAVS                         
001894                                                                          
001895           PERFORM IMS-REPL-WDGX4102                                      
001896           MOVE JA TO GODKAND-RAD-SW                                      
001897         END-IF                                                           
001898         PERFORM IMS-GHNP-WDGX4102                                        
001899       END-PERFORM                                                        
001900     END-IF                                                               
001901                                                                          
001902     .                                                                    
001903     EJECT                                                                
001904 HEB-INSERT-RAD SECTION.                                                  
001905                                                                          
001906     MOVE +1 TO INDX                                                      
001907     PERFORM UNTIL INDX > MAX-INDX                                        
001908                                                                          
001909       IF MID-IDARTNR (INDX) = ALL '+'                                    
001910         CONTINUE                                                         
001911       ELSE                                                               
001912         MOVE SPACE              TO 4102-WDGX4102                         
001913         MOVE MSGI-IDKUNDNR      TO 4102-IDKUNDNR                         
001914         MOVE MSGI-IDRAPP        TO 4102-IDRAPP                           
001915         IF MID-IDKOLLI-UPD = ALL '+'                                     
001916           MOVE MSGI-IDKOLLI     TO 4102-IDKOLLI                          
001917         ELSE                                                             
001918           MOVE MID-IDKOLLI-UPD  TO 4102-IDKOLLI                          
001919         END-IF                                                           
001920                                                                          
001921*-HAR SPARAT UNDAN KOLLIKOD OCH KOLLIVIKT FÖR TILLÄGG AV RAD.             
001922                                                                          
001923         IF MID-KDKOLLI-UPD = ALL '+'                                     
001924           IF NY-RAPPORT                                                  
001925             MOVE SPACE            TO 4102-KDKOLLI                        
001926           ELSE                                                           
001927             MOVE SPAR-KDKOLLI     TO 4102-KDKOLLI                        
001928           END-IF                                                         
001929         ELSE                                                             
001930           MOVE MID-KDKOLLI-UPD  TO 4102-KDKOLLI                          
001931         END-IF                                                           
001932                                                                          
001933*FÖR NDC:ERNAS SKULL HÄMTAR MAN LOKAL TID MHA ETT ANROP TILL              
001934*W005INIT I B-SECTION.(SKA KUNNA GÄLLA FÖR SAMTLIGA DC:N ÄVEN CDC)        
001935*                                                                         
001936         MOVE MSGI-TILOKDAT            TO 4102-DAREGDAT (3:6)             
001937         MOVE FUNCTION CURRENT-DATE (1:2)                                 
001938                                       TO 4102-DAREGDAT (1:2)             
001939                                                                          
001940         MOVE ZERO                     TO 4102-DARFSDAT                   
001941         MOVE SPACE                    TO 4102-FLFAKT                     
001942                                                                          
001943         IF MID-IDAVS-UPD = ALL '+'                                       
001944           MOVE SPAR-IDAVS            TO 4102-IDAVS                       
001945         ELSE                                                             
001946           MOVE MID-IDAVS-UPD         TO 4102-IDAVS                       
001947         END-IF                                                           
001948                                                                          
001949         IF MID-VKORDBTO-KOLLI-UPD = ALL '+'                              
001950           IF NY-RAPPORT                                                  
001951             MOVE ZERO                TO 4102-VKORDBTO-KOLLI              
001952           ELSE                                                           
001953             MOVE SPAR-VKORDBTO-KOLLI TO 4102-VKORDBTO-KOLLI              
001954           END-IF                                                         
001955         ELSE                                                             
001956           MOVE WS-VKORDBTO-KOLLI     TO 4102-VKORDBTO-KOLLI              
001957         END-IF                                                           
001958                                                                          
001959         IF MID-IDARTNR (INDX) = ALL '+'                                  
001960           MOVE ZERO               TO 4102-IDARTNR                        
001961         ELSE                                                             
001962           MOVE MID-IDARTNR (INDX) TO 4102-IDARTNR                        
001963         END-IF                                                           
001964                                                                          
001965         IF MID-KVANTAL (INDX) = ALL '+'                                  
001966           MOVE ZERO               TO 4102-KVANTAL                        
001967         ELSE                                                             
001968           MOVE MID-KVANTAL (INDX) TO 4102-KVANTAL                        
001969         END-IF                                                           
001970                                                                          
001971*- pris skall vara 0 då man hädanefter sätter pris på 4707                
001972*-                                                                        
001973                                                                          
001974         IF MID-PRARTBTO (INDX) = ALL '+'                                 
001975           IF NDC-JP OR NDC-AU                                            
001976             MOVE ZERO              TO 4102-PRARTBTO                      
001977           ELSE                                                           
001978             MOVE ZERO              TO 4102-PRARTBTO                      
001979           END-IF                                                         
001980         ELSE                                                             
001981           MOVE ZERO                TO 4102-PRARTBTO                      
001982         END-IF                                                           
001983                                                                          
001984         IF MID-KDFEL(INDX)  = ALL '+'                                    
001985           MOVE ZERO               TO 4102-KDFEL                          
001986         ELSE                                                             
001987           MOVE MID-KDFEL(INDX)    TO 4102-KDFEL                          
001988         END-IF                                                           
001989                                                                          
001990**- FINNS FELKOD ANGIVEN SKALL SVENSK/ENG.TEXT HÄMTAS FRÅN TABELL.        
001991         IF MID-TENOTE (INDX) = ALL '+'                                   
001992           IF MID-KDFEL(INDX) = ALL '+'                                   
001993             MOVE SPACE           TO 4102-TENOTE                          
001994           ELSE                                                           
001995             MOVE MID-KDFEL(INDX) TO TAB-INDX                             
001996             IF TAB-INDX > 130                                            
001997               MOVE SPACE                  TO 4102-TENOTE                 
001998             ELSE                                                         
001999               MOVE TAB-TENOTE (TAB-INDX)  TO 4102-TENOTE                 
002000             END-IF                                                       
002001           END-IF                                                         
002002         ELSE                                                             
002003           INSPECT MID-TENOTE(INDX) (2:39) CONVERTING STOR-BOKSTAV        
002004                      TO LITEN-BOKSTAV                                    
002005           MOVE MID-TENOTE(INDX)   TO 4102-TENOTE                         
002006         END-IF                                                           
002007                                                                          
002008         PERFORM IMS-ISRT-WDGX4102                                        
002009         IF SEGMENT-FINNS-REDAN                                           
002010           MOVE ERR-RAD-FINNS-REDAN TO MED-IDMFSFEL                       
002011           CALL WMEDKONV USING MED-WMEDAREA                               
002012           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
002013           MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-ATTR (INDX)              
002014           PERFORM MFS-ROER-EJ-FAELT-UT                                   
002015         ELSE                                                             
002016           MOVE JA TO GODKAND-RAD-SW                                      
002017         END-IF                                                           
002018       END-IF                                                             
002019                                                                          
002020       ADD +1 TO INDX                                                     
002021     END-PERFORM                                                          
002022                                                                          
002023     IF GODKAND-RAD                                                       
002024       MOVE INF-UPDATE-DONE TO MED-IDMFSINF                               
002025       CALL WMEDKONV USING MED-WMEDAREA                                   
002026       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
002027       IF MED-IDMFSFEL = '245'                                            
002028         CONTINUE                                                         
002029       ELSE                                                               
002030         PERFORM MFS-FORM-ATTR                                            
002031         PERFORM MFS-RENSA-FAELT-INPUT                                    
002032       END-IF                                                             
002033     END-IF                                                               
002034     .                                                                    
002035     EJECT                                                                
002036 HF-STARTA-W40799-PRINT SECTION.                                          
002037                                                                          
002038     MOVE MFS-KDMFSFOR          TO P-TO-P2-KDMFSFOR                       
002039                                                                          
002040     MOVE 'W40745'              TO 4799-MID-IDPGM                         
002041     MOVE MSGI-IDDISTR          TO 4799-MID-IDDISTR                       
002042     MOVE MSGI-IDKUNDNR         TO 4799-MID-IDKUNDNR                      
002043     MOVE MSGI-IDRAPP           TO 4799-MID-IDRAPP                        
002044     MOVE WS-IDKOLLI            TO 4799-MID-IDKOLLI                       
002045     MOVE MSGI-IDDC             TO 4799-MID-IDDC                          
002046                                                                          
002047     COMPUTE P-TO-P2-LL = LENGTH OF 4799-MID-W4I79901 + 17                
002048                                                                          
002049     PERFORM IMS-ISRT-MSG-ALT-4799                                        
002050                                                                          
002051     MOVE INF-PRINT-BEG TO MED-IDMFSINF                                   
002052     CALL WMEDKONV USING MED-WMEDAREA                                     
002053     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
002054*-VISAR BÅDE UPPDAT-GJORD OCH PRINT-BEGÄRD VID FLKLAR=JA                  
002055     PERFORM MFS-FORM-ATTR                                                
002056     PERFORM MFS-RENSA-FAELT-IN                                           
002057     .                                                                    
002058     EJECT                                                                
002059 S01-INFO-UPDATE SECTION.                                                 
002060                                                                          
002061     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
002062     CALL WMEDKONV USING MED-WMEDAREA                                     
002063     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
002064     PERFORM MFS-FORM-ATTR                                                
002065     PERFORM MFS-RENSA-FAELT-INPUT                                        
002066     .                                                                    
002067     EJECT                                                                
002068 S10-HAMTA-KDVALISO   SECTION.                                            
002069                                                                          
002070     PERFORM IMS-GU-WDB201                                                
002071     IF SEGMENT-FINNS                                                     
002072       CONTINUE                                                           
002073     ELSE                                                                 
002074       PERFORM IMS-GU-WDB201                                              
002075     END-IF                                                               
002076     MOVE GMT-IDPARTNR       TO W-WDB1-IDPARTNR                           
002077     MOVE GMT-IDFTG          TO W-WDB1-IDFTG                              
002078     PERFORM IMS-GU-WDB1-WDB101                                           
002079     IF SEGMENT-FINNS                                                     
002080       IF DIST79-DEALER-PRICE                                             
002081         MOVE BET-KDVALISO   TO MOD-KDVALISO                              
002082       ELSE                                                               
002083         MOVE 'SEK'          TO MOD-KDVALISO                              
002084       END-IF                                                             
002085     ELSE                                                                 
002086       MOVE SPACE            TO MOD-KDVALISO                              
002087     END-IF                                                               
002088     .                                                                    
002089     EJECT                                                                
002090 MFS-RENSA-FAELT-IN SECTION.                                              
002091                                                                          
002092*    --- ALLA INDATA-FÄLT                                                 
002093     MOVE MFS-RENSA-FAELT TO MOD-IDAVS-UPD                                
002094                             MOD-FLTABORT                                 
002095                             MOD-IDKOLLI-UPD                              
002096                             MOD-KDKOLLI-UPD                              
002097                             MOD-VKORDBTO-KOLLI-UPD                       
002098                             MOD-FLKLAR                                   
002099                             MOD-KDPRTVAL                                 
002100                                                                          
002101     MOVE +1 TO INDX                                                      
002102     PERFORM UNTIL INDX > MAX-INDX                                        
002103       MOVE MFS-RENSA-FAELT TO MOD-KDCMD (INDX)                           
002104       ADD +1 TO INDX                                                     
002105     END-PERFORM                                                          
002106     .                                                                    
002107     EJECT                                                                
002108 MFS-RENSA-FAELT-UT SECTION.                                              
002109                                                                          
002110*    --- ALLA UTDATA-FÄLT                                                 
002111*    --- INKL. BLÄDDRINGSNYCKLAR                                          
002112     MOVE MFS-RENSA-FAELT TO MOD-IDAVS                                    
002113                             MOD-DAREGDAT                                 
002114                             MOD-DARFSDAT                                 
002115                             MOD-IDKOLLI                                  
002116                             MOD-KDKOLLI                                  
002117                             MOD-VKORDBTO-KOLLI                           
002118                                                                          
002119     MOVE +1 TO INDX                                                      
002120     PERFORM UNTIL INDX > MAX-INDX                                        
002121       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
002122       ADD +1 TO INDX                                                     
002123     END-PERFORM                                                          
002124     .                                                                    
002125     SKIP3                                                                
002126 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
002127                                                                          
002128*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
002129     MOVE MFS-RENSA-FAELT TO MOD-IDKOLLI-RAD  (INDX)                      
002130                             MOD-IDARTNR  (INDX)                          
002131                             MOD-KVANTAL  (INDX)                          
002132                             MOD-PRARTBTO (INDX)                          
002133                             MOD-KDFEL    (INDX)                          
002134                             MOD-TENOTE   (INDX)                          
002135     .                                                                    
002136     SKIP3                                                                
002137 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
002138                                                                          
002139*    --- ALLA INDATA-FÄLT                                                 
002140     MOVE MFS-ROER-EJ-FAELT TO MOD-IDAVS-UPD                              
002141                               MOD-FLTABORT                               
002142                               MOD-IDKOLLI-UPD                            
002143                               MOD-KDKOLLI-UPD                            
002144                               MOD-VKORDBTO-KOLLI-UPD                     
002145                               MOD-FLKLAR                                 
002146                               MOD-KDPRTVAL                               
002147                                                                          
002148     MOVE +1 TO INDX                                                      
002149     PERFORM UNTIL INDX > MAX-INDX                                        
002150       MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD (INDX)                         
002151       ADD +1 TO INDX                                                     
002152     END-PERFORM                                                          
002153     .                                                                    
002154     EJECT                                                                
002155 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
002156                                                                          
002157*    --- ALLA UTDATA-FÄLT                                                 
002158*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
002159     MOVE MFS-ROER-EJ-FAELT TO MOD-IDAVS                                  
002160                               MOD-DAREGDAT                               
002161                               MOD-DARFSDAT                               
002162                               MOD-IDKOLLI                                
002163                               MOD-KDKOLLI                                
002164                               MOD-VKORDBTO-KOLLI                         
002165                                                                          
002166     MOVE +1 TO INDX                                                      
002167     PERFORM UNTIL INDX > MAX-INDX                                        
002168       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
002169       ADD +1 TO INDX                                                     
002170     END-PERFORM                                                          
002171     .                                                                    
002172     SKIP2                                                                
002173 MFS-ROER-EJ-RAD-FAELT-UT SECTION.                                        
002174                                                                          
002175*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
002176     MOVE MFS-ROER-EJ-FAELT TO MOD-IDKOLLI-RAD (INDX)                     
002177                               MOD-IDARTNR  (INDX)                        
002178                               MOD-KVANTAL  (INDX)                        
002179                               MOD-PRARTBTO (INDX)                        
002180                               MOD-KDFEL    (INDX)                        
002181                               MOD-TENOTE   (INDX)                        
002182     .                                                                    
002183     SKIP3                                                                
002184 MFS-STAENG-RAD-FAELT-UT  SECTION.                                        
002185                                                                          
002186*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
002187     MOVE MFS-STAENG-FAELT-NOMOD  TO MOD-IDARTNR-ATTR  (INDX)             
002188                                     MOD-KVANTAL-ATTR  (INDX)             
002189                                     MOD-PRARTBTO-ATTR (INDX)             
002190                                     MOD-KDFEL-ATTR    (INDX)             
002191                                     MOD-TENOTE-ATTR   (INDX)             
002192     .                                                                    
002193     SKIP3                                                                
002194 MFS-RENSA-FAELT-INPUT SECTION.                                           
002195                                                                          
002196*    --- ALLA INDATA-FÄLT                                                 
002197     MOVE MFS-RENSA-FAELT TO   MOD-IDAVS-UPD                              
002198                               MOD-IDKOLLI-UPD                            
002199                               MOD-KDKOLLI-UPD                            
002200                               MOD-VKORDBTO-KOLLI-UPD                     
002201                               MOD-FLTABORT                               
002202                               MOD-FLKLAR                                 
002203                               MOD-KDPRTVAL                               
002204                                                                          
002205     MOVE +1 TO INDX                                                      
002206     PERFORM UNTIL INDX > MAX-INDX                                        
002207       PERFORM MFS-RENSA-FAELT-RADER                                      
002208       ADD +1 TO INDX                                                     
002209     END-PERFORM                                                          
002210     .                                                                    
002211     EJECT                                                                
002212 MFS-RENSA-FAELT-RADER SECTION.                                           
002213                                                                          
002214*    --- ALLA INDATA-FÄLT                                                 
002215     MOVE MFS-RENSA-FAELT TO   MOD-KDCMD    (INDX)                        
002216                               MOD-IDARTNR  (INDX)                        
002217                               MOD-KVANTAL  (INDX)                        
002218                               MOD-PRARTBTO (INDX)                        
002219                               MOD-KDFEL    (INDX)                        
002220                               MOD-TENOTE   (INDX)                        
002221     .                                                                    
002222     EJECT                                                                
002223 MFS-FORM-ATTR SECTION.                                                   
002224                                                                          
002225**    --- ALLA INDATA-FÄLT                                                
002226     MOVE MFS-FORMATETS-ATTR TO MOD-IDAVS-ATTR                            
002227                                MOD-IDKOLLI-ATTR                          
002228                                MOD-KDKOLLI-ATTR                          
002229                                MOD-VKORDBTO-KOLLI-ATTR                   
002230                                MOD-FLTABORT-ATTR                         
002231                                MOD-FLKLAR-ATTR                           
002232                                MOD-KDPRTVAL-ATTR                         
002233                                                                          
002234     MOVE +1 TO INDX                                                      
002235     PERFORM UNTIL INDX > MAX-INDX                                        
002236       MOVE MFS-FORMATETS-ATTR TO MOD-KDCMD-ATTR   (INDX)                 
002237                                  MOD-IDARTNR-ATTR (INDX)                 
002238                                  MOD-KVANTAL-ATTR (INDX)                 
002239                                  MOD-PRARTBTO-ATTR(INDX)                 
002240                                  MOD-KDFEL-ATTR   (INDX)                 
002241                                  MOD-TENOTE-ATTR  (INDX)                 
002242       ADD +1 TO INDX                                                     
002243     END-PERFORM                                                          
002244     .                                                                    
002245     SKIP2                                                                
002246 MFS-LAES-IN-IGEN SECTION.                                                
002247                                                                          
002248*    --- ALLA INDATA-FÄLT                                                 
002249     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDAVS-ATTR                         
002250                                   MOD-IDKOLLI-ATTR                       
002251                                   MOD-KDKOLLI-ATTR                       
002252                                   MOD-VKORDBTO-KOLLI-ATTR                
002253                                   MOD-FLTABORT-ATTR                      
002254                                   MOD-FLKLAR-ATTR                        
002255                                   MOD-KDPRTVAL-ATTR                      
002256                                                                          
002257     MOVE +1 TO INDX                                                      
002258     PERFORM UNTIL INDX > MAX-INDX                                        
002259       PERFORM MFS-LAES-IN-IGEN-RAD-FAELT                                 
002260       ADD +1 TO INDX                                                     
002261     END-PERFORM                                                          
002262     .                                                                    
002263     SKIP3                                                                
002264 MFS-LAES-IN-IGEN-RAD-FAELT SECTION.                                      
002265                                                                          
002266*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
002267     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMD-ATTR   (INDX)                
002268                                   MOD-IDARTNR-ATTR (INDX)                
002269                                   MOD-KVANTAL-ATTR (INDX)                
002270                                   MOD-PRARTBTO-ATTR(INDX)                
002271                                   MOD-KDFEL-ATTR   (INDX)                
002272                                   MOD-TENOTE-ATTR  (INDX)                
002273     .                                                                    
002274                                                                          
002275     EJECT                                                                
002276* --- IMS SEKTIONER ---                                                   
002277     SKIP3                                                                
002278 IMS-GET-MSG SECTION.                                                     
002279                                                                          
002280     MOVE '  QC' TO GODK-STATUSKODER                                      
002281     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
002282     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
002283     PERFORM IMS-STATUSKONTROLL                                           
002284     .                                                                    
002285     SKIP3                                                                
002286 IMS-INSERT-MSG SECTION.                                                  
002287                                                                          
002288     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
002289     MOVE SPACE TO GODK-STATUSKODER                                       
002290     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
002291     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
002292     PERFORM IMS-STATUSKONTROLL                                           
002293     .                                                                    
002294     EJECT                                                                
002295 IMS-ISRT-MSG-ALT-4749 SECTION.                                           
002296                                                                          
002297     MOVE SPACE              TO GODK-STATUSKODER                          
002298     CALL CBLTDLI USING ISRT W4749-PCB P-TO-P-T49                         
002299     MOVE W4749-STATUS-CODE   TO STATUS-WS                                
002300     PERFORM IMS-STATUSKONTROLL                                           
002301     .                                                                    
002302     SKIP3                                                                
002303 IMS-ISRT-MSG-ALT-4799 SECTION.                                           
002304                                                                          
002305     MOVE SPACE              TO GODK-STATUSKODER                          
002306     CALL CBLTDLI USING ISRT W4799-PCB P-TO-P-T99                         
002307     MOVE W4799-STATUS-CODE   TO STATUS-WS                                
002308     PERFORM IMS-STATUSKONTROLL                                           
002309     .                                                                    
002310     SKIP3                                                                
002311 IMS-GU-4101-ROT SECTION.                                                 
002312                                                                          
002313     STRING 'WDR401  (WDGXKEY  =' W-WDGX4101-X ')'                        
002314          DELIMITED BY SIZE INTO SSA1                                     
002315     MOVE '  GE' TO GODK-STATUSKODER                                      
002316     CALL CBLTDLI USING GU 4101-PCB DLI-IO-WDGX4101 SSA1                  
002317     MOVE 4101-STATUS-CODE TO STATUS-WS                                   
002318     PERFORM IMS-STATUSKONTROLL                                           
002319     .                                                                    
002320     SKIP3                                                                
002321 IMS-ISRT-4101-ROT SECTION.                                               
002322                                                                          
002323     MOVE 'WDR401  ' TO SSA1                                              
002324     MOVE '  II' TO GODK-STATUSKODER                                      
002325     CALL CBLTDLI USING ISRT 4101-PCB DLI-IO-WDGX4101 SSA1                
002326     MOVE 4101-STATUS-CODE TO STATUS-WS                                   
002327     PERFORM IMS-STATUSKONTROLL                                           
002328     .                                                                    
002329     EJECT                                                                
002330 IMS-GNP-WDGX4102 SECTION.                                                
002331                                                                          
002332     STRING  'WDGX4102(KY4102  >=' W-WDGX4102-MIN-X                       
002333                     '&KY4102  <=' W-WDGX4102-MAX-X ')'                   
002334              DELIMITED BY SIZE INTO SSA1                                 
002335     MOVE '  GE' TO GODK-STATUSKODER                                      
002336     CALL CBLTDLI USING GNP 4101-PCB DLI-IO-WDGX4102 SSA1                 
002337     MOVE 4101-STATUS-CODE TO STATUS-WS                                   
002338     PERFORM IMS-STATUSKONTROLL                                           
002339     .                                                                    
002340     SKIP3                                                                
002341 IMS-GHNP-WDGX4102 SECTION.                                               
002342                                                                          
002343     STRING  'WDGX4102(KY4102  >=' W-WDGX4102-MIN-X                       
002344                     '&KY4102  <=' W-WDGX4102-MAX-X ')'                   
002345              DELIMITED BY SIZE INTO SSA1                                 
002346     MOVE '  GE' TO GODK-STATUSKODER                                      
002347     CALL CBLTDLI USING GHNP 4101-PCB DLI-IO-WDGX4102 SSA1                
002348     MOVE 4101-STATUS-CODE TO STATUS-WS                                   
002349     PERFORM IMS-STATUSKONTROLL                                           
002350     .                                                                    
002351     SKIP3                                                                
002352 IMS-GHNP-4102 SECTION.                                                   
002353                                                                          
002354     STRING 'WDGX4102(KY4102   =' W-WDGX4102-X ')'                        
002355          DELIMITED BY SIZE INTO SSA1                                     
002356     MOVE '  GE' TO GODK-STATUSKODER                                      
002357     CALL CBLTDLI USING GHNP 4101-PCB DLI-IO-WDGX4102 SSA1                
002358     MOVE 4101-STATUS-CODE TO STATUS-WS                                   
002359     PERFORM IMS-STATUSKONTROLL                                           
002360     .                                                                    
002361     SKIP3                                                                
002362 IMS-ISRT-WDGX4102 SECTION.                                               
002363                                                                          
002364     STRING 'WDR401  (WDGXKEY  =' W-WDGX4101-X ')'                        
002365          DELIMITED BY SIZE INTO SSA1                                     
002366     MOVE 'WDGX4102 ' TO SSA2                                             
002367     MOVE '  II' TO GODK-STATUSKODER                                      
002368     CALL CBLTDLI USING ISRT 4101-PCB DLI-IO-WDGX4102 SSA1 SSA2           
002369     MOVE 4101-STATUS-CODE TO STATUS-WS                                   
002370     PERFORM IMS-STATUSKONTROLL                                           
002371     .                                                                    
002372     SKIP3                                                                
002373 IMS-REPL-WDGX4102 SECTION.                                               
002374                                                                          
002375     MOVE '  ' TO GODK-STATUSKODER                                        
002376     CALL CBLTDLI USING REPL 4101-PCB DLI-IO-WDGX4102                     
002377     MOVE 4101-STATUS-CODE TO STATUS-WS                                   
002378     PERFORM IMS-STATUSKONTROLL                                           
002379     .                                                                    
002380     SKIP3                                                                
002381 IMS-DLET-WDGX4102 SECTION.                                               
002382                                                                          
002383     MOVE '  ' TO GODK-STATUSKODER                                        
002384     CALL CBLTDLI USING DLET 4101-PCB DLI-IO-WDGX4102                     
002385     MOVE 4101-STATUS-CODE TO STATUS-WS                                   
002386     PERFORM IMS-STATUSKONTROLL                                           
002387     .                                                                    
002388     EJECT                                                                
002389 IMS-GU-WDB201 SECTION.                                                   
002390                                                                          
002391     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
002392          DELIMITED BY SIZE INTO SSA1                                     
002393     MOVE '  GE' TO GODK-STATUSKODER                                      
002394     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
002395     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
002396     PERFORM IMS-STATUSKONTROLL                                           
002397     .                                                                    
002398     EJECT                                                                
002399 IMS-GU-WDB1-WDB101              SECTION.                                 
002400                                                                          
002401     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
002402          DELIMITED BY SIZE INTO SSA1                                     
002403     MOVE '  GE'              TO GODK-STATUSKODER                         
002404                                                                          
002405     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                    
002406     MOVE WDB1-STATUS-CODE    TO STATUS-WS                                
002407     PERFORM IMS-STATUSKONTROLL                                           
002408     .                                                                    
002409     EJECT                                                                
002410 IMS-GU-WDK601 SECTION.                                                   
002411                                                                          
002412     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
002413          DELIMITED BY SIZE INTO SSA1                                     
002414     MOVE '  GE' TO GODK-STATUSKODER                                      
002415     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
002416     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
002417     PERFORM IMS-STATUSKONTROLL                                           
002418     .                                                                    
002419     EJECT                                                                
002420 IMS-STATUSKONTROLL SECTION.                                              
002421                                                                          
002422     SET STATUS-IX TO 1                                                   
002423     SEARCH GODK-STATUS                                                   
002424       AT END                                                             
002425         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
002426         DELIMITED BY SIZE INTO FELTEXT                                   
002427         CALL FELLOG                                                      
002428       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
002429         CONTINUE                                                         
002430     END-SEARCH                                                           
002440     .                                                                    
