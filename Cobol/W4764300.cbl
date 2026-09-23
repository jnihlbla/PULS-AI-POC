000001 ID  DIVISION.                                                            
000002 PROGRAM-ID.    W4764300.                                                 
000003 AUTHOR.        CAMELIA OLGRENER.                                         
000004 DATE-WRITTEN.  JANUARI 2005.                                             
000005 DATE-COMPILED.                                                           
000006                                                                          
000007*    FUNKTION:                                                            
000008*        PROGRAMMET LÄSER WDR4, HTYP 4503, CH SKAPAR FIL TILL             
000009*        TRANSPORTÖR HIT FÖR SVERIGE, FINLAND DANMARK OCH NORGE           
000010*        GODS.                                                            
000011*                                                                         
000012*        PGM:ET LÄSER WDR4 OCH WDE6                                       
000013*                                                                         
000014* CAMELIA OLGRENER 130327                                                 
000015* ETRACKER 10189934 (EDI TILL HIT-DK)                                     
000016*                                                                         
000017* CAMELIA OLGRENER 151215                                                 
000018* ETRACKER 10271301 (EDI TILL HIT-NO)                                     
000019* HIT=POSTNORD                                                            
000020*                                                                         
000021* CAMELIA OLGRENER 160222                                                 
000022* ETRACKER 10272871 (EDI TILL HIT-NO, DDGS)                               
000023* HIT=POSTNORD                                                            
000024*                                                                         
000025* CAMELIA OLGRENER 181211                                                 
000026* JIRA 3000         (EDI TILL HIT-DK, LDC-1B)                             
000027* HIT=POSTNORD                                                            
000028*                                                                         
000029* CAMELIA OLGRENER 191016                                                 
000030* PBI 1540086 LDC-1B TILL DK BORT FRÅN URVAL TILL POSTNORD.               
000031* HIT=POSTNORD                                                            
000032*                                                                         
000033* CAMELIA OLGRENER 210930                                                 
000034* HIT=POSTNORD                                                            
000035* POSTNORD SKALL INTE HA NORGE LÄNGRE MEN KODEN ÄR INTE RENSAD            
000037* UTAN WWDIST83->DIST83-HIT-NO=9999 SÅ ATT DET INTE INTRÄFFAR             
000038* LÄNGRE.                                                                 
000039* OBS --> ATT RENSA VID TILLFÄLLE.                                        
000040*                                                                         
000041     EJECT                                                                
000042 ENVIRONMENT DIVISION.                                                    
000043     SKIP2                                                                
000044 INPUT-OUTPUT SECTION.                                                    
000045                                                                          
000046 FILE-CONTROL.                                                            
000047     SKIP2                                                                
000048*- - - - - - - - - - - - UTFIL:                                           
000049*          --- FIL TRANSP                                                 
000050     SELECT W47643A                      ASSIGN TO W47643D1.              
000051     SKIP2                                                                
000052*          --- FIL RENSNING                                               
000053     SELECT W47643B                      ASSIGN TO W47643D2.              
000054     SKIP2                                                                
000055 DATA DIVISION.                                                           
000056     SKIP2                                                                
000057 FILE SECTION.                                                            
000058 FD  W47643A                                                              
000059     RECORDING       V                                                    
000060     BLOCK CONTAINS  0.                                                   
000061                                                                          
000062 01  HIT-POST          PIC X(143).                                        
000063     EJECT                                                                
000064 FD  W47643B                                                              
000065     RECORDING       F                                                    
000066     BLOCK CONTAINS  0.                                                   
000067                                                                          
000068 01  RENS-POST.                                                           
000069*03   -COPY WDGX4504   -L.                                                
000070     SKIP2                                                                
000071 WORKING-STORAGE SECTION.                                                 
000072                                                                          
000073*    -- CHECKED BY WY2000                                                 
000074 77  PROGRAM-NAMN                PIC X(8) VALUE 'W4764300'.               
000075                                                                          
000076 77  IX                          PIC S9(9) COMP SYNC VALUE +0.            
000077 77  MAX-IX                      PIC S9(9) COMP SYNC VALUE +1000.         
000078 77  WS-SUORDV                   PIC S9(9)V9(2) VALUE +0 COMP-3.          
000079 77  WS-SUORDV-KOLLI             PIC S9(9)V9(2) VALUE +0 COMP-3.          
000080 77  WS-VKORDBTO                 PIC S9(6)V9(1) VALUE +0 COMP-3.          
000081 77  WS-VLORDBTO                 PIC S9(4)V9(3) VALUE +0 COMP-3.          
000082 77  WS-KVKOLLI                  PIC S9(5)      VALUE +0 COMP-3.          
000083 77  WS-SKEPPAT                  PIC 9(1)       VALUE 6.                  
000084 77  WS-IDLAND-SE                PIC X(02)      VALUE 'SE'.               
000085 77  WS-IDLAND-FI                PIC X(02)      VALUE 'FI'.               
000086 77  WS-IDLAND-DK                PIC X(02)      VALUE 'DK'.               
000087 77  WS-IDLAND-FO                PIC X(02)      VALUE 'FO'.               
000088 77  WS-IDLAND-NO                PIC X(02)      VALUE 'NO'.               
000089 77  WS-BUMPERS                  PIC X(07)      VALUE 'BUMPERS'.          
000090 77  WS-CLASSIC                  PIC X(07)      VALUE 'CLASSIC'.          
000091 77  WS-BRANDON                  PIC X(07)      VALUE 'BRANDON'.          
000092 77  WS-TYRES                    PIC X(07)      VALUE 'TYRES  '.          
000093 77  WS-SPARE-PARTS              PIC X(11)      VALUE                     
000094                                             'SPARE PARTS'.               
000095 77  WS-IDKUND-SE                PIC X(10)   VALUE '1245883002'.          
000096 77  WS-IDKUND-FI                PIC X(10)   VALUE '0205098825'.          
000097 77  WS-IDKUND-DK                PIC X(10)   VALUE '0206287070'.          
000098 77  WS-IDKUND-NO                PIC X(10)   VALUE '0206717191'.          
000099 77  WS-IDKUND-DDGS-NO           PIC X(10)   VALUE '0206721680'.          
000100 77  WS-IDKUND-SPEC-NO           PIC X(10)   VALUE '0003731569'.          
000101 77  WS-IDKUND-SPEC-DK           PIC X(10)   VALUE '0020010099'.          
000102                                                                          
000103                                                                          
000104 77  SKRIV-HEADER-SW             PIC X       VALUE 'J'.                   
000105     88 SKRIV-HEADER                         VALUE 'J'.                   
000106                                                                          
000107 01  ARBETSFALT.                                                          
000108                                                                          
000109     03 WS-POSTNR.                                                        
000110        05 WS-SIFFR-1               PIC X(1)  VALUE SPACE.                
000111        05 WS-SIFFR-2               PIC X(1)  VALUE SPACE.                
000112        05 WS-SIFFR-3               PIC X(1)  VALUE SPACE.                
000113        05 WS-SIFFR-4               PIC X(1)  VALUE SPACE.                
000114        05 WS-SIFFR-5               PIC X(1)  VALUE SPACE.                
000115        05 WS-SIFFR-6               PIC X(1)  VALUE SPACE.                
000116                                                                          
000117     03 WS-POSTNR-RED.                                                    
000118        05 WS-SIFFRA1               PIC X(1)  VALUE SPACE.                
000119        05 WS-SIFFRA2               PIC X(1)  VALUE SPACE.                
000120        05 WS-SIFFRA3               PIC X(1)  VALUE SPACE.                
000121        05 WS-SIFFRA4               PIC X(1)  VALUE SPACE.                
000122        05 WS-SIFFRA5               PIC X(1)  VALUE SPACE.                
000123                                                                          
000124     03 WS-KUNDREF.                                                       
000125        05 WS-IDDISTR               PIC 9(4)  VALUE ZERO.                 
000126        05 FILLER                   PIC X     VALUE '/'.                  
000127        05 WS-IDKUNDNR              PIC 9(6)  VALUE ZERO.                 
000128        05 FILLER                   PIC X     VALUE '/'.                  
000129        05 WS-IDORDNR               PIC 9(5)  VALUE ZERO.                 
000130        05 FILLER                   PIC X     VALUE '/'.                  
000131        05 WS-IDKOLLI               PIC 9(5)  VALUE ZERO.                 
000132        05 FILLER                   PIC X     VALUE SPACE.                
000133                                                                          
000134     03 WS-KUNDREF-DDGS.                                                  
000135        05 WS-DATUM-REF             PIC 9(8)  VALUE ZERO.                 
000136        05 FILLER                   PIC X     VALUE '/'.                  
000137        05 WS-IDKUNDNR-REF          PIC 9(6)  VALUE ZERO.                 
000138        05 FILLER                   PIC X     VALUE '/'.                  
000139        05 WS-TEXT-REF              PIC X(11) VALUE SPACE.                
000140        05 FILLER                   PIC X     VALUE SPACE.                
000141                                                                          
000142     03 WS-IDKLIID-DDGS.                                                  
000143        05 WS-IDDISTR-DDGS          PIC 9(4)  VALUE ZERO.                 
000144        05 WS-IDKUNDNR-DDGS         PIC 9(6)  VALUE ZERO.                 
000145        05 FILLER                   PIC X(2)  VALUE '00'.                 
000146        05 WS-IDORDNR-DDGS          PIC 9(5)  VALUE ZERO.                 
000147        05 WS-IDKOLLI-DDGS          PIC 9(5)  VALUE ZERO.                 
000148                                                                          
000149 01  FELTEXT.                                                             
000150     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
000151     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
000152     EJECT                                                                
000153******************************************************************        
000154*       CONSTANTS                                                *        
000155******************************************************************        
000156     SKIP2                                                                
000157 01  FILLER                      PIC X(16)   VALUE 'CONSTANTS '.          
000158 01  KONSTANTER.                                                          
000159     03  JA                      PIC X(1)    VALUE 'J'.                   
000160     03  NEJ                     PIC X(1)    VALUE 'N'.                   
000161     SKIP2                                                                
000162                                                                          
000163 01  SPAR-IDDISTR                PIC S9(5)  VALUE ZERO COMP-3.            
000164                                                                          
000165     SKIP3                                                                
000166******************************************************************        
000167*       VARIABLES                                                *        
000168******************************************************************        
000169     SKIP2                                                                
000170 01  FILLER                      PIC X(16)   VALUE 'VARIABLES'.           
000171 01  INFIL-EOF                   PIC X(1)    VALUE 'N'.                   
000172     SKIP3                                                                
000173     EJECT                                                                
000174******************************************************************        
000175*       WORK AREA                                                *        
000176******************************************************************        
000177     SKIP2                                                                
000178     EJECT                                                                
000179******************************************************************        
000180*       OUTPUT AREA                                                       
000181******************************************************************        
000182     SKIP2                                                                
000183 01  FILLER                 PIC X(16) VALUE 'HEADER-AREA '.               
000184 01  HEADER-AREA            PIC X(143) VALUE                              
000185     'HEADER2001445560743089                         HIT                  
000186-    '     5560423203                         PST GRP           HI        
000187-    'TPAK2               END'.                                           
000188     EJECT                                                                
000189 01  FILLER                 PIC X(16) VALUE 'HIT1-AREA '.                 
000190 01  HIT1-AREA.                                                           
000191*    03  -COPY W4764301   -PRE HIT1-.                                     
000192     EJECT                                                                
000193 01  FILLER                 PIC X(16) VALUE 'HIT2-AREA '.                 
000194 01  HIT2-AREA.                                                           
000195*    03  -COPY W4764302   -PRE HIT2-.                                     
000196     EJECT                                                                
000197*                                                                         
000198 01  FILLER                 PIC X(16) VALUE 'WWKUND13  '.                 
000199*01    -COPY WWKUND14                                                     
000200*                                                                         
000201 01  TEST-IDDISTR                PIC 9(5)    COMP-3.                      
000202                                                                          
000203 01     FILLER REDEFINES TEST-IDDISTR.                                    
000204*  03   -COPY WWDIST83.                                                   
000205     EJECT                                                                
000206*01    -COPY WWDC99                                                       
000207     EJECT                                                                
000208 01  DYNAMISKA-SUBPROGRAM.                                                
000209   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
000210   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
000211   03  ABEND                     PIC X(8)    VALUE 'ABEND   '.            
000212   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
000213     SKIP2                                                                
000214 01  RETURKODER.                                                          
000215     03  RKOD-ABEND-UTAN-DUMP    PIC S9(4) VALUE +16   COMP SYNC.         
000216     03  RKOD-ABEND-MED-DUMP     PIC S9(4) VALUE +1000 COMP SYNC.         
000217     SKIP2                                                                
000218*01   -COPY W0005       -PRE POSTSUM-.                                    
000219     EJECT                                                                
000220 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000221     SKIP3                                                                
000222 01  NYCKLAR-TILL-DLI.                                                    
000223                                                                          
000224   03  W-WDGXKEY-4503-X.                                                  
000225     05  W-IDHTYP-4503        PIC X(4)    VALUE '4503'.                   
000226     05  FILLER               PIC X(26)   VALUE LOW-VALUE.                
000227                                                                          
000228   03 W-IDPRODNR-X.                                                       
000229     05  W-IDPRODNR           PIC S9(7)   VALUE ZERO  COMP-3.             
000230                                                                          
000231   03 W-IDKOLLI-X.                                                        
000232     05  W-IDKOLLI            PIC S9(5)   VALUE ZERO  COMP-3.             
000233                                                                          
000234     SKIP2                                                                
000235*    --- STATUS-KOD FRÅN IMS                                              
000236 01  STATUS-WS                   PIC XX.                                  
000237     88  SEGMENT-FINNS                       VALUE '  '.                  
000238     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
000239     SKIP2                                                                
000240 01  GODK-STATUSKODER.                                                    
000241     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000242     SKIP3                                                                
000243 01  SSA1                        PIC X(64).                               
000244 01  SSA2                        PIC X(64).                               
000245     EJECT                                                                
000246*    --- IMS FUNKTIONSKODER                                               
000247*01  -COPY W0003                                                          
000248     EJECT                                                                
000249*    ---  DLI INPUT-OUTPUT AREA                                           
000250 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-GX01'.          
000251 01  DLI-IO-GX01.                                                         
000252*    03  -COPY WDGX01                                                     
000253     EJECT                                                                
000254 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-4504'.          
000255 01  DLI-IO-4504.                                                         
000256*    03  -COPY WDGX4504                                                   
000257     EJECT                                                                
000258 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDE611'.        
000259 01  DLI-IO-WDE611.                                                       
000260*    03  -COPY WDE611                                                     
000261     EJECT                                                                
000262 LINKAGE SECTION.                                                         
000263*01  -COPY W0008  -PRE WDE6-                                              
000264     05  FILLER                  PIC X.                                   
000265*01  -COPY W0008  -PRE 4503-                                              
000266     05  FILLER                  PIC X.                                   
000267     EJECT                                                                
000268 PROCEDURE DIVISION  USING WDE6-PCB 4503-PCB.                             
000269 MAIN SECTION.                                                            
000270     ENTRY 'DLITCBL' USING WDE6-PCB 4503-PCB.                             
000271     PERFORM A-INIT                                                       
000272                                                                          
000273     PERFORM IMS-GU-WDR401                                                
000274     PERFORM IMS-GNP-WDGX4504                                             
000275                                                                          
000276     PERFORM UNTIL SEGMENT-SAKNAS                                         
000277                                                                          
000278       MOVE 4504-IDPRODNR TO W-IDPRODNR                                   
000279       MOVE 4504-IDKOLLI  TO W-IDKOLLI                                    
000280       MOVE 4504-IDDISTR  TO TEST-IDDISTR                                 
000281       PERFORM IMS-GU-WDE611                                              
000282       IF SEGMENT-FINNS                                                   
000283         IF KOLLI-KDKOLSTA = WS-SKEPPAT OR                                
000284            KOLLI-KDKOLSTA > WS-SKEPPAT                                   
000285           IF DIST83-HIT-SE OR                                            
000286              DIST83-HIT-FI OR                                            
000287             (DIST83-HIT-NO AND 4504-IDKLIID > ZERO)                      
000288                            OR                                            
000289             (DIST83-HIT-DK AND 4504-IDKLIID > ZERO)                      
000290             PERFORM B-SKAPA-UTFIL                                        
000291           ELSE                                                           
000292**             FÖR ATT KUNNA SKILJA MELLAN VANLIGT OCH DDGS GODS          
000293**             FÖR NORGE --> DDGS-NO HAR ALLTID 0 I IDKLIID               
000294             IF (DIST83-HIT-NO AND 4504-IDKLIID  = ZERO)                  
000295               PERFORM C-SKAPA-UTFIL-DDGS-NO                              
000296             ELSE                                                         
000297**               FÖR ATT KUNNA SKILJA MELLAN VANLIGT OCH DDGS GODS        
000298**               FÖR DANMARK --> DDGS-DK HAR ALLTID 0 I IDKLIID           
000299               IF (DIST83-HIT-DK AND 4504-IDKLIID = ZERO)                 
000300                 PERFORM D-SKAPA-UTFIL-DDGS-DK                            
000301               END-IF                                                     
000302             END-IF                                                       
000303           END-IF                                                         
000304                                                                          
000305           PERFORM E-SKAPA-RENSNINGSFIL                                   
000306         END-IF                                                           
000307       END-IF                                                             
000308                                                                          
000309       PERFORM IMS-GNP-WDGX4504                                           
000310     END-PERFORM                                                          
000311                                                                          
000312     PERFORM Z-FINIT                                                      
000313                                                                          
000314     MOVE ZERO TO RETURN-CODE                                             
000315     GOBACK                                                               
000316     .                                                                    
000317     EJECT                                                                
000318 A-INIT SECTION.                                                          
000319                                                                          
000320     OPEN OUTPUT W47643A                                                  
000321                 W47643B                                                  
000322                                                                          
000323     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
000324     .                                                                    
000325     EJECT                                                                
000326                                                                          
000327 B-SKAPA-UTFIL SECTION.                                                   
000328                                                                          
000329     IF SKRIV-HEADER                                                      
000330       WRITE HIT-POST          FROM HEADER-AREA                           
000331       MOVE NEJ TO SKRIV-HEADER-SW                                        
000332     END-IF                                                               
000333                                                                          
000334     MOVE 'HI2'                  TO HIT1-IDPTYP                           
000335     MOVE 4504-IDKLIID           TO HIT1-IDKLIID                          
000336     MOVE WS-IDLAND-SE           TO HIT1-IDLANDX2                         
000337     MOVE FUNCTION CURRENT-DATE(1:8)                                      
000338                             TO HIT1-DAREGDAT                             
000339     MOVE 4504-TIHHMM            TO HIT1-TIHHMM                           
000340                                                                          
000341     MOVE 4504-IDGODS            TO HIT1-IDGODS                           
000342     MOVE 4504-IDDISTR           TO TEST-IDDISTR                          
000343     IF DIST83-HIT-FI                                                     
000344       MOVE WS-IDKUND-FI         TO HIT1-IDKUND                           
000345       MOVE WS-IDLAND-FI         TO HIT1-IDLANDX2-SORT                    
000346       MOVE 4504-ADGMT-PADR(1:5) TO HIT1-ADPOSTNR-005                     
000347       MOVE 48                   TO HIT1-KDPRODUKT                        
000348     ELSE                                                                 
000349       IF DIST83-HIT-DK                                                   
000350         MOVE WS-IDKUND-DK       TO HIT1-IDKUND                           
000351         MOVE WS-IDLAND-DK       TO HIT1-IDLANDX2-SORT                    
000352*                                                                         
000353         MOVE 4504-IDKUNDNR      TO KUND14-IDKUNDNR                       
000354         IF DIST83-HIT-DK-FO AND                                          
000355            KUND14-FAROARNA                                               
000356           MOVE WS-IDLAND-FO     TO HIT1-IDLANDX2-SORT                    
000357         END-IF                                                           
000358*                                                                         
000359         PERFORM S01-TA-FRAM-POSTNR                                       
000360         MOVE WS-POSTNR-RED      TO HIT1-ADPOSTNR-005                     
000361         MOVE 48                 TO HIT1-KDPRODUKT                        
000362       ELSE                                                               
000363         IF DIST83-HIT-NO                                                 
000364           MOVE WS-IDKUND-NO     TO HIT1-IDKUND                           
000365           MOVE WS-IDLAND-NO     TO HIT1-IDLANDX2-SORT                    
000366           PERFORM S01-TA-FRAM-POSTNR                                     
000367           MOVE WS-POSTNR-RED    TO HIT1-ADPOSTNR-005                     
000368           MOVE 48               TO HIT1-KDPRODUKT                        
000369         ELSE                                                             
000370           MOVE WS-IDKUND-SE     TO HIT1-IDKUND                           
000371           MOVE WS-IDLAND-SE     TO HIT1-IDLANDX2-SORT                    
000372           MOVE 48               TO HIT1-KDPRODUKT                        
000373           MOVE 06               TO HIT1-IDGODS                           
000374           PERFORM BB-TA-FRAM-POSTNR-SE                                   
000375           MOVE WS-POSTNR-RED    TO HIT1-ADPOSTNR-005                     
000376         END-IF                                                           
000377       END-IF                                                             
000378     END-IF                                                               
000379                                                                          
000380     MOVE 4504-ADFLGEO           TO HIT1-ADFLGEO                          
000381     MOVE 4504-VKORDBTO-KOLLI    TO HIT1-VKORDBTO-KOLLI                   
000382     MOVE 4504-VLORDBTO-KOLLI    TO HIT1-VLORDBTO-KOLLI                   
000383     MOVE 4504-IDKUNDNR          TO HIT1-IDKUNDNR                         
000384                                                                          
000385     MOVE ZERO                   TO HIT1-KDPOD                            
000386                                                                          
000387     MOVE 4504-IDDISTR           TO WS-IDDISTR                            
000388     MOVE 4504-IDKUNDNR          TO WS-IDKUNDNR                           
000389     MOVE 4504-IDORDNR5          TO WS-IDORDNR                            
000390     MOVE 4504-IDKOLLI           TO WS-IDKOLLI                            
000391     MOVE WS-KUNDREF             TO HIT1-KUNDREFERENS                     
000392                                                                          
000393     WRITE HIT-POST            FROM HIT1-AREA                             
000394     MOVE 'W47643A'              TO POSTSUM-FDNAMN                        
000395     MOVE 'W47643D1'             TO POSTSUM-DDNAMN2                       
000396     MOVE 'HIT1'                 TO POSTSUM-TRANSTYP                      
000397     SKIP2                                                                
000398     CALL POSTSUM                USING  POSTSUM-PARM                      
000399     .                                                                    
000400     EJECT                                                                
000401 BB-TA-FRAM-POSTNR-SE SECTION.                                            
000402                                                                          
000403* POSTNR-ET I SE ÄR 2+SPACE+3 POSITIONER --> SPACE SKALL TAS BORT         
000404                                                                          
000405     MOVE 4504-ADGMT-PADR        TO WS-POSTNR                             
000406                                                                          
000407     MOVE WS-SIFFR-1             TO WS-SIFFRA1                            
000408     MOVE WS-SIFFR-2             TO WS-SIFFRA2                            
000409     MOVE WS-SIFFR-3             TO WS-SIFFRA3                            
000410     MOVE WS-SIFFR-5             TO WS-SIFFRA4                            
000411     MOVE WS-SIFFR-6             TO WS-SIFFRA5                            
000412     .                                                                    
000413     EJECT                                                                
000414 C-SKAPA-UTFIL-DDGS-NO SECTION.                                           
000415                                                                          
000416     IF SKRIV-HEADER                                                      
000417       WRITE HIT-POST          FROM HEADER-AREA                           
000418       MOVE NEJ TO SKRIV-HEADER-SW                                        
000419     END-IF                                                               
000420                                                                          
000421     MOVE 'HI2'                  TO HIT2-IDPTYP                           
000422     MOVE 4504-IDDISTR           TO WS-IDDISTR-DDGS                       
000423     MOVE 4504-IDKUNDNR          TO WS-IDKUNDNR-DDGS                      
000424     MOVE 4504-IDORDNR5          TO WS-IDORDNR-DDGS                       
000425     MOVE 4504-IDKOLLI           TO WS-IDKOLLI-DDGS                       
000426     MOVE WS-IDKLIID-DDGS        TO HIT2-IDKLIID-DDGS                     
000427     MOVE FUNCTION CURRENT-DATE(1:8)                                      
000428                                 TO HIT2-DAREGDAT                         
000429     MOVE 4504-TIHHMM            TO HIT2-TIHHMM                           
000430                                                                          
000431     MOVE KOLLI-IDDC             TO WS-IDDC                               
000432     IF LDC-NO-3J                                                         
000433       MOVE WS-IDKUND-SPEC-NO    TO HIT2-IDKUND                           
000434       MOVE 4504-IDORDNR7        TO WS-IDORDNR-DDGS                       
000435       MOVE WS-IDKLIID-DDGS      TO HIT2-IDKLIID-DDGS                     
000436     ELSE                                                                 
000437       MOVE WS-IDKUND-DDGS-NO    TO HIT2-IDKUND                           
000438     END-IF                                                               
000439                                                                          
000440     MOVE WS-IDLAND-NO           TO HIT2-IDLANDX2-SORT                    
000441     PERFORM S01-TA-FRAM-POSTNR                                           
000442     MOVE WS-POSTNR-RED          TO HIT2-ADPOSTNR-005                     
000443     MOVE 48                     TO HIT2-KDPRODUKT                        
000444                                                                          
000445     MOVE 4504-ADFLGEO           TO HIT2-ADFLGEO                          
000446     MOVE 4504-VKORDBTO-KOLLI    TO HIT2-VKORDBTO-KOLLI                   
000447     MOVE 4504-VLORDBTO-KOLLI    TO HIT2-VLORDBTO-KOLLI                   
000448     MOVE 4504-IDGODS            TO HIT2-IDGODS                           
000449     MOVE 4504-IDKUNDNR          TO HIT2-IDKUNDNR                         
000450                                                                          
000451     MOVE ZERO                   TO HIT2-KDPOD                            
000452                                                                          
000453     MOVE SPACE                  TO HIT2-KUNDREFERENS                     
000454                                    WS-TEXT-REF                           
000455     MOVE FUNCTION CURRENT-DATE(1:8)                                      
000456                                 TO WS-DATUM-REF                          
000457     MOVE 4504-IDKUNDNR          TO WS-IDKUNDNR-REF                       
000458                                                                          
000459     IF LDC-NO-3J                                                         
000460       MOVE WS-SPARE-PARTS       TO WS-TEXT-REF                           
000461     ELSE                                                                 
000462       IF KOLLI-IDLEVNR = 'AD0VV' OR 'AEBR5' OR 'AEO7U'                   
000463         MOVE WS-TYRES           TO WS-TEXT-REF                           
000464       END-IF                                                             
000465       IF KOLLI-IDLEVNR = 'BQ8VA'                                         
000466         MOVE WS-CLASSIC         TO WS-TEXT-REF                           
000467       END-IF                                                             
000468       IF KOLLI-IDLEVNR = 'BP3EA' OR 'BP7YA' OR 'AEF31'                   
000469         MOVE WS-BUMPERS         TO WS-TEXT-REF                           
000470       END-IF                                                             
000471       IF KOLLI-IDLEVNR = 'CWZYA'                                         
000472         MOVE WS-BRANDON         TO WS-TEXT-REF                           
000473       END-IF                                                             
000480     END-IF                                                               
000490                                                                          
000491     MOVE WS-KUNDREF-DDGS        TO HIT2-KUNDREFERENS                     
000492                                                                          
000493     WRITE HIT-POST            FROM HIT2-AREA                             
000494     MOVE 'W47643A'              TO POSTSUM-FDNAMN                        
000495     MOVE 'W47643D1'             TO POSTSUM-DDNAMN2                       
000496     MOVE 'HIT1'                 TO POSTSUM-TRANSTYP                      
000497     SKIP2                                                                
000498     CALL POSTSUM                USING  POSTSUM-PARM                      
000499     .                                                                    
000500     EJECT                                                                
000501 D-SKAPA-UTFIL-DDGS-DK SECTION.                                           
000502                                                                          
000503     IF SKRIV-HEADER                                                      
000504       WRITE HIT-POST          FROM HEADER-AREA                           
000505       MOVE NEJ TO SKRIV-HEADER-SW                                        
000506     END-IF                                                               
000507                                                                          
000508     MOVE 'HI2'                  TO HIT2-IDPTYP                           
000509     MOVE 4504-IDDISTR           TO WS-IDDISTR-DDGS                       
000510     MOVE 4504-IDKUNDNR          TO WS-IDKUNDNR-DDGS                      
000511     MOVE 4504-IDORDNR5          TO WS-IDORDNR-DDGS                       
000512     MOVE 4504-IDKOLLI           TO WS-IDKOLLI-DDGS                       
000513     MOVE WS-IDKLIID-DDGS        TO HIT2-IDKLIID-DDGS                     
000514     MOVE FUNCTION CURRENT-DATE(1:8)                                      
000515                                 TO HIT2-DAREGDAT                         
000516     MOVE 4504-TIHHMM            TO HIT2-TIHHMM                           
000517                                                                          
000518     MOVE KOLLI-IDDC             TO WS-IDDC                               
000519     MOVE WS-IDKUND-DK           TO HIT2-IDKUND                           
000520                                                                          
000521     MOVE WS-IDLAND-DK           TO HIT2-IDLANDX2-SORT                    
000522     PERFORM S01-TA-FRAM-POSTNR                                           
000523     MOVE WS-POSTNR-RED          TO HIT2-ADPOSTNR-005                     
000524     MOVE 48                     TO HIT2-KDPRODUKT                        
000525                                                                          
000526     MOVE 4504-ADFLGEO           TO HIT2-ADFLGEO                          
000527     MOVE 4504-VKORDBTO-KOLLI    TO HIT2-VKORDBTO-KOLLI                   
000528     MOVE 4504-VLORDBTO-KOLLI    TO HIT2-VLORDBTO-KOLLI                   
000529     MOVE 4504-IDGODS            TO HIT2-IDGODS                           
000530     MOVE 4504-IDKUNDNR          TO HIT2-IDKUNDNR                         
000531                                                                          
000532     MOVE ZERO                   TO HIT2-KDPOD                            
000533                                                                          
000534     MOVE SPACE                  TO HIT2-KUNDREFERENS                     
000535                                    WS-TEXT-REF                           
000536     MOVE FUNCTION CURRENT-DATE(1:8)                                      
000537                                 TO WS-DATUM-REF                          
000538     MOVE 4504-IDKUNDNR          TO WS-IDKUNDNR-REF                       
000539                                                                          
000540     IF KOLLI-IDLEVNR = 'AD0VV' OR 'AEBR5'                                
000541       MOVE WS-TYRES             TO WS-TEXT-REF                           
000542     END-IF                                                               
000543     IF KOLLI-IDLEVNR = 'BQ8VA'                                           
000544       MOVE WS-CLASSIC           TO WS-TEXT-REF                           
000545     END-IF                                                               
000546     IF KOLLI-IDLEVNR = 'BP3EA' OR 'BP7YA' OR 'AEF31'                     
000547       MOVE WS-BUMPERS           TO WS-TEXT-REF                           
000548     END-IF                                                               
000549     IF KOLLI-IDLEVNR = 'CWZYA'                                           
000550       MOVE WS-BRANDON           TO WS-TEXT-REF                           
000551     END-IF                                                               
000552                                                                          
000553     MOVE WS-KUNDREF-DDGS        TO HIT2-KUNDREFERENS                     
000554                                                                          
000555     WRITE HIT-POST            FROM HIT2-AREA                             
000556     MOVE 'W47643A'              TO POSTSUM-FDNAMN                        
000557     MOVE 'W47643D1'             TO POSTSUM-DDNAMN2                       
000558     MOVE 'HIT1'                 TO POSTSUM-TRANSTYP                      
000559     SKIP2                                                                
000560     CALL POSTSUM                USING  POSTSUM-PARM                      
000561     .                                                                    
000562     EJECT                                                                
000563 E-SKAPA-RENSNINGSFIL SECTION.                                            
000564                                                                          
000565     WRITE RENS-POST         FROM DLI-IO-4504                             
000566     MOVE 'W47643B'            TO POSTSUM-FDNAMN                          
000567     MOVE 'W47643D2'           TO POSTSUM-DDNAMN2                         
000568     MOVE 'RENS'               TO POSTSUM-TRANSTYP                        
000569     SKIP2                                                                
000570     CALL POSTSUM              USING  POSTSUM-PARM                        
000571     .                                                                    
000572     EJECT                                                                
000573 S01-TA-FRAM-POSTNR SECTION.                                              
000574                                                                          
000575* POSTNR-ET I DK OCH NO ÄR 4 POSITIONER --> SKALL INLEDAS MED 0           
000576* FÄRÖARNA HAR BARA 3 POSITIONER --> SKALL INLEDAS MED 2 NOLLOR           
000577                                                                          
000578     MOVE ZERO                   TO WS-POSTNR                             
000579     MOVE 4504-ADGMT-PADR        TO WS-POSTNR                             
000580                                                                          
000581     MOVE ZERO                   TO WS-SIFFRA1                            
000582     MOVE WS-SIFFR-1             TO WS-SIFFRA2                            
000583     MOVE WS-SIFFR-2             TO WS-SIFFRA3                            
000584     MOVE WS-SIFFR-3             TO WS-SIFFRA4                            
000585     MOVE WS-SIFFR-4             TO WS-SIFFRA5                            
000586                                                                          
000587     IF WS-SIFFRA5 = SPACE                                                
000588       MOVE ZERO                 TO WS-SIFFRA1                            
000589                                    WS-SIFFRA2                            
000590       MOVE WS-SIFFR-1           TO WS-SIFFRA3                            
000591       MOVE WS-SIFFR-2           TO WS-SIFFRA4                            
000592       MOVE WS-SIFFR-3           TO WS-SIFFRA5                            
000593     END-IF                                                               
000594     .                                                                    
000595     EJECT                                                                
000596 Z-FINIT SECTION.                                                         
000597                                                                          
000598     MOVE 'S' TO POSTSUM-OPKOD                                            
000599     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
000600                                                                          
000601     CLOSE W47643A                                                        
000602           W47643B                                                        
000603     .                                                                    
000604     EJECT                                                                
000605*****IMS-LÄSNINGAR******                                                  
000606                                                                          
000607 IMS-GU-WDR401  SECTION.                                                  
000608                                                                          
000609     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-4503-X ')'                    
000610          DELIMITED BY SIZE INTO SSA1                                     
000611     MOVE '  ' TO GODK-STATUSKODER                                        
000612     CALL CBLTDLI USING GU 4503-PCB DLI-IO-GX01 SSA1                      
000613     MOVE 4503-STATUS-CODE TO STATUS-WS                                   
000614     PERFORM IMS-STATUSKONTROLL                                           
000615     .                                                                    
000616     SKIP2                                                                
000617 IMS-GNP-WDGX4504 SECTION.                                                
000618                                                                          
000619     MOVE 'WDGX4504' TO SSA1                                              
000620     MOVE '  GE' TO GODK-STATUSKODER                                      
000621     CALL CBLTDLI USING GNP 4503-PCB DLI-IO-4504 SSA1                     
000622     MOVE 4503-STATUS-CODE TO STATUS-WS                                   
000623     PERFORM IMS-STATUSKONTROLL                                           
000624     .                                                                    
000625     EJECT                                                                
000626 IMS-GU-WDE611  SECTION.                                                  
000627     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
000628            DELIMITED BY SIZE INTO SSA1                                   
000629     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
000630            DELIMITED BY SIZE INTO SSA2                                   
000631     MOVE '  GE' TO GODK-STATUSKODER                                      
000632     CALL CBLTDLI USING GU    WDE6-PCB DLI-IO-WDE611 SSA1 SSA2            
000633     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
000634     PERFORM IMS-STATUSKONTROLL                                           
000635     .                                                                    
000636     EJECT                                                                
000637                                                                          
000638 IMS-STATUSKONTROLL SECTION.                                              
000639     SET STATUS-IX TO 1                                                   
000640     SEARCH GODK-STATUS                                                   
000641       AT END                                                             
000642         CALL FELLOG                                                      
000643       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
000644         CONTINUE                                                         
000645     END-SEARCH                                                           
000646     .                                                                    
000647     EJECT                                                                
