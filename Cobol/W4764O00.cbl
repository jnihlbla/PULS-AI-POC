000001 ID  DIVISION.                                                            
000002 PROGRAM-ID.    W4764O00.                                                 
000003 AUTHOR.        CAMELIA OLGRENER.                                         
000004 DATE-WRITTEN.  AUGUSTI 2020.                                             
000005 DATE-COMPILED.                                                           
000006                                                                          
000007*    FUNKTION:                                                            
000008*        PROGRAMMET STARTAS FRÅN MPP 4632                                 
000009*        VIA SOP-RUTIN W476SO                                             
000010*        IDSHIPM ÄR PARAMETER IN                                          
000011*                                                                         
000012*        PGM:ET LÄSER WDE1, WDE2, WDB2 OCH WDQ2                           
000013*                                                                         
000014*        SKAPAR FIL MED INFO TILL DIV. TRANSPORTÖRER                      
000015*                                                                         
000016*    STORY 3421774  23/08-17 NEW SELECTION NEOVIA, TRUCK&WHEEL            
000017*                                                                         
000018     EJECT                                                                
000019 ENVIRONMENT DIVISION.                                                    
000020     SKIP2                                                                
000021 INPUT-OUTPUT SECTION.                                                    
000022                                                                          
000030 FILE-CONTROL.                                                            
000031     SKIP2                                                                
000032*- - - - - - - - - - - - INFIL:                                           
000033     SELECT W476IN                       ASSIGN TO W4764OD1.              
000034*- - - - - - - - - - - - UTFIL:                                           
000035*          --- FIL TRANSP                                                 
000036     SELECT W4764O                       ASSIGN TO W4764OD2.              
000037     SKIP2                                                                
000038 DATA DIVISION.                                                           
000039     SKIP2                                                                
000040 FILE SECTION.                                                            
000041 FD  W476IN                                                               
000042     RECORDING      F                                                     
000043     BLOCK CONTAINS 0.                                                    
000044 01  PARM            PIC X(80).                                           
000045 FD  W4764O                                                               
000046     RECORDING       V                                                    
000047     BLOCK CONTAINS  0.                                                   
000048                                                                          
000049 01  UT-TRP-HUV.                                                          
000050*03   -COPY W4764O01   -L.                                                
000051     SKIP2                                                                
000052 01  UT-TRP-KND.                                                          
000053*03   -COPY W4764O11   -L.                                                
000054     SKIP2                                                                
000055 01  UT-TRP-KLI.                                                          
000056*03   -COPY W4764O21   -L.                                                
000057     SKIP2                                                                
000058 01  UT-TRP-RAD.                                                          
000059*03   -COPY W4764O31   -L.                                                
000060     SKIP2                                                                
000061 WORKING-STORAGE SECTION.                                                 
000062                                                                          
000063*    -- CHECKED BY WY2000                                                 
000064 77  PROGRAM-NAMN                PIC X(8) VALUE 'W4764O00'.               
000065                                                                          
000066 77  IX                          PIC S9(9) COMP SYNC VALUE +0.            
000067 77  MAX-IX                      PIC S9(9) COMP SYNC VALUE +1000.         
000068 77  WS-SUORDV                   PIC S9(9)V9(2) VALUE +0 COMP-3.          
000069 77  WS-SUORDV-KOLLI             PIC S9(9)V9(2) VALUE +0 COMP-3.          
000070 77  WS-VKORDBTO                 PIC S9(6)V9(1) VALUE +0 COMP-3.          
000071 77  WS-VLORDBTO                 PIC S9(4)V9(3) VALUE +0 COMP-3.          
000072 77  WS-KVKOLLI                  PIC S9(5)      VALUE +0 COMP-3.          
000073 77  WS-VKORDBTO-KND             PIC S9(6)V9(1) VALUE +0 COMP-3.          
000074 77  WS-VKARTNTO-KND             PIC S9(6)V9(3) VALUE +0 COMP-3.          
000075 77  WS-VLORDBTO-KND             PIC S9(4)V9(3) VALUE +0 COMP-3.          
000076 77  WS-KVKOLLI-KND              PIC S9(5)      VALUE +0 COMP-3.          
000077 77  FOREG-IDORDER               PIC S9(7)      VALUE +0 COMP-3.          
000078 77  WS-BEGMT-RAD1               PIC X(35)      VALUE SPACE.              
000079 77  WS-BEGMT-RAD2               PIC X(35)      VALUE SPACE.              
000080 77  WS-ADGMT-GATA               PIC X(35)      VALUE SPACE.              
000081 77  WS-ADGMT-PADR               PIC X(35)      VALUE SPACE.              
000082 77  WS-ADGMT-LAND               PIC X(35)      VALUE SPACE.              
000083 77  WS-BEKUNDRF                 PIC X(15)      VALUE SPACE.              
000084                                                                          
000085 77  FL-BCUBE                    PIC X(1)    VALUE 'N'.                   
000086 77  FL-TRUCKWHEEL               PIC X(1)    VALUE 'N'.                   
000087 77  FL-NEOVIA                   PIC X(1)    VALUE 'N'.                   
000088 77  FL-JAPAN                    PIC X(1)    VALUE 'N'.                   
000089                                                                          
000090 77  SKRIV-POST-SW               PIC X       VALUE 'N'.                   
000091     88 SKRIV-POST                           VALUE 'J'.                   
000092                                                                          
000093 77  SKRIV-POST-H-SW             PIC X       VALUE 'N'.                   
000094     88 SKRIV-HUV-POST                       VALUE 'J'.                   
000095                                                                          
000096 77  SKRIV-POST-K-SW             PIC X       VALUE 'N'.                   
000097     88 SKRIV-KND-POST                       VALUE 'J'.                   
000098                                                                          
000099 77  INFIL-EOF-SW                PIC X    VALUE 'N'.                      
000100     88  END-OF-W476IN                    VALUE 'J'.                      
000101                                                                          
000102 01  FELTEXT.                                                             
000103     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
000104     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
000105     EJECT                                                                
000106*- - - - - - - - - - - - - -                                              
000107*      --- VALID IDDC CODES                                               
000108*                                                                         
000109*01    -COPY WWDC99                                                       
000110 01  TEST-IDDISTR     PIC S9(5)        VALUE ZERO  COMP-3.                
000111*01  FILLER     -COPY WWDIST87   -RED  TEST-IDDISTR.                      
000112     EJECT                                                                
000113*01  FILLER     -COPY WWDIST79   -RED  TEST-IDDISTR.                      
000114     EJECT                                                                
000115******************************************************************        
000116*       CONSTANTS                                                *        
000117******************************************************************        
000118     SKIP2                                                                
000119 01  FILLER                      PIC X(16)   VALUE 'CONSTANTS '.          
000120 01  KONSTANTER.                                                          
000121     03  JA                      PIC X(1)    VALUE 'J'.                   
000122     03  NEJ                     PIC X(1)    VALUE 'N'.                   
000123     SKIP2                                                                
000124                                                                          
000125 01  PARM-AREA                   PIC X(80)   VALUE SPACE.                 
000126 01  FILLER                      REDEFINES PARM-AREA.                     
000127     03  WS-IDSHIPM              PIC 9(7).                                
000128                                                                          
000129 01  SPAR-IDDISTR                PIC S9(5)  VALUE ZERO COMP-3.            
000130                                                                          
000131     SKIP3                                                                
000132******************************************************************        
000133*       VARIABLES                                                *        
000134******************************************************************        
000135     SKIP2                                                                
000136 01  FILLER                      PIC X(16)   VALUE 'VARIABLES'.           
000137 01  INFIL-EOF                   PIC X(1)    VALUE 'N'.                   
000138     SKIP3                                                                
000139     EJECT                                                                
000140******************************************************************        
000141*       WORK AREA                                                *        
000142******************************************************************        
000143     SKIP2                                                                
000144     EJECT                                                                
000145******************************************************************        
000146*       OUTPUT AREA                                                       
000147******************************************************************        
000148     SKIP2                                                                
000149 01  FILLER                 PIC X(16) VALUE 'OUTPUT TRANSP '.             
000150                                                                          
000151 01  ARB-UTAREA             PIC X(250).                                   
000152                                                                          
000153 01  TRP-HUVUD     REDEFINES  ARB-UTAREA.                                 
000154*    03  -COPY W4764O01   -PRE UT-.                                       
000155     EJECT                                                                
000156 01  TRP-KUND      REDEFINES  ARB-UTAREA.                                 
000157*    03  -COPY W4764O11   -PRE UT-.                                       
000158     EJECT                                                                
000159 01  TRP-KOLLI     REDEFINES  ARB-UTAREA.                                 
000160*    03  -COPY W4764O21   -PRE UT-.                                       
000161*                                                                         
000162 01  TRP-RAD       REDEFINES  ARB-UTAREA.                                 
000163*    03  -COPY W4764O31   -PRE UT-.                                       
000164*                                                                         
000165     EJECT                                                                
000166 01  FILLER                 PIC X(16) VALUE 'SPARAREA      '.             
000167                                                                          
000168 01  SPAR-UTAREA-H          PIC X(160).                                   
000169                                                                          
000170 01  SPAR-TRP-HUV  REDEFINES  SPAR-UTAREA-H.                              
000171*    03  -COPY W4764O01   -PRE SPAR-.                                     
000172     EJECT                                                                
000173 01  SPAR-UTAREA-K          PIC X(160).                                   
000174                                                                          
000175 01  SPAR-TRP-KND  REDEFINES  SPAR-UTAREA-K.                              
000176*    03  -COPY W4764O11   -PRE SPAR-.                                     
000177     EJECT                                                                
000178 01  DYNAMISKA-SUBPROGRAM.                                                
000179   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
000180   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
000181   03  ABEND                     PIC X(8)    VALUE 'ABEND   '.            
000182   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
000183     SKIP2                                                                
000184 01  RETURKODER.                                                          
000185     03  RKOD-ABEND-UTAN-DUMP    PIC S9(4) VALUE +16   COMP SYNC.         
000186     03  RKOD-ABEND-MED-DUMP     PIC S9(4) VALUE +1000 COMP SYNC.         
000187     SKIP2                                                                
000188*01   -COPY W0005       -PRE POSTSUM-.                                    
000189     EJECT                                                                
000190 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000191     SKIP3                                                                
000192 01  NYCKLAR-TILL-DLI.                                                    
000193     03  W-IDSHIPM-X.                                                     
000194         05  W-IDSHIPM           PIC  9(7)   VALUE ZERO.                  
000195                                                                          
000196     03  W-WDE211KY-X.                                                    
000197         05  W-IDDISTR-E2        PIC S9(5)   VALUE ZERO COMP-3.           
000198         05  W-IDKUNDNR-E2       PIC S9(7)   VALUE ZERO COMP-3.           
000199     03  W-WDE221KY-X.                                                    
000200         05  W-IDPRODNR-E2       PIC S9(7)   VALUE ZERO COMP-3.           
000201         05  W-IDKOLLI-E2        PIC S9(5)   VALUE ZERO COMP-3.           
000202                                                                          
000203     03  W-IDPURAD-X.                                                     
000204         05  W-IDPURAD           PIC S9(5)   VALUE ZERO COMP-3.           
000205                                                                          
000206     03  W-WDE111KY-X.                                                    
000207         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
000208         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
000209     03  W-WDE121KY-X.                                                    
000210         05  W-IDPRODNR          PIC S9(7)   VALUE ZERO COMP-3.           
000211         05  W-IDKOLLI           PIC S9(5)   VALUE ZERO COMP-3.           
000212                                                                          
000213     03  W-IDGMT-X.                                                       
000214         05  W-IDDISTR-B2        PIC S9(5)   VALUE ZERO  COMP-3.          
000215         05  W-IDKUNDNR-B2       PIC S9(7)   VALUE ZERO  COMP-3.          
000216                                                                          
000217    03  W-IDORDER-X.                                                      
000218      05  W-IDORDER              PIC S9(7)   VALUE ZERO  COMP-3.          
000219                                                                          
000220     SKIP2                                                                
000221*    --- STATUS-KOD FRÅN IMS                                              
000222 01  STATUS-WS                   PIC XX.                                  
000223     88  SEGMENT-FINNS                       VALUE '  '.                  
000224     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
000225     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
000226     88  SEGMENT-SLUT                        VALUE 'GB'.                  
000227     SKIP2                                                                
000228 01  GODK-STATUSKODER.                                                    
000229     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000230     SKIP3                                                                
000231 01  SSA1                        PIC X(64).                               
000232 01  SSA2                        PIC X(64).                               
000233 01  SSA3                        PIC X(64).                               
000234 01  SSA4                        PIC X(64).                               
000235     EJECT                                                                
000236*    --- IMS FUNKTIONSKODER                                               
000237*01  -COPY W0003                                                          
000238     EJECT                                                                
000239*    ---  DLI INPUT-OUTPUT AREA                                           
000240 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDE101'.         
000241 01  DLI-IO-WDE101.                                                       
000242*    03  -COPY WDE101                                                     
000243     EJECT                                                                
000244 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDE111'.         
000245 01  DLI-IO-WDE111.                                                       
000246*    03  -COPY WDE111                                                     
000247     EJECT                                                                
000248 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDE121'.         
000249 01  DLI-IO-WDE121.                                                       
000250*    03  -COPY WDE121                                                     
000251     EJECT                                                                
000252 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDE131'.         
000253 01  DLI-IO-WDE131.                                                       
000254*    03  -COPY WDE131                                                     
000255     EJECT                                                                
000256 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDE231'.         
000257 01  DLI-IO-WDE231.                                                       
000258*    03  -COPY WDE231                                                     
000259     EJECT                                                                
000260 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDB201'.         
000261 01  DLI-IO-WDB201.                                                       
000262*    03  -COPY WDB201                                                     
000263     EJECT                                                                
000264 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDQ201'.         
000265 01  DLI-IO-WDQ201.                                                       
000266*    03  -COPY WDQ201                                                     
000267     EJECT                                                                
000268 LINKAGE SECTION.                                                         
000269*01  -COPY W0008  -PRE WDE1-                                              
000270     05  FILLER                  PIC X.                                   
000271*01  -COPY W0008  -PRE WDE2-                                              
000272     05  FILLER                  PIC X.                                   
000273*01  -COPY W0008  -PRE WDB2-                                              
000274     05  FILLER                  PIC X.                                   
000275*01  -COPY W0008  -PRE WDQ2-                                              
000276     05  FILLER                  PIC X.                                   
000277     EJECT                                                                
000278 PROCEDURE DIVISION  USING WDE1-PCB WDE2-PCB                              
000279                           WDB2-PCB WDQ2-PCB.                             
000280 MAIN SECTION.                                                            
000281     ENTRY 'DLITCBL' USING WDE1-PCB WDE2-PCB                              
000282                           WDB2-PCB WDQ2-PCB.                             
000283     PERFORM A-INIT                                                       
000284                                                                          
000285     PERFORM B-LAES-PARAMETER                                             
000286                                                                          
000287     MOVE WS-IDSHIPM           TO W-IDSHIPM                               
000288     PERFORM IMS-GU-WDE101                                                
000289     PERFORM C-SKAPA-SKEPPN-POST                                          
000290                                                                          
000291     PERFORM IMS-GNP-WDE111                                               
000292                                                                          
000293     PERFORM UNTIL SEGMENT-SAKNAS                                         
000294       PERFORM D-KOLLA-TRANSP                                             
000295                                                                          
000296       MOVE SGMT-IDDISTR       TO TEST-IDDISTR                            
000297                                                                          
000298       IF (FL-BCUBE          = JA AND DIST87-BCUBE)         OR            
000299          (FL-BCUBE          = JA AND DIST87-BCUBE-PLUS)    OR            
000300          (FL-TRUCKWHEEL     = JA AND DIST87-TRUCKWHEEL)    OR            
000301          (FL-NEOVIA         = JA AND DIST87-NEOVIA-ES)     OR            
000302          (FL-NEOVIA         = JA AND DIST87-NEOVIA-AFRIKA)               
000303                                                                          
000304         PERFORM E-SPARA-KUNDPOST                                         
000305                                                                          
000306         PERFORM IMS-GNP-WDE121                                           
000307         PERFORM UNTIL SEGMENT-SAKNAS                                     
000308                                                                          
000309           PERFORM F-SKAPA-KOLLI-RAD-POST                                 
000310                                                                          
000311           PERFORM IMS-GNP-WDE121                                         
000312         END-PERFORM                                                      
000313       END-IF                                                             
000314                                                                          
000315       IF SKRIV-KND-POST                                                  
000316         PERFORM G-SKRIV-KUNDPOST                                         
000317       END-IF                                                             
000318       PERFORM IMS-GNP-WDE111                                             
000319                                                                          
000320                                                                          
000321     END-PERFORM                                                          
000322                                                                          
000323     IF SKRIV-HUV-POST                                                    
000324       PERFORM H-SKRIV-HUV-POST                                           
000325     END-IF                                                               
000326                                                                          
000327     PERFORM Z-FINIT                                                      
000328                                                                          
000329     MOVE ZERO TO RETURN-CODE                                             
000330     GOBACK                                                               
000331     .                                                                    
000332     EJECT                                                                
000333 A-INIT SECTION.                                                          
000334                                                                          
000335     OPEN OUTPUT W4764O                                                   
000336     OPEN INPUT  W476IN                                                   
000337                                                                          
000338     MOVE NEJ         TO INFIL-EOF                                        
000339                         SKRIV-POST-SW                                    
000340                         SKRIV-POST-H-SW                                  
000341                         SKRIV-POST-K-SW                                  
000342                                                                          
000343     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
000344     .                                                                    
000345     EJECT                                                                
000346 B-LAES-PARAMETER SECTION.                                                
000347                                                                          
000348     PERFORM S12-LAES-W476IN                                              
000349     IF WS-IDSHIPM NUMERIC AND WS-IDSHIPM NOT = ZERO                      
000350       CONTINUE                                                           
000351     ELSE                                                                 
000352       MOVE 'SKEPPNINGS-NR SAKNAS' TO FELTEXT-STR                         
000353       DISPLAY FELTEXT                                                    
000354       CALL FELLOG                                                        
000355     END-IF                                                               
000356     .                                                                    
000357     EJECT                                                                
000358 C-SKAPA-SKEPPN-POST SECTION.                                             
000359                                                                          
000360     MOVE 'E101'                 TO SPAR-HUV-IDPTYP                       
000361     MOVE SHIP-IDSHIPM           TO SPAR-HUV-IDSHIPM                      
000362     MOVE SHIP-IDTRPTNR          TO SPAR-HUV-IDTRPTNR                     
000363     MOVE SHIP-IDLBBET           TO SPAR-HUV-IDLBBET                      
000364     MOVE SHIP-IDDC              TO SPAR-HUV-IDDC                         
000365     MOVE JA                     TO SKRIV-POST-H-SW                       
000366                                                                          
000367     MOVE ZERO                   TO SPAR-HUV-IDDISTR                      
000368                                    SPAR-HUV-IDKUNDNR                     
000369                                    SPAR-HUV-IDPRODNR                     
000370                                    SPAR-HUV-IDKOLLI                      
000371                                    SPAR-HUV-SUORDV                       
000372                                    SPAR-HUV-VKORDBTO                     
000373                                    SPAR-HUV-VLORDBTO                     
000374     MOVE SPACE                  TO SPAR-HUV-KDVALISO                     
000375     .                                                                    
000376     EJECT                                                                
000377 D-KOLLA-TRANSP SECTION.                                                  
000378                                                                          
000379     MOVE SGMT-IDDISTR           TO TEST-IDDISTR                          
000380     MOVE SGMT-IDDC              TO WS-IDDC                               
000381                                                                          
000382     IF DIST87-BCUBE                                                      
000383       MOVE JA                   TO FL-BCUBE                              
000384                                    SKRIV-POST-SW                         
000390     END-IF                                                               
000391                                                                          
000392     IF DIST87-BCUBE-PLUS                                                 
000393       MOVE JA                   TO FL-BCUBE                              
000394                                    SKRIV-POST-SW                         
000395     END-IF                                                               
000396                                                                          
000397     IF DIST87-TRUCKWHEEL                                                 
000398       MOVE JA                   TO FL-TRUCKWHEEL                         
000399                                    SKRIV-POST-SW                         
000400     END-IF                                                               
000401                                                                          
000403     IF DIST87-NEOVIA-ES                                                  
000404       MOVE JA                   TO FL-NEOVIA                             
000405                                    SKRIV-POST-SW                         
000406     END-IF                                                               
000407                                                                          
000408     IF DIST87-NEOVIA-AFRIKA                                              
000409       MOVE JA                   TO FL-NEOVIA                             
000410                                    SKRIV-POST-SW                         
000411     END-IF                                                               
000412                                                                          
000413*    IF DIST87-JAPAN                                                      
000414*      MOVE JA                   TO FL-JAPAN                              
000415*                                   SKRIV-POST-SW                         
000416*    END-IF                                                               
000417                                                                          
000418     IF SPAR-HUV-IDDISTR = ZERO                                           
000419       IF SKRIV-POST                                                      
000420         MOVE SGMT-IDDISTR       TO SPAR-HUV-IDDISTR                      
000421       END-IF                                                             
000422     END-IF                                                               
000423     .                                                                    
000424     EJECT                                                                
000425 E-SPARA-KUNDPOST SECTION.                                                
000426                                                                          
000427     MOVE 'E111'                 TO SPAR-KND-IDPTYP                       
000428     MOVE SHIP-IDSHIPM           TO SPAR-KND-IDSHIPM                      
000429     MOVE SGMT-IDDISTR           TO SPAR-KND-IDDISTR                      
000430                                    W-IDDISTR-B2                          
000431                                    W-IDDISTR                             
000432     MOVE SGMT-IDKUNDNR          TO SPAR-KND-IDKUNDNR                     
000433                                    W-IDKUNDNR-B2                         
000434                                    W-IDKUNDNR                            
000435     MOVE SGMT-IDDC              TO SPAR-KND-IDDC                         
000436     MOVE ZERO                   TO SPAR-KND-IDPRODNR                     
000437                                    SPAR-KND-IDKOLLI                      
000438                                    SPAR-KND-KVKOLLI                      
000439                                    SPAR-KND-VKORDBTO                     
000440                                    SPAR-KND-VKARTNTO                     
000441                                    SPAR-KND-VLORDBTO                     
000442                                                                          
000443     PERFORM  IMS-GU-WDB201                                               
000444     IF SEGMENT-FINNS                                                     
000445       MOVE GMT-BEGMT-RAD1      TO SPAR-KND-BEGMT-RAD1                    
000446       MOVE GMT-BEGMT-RAD2      TO SPAR-KND-BEGMT-RAD2                    
000447       MOVE GMT-ADGMT-GATA      TO SPAR-KND-ADGMT-GATA                    
000448       MOVE GMT-ADGMT-PADR      TO SPAR-KND-ADGMT-PADR                    
000449       MOVE GMT-ADGMT-LAND      TO SPAR-KND-ADGMT-LAND                    
000450     ELSE                                                                 
000451       MOVE SPACES              TO SPAR-KND-BEGMT-RAD1                    
000452                                   SPAR-KND-BEGMT-RAD2                    
000453                                   SPAR-KND-ADGMT-GATA                    
000454                                   SPAR-KND-ADGMT-PADR                    
000455                                   SPAR-KND-ADGMT-LAND                    
000456     END-IF                                                               
000457                                                                          
000458     MOVE SGMT-KDORDKL-MAX      TO SPAR-HUV-KDORDKL                       
000459     MOVE SGMT-KDVALISO         TO SPAR-HUV-KDVALISO                      
000460                                                                          
000461     MOVE JA                    TO SKRIV-POST-K-SW                        
000462                                                                          
000463*    WRITE UT-TRP-KND  FROM SPAR-UTAREA-KND                               
000464*    MOVE 'W4764O'              TO POSTSUM-FDNAMN                         
000465*    MOVE 'W4764OD2'            TO POSTSUM-DDNAMN2                        
000466*    MOVE UT-KND-IDPTYP         TO POSTSUM-TRANSTYP                       
000467*    SKIP2                                                                
000468*    CALL POSTSUM               USING  POSTSUM-PARM                       
000469     .                                                                    
000470     EJECT                                                                
000471 F-SKAPA-KOLLI-RAD-POST SECTION.                                          
000472                                                                          
000473     PERFORM FA-SKAPA-SKRIV-KOLLIPOST                                     
000474                                                                          
000475     PERFORM IMS-GNP-WDE131                                               
000476     PERFORM UNTIL SEGMENT-SAKNAS                                         
000477       PERFORM FB-SKAPA-SKRIV-RADPOST                                     
000478                                                                          
000479       PERFORM IMS-GNP-WDE131                                             
000480     END-PERFORM                                                          
000481     .                                                                    
000482     EJECT                                                                
000483 FA-SKAPA-SKRIV-KOLLIPOST SECTION.                                        
000484                                                                          
000485     MOVE 'E121'                 TO UT-KLI-IDPTYP                         
000486     MOVE SHIP-IDSHIPM           TO UT-KLI-IDSHIPM                        
000487     MOVE SGMT-IDDISTR           TO UT-KLI-IDDISTR                        
000488                                    TEST-IDDISTR                          
000489     MOVE SGMT-IDKUNDNR          TO UT-KLI-IDKUNDNR                       
000490     MOVE SGMT-IDDC              TO UT-KLI-IDDC                           
000491                                                                          
000492     MOVE SKOLLI-IDPRODNR        TO UT-KLI-IDPRODNR                       
000493                                    W-IDPRODNR                            
000494     MOVE SKOLLI-IDKOLLI         TO UT-KLI-IDKOLLI                        
000495                                    W-IDKOLLI                             
000496     MOVE SKOLLI-IDKUNDRF(3:5)   TO UT-KLI-IDORDNR5                       
000497     MOVE SKOLLI-KDFARLIG-KOLLI  TO UT-KLI-KDFARLIG-KOLLI                 
000498     MOVE SKOLLI-KDFRAKT         TO UT-KLI-KDFRAKT                        
000499                                                                          
000500     IF SKOLLI-SUORDV > ZERO                                              
000501       MOVE SKOLLI-SUORDV        TO UT-KLI-SUORDV-KOLLI                   
000502     ELSE                                                                 
000503       IF SKOLLI-SUORDV-LOC > ZERO                                        
000504         MOVE SKOLLI-SUORDV-LOC  TO UT-KLI-SUORDV-KOLLI                   
000505       ELSE                                                               
000506         IF SKOLLI-SUORDV-LOCPREL > ZERO                                  
000507           MOVE SKOLLI-SUORDV-LOCPREL TO UT-KLI-SUORDV-KOLLI              
000508         END-IF                                                           
000509       END-IF                                                             
000510     END-IF                                                               
000511     MOVE SKOLLI-KDVALISO        TO UT-KLI-KDVALISO                       
000512                                                                          
000513     MOVE SKOLLI-VKORDBTO-KOLLI  TO UT-KLI-VKORDBTO-KOLLI                 
000514     MOVE SKOLLI-VLORDBTO-KOLLI  TO UT-KLI-VLORDBTO-KOLLI                 
000515     MOVE SKOLLI-DIKOLLIL        TO UT-KLI-DIKOLLIL                       
000516     MOVE SKOLLI-DIKOLLIB        TO UT-KLI-DIKOLLIB                       
000517     MOVE SKOLLI-DIKOLLIH        TO UT-KLI-DIKOLLIH                       
000518     MOVE SKOLLI-KDORDKL         TO UT-KLI-KDORDKL                        
000519                                                                          
000520     IF FOREG-IDORDER NOT = SKOLLI-IDORDER                                
000521       MOVE SKOLLI-IDORDER       TO FOREG-IDORDER                         
000522                                    W-IDORDER                             
000523       PERFORM IMS-GU-WDQ201                                              
000524                                                                          
000525       IF SEGMENT-FINNS                                                   
000526         MOVE OHUV-BEGMT-RAD1   TO WS-BEGMT-RAD1                          
000527         MOVE OHUV-BEGMT-RAD2   TO WS-BEGMT-RAD2                          
000528         MOVE OHUV-ADGMT-GATA   TO WS-ADGMT-GATA                          
000529         MOVE OHUV-ADGMT-PADR   TO WS-ADGMT-PADR                          
000530         MOVE OHUV-ADGMT-LAND   TO WS-ADGMT-LAND                          
000531         MOVE OHUV-BEKUNDRF     TO WS-BEKUNDRF                            
000532       ELSE                                                               
000533         MOVE SPACE             TO WS-ADGMT-GATA                          
000534                                   WS-ADGMT-PADR                          
000535                                   WS-ADGMT-LAND                          
000536                                   WS-BEGMT-RAD1                          
000537                                   WS-BEGMT-RAD2                          
000538                                   WS-BEKUNDRF                            
000539       END-IF                                                             
000540     END-IF                                                               
000541                                                                          
000542     MOVE WS-BEGMT-RAD1       TO UT-KLI-BEGMT-RAD1                        
000543     MOVE WS-BEGMT-RAD2       TO UT-KLI-BEGMT-RAD2                        
000544     MOVE WS-ADGMT-GATA       TO UT-KLI-ADGMT-GATA                        
000545     MOVE WS-ADGMT-PADR       TO UT-KLI-ADGMT-PADR                        
000546     MOVE WS-ADGMT-LAND       TO UT-KLI-ADGMT-LAND                        
000547     MOVE WS-BEKUNDRF         TO UT-KLI-BEKUNDRF                          
000548                                                                          
000549     IF SKOLLI-SUORDV > ZERO                                              
000550                                                                          
000551       COMPUTE WS-SUORDV = WS-SUORDV +                                    
000552                           SKOLLI-SUORDV                                  
000553     ELSE                                                                 
000554       IF SKOLLI-SUORDV-LOC > ZERO                                        
000555                                                                          
000556         COMPUTE WS-SUORDV = WS-SUORDV +                                  
000557                             SKOLLI-SUORDV-LOC                            
000558                                                                          
000559       ELSE                                                               
000560         IF SKOLLI-SUORDV-LOCPREL > ZERO                                  
000561                                                                          
000562           COMPUTE WS-SUORDV = WS-SUORDV +                                
000563                               SKOLLI-SUORDV-LOCPREL                      
000564         END-IF                                                           
000565       END-IF                                                             
000566     END-IF                                                               
000567                                                                          
000568     COMPUTE WS-VKORDBTO = WS-VKORDBTO +                                  
000569                           SKOLLI-VKORDBTO-KOLLI                          
000570                                                                          
000571     COMPUTE WS-VLORDBTO = WS-VLORDBTO +                                  
000572                           SKOLLI-VLORDBTO-KOLLI                          
000573                                                                          
000574     COMPUTE WS-KVKOLLI  = WS-KVKOLLI  + 1                                
000575                                                                          
000576*    TOTALER PER KUND                                                     
000577     COMPUTE WS-VKORDBTO-KND = WS-VKORDBTO-KND       +                    
000578                               SKOLLI-VKORDBTO-KOLLI                      
000579                                                                          
000580     COMPUTE WS-VLORDBTO-KND = WS-VLORDBTO-KND       +                    
000581                               SKOLLI-VLORDBTO-KOLLI                      
000582                                                                          
000583     COMPUTE WS-KVKOLLI-KND  = WS-KVKOLLI-KND  + 1                        
000584                                                                          
000585     WRITE UT-TRP-KLI  FROM ARB-UTAREA                                    
000586     MOVE 'W4764O'               TO POSTSUM-FDNAMN                        
000587     MOVE 'W4764OD2'             TO POSTSUM-DDNAMN2                       
000588     MOVE UT-KLI-IDPTYP          TO POSTSUM-TRANSTYP                      
000589     SKIP2                                                                
000590     CALL POSTSUM             USING  POSTSUM-PARM                         
000591     .                                                                    
000592     EJECT                                                                
000593 FB-SKAPA-SKRIV-RADPOST SECTION.                                          
000594                                                                          
000595     MOVE 'E131'                 TO UT-RAD-IDPTYP                         
000596     MOVE SHIP-IDSHIPM           TO UT-RAD-IDSHIPM                        
000597                                    W-IDSHIPM                             
000598     MOVE SGMT-IDDISTR           TO UT-RAD-IDDISTR                        
000599                                    TEST-IDDISTR                          
000600                                    W-IDDISTR-E2                          
000601     MOVE SGMT-IDKUNDNR          TO UT-RAD-IDKUNDNR                       
000602                                    W-IDKUNDNR-E2                         
000603     MOVE SGMT-IDDC              TO UT-RAD-IDDC                           
000604*                                                                         
000605     MOVE SKOLLI-IDPRODNR        TO W-IDPRODNR-E2                         
000606     MOVE SKOLLI-IDKOLLI         TO W-IDKOLLI-E2                          
000607     MOVE SRAD-IDPURAD           TO W-IDPURAD                             
000608*                                                                         
000609     COMPUTE WS-VKARTNTO-KND = WS-VKARTNTO-KND       +                    
000610                              (SRAD-VKART-NTO-KG * SRAD-KVLEVART)         
000611                                                                          
000612     PERFORM IMS-GU-WDE231                                                
000613     IF SEGMENT-FINNS                                                     
000614       MOVE BRAD-IDARTNR         TO UT-RAD-IDARTNR                        
000615       MOVE BRAD-BEART-VIPS      TO UT-RAD-BEART                          
000616       MOVE BRAD-KVLEVART        TO UT-RAD-KVLEVART                       
000617     ELSE                                                                 
000618       MOVE SPACE                TO UT-RAD-BEART                          
000619       MOVE ZERO                 TO UT-RAD-KVLEVART                       
000620     END-IF                                                               
000621                                                                          
000622     WRITE UT-TRP-RAD  FROM ARB-UTAREA                                    
000623     MOVE 'W4764O'               TO POSTSUM-FDNAMN                        
000624     MOVE 'W4764OD2'             TO POSTSUM-DDNAMN2                       
000625     MOVE UT-RAD-IDPTYP          TO POSTSUM-TRANSTYP                      
000626     SKIP2                                                                
000627     .                                                                    
000628     EJECT                                                                
000629 G-SKRIV-KUNDPOST SECTION.                                                
000630                                                                          
000631                                                                          
000632     MOVE WS-VKORDBTO-KND     TO SPAR-KND-VKORDBTO                        
000633     MOVE WS-VKARTNTO-KND     TO SPAR-KND-VKARTNTO                        
000634     MOVE WS-VLORDBTO-KND     TO SPAR-KND-VLORDBTO                        
000635     MOVE WS-KVKOLLI-KND      TO SPAR-KND-KVKOLLI                         
000636                                                                          
000637     WRITE UT-TRP-KND  FROM SPAR-UTAREA-K                                 
000638     MOVE 'W4764O'            TO POSTSUM-FDNAMN                           
000639     MOVE 'W4764OD2'          TO POSTSUM-DDNAMN2                          
000640     MOVE SPAR-KND-IDPTYP     TO POSTSUM-TRANSTYP                         
000641     SKIP2                                                                
000642     CALL POSTSUM             USING  POSTSUM-PARM                         
000643                                                                          
000644     MOVE ZERO                TO WS-VKORDBTO-KND                          
000645                                 WS-VKARTNTO-KND                          
000646                                 WS-VLORDBTO-KND                          
000647                                 WS-KVKOLLI-KND                           
000648     MOVE NEJ                 TO SKRIV-POST-K-SW                          
000649     .                                                                    
000650     EJECT                                                                
000651 H-SKRIV-HUV-POST SECTION.                                                
000652                                                                          
000653     MOVE WS-SUORDV           TO SPAR-HUV-SUORDV                          
000654     MOVE WS-VKORDBTO         TO SPAR-HUV-VKORDBTO                        
000655     MOVE WS-VLORDBTO         TO SPAR-HUV-VLORDBTO                        
000656     MOVE WS-KVKOLLI          TO SPAR-HUV-KVKOLLI                         
000657     MOVE SPACE               TO SPAR-HUV-IDLEVNR                         
000658                                                                          
000659     WRITE UT-TRP-HUV  FROM SPAR-UTAREA-H                                 
000660     MOVE 'W4764O'            TO POSTSUM-FDNAMN                           
000661     MOVE 'W4764OD2'          TO POSTSUM-DDNAMN2                          
000662     MOVE SPAR-HUV-IDPTYP     TO POSTSUM-TRANSTYP                         
000663     SKIP2                                                                
000664     CALL POSTSUM             USING  POSTSUM-PARM                         
000665     .                                                                    
000666     EJECT                                                                
000667 S12-LAES-W476IN  SECTION.                                                
000668                                                                          
000669     READ W476IN INTO PARM-AREA                                           
000670     AT END                                                               
000671        SET END-OF-W476IN TO TRUE                                         
000672                                                                          
000673     NOT AT END                                                           
000674        MOVE 'INFIL'      TO POSTSUM-FDNAMN                               
000675        MOVE 'W4764OD1'   TO POSTSUM-DDNAMN2                              
000676        MOVE SPACE        TO POSTSUM-TRANSTYP                             
000677        CALL POSTSUM USING POSTSUM-PARM                                   
000678     END-READ                                                             
000679     .                                                                    
000680     EJECT                                                                
000681 Z-FINIT SECTION.                                                         
000682                                                                          
000683     MOVE 'S' TO POSTSUM-OPKOD                                            
000684     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
000685                                                                          
000686     CLOSE W476IN                                                         
000687           W4764O                                                         
000688     .                                                                    
000689     EJECT                                                                
000690*****IMS-LÄSNINGAR******                                                  
000691                                                                          
000692 IMS-GU-WDE101 SECTION.                                                   
000693                                                                          
000694     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
000695          DELIMITED BY SIZE INTO SSA1                                     
000696     MOVE '    ' TO GODK-STATUSKODER                                      
000697     CALL CBLTDLI USING GU WDE1-PCB DLI-IO-WDE101 SSA1                    
000698     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
000699     PERFORM IMS-STATUSKONTROLL                                           
000700     .                                                                    
000701     SKIP2                                                                
000702 IMS-GNP-WDE111 SECTION.                                                  
000703                                                                          
000704     MOVE 'WDE111 '          TO SSA1                                      
000705     MOVE '  GE' TO GODK-STATUSKODER                                      
000706     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE111 SSA1                   
000707     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
000708     PERFORM IMS-STATUSKONTROLL                                           
000709     .                                                                    
000710     EJECT                                                                
000711 IMS-GNP-WDE121  SECTION.                                                 
000712                                                                          
000713     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
000714          DELIMITED BY SIZE INTO SSA1                                     
000715     MOVE 'WDE121 '          TO SSA2                                      
000716     MOVE '  GE' TO GODK-STATUSKODER                                      
000717     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE121 SSA1 SSA2              
000718     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
000719     PERFORM IMS-STATUSKONTROLL                                           
000720     .                                                                    
000721     SKIP2                                                                
000722 IMS-GNP-WDE131  SECTION.                                                 
000723                                                                          
000724     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
000725          DELIMITED BY SIZE INTO SSA1                                     
000726     STRING 'WDE121  (WDE121KY =' W-WDE121KY-X ')'                        
000727          DELIMITED BY SIZE INTO SSA2                                     
000728     MOVE 'WDE131 '          TO SSA3                                      
000729     MOVE '  GE' TO GODK-STATUSKODER                                      
000730     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE131 SSA1 SSA2 SSA3         
000731     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
000732     PERFORM IMS-STATUSKONTROLL                                           
000733     .                                                                    
000734     SKIP2                                                                
000735 IMS-GU-WDE231 SECTION.                                                   
000736                                                                          
000737     STRING 'WDE201  (IDSHIPM  =' W-IDSHIPM-X ')'                         
000738          DELIMITED BY SIZE INTO SSA1                                     
000739     STRING 'WDE211  (WDE211KY =' W-WDE211KY-X ')'                        
000740          DELIMITED BY SIZE INTO SSA2                                     
000741     STRING 'WDE221  (WDE221KY =' W-WDE221KY-X ')'                        
000742          DELIMITED BY SIZE INTO SSA3                                     
000743     STRING 'WDE231  (IDPURAD  =' W-IDPURAD-X ')'                         
000744          DELIMITED BY SIZE INTO SSA4                                     
000745     MOVE '  GE' TO GODK-STATUSKODER                                      
000746     CALL CBLTDLI USING GU WDE2-PCB DLI-IO-WDE231 SSA1 SSA2               
000747                                                  SSA3 SSA4               
000748     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
000749     PERFORM IMS-STATUSKONTROLL                                           
000750     .                                                                    
000751     SKIP2                                                                
000752 IMS-GU-WDB201 SECTION.                                                   
000753                                                                          
000754     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
000755          DELIMITED BY SIZE INTO SSA1                                     
000756     MOVE '    ' TO GODK-STATUSKODER                                      
000757     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
000758     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
000759     PERFORM IMS-STATUSKONTROLL                                           
000760     .                                                                    
000761     EJECT                                                                
000762 IMS-GU-WDQ201            SECTION.                                        
000763                                                                          
000764     STRING 'WDQ201  (IDORDER  =' W-IDORDER-X ')'                         
000765          DELIMITED BY SIZE INTO SSA1                                     
000766     MOVE '  GE'               TO GODK-STATUSKODER                        
000767     CALL CBLTDLI USING GU WDQ2-PCB DLI-IO-WDQ201 SSA1                    
000768     MOVE WDQ2-STATUS-CODE     TO STATUS-WS                               
000769     PERFORM IMS-STATUSKONTROLL                                           
000770     .                                                                    
000771     SKIP3                                                                
000772     EJECT                                                                
000773 IMS-STATUSKONTROLL SECTION.                                              
000774     SET STATUS-IX TO 1                                                   
000775     SEARCH GODK-STATUS                                                   
000776       AT END                                                             
000777         CALL FELLOG                                                      
000778       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
000779         CONTINUE                                                         
000780     END-SEARCH                                                           
000781     .                                                                    
000790     EJECT                                                                
