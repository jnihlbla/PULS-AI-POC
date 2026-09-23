000001 ID DIVISION.                                                             
000002 PROGRAM-ID.             W4402200.                                        
000003 AUTHOR.                 CARINA VIKTORSSON.                               
000004 DATE-WRITTEN.           JULI 1988.                                       
000005 DATE-COMPILED.                                                           
000006                                                                          
000007*    FUNKTION:                                                            
000008*                                                                         
000009*        LÄSER INFIL MED INFORMATION OM ERSATTA OCH                       
000010*        TILLKOMMANDE FÖRÄNDRADE ARTIKLAR.                                
000011*                                                                         
000012*        KOMPLETTERING SKER MED RESTORDER INFO, BRUTTOPRIS,               
000013*        ARTIKELBENÄMNINGEN.                                              
000014*                                                                         
000015*        SPÄRR AV RADERNA ELLER NOTERING                                  
000016*        OM RESTNOTERING IGEN SKER.                                       
000017*                                                                         
000018*    ÄNDRING:                                                             
000019*        JAN-1997 / BOO HAMMARIN, GDC-G / ANPASSNING NDC-LAGER            
000020*                                                                         
000021*    ABENDKODER:                                                          
000022*        U0016 - RETURKOD FRÅN SORT                                       
000023     EJECT                                                                
000024 ENVIRONMENT DIVISION.                                                    
000025                                                                          
000026 INPUT-OUTPUT SECTION.                                                    
000027                                                                          
000028 FILE-CONTROL.                                                            
000029     SKIP2                                                                
000030*    ---- INFIL:                                                          
000031     SELECT  INFIL         ASSIGN  W44022D1.                              
000032     SKIP2                                                                
000033*    ---- UTFIL:                                                          
000034     SELECT  UTFIL         ASSIGN  W44022D2.                              
000035     SKIP2                                                                
000036*    ---- SORTFIL:                                                        
000037     SELECT  SORTFIL       ASSIGN  W44022DS.                              
000038     EJECT                                                                
000039 DATA DIVISION.                                                           
000040                                                                          
000041 FILE SECTION.                                                            
000042     SKIP3                                                                
000043 FD  INFIL                                                                
000044     LABEL RECORD STANDARD                                                
000045     RECORDING  F                                                         
000046     BLOCK CONTAINS 0.                                                    
000047                                                                          
000048 01  INPOST -COPY W440001       -L.                                       
000049     SKIP3                                                                
000050 FD  UTFIL                                                                
000051     LABEL RECORD STANDARD                                                
000052     RECORDING  F                                                         
000053     BLOCK CONTAINS 0.                                                    
000054                                                                          
000055 01  UTPOST -COPY W440001       -L.                                       
000056     SKIP3                                                                
000057 SD  SORTFIL                                                              
000058     RECORDING V.                                                         
000059 01  POST -COPY W440001         -PRE S-.                                  
000060     EJECT                                                                
000061 WORKING-STORAGE SECTION.                                                 
000062                                                                          
000063*    -- CHECKED BY WY2000                                                 
000064 77  PROGRAM-NAMN            PIC X(8)    VALUE 'W4402200'.                
000065 77  FELTEXT                 PIC X(80)   VALUE SPACE.                     
000066     SKIP2                                                                
000067 77  JA                      PIC X       VALUE 'J'.                       
000068 77  NEJ                     PIC X       VALUE 'N'.                       
000069     SKIP2                                                                
000070 77  IX                      PIC S9(9)   VALUE +0   COMP SYNC.            
000071 77  IX-DCCLEAR-MAX          PIC S9(3)   VALUE +99  COMP SYNC.            
000072 77  INDX                    PIC S9(9)   VALUE +0   COMP SYNC.            
000073     SKIP2                                                                
000074 77  INFIL-EOF               PIC X       VALUE 'N'.                       
000075 77  SORTFIL-EOF             PIC X       VALUE 'N'.                       
000076     SKIP2                                                                
000077 77  LOPNR-BRYT              PIC S9(3)   COMP-3  VALUE ZERO.              
000078 77  IDDISTR-BRYT            PIC S9(5)   COMP-3  VALUE ZERO.              
000079 77  IDKUNDNR-BRYT           PIC S9(7)   COMP-3  VALUE ZERO.              
000080     SKIP2                                                                
000081 77  KORTNR-NYTT             PIC S9(3)   COMP-3  VALUE ZERO.              
000082 77  LOPNR-NYTT              PIC S9(3)   COMP-3  VALUE ZERO.              
000083     SKIP2                                                                
000084 01  DUMMY-PCB                   PIC X(4)   VALUE LOW-VALUE.              
000085                                                                          
000086 01  IDKUNDRF-WS             PIC X(10).                                   
000087 01  ORDERNR  REDEFINES IDKUNDRF-WS.                                      
000088     03  IDORDNR-WS          PIC 9(5).                                    
000089     03  FILLER              PIC X(5).                                    
000090     EJECT                                                                
000091*      --- VALID IDDC CODES                                               
000092*                                                                         
000093*01    -COPY WWDCKONS                                                     
000094       EJECT                                                              
000095*    ---- SUBPROGRAM OCH PARAMETER-AREOR                                  
000096     SKIP3                                                                
000097 01  DYNAMISKA-SUBPROGRAM.                                                
000098   03  ABEND                 PIC X(8)    VALUE 'ABEND   '.                
000099   03  POSTSUM               PIC X(8)    VALUE 'POSTSUM '.                
000100   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
000101   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
000102   03  W411DLEV              PIC X(8)    VALUE 'W411DLEV'.                
000103   03  W335PRIS              PIC X(8)    VALUE 'W335PRIS'.                
000104     SKIP3                                                                
000105*    ----  PARAMETRAR TILL ABEND                                          
000106     SKIP1                                                                
000107 01  RKOD-ABEND-UTAN-DUMP    PIC S9(4) VALUE +16 COMP SYNC.               
000108 01  RKOD-ABEND-MED-DUMP     PIC S9(4) VALUE +33 COMP SYNC.               
000109     EJECT                                                                
000110*    ----  PARAMETRAR TILL POSTSUM                                        
000111                                                                          
000112 01  -COPY W0005       -PRE POSTSUM-.                                     
000113     EJECT                                                                
000114*    ----  AREA FÖR INPOSTER                                              
000115 01  FILLER                  PIC X(24) VALUE 'INPOST-AREA   '.            
000116     SKIP2                                                                
000117 01  -COPY W440001                                                        
000118     EJECT                                                                
000119*    ----  AREA FÖR UTPOSTER                                              
000120 01  FILLER                  PIC X(24) VALUE 'UTPOST-AREA   '.            
000121     SKIP2                                                                
000122 01  -COPY W440001     -PRE U-.                                           
000123     EJECT                                                                
000124*    ----  AREA FÖR PRISTILLÄMPNING                                       
000125 01  FILLER                  PIC X(24) VALUE 'W335PRIS-AREA '.            
000126     SKIP2                                                                
000127 01      -COPY W335PRIS                                                   
000128     EJECT                                                                
000129*    ----  AREA FÖR DIREKTLEVERANS                                        
000130 01  FILLER                  PIC X(24) VALUE 'W411DLEV-AREA '.            
000131     SKIP2                                                                
000132 01      -COPY W411DLEV                                                   
000133     EJECT                                                                
000134*    ---- TABELLAREA                                                      
000135 01  FILLER                  PIC X(24) VALUE 'TABELLAREA    '.            
000136 01  TABELL.                                                              
000137     03  TABELLEN            OCCURS 50.                                   
000138         05  TAB-IDARTNR         PIC S9(9)      COMP-3.                   
000139         05  TAB-IDKORTNR        PIC S9(3)      COMP-3.                   
000140         05  TAB-IDLOPNR         PIC S9(3)      COMP-3.                   
000141         05  TAB-KDRESTR         PIC S9(3)      COMP-3.                   
000142         05  TAB-IDDC            PIC X(2).                                
000143         05  TAB-DIERS-ERS       PIC S9(4)V9(3) COMP-3.                   
000144         05  TAB-DIERS-TILLK     PIC S9(4)V9(3) COMP-3.                   
000145         05  TAB-IDANSK          PIC S9(3)      COMP-3.                   
000146         05  TAB-KDERS           PIC S9(3)      COMP-3.                   
000147         05  TAB-KDLEVSP         PIC S9(3)      COMP-3.                   
000148         05  TAB-KDPRODSL        PIC S9(3)      COMP-3.                   
000149         05  TAB-PRARTSTD        PIC S9(7)V9(2) COMP-3.                   
000150         05  TAB-REKSIFFR        PIC S9(1)      COMP-3.                   
000151         05  TAB-KVQPACK-1       PIC S9(5)      COMP-3.                   
000152         05  TAB-BEERS           PIC X(20).                               
000153         05  TAB-KDSORT          PIC X(2).                                
000154         05  TAB-TIDISPIN        PIC S9(7)      COMP-3.                   
000155         05  TAB-REDIRLEV        PIC S9(1)V9(2) COMP-3.                   
000156         05  TAB-KDUART          PIC X(1).                                
000157         05  TAB-KDUART-URS      PIC X(1).                                
000158         05  TAB-IDARTNR-URS     PIC S9(9)      COMP-3.                   
000159         05  TAB-KVKORT          PIC S9(3)      COMP-3.                   
000160         05  TAB-KDRADERS        PIC S9(1)      COMP-3.                   
000161         05  TAB-KDTILLK         PIC S9(1)      COMP-3.                   
000162     EJECT                                                                
000163*    ---- SPARAD KUND & ARTIKEL INFORMATION                               
000164 01  FILLER                  PIC X(24) VALUE 'KUND ARTINFO  '.            
000165     SKIP3                                                                
000166 77  FLVR-WS                     PIC S9(1)       COMP-3.                  
000167 77  BEART-WS                    PIC X(25).                               
000168                                                                          
000169 77  FLPRERS-WS                  PIC X(1).                                
000170 77  BEVARREF-WS                 PIC X(10).                               
000171 77  IDORDER-WS                  PIC S9(7)       COMP-3.                  
000172 77  BEKUNDRF-WS                 PIC X(15).                               
000173 77  TIORDREG-WS                 PIC S9(7)       COMP-3.                  
000174     EJECT                                                                
000175*------- INFO FRÅN LARMKÖN                                                
000176 01  FILLER                  PIC X(16) VALUE 'LARMWS**********'.          
000177*                                                                         
000178 01  LARMWS.                                                              
000179  02 LARMWS-KDLARM               PIC S9(3)       COMP-3.                  
000180  02 LARMWS-FLNYLARM             PIC X(1).                                
000181  02 LARMWS-TIREGDAT             PIC S9(7)       COMP-3.                  
000182  02 LARMWS-IDANSK-LARM          PIC S9(3)       COMP-3.                  
000183     SKIP3                                                                
000184*------- SWITCHAR                                                         
000185 01  FILLER                  PIC X(16) VALUE 'SW**************'.          
000186*                                                                         
000187 01  SW.                                                                  
000188  02 SW-2232-LAST                PIC X(1).                                
000189  02 SW-LARMAD-RAD               PIC X(1).                                
000190  02 SW-KDRESTR-FELMRK           PIC X(1).                                
000191     SKIP3                                                                
000192*------- GENERELLA WORK-FÄLT                                              
000193 01      FILLER              PIC X(16) VALUE 'WS**************'.          
000194*                                                                         
000195 01      WS.                                                              
000196*                                                                         
000197  02     WS-IDKUNDRF-OLD         PIC X(10).                               
000198  02     FILLER                  REDEFINES WS-IDKUNDRF-OLD.               
000199   03    WS-IDKUNDRF-OLD-1-5     PIC X(5).                                
000200   03    FILLER                  PIC X(5).                                
000201*                                                                         
000202  02     WS-IDKUNDRF-NEW         PIC X(10).                               
000203  02     FILLER                  REDEFINES WS-IDKUNDRF-NEW.               
000204   03    WS-IDKUNDRF-NEW-1-2     PIC X(2).                                
000205   03    WS-IDKUNDRF-NEW-3-7     PIC X(5).                                
000206   03    WS-IDKUNDRF-NEW-8-10    PIC X(3).                                
000207     EJECT                                                                
000208 01  VARIABLER.                                                           
000209     03  TEST-IDDISTR            PIC S9(5)       COMP-3.                  
000210     SKIP2                                                                
000211 03  DIST03-POST   -COPY WWDIST03    -RED TEST-IDDISTR                    
000212     EJECT                                                                
000213 03  DIST13-POST   -COPY WWDIST13    -RED TEST-IDDISTR                    
000214     EJECT                                                                
000215 03  DIST19-POST   -COPY WWDIST19    -RED TEST-IDDISTR                    
000216     EJECT                                                                
000217 03  DIST48-POST   -COPY WWDIST48    -RED TEST-IDDISTR                    
000218     EJECT                                                                
000219 03  DIST65-POST   -COPY WWDIST65    -RED TEST-IDDISTR                    
000220     EJECT                                                                
000221 03  DIST79-POST   -COPY WWDIST79    -RED TEST-IDDISTR                    
000222     EJECT                                                                
000223     03  TEST-IDARTNR            PIC S9(9)       COMP-3.                  
000224 03  BYT03-POST   -COPY WWBYT03     -RED TEST-IDARTNR                     
000225     EJECT                                                                
000226 03  ART01-POST   -COPY WWART01     -RED TEST-IDARTNR                     
000227     EJECT                                                                
000228*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA                                
000229                                                                          
000230 01  FILLER                  PIC X(16) VALUE 'IMS-WS'.                    
000231     SKIP3                                                                
000232*    ---- STATUSKOD FRÅN IMS                                              
000233                                                                          
000234 01  STATUS-WS               PIC XX.                                      
000235     88  SEGMENT-FINNS                    VALUE '  '.                     
000236     88  SEGMENT-SAKNAS                   VALUE 'GE'.                     
000237     88  SEGMENT-SLUT                     VALUE 'GB'.                     
000238     SKIP3                                                                
000239 01  GODK-STATUSKODER.                                                    
000240   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
000241     SKIP3                                                                
000242 01  SSA1                    PIC X(128).                                  
000243 01  SSA2                    PIC X(32).                                   
000244 01  SSA3                    PIC X(32).                                   
000245 01  SSA4                    PIC X(32).                                   
000246     SKIP3                                                                
000247*    ----  NYCKLAR OCH SÖKFÄLT TILL DLI                                   
000248                                                                          
000249 01  NYCKLAR-TILL-DLI.                                                    
000250                                                                          
000251   03  W-WDA501KY-X.                                                      
000252     05  W-IDDISTR-01        PIC S9(5)    COMP-3.                         
000253     05  W-IDKUNDNR-01       PIC S9(7)    COMP-3.                         
000254     05  W-IDKUNDRF-01       PIC X(10).                                   
000255     05  W-IDARTNR-01        PIC S9(9)    COMP-3.                         
000256     05  W-IDLOPNR-01        PIC S9(3)    COMP-3.                         
000257   03    W-IDDC-01-X.                                                     
000258     05  W-IDDC-01           PIC X(2).                                    
000259                                                                          
000260   03  W-WDA5A1KY-MIN-X.                                                  
000261     05  W-IDARTNR-MIN       PIC S9(9)    COMP-3.                         
000262     05  W-IDDC-MIN          PIC X(2).                                    
000263     05  FILLER              PIC X(33).                                   
000264                                                                          
000265   03  W-WDA5A1KY-MAX-X.                                                  
000266     05  W-IDARTNR-MAX       PIC S9(9)    COMP-3.                         
000267     05  W-IDDC-MAX          PIC X(2).                                    
000268     05  FILLER              PIC X(33).                                   
000269                                                                          
000270   03  W-IDDISTR-X.                                                       
000271     05  W-IDDISTR           PIC S9(5)    COMP-3.                         
000272                                                                          
000273   03  W-IDGMT-X.                                                         
000274     05  W-IDDISTR-B2        PIC S9(5)    COMP-3.                         
000275     05  W-IDKUNDNR-B2       PIC S9(7)    COMP-3.                         
000276                                                                          
000277   03  W-IDARTNR-X.                                                       
000278     05  W-IDARTNR           PIC S9(9)    COMP-3.                         
000279                                                                          
000280   03  W-IDSKYLT-X.                                                       
000281     05  W-IDSKYLT           PIC X(3).                                    
000282                                                                          
000283   03  W-KDSTARAD-X.                                                      
000284     05  W-KDSTARAD          PIC X(1).                                    
000285                                                                          
000286   03  W-IDGMTREF-X.                                                      
000287     05  W-IDGMTREF-IDDISTR  PIC S9(5)    COMP-3.                         
000288     05  W-IDGMTREF-IDKUNDNR PIC S9(7)    COMP-3.                         
000289     05  W-IDGMTREF-IDKUNDRF PIC X(10).                                   
000290                                                                          
000291   03  W-2232-WDGXKEY.                                                    
000292     05  W-2232-IDANSK       PIC S9(3)    COMP-3.                         
000293     05  W-2232-LOW-VALUE    PIC X(3)     VALUE LOW-VALUE.                
000294                                                                          
000295   03  W-2224-WDGXKEY.                                                    
000296     05  W-2224-TISENBEK-DAG PIC S9(7)    COMP-3.                         
000297     05  W-2224-TISENBEK-KL  PIC S9(7)    COMP-3.                         
000298     05  W-2224-KDLARM       PIC S9(3)    COMP-3.                         
000299     EJECT                                                                
000300 01  -COPY WDGX01     -PRE W-2231-                                        
000301     EJECT                                                                
000302 01  -COPY WDGX2223   -PRE W-                                             
000303     EJECT                                                                
000304 01  -COPY W0003                                                          
000305     EJECT                                                                
000306 01  FILLER                  PIC X(16) VALUE 'DLI-IO-AREA'.               
000307     SKIP3                                                                
000308 01  DLI-IO-AREA.                                                         
000309   03  IO-AREA               PIC X(600).                                  
000310     SKIP3                                                                
000311 03  WLORDQ01 -COPY WDA5A1                     -RED IO-AREA               
000312     EJECT                                                                
000313 03  WLORDP01 -COPY WDA501                     -RED IO-AREA               
000314     EJECT                                                                
000315 03  WLBENA11 -COPY WDD311                     -RED IO-AREA               
000316     EJECT                                                                
000317 01  DLI-IO-AREA-WDB2.                                                    
000318 03  WLGMTA01 -COPY WDB201                                                
000319     EJECT                                                                
000320 01  DLI-IO-WDK611.                                                       
000321 03  WDK611   -COPY WDK611                                                
000322     EJECT                                                                
000323 01  FILLER                  PIC X(16) VALUE 'DLI-IO-AREA2'.              
000324     SKIP3                                                                
000325 01  DLI-IO-AREA2.                                                        
000326 03  WLORQI01 -COPY WDQ201      -PRE ORQI-                                
000327     EJECT                                                                
000328 01  FILLER                  PIC X(16) VALUE 'DLI-IO-AREA3'.              
000329     SKIP3                                                                
000330 01  DLI-IO-AREA3.                                                        
000331   03  IO-AREA3              PIC X(100).                                  
000332     SKIP3                                                                
000333 03  WLXXBX11 -COPY WDGX2232    -PRE XXBX-     -RED IO-AREA3              
000334     EJECT                                                                
000335 03  WLXXBU11 -COPY WDGX2224    -PRE XXBU-     -RED IO-AREA3              
000336     EJECT                                                                
000337 LINKAGE SECTION.                                                         
000338                                                                          
000339*01  -COPY W0008      -PRE  ORDQ-                                         
000340       05  FILLER                PIC X.                                   
000341     EJECT                                                                
000342*01  -COPY W0008      -PRE  ORDP-                                         
000343       05  FILLER                PIC X.                                   
000344     EJECT                                                                
000345*01  -COPY W0008      -PRE  GMTA-                                         
000346       05  FILLER                PIC X.                                   
000347     EJECT                                                                
000348*01  -COPY W0008      -PRE  BENA-                                         
000349       05  FILLER                PIC X.                                   
000350     EJECT                                                                
000351*01  -COPY W0008      -PRE  ORQI-                                         
000352       05  FILLER                PIC X.                                   
000353     EJECT                                                                
000354*01  -COPY W0008      -PRE  XXBX-                                         
000355       05  FILLER                PIC X.                                   
000356     EJECT                                                                
000357*01  -COPY W0008      -PRE  XXBU-                                         
000358       05  FILLER                PIC X.                                   
000359     EJECT                                                                
000360*01  -COPY W0008      -PRE  WDK6-                                         
000361       05  FILLER                PIC X.                                   
000362     EJECT                                                                
000363 01  PRIS-ARTC-PCB               PIC X.                                   
000364 01  PRIS-WDK7-PCB               PIC X.                                   
000365 01  PRIS-GMTA-PCB               PIC X.                                   
000366 01  PRIS-BETC-PCB               PIC X.                                   
000367 01  PRIS-PRIA-PCB               PIC X.                                   
000368 01  PRIS-PRIB-PCB               PIC X.                                   
000369 01  PRIS-COST-WDK6-PCB          PIC X.                                   
000370 01  PRIS-COST-WDK7-PCB          PIC X.                                   
000371 01  PRIS-COST-WDF1-PCB          PIC X.                                   
000372 01  PRIS-COST-9305-PCB          PIC X.                                   
000373 01  PRIS-COST-WDK72-PCB         PIC X.                                   
000374 01  PRIS-COST-WDB6-PCB          PIC X.                                   
000375 01  DLEV-LEVF-PCB               PIC X.                                   
000376 01  DLEV-LEVG-PCB               PIC X.                                   
000377 01  DLEV-LEVA-PCB               PIC X.                                   
000378 01  DLEV-ARTS-PCB               PIC X.                                   
000379 01  DLEV-WDB6-PCB               PIC X.                                   
000380     EJECT                                                                
000381 PROCEDURE DIVISION  USING  ORDQ-PCB ORDP-PCB GMTA-PCB                    
000382                            BENA-PCB                                      
000383                                     ORQI-PCB                             
000384                            XXBX-PCB XXBU-PCB WDK6-PCB                    
000385                            PRIS-ARTC-PCB                                 
000386                            PRIS-WDK7-PCB                                 
000387                            PRIS-GMTA-PCB                                 
000388                            PRIS-BETC-PCB                                 
000389                            PRIS-PRIA-PCB                                 
000390                            PRIS-PRIB-PCB                                 
000391                            PRIS-COST-WDK6-PCB                            
000392                            PRIS-COST-WDK7-PCB                            
000393                            PRIS-COST-WDF1-PCB                            
000394                            PRIS-COST-9305-PCB                            
000395                            PRIS-COST-WDK72-PCB                           
000396                            PRIS-COST-WDB6-PCB                            
000397                            DLEV-LEVF-PCB                                 
000398                            DLEV-LEVG-PCB                                 
000399                            DLEV-LEVA-PCB                                 
000400                            DLEV-ARTS-PCB                                 
000401                            DLEV-WDB6-PCB.                                
000402 MAIN SECTION.                                                            
000403     ENTRY 'DLITCBL' USING  ORDQ-PCB ORDP-PCB GMTA-PCB                    
000404                            BENA-PCB                                      
000405                                     ORQI-PCB                             
000406                            XXBX-PCB XXBU-PCB WDK6-PCB                    
000407                            PRIS-ARTC-PCB                                 
000408                            PRIS-WDK7-PCB                                 
000409                            PRIS-GMTA-PCB                                 
000410                            PRIS-BETC-PCB                                 
000411                            PRIS-PRIA-PCB                                 
000412                            PRIS-PRIB-PCB                                 
000413                            PRIS-COST-WDK6-PCB                            
000414                            PRIS-COST-WDK7-PCB                            
000415                            PRIS-COST-WDF1-PCB                            
000416                            PRIS-COST-9305-PCB                            
000417                            PRIS-COST-WDK72-PCB                           
000418                            PRIS-COST-WDB6-PCB                            
000419                            DLEV-LEVF-PCB                                 
000420                            DLEV-LEVG-PCB                                 
000421                            DLEV-LEVA-PCB                                 
000422                            DLEV-ARTS-PCB                                 
000423                            DLEV-WDB6-PCB.                                
000424                                                                          
000425     PERFORM A-INIT                                                       
000426                                                                          
000427     SORT SORTFIL                                                         
000428        ASCENDING S-FOR-IDDISTR S-FOR-IDKUNDNR                            
000429                  S-FOR-IDLOPNRE S-FOR-IDKORTNR-ERS                       
000430        INPUT  PROCEDURE B-BEHANDLA-INPOSTER                              
000431        OUTPUT PROCEDURE C-BEHANDLA-UTPOSTER.                             
000432                                                                          
000433     IF SORT-RETURN > ZERO                                                
000434        DISPLAY '*** W44022 - FEL VID SORTERING'                          
000435        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
000436     ELSE                                                                 
000437        PERFORM Z-FINIT                                                   
000438        MOVE ZERO TO RETURN-CODE                                          
000439        GOBACK                                                            
000440     END-IF                                                               
000441     .                                                                    
000442     EJECT                                                                
000443 A-INIT SECTION.                                                          
000444     SKIP2                                                                
000445     OPEN INPUT  INFIL                                                    
000446     OPEN OUTPUT UTFIL                                                    
000447                                                                          
000448     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
000449     .                                                                    
000450     EJECT                                                                
000451 B-BEHANDLA-INPOSTER SECTION.                                             
000452     SKIP2                                                                
000453     PERFORM BB-NOLLSTALL-TABELL                                          
000454     PERFORM BA-LAES-INPOST                                               
000455                                                                          
000456     PERFORM UNTIL INFIL-EOF = JA                                         
000457       IF NOT (FOR-IDLOPNRE = LOPNR-BRYT)                                 
000458          MOVE FOR-IDLOPNRE TO LOPNR-BRYT                                 
000459          PERFORM BB-NOLLSTALL-TABELL                                     
000460          MOVE +1 TO IX                                                   
000461          PERFORM BC-LAGG-I-TABELL                                        
000462          PERFORM BA-LAES-INPOST                                          
000463       ELSE                                                               
000464          ADD +1 TO IX                                                    
000465          PERFORM BC-LAGG-I-TABELL                                        
000466          PERFORM BA-LAES-INPOST                                          
000467       END-IF                                                             
000468       IF INFIL-EOF = NEJ                                                 
000469          IF NOT (FOR-IDLOPNRE = LOPNR-BRYT)                              
000470             PERFORM BD-LAS-RESTORDER                                     
000471          END-IF                                                          
000472       END-IF                                                             
000473     END-PERFORM                                                          
000474     IF TAB-IDKORTNR(1) > +0 OR TAB-IDLOPNR(1) > +0                       
000475        PERFORM BD-LAS-RESTORDER                                          
000476     END-IF                                                               
000477     .                                                                    
000478     EJECT                                                                
000479 BA-LAES-INPOST SECTION.                                                  
000480     SKIP2                                                                
000481     READ INFIL INTO FOR-W440001                                          
000482       AT END MOVE JA TO INFIL-EOF                                        
000483     END-READ                                                             
000484                                                                          
000485     IF INFIL-EOF = NEJ                                                   
000486       MOVE 'W44022'         TO POSTSUM-FDNAMN                            
000487       MOVE 'W44022D1'       TO POSTSUM-DDNAMN2                           
000488       MOVE 'IN'             TO POSTSUM-TRANSTYP                          
000489       CALL POSTSUM USING POSTSUM-PARM                                    
000490                                                                          
000491                                                                          
000492     END-IF                                                               
000493     .                                                                    
000494     EJECT                                                                
000495 BB-NOLLSTALL-TABELL SECTION.                                             
000496     SKIP2                                                                
000497     MOVE +1 TO IX                                                        
000498     PERFORM UNTIL IX > +50                                               
000499       MOVE ZERO TO          TAB-IDARTNR(IX)                              
000500                             TAB-IDKORTNR(IX)                             
000501                             TAB-IDLOPNR(IX)                              
000502                             TAB-KDRESTR(IX)                              
000503                             TAB-DIERS-ERS(IX)                            
000504                             TAB-DIERS-TILLK(IX)                          
000505                             TAB-IDANSK(IX)                               
000506                             TAB-KDERS(IX)                                
000507                             TAB-KDLEVSP(IX)                              
000508                             TAB-KDPRODSL(IX)                             
000509                             TAB-PRARTSTD(IX)                             
000510                             TAB-REKSIFFR(IX)                             
000511                             TAB-KVQPACK-1(IX)                            
000512                             TAB-TIDISPIN (IX)                            
000513                             TAB-REDIRLEV (IX)                            
000514                             TAB-IDARTNR-URS (IX)                         
000515                             TAB-KVKORT (IX)                              
000516                             TAB-KDRADERS(IX)                             
000517                             TAB-KDTILLK(IX)                              
000518       MOVE SPACE TO         TAB-IDDC(IX)                                 
000519                             TAB-BEERS(IX)                                
000520                             TAB-KDSORT(IX)                               
000521                             TAB-KDUART (IX)                              
000522                             TAB-KDUART-URS (IX)                          
000523       ADD +1 TO IX                                                       
000524     END-PERFORM                                                          
000525     .                                                                    
000526     EJECT                                                                
000527 BC-LAGG-I-TABELL SECTION.                                                
000528     SKIP2                                                                
000529     IF IX > +50                                                          
000530        DISPLAY '*** TABELL FÖR LITEN SECTION BC-'                        
000531        DISPLAY 'IDARTNR = ' FOR-IDARTNR                                  
000532        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
000533     END-IF                                                               
000534     MOVE FOR-IDARTNR      TO TAB-IDARTNR(IX)                             
000535     MOVE FOR-IDKORTNR-ERS TO TAB-IDKORTNR(IX)                            
000536     MOVE FOR-IDLOPNRE     TO TAB-IDLOPNR(IX)                             
000537     MOVE FOR-KDRESTR      TO TAB-KDRESTR(IX)                             
000538     MOVE FOR-IDDC         TO TAB-IDDC(IX)                                
000539     MOVE FOR-DIERS-ERS    TO TAB-DIERS-ERS(IX)                           
000540     MOVE FOR-DIERS-TILLK  TO TAB-DIERS-TILLK(IX)                         
000541     MOVE FOR-IDANSK       TO TAB-IDANSK(IX)                              
000542     MOVE FOR-KDERS        TO TAB-KDERS(IX)                               
000543     MOVE FOR-KDLEVSP      TO TAB-KDLEVSP(IX)                             
000544     MOVE FOR-KDPRODSL     TO TAB-KDPRODSL(IX)                            
000545     MOVE FOR-PRARTSTD     TO TAB-PRARTSTD(IX)                            
000546     MOVE FOR-REKSIFFR     TO TAB-REKSIFFR(IX)                            
000547     MOVE FOR-KVQPACK-1    TO TAB-KVQPACK-1(IX)                           
000548     MOVE FOR-TIDISPIN     TO TAB-TIDISPIN (IX)                           
000549     MOVE FOR-REDIRLEV     TO TAB-REDIRLEV (IX)                           
000550     MOVE FOR-KDRADERS     TO TAB-KDRADERS(IX)                            
000551     MOVE FOR-KDTILLK      TO TAB-KDTILLK(IX)                             
000552     MOVE FOR-BEERS        TO TAB-BEERS(IX)                               
000553     MOVE FOR-KDSORT       TO TAB-KDSORT(IX)                              
000554     MOVE FOR-KDUART       TO TAB-KDUART (IX)                             
000555     MOVE FOR-KDUART-URS   TO TAB-KDUART-URS (IX)                         
000556     MOVE FOR-IDARTNR-URS  TO TAB-IDARTNR-URS (IX)                        
000557     MOVE FOR-KVKORT       TO TAB-KVKORT (IX)                             
000558     .                                                                    
000559     EJECT                                                                
000560 BD-LAS-RESTORDER SECTION.                                                
000561     SKIP2                                                                
000562     MOVE NEJ              TO SW-2232-LAST                                
000563     MOVE LOW-VALUE  TO W-WDA5A1KY-MIN-X                                  
000564     MOVE HIGH-VALUE TO W-WDA5A1KY-MAX-X                                  
000565     MOVE TAB-IDARTNR(1) TO W-IDARTNR-MIN W-IDARTNR-MAX                   
000566     MOVE TAB-IDDC   (1) TO W-IDDC-MIN    W-IDDC-MAX                      
000567     MOVE '3' TO W-KDSTARAD                                               
000568     PERFORM IMS-GU-WDA5A1                                                
000569     MOVE +1 TO IX                                                        
000570     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
000571        MOVE SEQA-IDDISTR  TO TEST-IDDISTR                                
000572        IF SEQA-KDTPOTYP NOT = 4                                          
000573          IF  (NOT DIST19-SATS)                                           
000574               MOVE SEQA-IDDISTR  TO W-IDDISTR-01                         
000575               MOVE SEQA-IDKUNDNR TO W-IDKUNDNR-01                        
000576               MOVE SEQA-IDKUNDRF TO W-IDKUNDRF-01                        
000577               MOVE SEQA-IDARTNR  TO W-IDARTNR-01                         
000578               MOVE SEQA-IDLOPNR  TO W-IDLOPNR-01                         
000579               MOVE SEQA-IDDC     TO W-IDDC-01                            
000580               PERFORM IMS-GU-WDA501                                      
000581               IF SEGMENT-FINNS                                           
000582                                                                          
000583                  PERFORM BDB-LAS-ORQI                                    
000584                  PERFORM BDC-LAS-LARM                                    
000585                                                                          
000586                  MOVE +1 TO KORTNR-NYTT                                  
000587                  ADD  +1 TO LOPNR-NYTT                                   
000588                  PERFORM BDA-REDIGERA-SORTPOST                           
000589                  PERFORM BDD-RELEASE-SORTPOST                            
000590                  ADD +1 TO IX                                            
000591                  PERFORM UNTIL IX > +50 OR                               
000592                    (TAB-IDARTNR(IX) = +0 AND                             
000593                     TAB-BEERS(IX) = SPACE)                               
000594                    ADD +1 TO KORTNR-NYTT                                 
000595                    PERFORM BDA-REDIGERA-SORTPOST                         
000596                    PERFORM BDD-RELEASE-SORTPOST                          
000597                    ADD +1 TO IX                                          
000598                  END-PERFORM                                             
000599               END-IF                                                     
000600           ELSE                                                           
000601*            * SATS-RO BEHANDLAS EJ.                                      
000602               CONTINUE                                                   
000603           END-IF                                                         
000604         END-IF                                                           
000605         PERFORM IMS-GN-WDA5A1                                            
000606         MOVE +1 TO IX                                                    
000607     END-PERFORM                                                          
000608     .                                                                    
000609     EJECT                                                                
000610 BDA-REDIGERA-SORTPOST SECTION.                                           
000611     SKIP2                                                                
000612     MOVE '440'                     TO S-FOR-IDPTYP                       
000613     MOVE TAB-DIERS-ERS(IX)         TO S-FOR-DIERS-ERS                    
000614     MOVE TAB-DIERS-TILLK(IX)       TO S-FOR-DIERS-TILLK                  
000615     MOVE TAB-IDANSK(IX)            TO S-FOR-IDANSK                       
000616     MOVE TAB-IDARTNR(IX)           TO S-FOR-IDARTNR                      
000617     MOVE KORTNR-NYTT               TO S-FOR-IDKORTNR-ERS                 
000618     MOVE LOPNR-NYTT                TO S-FOR-IDLOPNRE                     
000619     MOVE TAB-IDDC(IX)              TO S-FOR-IDDC                         
000620     MOVE TAB-KDERS(IX)             TO S-FOR-KDERS                        
000621     MOVE TAB-KDLEVSP(IX)           TO S-FOR-KDLEVSP                      
000622     MOVE TAB-KDPRODSL(IX)          TO S-FOR-KDPRODSL                     
000623     MOVE TAB-KDRESTR(IX)           TO S-FOR-KDRESTR                      
000624     MOVE TAB-PRARTSTD(IX)          TO S-FOR-PRARTSTD                     
000625     MOVE TAB-REKSIFFR(IX)          TO S-FOR-REKSIFFR                     
000626     MOVE TAB-KVQPACK-1(IX)         TO S-FOR-KVQPACK-1                    
000627     MOVE TAB-BEERS(IX)             TO S-FOR-BEERS                        
000628     MOVE TAB-KDSORT(IX)            TO S-FOR-KDSORT                       
000629     MOVE TAB-TIDISPIN (IX)         TO S-FOR-TIDISPIN                     
000630     MOVE TAB-REDIRLEV (IX)         TO S-FOR-REDIRLEV                     
000631     MOVE TAB-KDUART (IX)           TO S-FOR-KDUART                       
000632     MOVE TAB-KDUART-URS (IX)       TO S-FOR-KDUART-URS                   
000633     MOVE TAB-IDARTNR-URS (IX)      TO S-FOR-IDARTNR-URS                  
000634     MOVE TAB-KVKORT (IX)           TO S-FOR-KVKORT                       
000635     MOVE TAB-KDRADERS(IX)          TO S-FOR-KDRADERS                     
000636     MOVE TAB-KDTILLK(IX)           TO S-FOR-KDTILLK                      
000637     MOVE RAD-BERADREF              TO S-FOR-BERADREF                     
000638     MOVE RAD-IDDISTR               TO S-FOR-IDDISTR                      
000639     MOVE RAD-IDKUNDRF              TO S-FOR-IDKUNDRF                     
000640     MOVE RAD-IDLOPNR               TO S-FOR-IDLOPNR                      
000641     MOVE RAD-KDFAKTYP              TO S-FOR-KDFAKTYP                     
000642     MOVE RAD-KDSTARAD              TO S-FOR-KDSTARAD                     
000643     MOVE RAD-IDKONTO               TO S-FOR-IDKONTO                      
000644     MOVE RAD-IDKUNDNR              TO S-FOR-IDKUNDNR                     
000645     MOVE RAD-IDKST                 TO S-FOR-IDKST                        
000646     MOVE RAD-IDANALYS              TO S-FOR-IDANALYS                     
000647     MOVE RAD-IDKUNDNR              TO S-FOR-IDKUNDNR                     
000648     MOVE RAD-IDDC-RO               TO S-FOR-IDDC-RO                      
000649     MOVE RAD-KDOI                  TO S-FOR-KDOI                         
000650     MOVE RAD-CLEARGROUP            TO S-FOR-CLEARGROUP                   
000651     MOVE RAD-KDDSP                 TO S-FOR-KDDSP                        
000652     MOVE RAD-KDFRAKT               TO S-FOR-KDFRAKT                      
000653     MOVE RAD-KDKVBRYT              TO S-FOR-KDKVBRYT                     
000654     MOVE RAD-KDORDKL               TO S-FOR-KDORDKL                      
000655     MOVE RAD-KDRAPRIO              TO S-FOR-KDRAPRIO                     
000656     MOVE RAD-KDROO                 TO S-FOR-KDROO                        
000657     MOVE RAD-KDTPOTYP              TO S-FOR-KDTPOTYP                     
000658     MOVE RAD-KDVRINFO              TO S-FOR-KDVRINFO                     
000659     MOVE RAD-KVART                 TO S-FOR-KVART                        
000660     MOVE RAD-PRARTNTO              TO S-FOR-PRARTNTO                     
000661     MOVE RAD-TIREGDAT              TO S-FOR-TIREGDAT                     
000662     MOVE RAD-TIRES                 TO S-FOR-TIRES                        
000663     MOVE RAD-DARODAT (3:6)         TO S-FOR-TIRODAT                      
000664     MOVE RAD-TITPO                 TO S-FOR-TITPO                        
000665     MOVE RAD-KDORDING              TO S-FOR-KDORDING                     
000666     MOVE RAD-KDPRTYP               TO S-FOR-KDPRTYP                      
000667     MOVE RAD-BEVOLREF              TO S-FOR-BEVOLREF                     
000668     MOVE RAD-FLINVEST              TO S-FOR-FLINVEST                     
000669     MOVE RAD-FLPRTILL              TO S-FOR-FLPRTILL                     
000670     MOVE RAD-FLTPOBEK              TO S-FOR-FLTPOBEK                     
000671     MOVE BEKUNDRF-WS               TO S-FOR-BEKUNDRF                     
000672     MOVE RAD-IDKAMPRF              TO S-FOR-IDKAMPRF                     
000673     MOVE RAD-IDLEVNR               TO S-FOR-IDLEVNR                      
000674     MOVE RAD-IDSYSTEM              TO S-FOR-IDSYSTEM                     
000675     MOVE RAD-KVBEART-Q             TO S-FOR-KVBEART-Q                    
000676     MOVE RAD-TIREGTID              TO S-FOR-TIREGTID                     
000677     MOVE RAD-DASENDAT (3:6)        TO S-FOR-TISENBEK-DAG                 
000678     MOVE RAD-TISENBEK-KL           TO S-FOR-TISENBEK-KL                  
000679     MOVE BEVARREF-WS               TO S-FOR-BEVARREF                     
000680     MOVE IDORDER-WS                TO S-FOR-IDORDER                      
000681     MOVE TIORDREG-WS               TO S-FOR-TIORDREG                     
000682     MOVE RAD-DEAL-PR-LINE          TO S-FOR-DEAL-PR-LINE                 
000683     MOVE RAD-PRAVCOST              TO S-FOR-PRAVCOST                     
000684     MOVE RAD-KDROPACK              TO S-FOR-KDROPACK                     
000685     MOVE RAD-IDARBREF              TO S-FOR-IDARBREF                     
000686                                                                          
000687*   FIX FIX  FLYTT FRÅN WDA5 BORTTAGEN TILLS PROBLEM                      
000688*   MED FELAKTIGA FÄLT ÄR LÖST   GK 050421                                
000689     MOVE RAD-KDORDTYP-LDC          TO S-FOR-KDORDTYP-LDC                 
000690     MOVE RAD-TIREPDAT              TO S-FOR-TIREPDAT                     
000691     MOVE RAD-IDKUNDRF-WIP          TO S-FOR-IDKUNDRF-WIP                 
000692                                                                          
000693*    MOVE SPACE                     TO S-FOR-KDORDTYP-LDC                 
000694*    MOVE ZERO                      TO S-FOR-TIREPDAT                     
000695*    MOVE SPACE                     TO S-FOR-IDKUNDRF-WIP                 
000696                                                                          
000697     MOVE ZERO                      TO S-FOR-KDLARM                       
000698     MOVE SPACE                     TO S-FOR-FLNYLARM                     
000699     MOVE ZERO                      TO S-FOR-TIREGDAT-LARM                
000700     MOVE ZERO                      TO S-FOR-IDANSK-LARM                  
000701                                                                          
000702     IF  SW-LARMAD-RAD = JA                                               
000703                                                                          
000704       IF TAB-KDRADERS (IX) = +1 OR                                       
000705         (TAB-KDRADERS (IX) = +0 AND TAB-KDTILLK (IX) = +0)               
000706           MOVE LARMWS-KDLARM       TO S-FOR-KDLARM                       
000707           MOVE LARMWS-IDANSK-LARM  TO S-FOR-IDANSK-LARM                  
000708       ELSE                                                               
000709         IF TAB-KDTILLK  (IX) = +1                                        
000710           MOVE LARMWS-KDLARM       TO S-FOR-KDLARM                       
000711           MOVE LARMWS-FLNYLARM     TO S-FOR-FLNYLARM                     
000712           MOVE LARMWS-TIREGDAT     TO S-FOR-TIREGDAT-LARM                
000713         END-IF                                                           
000714       END-IF                                                             
000715                                                                          
000716     ELSE                                                                 
000717       CONTINUE                                                           
000718     END-IF                                                               
000719                                                                          
000720     MOVE SPACE                     TO S-FOR-FLROSTYR                     
000721                                       S-FOR-BEART                        
000722     MOVE ZERO                      TO S-FOR-KVRO                         
000723                                       S-FOR-FLVR                         
000724     .                                                                    
000725     EJECT                                                                
000726 BDB-LAS-ORQI SECTION.                                                    
000727     SKIP2                                                                
000728     MOVE W-IDDISTR-01           TO W-IDGMTREF-IDDISTR                    
000729     MOVE W-IDKUNDNR-01          TO W-IDGMTREF-IDKUNDNR                   
000730     MOVE W-IDKUNDRF-01          TO WS-IDKUNDRF-OLD                       
000731     MOVE ZERO                   TO WS-IDKUNDRF-NEW-1-2                   
000732     MOVE WS-IDKUNDRF-OLD-1-5    TO WS-IDKUNDRF-NEW-3-7                   
000733     MOVE SPACE                  TO WS-IDKUNDRF-NEW-8-10                  
000734     MOVE WS-IDKUNDRF-NEW        TO W-IDGMTREF-IDKUNDRF                   
000735     PERFORM IMS-GU-ORQI-OHUV-CSEQ                                        
000736     MOVE ORQI-OHUV-BEVARREF     TO BEVARREF-WS                           
000737     MOVE ORQI-OHUV-IDORDER      TO IDORDER-WS                            
000738     MOVE ORQI-OHUV-BEKUNDRF     TO BEKUNDRF-WS                           
000739     MOVE ORQI-OHUV-TIREGDAT     TO TIORDREG-WS                           
000740     .                                                                    
000741     EJECT                                                                
000742 BDC-LAS-LARM SECTION.                                                    
000743     SKIP2                                                                
000744     MOVE NEJ                    TO SW-LARMAD-RAD                         
000745                                                                          
000746     IF  RAD-KDSTARAD = '1'                                               
000747     AND (RAD-KDTPOTYP = 2                                                
000748      OR  RAD-KDTPOTYP = 6)                                               
000749     AND RAD-FLTPOBEK = NEJ                                               
000750*      * OM LARM FINNS FÖR TPO SKALL DE ERSÄTTAS                          
000751                                                                          
000752       MOVE '2231'             TO W-2231-IDHTYP                           
000753       MOVE LOW-VALUE          TO W-2231-NYCKEL-VALFRI                    
000754       MOVE RAD-IDANSK         TO W-2232-IDANSK                           
000755       PERFORM IMS-GU-XXBX-2232                                           
000756       IF SEGMENT-FINNS                                                   
000757         MOVE XXBX-2232-IDANSK-LARM                                       
000758                               TO LARMWS-IDANSK-LARM                      
000759       ELSE                                                               
000760         MOVE ZERO             TO LARMWS-IDANSK-LARM                      
000761       END-IF                                                             
000762                                                                          
000763       MOVE '2223'               TO W-2223-IDHTYP                         
000764       MOVE LARMWS-IDANSK-LARM   TO W-2223-IDANSK                         
000765       MOVE LOW-VALUE            TO W-2223-LOW-VALUE                      
000766                                                                          
000767       MOVE RAD-DASENDAT(3:6)    TO W-2224-TISENBEK-DAG                   
000768       MOVE RAD-TISENBEK-KL      TO W-2224-TISENBEK-KL                    
000769                                                                          
000770       IF  RAD-KDTPOTYP = 2                                               
000771         MOVE 110                TO W-2224-KDLARM                         
000772       ELSE                                                               
000773         MOVE 100                TO W-2224-KDLARM                         
000774       END-IF                                                             
000775                                                                          
000776       PERFORM IMS-GU-XXBU-2224                                           
000777                                                                          
000778       IF  SEGMENT-FINNS                                                  
000779         MOVE XXBU-2224-KDLARM   TO LARMWS-KDLARM                         
000780         MOVE XXBU-2224-FLNYLARM TO LARMWS-FLNYLARM                       
000781         MOVE XXBU-2224-TIREGDAT TO LARMWS-TIREGDAT                       
000782         MOVE JA                 TO SW-LARMAD-RAD                         
000783       ELSE                                                               
000784         MOVE ZERO               TO LARMWS-KDLARM                         
000785         MOVE SPACE              TO LARMWS-FLNYLARM                       
000786         MOVE ZERO               TO LARMWS-TIREGDAT                       
000787       END-IF                                                             
000788     END-IF                                                               
000789     .                                                                    
000790     EJECT                                                                
000791 BDD-RELEASE-SORTPOST SECTION.                                            
000792     SKIP2                                                                
000793     RELEASE S-POST                                                       
000794                                                                          
000795     .                                                                    
000796     EJECT                                                                
000797 C-BEHANDLA-UTPOSTER SECTION.                                             
000798     SKIP2                                                                
000799     PERFORM CA-RETURN-SORTPOST                                           
000800     PERFORM UNTIL SORTFIL-EOF = JA                                       
000801                                                                          
000802        IF NOT (FOR-IDDISTR  = IDDISTR-BRYT) OR                           
000803           NOT (FOR-IDKUNDNR = IDKUNDNR-BRYT)                             
000804*----------* BRYTNING IDDISTR/IDKUNDNR (NY KUND)                          
000805           PERFORM CB-LAS-KUND                                            
000806           MOVE FOR-IDDISTR  TO IDDISTR-BRYT                              
000807           MOVE FOR-IDKUNDNR TO IDKUNDNR-BRYT                             
000808        END-IF                                                            
000809                                                                          
000810        IF FOR-IDARTNR > +0 AND FOR-BEERS = SPACE                         
000811*----------------* NORMAL POST (EJ TEXT-POST)                             
000812           PERFORM CC-LAS-ARTIKEL-OCH-BENAMNING                           
000813           PERFORM CD-REDIGERA-UTPOST                                     
000814           IF (FOR-KDTILLK = +1 AND NOT FOR-KDRESTR = +61) OR             
000815              (FOR-KDRADERS = +0 AND FOR-KDTILLK = +0)                    
000816*----------------* TILLK. ENTYDIG                                         
000817              PERFORM CE-STYRNING-SPARR                                   
000818           ELSE                                                           
000819              IF FOR-KDRADERS = +1 AND FOR-KDRESTR = +41                  
000820*----------------* ERSATT ENTYDIG                                         
000821                 PERFORM CH-UTGANGEN-ART                                  
000822              END-IF                                                      
000823           END-IF                                                         
000824           IF FOR-KDTILLK = +1 AND FOR-KDRESTR = +61                      
000825*---------------* TILLK. ICKE ENTYDIG                                     
000826              PERFORM CG-MULT-KV                                          
000827           END-IF                                                         
000828           PERFORM CF-SKRIV-UTPOST                                        
000829* OBS OBS OBS - DENNA RETURN ERSÄTTS AV RETURN FÖRE END-PERFORM           
000830*          PERFORM CA-RETURN-SORTPOST                                     
000831        ELSE                                                              
000832                                                                          
000833*-------------* TEXT-POST                                                 
000834           MOVE FOR-W440001     TO U-FOR-W440001                          
000835           PERFORM CF-SKRIV-UTPOST                                        
000836* OBS OBS OBS - DENNA RETURN ERSÄTTS AV RETURN FÖRE END-PERFORM           
000837*          PERFORM CA-RETURN-SORTPOST                                     
000838        END-IF                                                            
000839                                                                          
000840* OBS OBS OBS - INNAN DENNA RETURN ERSATTE OVANSTÅENDE RETURNER           
000841* OBS OBS OBS - SPRACK JOBBET PÅ CPU-TID NÄR INDATA BESTOD AV             
000842* OBS OBS OBS - EN "RIKTIG" POST + TILLHÖRANDE TEXTPOSTER                 
000843        PERFORM CA-RETURN-SORTPOST                                        
000844     END-PERFORM                                                          
000845     .                                                                    
000846     EJECT                                                                
000847 CA-RETURN-SORTPOST SECTION.                                              
000848     RETURN SORTFIL INTO FOR-W440001                                      
000849       AT END MOVE JA TO SORTFIL-EOF                                      
000850     END-RETURN                                                           
000851                                                                          
000852     IF SORTFIL-EOF = NEJ                                                 
000853       MOVE 'W44022'         TO POSTSUM-FDNAMN                            
000854       MOVE 'W44022DS'       TO POSTSUM-DDNAMN2                           
000855       MOVE 'SORT'           TO POSTSUM-TRANSTYP                          
000856       CALL POSTSUM USING POSTSUM-PARM                                    
000857                                                                          
000858                                                                          
000859     END-IF                                                               
000860     .                                                                    
000861     EJECT                                                                
000862 CB-LAS-KUND SECTION.                                                     
000863     SKIP2                                                                
000864     MOVE FOR-IDDISTR      TO W-IDDISTR-B2                                
000865     MOVE FOR-IDKUNDNR     TO W-IDKUNDNR-B2                               
000866                                                                          
000867     PERFORM IMS-GU-WLGMTA01                                              
000868     MOVE GMT-FLVR         TO FLVR-WS                                     
000869     MOVE GMT-IDSKYLT      TO W-IDSKYLT                                   
000870     MOVE GMT-FLPRERS      TO FLPRERS-WS                                  
000871     .                                                                    
000872     EJECT                                                                
000873 CC-LAS-ARTIKEL-OCH-BENAMNING SECTION.                                    
000874     SKIP2                                                                
000875     MOVE FOR-IDARTNR      TO W-IDARTNR                                   
000876                                                                          
000877     PERFORM IMS-GU-WLBENA11                                              
000878     MOVE TEXT-BEART       TO BEART-WS                                    
000879     .                                                                    
000880     EJECT                                                                
000881 CD-REDIGERA-UTPOST SECTION.                                              
000882     SKIP2                                                                
000883     MOVE FOR-W440001               TO U-FOR-W440001                      
000884     MOVE BEART-WS                  TO U-FOR-BEART                        
000885     MOVE FLVR-WS                   TO U-FOR-FLVR                         
000886     .                                                                    
000887     EJECT                                                                
000888 CE-STYRNING-SPARR          SECTION.                                      
000889******************************************************************        
000890*                                                                         
000891*    HÄR KONTROLLERAS TILLKOMMANDE-ÄNDRADE RADER.                         
000892*    AKTUELL RAD SOM "FELMÄRKS" KONTROLLERAS EJ VIDARE.                   
000893*    (SW-KDRESTR-FELMRK STYR DETTA.)                                      
000894*                                                                         
000895******************************************************************        
000896     SKIP2                                                                
000897     MOVE FOR-IDARTNR    TO TEST-IDARTNR                                  
000898     MOVE FOR-IDDISTR    TO TEST-IDDISTR                                  
000899                                                                          
000900     MOVE NEJ            TO SW-KDRESTR-FELMRK                             
000901                                                                          
000902     IF  SW-KDRESTR-FELMRK = NEJ                                          
000903       IF  BYT03-OBJEKT                                                   
000904         IF FOR-KDFAKTYP NOT = 'N'                                        
000905*----------* BYTESOBJEKT C1&<>N                                           
000906           MOVE +58 TO U-FOR-KDRESTR                                      
000907           MOVE JA TO SW-KDRESTR-FELMRK                                   
000908         END-IF                                                           
000909       END-IF                                                             
000910     END-IF                                                               
000911                                                                          
000912     IF  SW-KDRESTR-FELMRK = NEJ                                          
000913       IF  DIST03-SVERIGE                                                 
000914         IF  ART01-ASBEST-OVRIGT                                          
000915         OR  ART01-ASBEST-BROMS                                           
000916*----------* SVERIGE & ASBEST                                             
000917           MOVE +58 TO U-FOR-KDRESTR                                      
000918           MOVE JA TO SW-KDRESTR-FELMRK                                   
000919         END-IF                                                           
000920       END-IF                                                             
000921     END-IF                                                               
000922                                                                          
000923     IF  SW-KDRESTR-FELMRK = NEJ                                          
000924       IF  DIST03-NORGE                                                   
000925         IF  ART01-ASBEST-BROMS                                           
000926*----------* NORGE & ASBEST                                               
000927           MOVE +58 TO U-FOR-KDRESTR                                      
000928           MOVE JA TO SW-KDRESTR-FELMRK                                   
000929         END-IF                                                           
000930       END-IF                                                             
000931     END-IF                                                               
000932                                                                          
000933     IF  SW-KDRESTR-FELMRK = NEJ                                          
000934       IF  DIST65-BAHREIN                                                 
000935         IF  ART01-ASBEST-BAHREIN                                         
000936*----------* BAHREIN & ASBEST                                             
000937           MOVE +58 TO U-FOR-KDRESTR                                      
000938           MOVE JA TO SW-KDRESTR-FELMRK                                   
000939         END-IF                                                           
000940       END-IF                                                             
000941     END-IF                                                               
000942                                                                          
000943     IF  SW-KDRESTR-FELMRK = NEJ                                          
000944       IF  DIST03-SVERIGE                                                 
000945         IF  ART01-SPARR-SVERIGE                                          
000946*----------* SPÄRR SVERIGE                                                
000947           MOVE +58 TO U-FOR-KDRESTR                                      
000948           MOVE JA TO SW-KDRESTR-FELMRK                                   
000949         END-IF                                                           
000950       END-IF                                                             
000951     END-IF                                                               
000952                                                                          
000953     IF  SW-KDRESTR-FELMRK = NEJ                                          
000954       IF  FOR-PRARTSTD = +0                                              
000955*--------* STANDARDPRIS SAKNAS                                            
000956         MOVE +58 TO U-FOR-KDRESTR                                        
000957         MOVE JA TO SW-KDRESTR-FELMRK                                     
000958       END-IF                                                             
000959     END-IF                                                               
000960                                                                          
000961     IF  SW-KDRESTR-FELMRK = NEJ                                          
000962       IF  FOR-KDSTARAD = '2'                                             
000963         PERFORM CEB-ANROP-DLEV                                           
000964         IF  DLEV-IDLEVNR-UT NOT = SPACE  OR                              
000965           DLEV-FLSDCLEV-UT = JA                                          
000966*----------* DIRLEV RO                                                    
000967           MOVE +80 TO U-FOR-KDRESTR                                      
000968           MOVE JA TO SW-KDRESTR-FELMRK                                   
000969         END-IF                                                           
000970       END-IF                                                             
000971     END-IF                                                               
000972                                                                          
000973     IF  SW-KDRESTR-FELMRK = NEJ                                          
000974       IF  FOR-KDLEVSP = +20 OR +21                                       
000975*--------* KVALITETSSPÄRR                                                 
000976         MOVE +91 TO U-FOR-KDRESTR                                        
000977         MOVE +2 TO U-FOR-KDROO                                           
000978         MOVE JA TO SW-KDRESTR-FELMRK                                     
000979       END-IF                                                             
000980     END-IF                                                               
000981                                                                          
000982     IF NOT (FOR-KDRADERS = +0 AND FOR-KDTILLK = +0)                      
000983*-------* ERSATT ELLER TILLK.                                             
000984        COMPUTE U-FOR-KVART ROUNDED =                                     
000985            U-FOR-KVART * U-FOR-DIERS-TILLK                               
000986        COMPUTE U-FOR-KVBEART-Q ROUNDED =                                 
000987            U-FOR-KVBEART-Q * U-FOR-DIERS-TILLK                           
000988        END-COMPUTE                                                       
000989     END-IF                                                               
000990                                                                          
000991     IF FOR-KDTILLK = +1                                                  
000992*---TILLK                                                                 
000993       IF FLPRERS-WS = JA                                                 
000994         IF FOR-KDSTARAD NOT = '1'                                        
000995            PERFORM CEA-ANROP-PRIS                                        
000996            MOVE PRIS-PRAVCOST TO U-FOR-PRAVCOST                          
000997            MOVE PRIS-KDPRTYP  TO U-FOR-KDPRTYP                           
000998            MOVE PRIS-FLPRTILL TO U-FOR-FLPRTILL                          
001003            MOVE PRIS-PRARTNTO TO U-FOR-PRARTNTO                          
001005            IF DIST79-DEALER-PRICE OR                                     
001006               DIST79-ECOM-PRICE                                          
001007              CONTINUE                                                    
001008            ELSE                                                          
001009              MOVE PRIS-KDVALISO   TO U-FOR-KDVALISO                      
001010            END-IF                                                        
001011         ELSE                                                             
001012            MOVE ZERO          TO U-FOR-PRARTNTO                          
001013            MOVE ZERO          TO U-FOR-PRAVCOST                          
001014            MOVE SPACE         TO U-FOR-KDPRTYP                           
001015            MOVE NEJ           TO U-FOR-FLPRTILL                          
001016         END-IF                                                           
001017       ELSE                                                               
001018*---NORMALT EJ PRISERS                                                    
001019         IF FOR-KVKORT = +1                                               
001020*-----*ETT-TILL-ETT --> BEHÅLL PRIS                                       
001021            CONTINUE                                                      
001022         ELSE                                                             
001023*-----*ETT-TILL-FLERA--> BERÄKNA ELLER NOLLA                              
001024            IF FOR-KDSTARAD NOT = '1'                                     
001025              PERFORM CEA-ANROP-PRIS                                      
001026              MOVE PRIS-PRAVCOST TO U-FOR-PRAVCOST                        
001027              MOVE PRIS-KDPRTYP  TO U-FOR-KDPRTYP                         
001028              MOVE PRIS-FLPRTILL TO U-FOR-FLPRTILL                        
001032              MOVE PRIS-PRARTNTO TO U-FOR-PRARTNTO                        
001034             IF DIST79-DEALER-PRICE OR                                    
001035                DIST79-ECOM-PRICE                                         
001036              CONTINUE                                                    
001037             ELSE                                                         
001038               MOVE PRIS-KDVALISO   TO U-FOR-KDVALISO                     
001039             END-IF                                                       
001040            ELSE                                                          
001041              MOVE ZERO          TO U-FOR-PRARTNTO                        
001042              MOVE ZERO          TO U-FOR-PRAVCOST                        
001043              MOVE SPACE         TO U-FOR-KDPRTYP                         
001044              MOVE NEJ           TO U-FOR-FLPRTILL                        
001045            END-IF                                                        
001046         END-IF                                                           
001047       END-IF                                                             
001048     END-IF                                                               
001049                                                                          
001050     .                                                                    
001051     EJECT                                                                
001052 CEA-ANROP-PRIS SECTION.                                                  
001053     MOVE 1                  TO PRIS-KDCALL                               
001054     MOVE PROGRAM-NAMN       TO PRIS-IDPGM                                
001055     MOVE FOR-IDARTNR        TO PRIS-IDARTNR                              
001056     MOVE FOR-IDDISTR        TO PRIS-IDDISTR                              
001057                                TEST-IDDISTR                              
001058     MOVE FOR-IDKUNDNR       TO PRIS-IDKUNDNR                             
001059     MOVE FOR-IDDC           TO PRIS-IDDC                                 
001060     MOVE FOR-KDORDKL        TO PRIS-KDORDKL                              
001061     MOVE U-FOR-KVART        TO PRIS-KVBEART                              
001062     MOVE FOR-FLINVEST       TO PRIS-FLINVEST                             
001063                                                                          
001064     CALL W335PRIS           USING PRIS-W335PRIS                          
001065                                   PRIS-ARTC-PCB                          
001066                                   PRIS-WDK7-PCB                          
001067                                   PRIS-GMTA-PCB                          
001068                                   PRIS-BETC-PCB                          
001069                                   PRIS-PRIA-PCB                          
001070                                   PRIS-PRIB-PCB                          
001071                                   PRIS-COST-WDK6-PCB                     
001072                                   PRIS-COST-WDK7-PCB                     
001073                                   PRIS-COST-WDF1-PCB                     
001074                                   PRIS-COST-9305-PCB                     
001075                                   PRIS-COST-WDK72-PCB                    
001076                                   PRIS-COST-WDB6-PCB                     
001077                                                                          
001078     IF PRIS-KDSVAR = '2'                                                 
001079        MOVE 'CEA-: DIST/KUND SAKNAS NÄR W335PRIS LÄSER KUNDREG'          
001080                                                    TO FELTEXT            
001081        CALL ABEND USING RKOD-ABEND-MED-DUMP                              
001082     END-IF                                                               
001083                                                                          
001084     .                                                                    
001085     EJECT                                                                
001086 CEB-ANROP-DLEV SECTION.                                                  
001087                                                                          
001088     MOVE FOR-IDDISTR        TO DLEV-IDDISTR-IN                           
001089     MOVE FOR-IDKUNDNR       TO DLEV-IDKUNDNR-IN                          
001090     MOVE FOR-KDORDKL        TO DLEV-KDORDKL-IN                           
001091     MOVE FOR-IDARTNR        TO DLEV-IDARTNR-IN                           
001092     MOVE FOR-IDLEVNR        TO DLEV-IDLEVNR-IN                           
001093     MOVE FOR-KVART          TO DLEV-KVBEART-Q-IN                         
001094     MOVE FOR-REDIRLEV       TO DLEV-REDIRLEV-IN                          
001095     MOVE FOR-IDDC           TO DLEV-IDDC-IN                              
001096     MOVE FOR-IDDC           TO DLEV-IDDC-ORD-IN                          
001097     MOVE FOR-IDKAMPRF       TO DLEV-IDKAMPRF-IN                          
001098     MOVE FOR-KDTPOTYP       TO DLEV-KDTPOTYP-IN                          
001099     MOVE SPACE              TO DLEV-KDUART-IN                            
001100     MOVE NEJ                TO DLEV-FLFORBI-IN                           
001101     MOVE NEJ                TO DLEV-FLRESTN-IN                           
001102     MOVE FOR-IDARTNR        TO W-IDARTNR                                 
001103     PERFORM IMS-GU-WDK611                                                
001104     IF SEGMENT-FINNS                                                     
001105       MOVE CLAG-FLREFILL    TO DLEV-FLREFILL-IN                          
001106     ELSE                                                                 
001107       MOVE NEJ              TO DLEV-FLREFILL-IN                          
001108     END-IF                                                               
001109     MOVE FOR-KDORDING       TO DLEV-KDORDING-IN                          
001110     MOVE SPACE              TO DLEV-CLEARGROUP                           
001111                                DLEV-KDOI-UT                              
001120                                                                          
001130     MOVE +1 TO IX                                                        
001140     PERFORM UNTIL IX > IX-DCCLEAR-MAX                                    
001150        MOVE SPACE             TO DLEV-IDDC-CLEAR-IN(IX)                  
001160        ADD +1 TO IX                                                      
001170     END-PERFORM                                                          
001171                                                                          
001172     MOVE ZERO               TO DLEV-KDCALL                               
001173                                DLEV-IDKUNDRF-IN                          
001174                                                                          
001175     CALL W411DLEV USING     DLEV-W411DLEV                                
001176                             DLEV-LEVF-PCB                                
001177                             DLEV-LEVG-PCB                                
001178                             DLEV-LEVA-PCB                                
001179                             DLEV-ARTS-PCB                                
001180                             DLEV-WDB6-PCB                                
001181                             DUMMY-PCB                                    
001182     .                                                                    
001183     EJECT                                                                
001184 CF-SKRIV-UTPOST SECTION.                                                 
001185     SKIP2                                                                
001186     WRITE UTPOST FROM U-FOR-W440001                                      
001187                                                                          
001188     MOVE 'W44022'           TO POSTSUM-FDNAMN                            
001189     MOVE 'W44022D2'         TO POSTSUM-DDNAMN2                           
001190     MOVE 'UT'               TO POSTSUM-TRANSTYP                          
001191     CALL POSTSUM USING POSTSUM-PARM                                      
001192                                                                          
001193     .                                                                    
001194     EJECT                                                                
001195 CG-MULT-KV SECTION.                                                      
001196     SKIP2                                                                
001197     COMPUTE U-FOR-KVART ROUNDED =                                        
001198            (U-FOR-KVART * U-FOR-DIERS-TILLK) / U-FOR-DIERS-ERS           
001199             ON SIZE ERROR MOVE +0 TO U-FOR-KVART                         
001200     COMPUTE U-FOR-KVBEART-Q ROUNDED =                                    
001201            (U-FOR-KVBEART-Q * U-FOR-DIERS-TILLK)                         
001202             / U-FOR-DIERS-ERS                                            
001203             ON SIZE ERROR MOVE +0 TO U-FOR-KVBEART-Q                     
001204     END-COMPUTE                                                          
001205     .                                                                    
001206     EJECT                                                                
001207 CH-UTGANGEN-ART SECTION.                                                 
001208     SKIP2                                                                
001209     IF FOR-KDERS = +19 OR +29                                            
001210        MOVE +54 TO U-FOR-KDRESTR                                         
001211     ELSE                                                                 
001212        IF FOR-KDERS = +52                                                
001213           MOVE +52 TO U-FOR-KDRESTR                                      
001214        END-IF                                                            
001215     END-IF                                                               
001216     .                                                                    
001217     EJECT                                                                
001218 Z-FINIT SECTION.                                                         
001219     SKIP2                                                                
001220     CLOSE  INFIL                                                         
001221            UTFIL                                                         
001222                                                                          
001223     MOVE 'S' TO POSTSUM-OPKOD                                            
001224     CALL POSTSUM USING POSTSUM-PARM                                      
001225     .                                                                    
001226     EJECT                                                                
001227*    ---- IMS SEKTIONER                                                   
001228                                                                          
001229 IMS-GN-WDA5A1 SECTION.                                                   
001230                                                                          
001231     STRING 'WLORDQ01(WDA5A1KY=>' W-WDA5A1KY-MIN-X                        
001232                    '&WDA5A1KY<=' W-WDA5A1KY-MAX-X                        
001233                    '&KDSTARAD <' W-KDSTARAD-X ')'                        
001234            DELIMITED BY SIZE INTO SSA1                                   
001235     MOVE '  GEGB' TO GODK-STATUSKODER                                    
001236     CALL CBLTDLI USING GN  ORDQ-PCB DLI-IO-AREA SSA1                     
001237     MOVE ORDQ-STATUS-CODE TO STATUS-WS                                   
001238     PERFORM IMS-STATUSKONTROLL                                           
001239     .                                                                    
001240     SKIP3                                                                
001241 IMS-GU-WDA5A1 SECTION.                                                   
001242                                                                          
001243     STRING 'WLORDQ01(WDA5A1KY=>' W-WDA5A1KY-MIN-X                        
001244                    '&WDA5A1KY<=' W-WDA5A1KY-MAX-X                        
001245                    '&KDSTARAD <' W-KDSTARAD-X ')'                        
001246            DELIMITED BY SIZE INTO SSA1                                   
001247     MOVE '  GE' TO GODK-STATUSKODER                                      
001248     CALL CBLTDLI USING GU  ORDQ-PCB DLI-IO-AREA SSA1                     
001249     MOVE ORDQ-STATUS-CODE TO STATUS-WS                                   
001250     PERFORM IMS-STATUSKONTROLL                                           
001251     .                                                                    
001252     SKIP3                                                                
001253 IMS-GU-WDA501 SECTION.                                                   
001254                                                                          
001255     STRING 'WLORDP01(WDA501KY =' W-WDA501KY-X                            
001256                    '&IDDC     =' W-IDDC-01-X ')'                         
001257            DELIMITED BY SIZE INTO SSA1                                   
001258     MOVE '  ' TO GODK-STATUSKODER                                        
001259     CALL CBLTDLI USING GU  ORDP-PCB DLI-IO-AREA SSA1                     
001260     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
001261     PERFORM IMS-STATUSKONTROLL                                           
001262     .                                                                    
001263     EJECT                                                                
001264 IMS-GU-WLGMTA01 SECTION.                                                 
001265                                                                          
001266     STRING 'WLGMTA01(IDGMT    =' W-IDGMT-X   ')'                         
001267            DELIMITED BY SIZE INTO SSA1                                   
001268     MOVE '  ' TO GODK-STATUSKODER                                        
001269     CALL CBLTDLI USING GU  GMTA-PCB DLI-IO-AREA-WDB2 SSA1                
001270     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
001271     PERFORM IMS-STATUSKONTROLL                                           
001272     .                                                                    
001273     EJECT                                                                
001274 IMS-GU-WLBENA11 SECTION.                                                 
001275                                                                          
001276     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
001277            DELIMITED BY SIZE INTO SSA1                                   
001278     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
001279            DELIMITED BY SIZE INTO SSA2                                   
001280     MOVE '  ' TO GODK-STATUSKODER                                        
001281     CALL CBLTDLI USING GU  BENA-PCB DLI-IO-AREA SSA1 SSA2                
001282     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
001283     PERFORM IMS-STATUSKONTROLL                                           
001284     .                                                                    
001285     EJECT                                                                
001286 IMS-GU-ORQI-OHUV-CSEQ SECTION.                                           
001287                                                                          
001288     STRING 'WLORQI01(WDQ2CSEQ =' W-IDGMTREF-X ')'                        
001289            DELIMITED BY SIZE INTO SSA1                                   
001290     MOVE '  ' TO GODK-STATUSKODER                                        
001291     CALL CBLTDLI USING GU  ORQI-PCB DLI-IO-AREA2 SSA1                    
001292     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
001293     PERFORM IMS-STATUSKONTROLL                                           
001294     .                                                                    
001295     EJECT                                                                
001296 IMS-GU-XXBX-2232 SECTION.                                                
001297                                                                          
001298     STRING 'WLXXBX01(WDGXKEY  =' W-2231-WDGX01 ')'                       
001299            DELIMITED BY SIZE INTO SSA1                                   
001300     STRING 'WLXXBX11(WDGXKEY  =' W-2232-WDGXKEY ')'                      
001301            DELIMITED BY SIZE INTO SSA2                                   
001302     MOVE '  GE' TO GODK-STATUSKODER                                      
001303     CALL CBLTDLI USING GU  XXBX-PCB DLI-IO-AREA3 SSA1 SSA2               
001304     MOVE XXBX-STATUS-CODE TO STATUS-WS                                   
001305     PERFORM IMS-STATUSKONTROLL                                           
001306     .                                                                    
001307     SKIP3                                                                
001308 IMS-GU-XXBU-2224 SECTION.                                                
001309                                                                          
001310     STRING 'WLXXBU01(WDGXKEY  =' W-2223-WDGX2223 ')'                     
001311            DELIMITED BY SIZE INTO SSA1                                   
001312     STRING 'WLXXBU11(WDGXKEY  =' W-2224-WDGXKEY ')'                      
001313            DELIMITED BY SIZE INTO SSA2                                   
001314     MOVE '  GE' TO GODK-STATUSKODER                                      
001315     CALL CBLTDLI USING GU  XXBU-PCB DLI-IO-AREA3 SSA1 SSA2               
001316     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
001317     PERFORM IMS-STATUSKONTROLL                                           
001318     .                                                                    
001319     EJECT                                                                
001320 IMS-GU-WDK611 SECTION.                                                   
001321                                                                          
001322     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
001323             DELIMITED BY SIZE INTO SSA1                                  
001324     MOVE 'WDK611  '            TO SSA2                                   
001325     MOVE '  GE'                TO GODK-STATUSKODER                       
001326     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
001327     MOVE WDK6-STATUS-CODE      TO STATUS-WS                              
001328     PERFORM IMS-STATUSKONTROLL                                           
001329     .                                                                    
001330     EJECT                                                                
001331 IMS-STATUSKONTROLL SECTION.                                              
001332                                                                          
001333     SET STATUS-IX TO 1                                                   
001334     SEARCH GODK-STATUS                                                   
001335       AT END                                                             
001336         STRING 'STATUSKOD FRÅN IMS ' STATUS-WS                           
001337           DELIMITED BY SIZE INTO FELTEXT                                 
001338         CALL FELLOG                                                      
001339       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
001340         CONTINUE                                                         
001350     END-SEARCH.                                                          
