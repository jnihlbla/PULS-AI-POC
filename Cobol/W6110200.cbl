000001 ID DIVISION.                                                             
000002     SKIP2                                                                
000003 PROGRAM-ID.     W6110200.                                                
000004*AUTHOR.         LARS THELL.                                              
000005*DATE-WRITTEN.   92/08/21.                                                
000006                                                                          
000007*    REMARKS.                                                             
000008*                                                                         
000009*    FUNKTION:                                                            
000010*        PROGRAMMET TAR EMOT FIL FRÅN FAKTURERINGEN MED SATSORDER.        
000011*        FÖLJESEDLAR LÄGGS UPP PÅ W6D1 SOM MOTTAGNA DIREKT.               
000012*                                                                         
000013*        PROGRAMMET           UPPDATERAR  W6INLA (W6D1)                   
000014*        PROGRAMMET           UPPDATERAR  W6LOPA (W6G1)                   
000015*        PROGRAMMET           UPPDATERAR  W6CKPF (W6G2)                   
000016*        PROGRAMMET           LÄSER       W6PLAA (W6G1)                   
000017*        PROGRAMMET           LÄSER       WLARTC (WDK6)                   
000018*        PROGRAMMET           LÄSER       WLBENA (WDD3)                   
000019*        PROGRAMMET           LÄSER       WDJ2                            
000020*    SUB PROGRAMMET W006KOM   UPPDATERAR  WLKOMA (WDP8)                   
000021*                                                                         
000022     EJECT                                                                
000023 ENVIRONMENT DIVISION.                                                    
000024     SKIP2                                                                
000025 INPUT-OUTPUT SECTION.                                                    
000026                                                                          
000027 FILE-CONTROL.                                                            
000028     SKIP2                                                                
000029*          --- SATSORDER                                                  
000030     SELECT W4758B                     ASSIGN TO W61102D1.                
000031*          --- FELFIL                                                     
000032     SELECT W61102                     ASSIGN TO W61102D2.                
000033     EJECT                                                                
000034 DATA DIVISION.                                                           
000035     SKIP3                                                                
000036 FILE SECTION.                                                            
000037     SKIP3                                                                
000038 FD  W4758B                                                               
000039     RECORDING       F                                                    
000040     BLOCK CONTAINS  0.                                                   
000041     SKIP2                                                                
000042*01  -COPY W211R31     -PRE IN- -L.                                       
000043     SKIP2                                                                
000044 FD  W61102                                                               
000045     RECORDING       V                                                    
000046     BLOCK CONTAINS  0.                                                   
000047     SKIP2                                                                
000048*01  POST -COPY W211R31 -PRE UT- -L.                                      
000049     EJECT                                                                
000050 WORKING-STORAGE SECTION.                                                 
000051     SKIP2                                                                
000052*    -- CHECKED BY WY2000                                                 
000053 01  FELTEXT.                                                             
000054     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
000055     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
000056                                                                          
000057 01  W-IDLOPNRM                  PIC 9(9).                                
000058 01  W-0VVDLLLLK  REDEFINES W-IDLOPNRM.                                   
000059  03 FILLER                      PIC 9(1).                                
000060  03 W-VVD                       PIC 9(3).                                
000061  03 W-LLLL                      PIC 9(4).                                
000062  03 W-K                         PIC 9(1).                                
000063                                                                          
000064                                                                          
000065 77  IDPGM                       PIC X(8)    VALUE 'W6110200'.            
000066 77  6191-IX                     PIC S9(4)  VALUE +0    COMP SYNC.        
000067 77  MAX-6191-IX                 PIC S9(4)  VALUE +24   COMP SYNC.        
000068 77  6196-IX                     PIC S9(4)  VALUE +0    COMP SYNC.        
000069 77  MAX-6196-IX                 PIC S9(4)  VALUE +15   COMP SYNC.        
000070 77  TRANS-RAKN                  PIC S9(3)   VALUE ZERO COMP-3.           
000071 77  W-IDILIST                   PIC S9(5)   VALUE ZERO.                  
000072 01  CHKP-VAR.                                                            
000073   03 CHKP-MSG-IO-AREA-LENGTH    PIC S9(9)   VALUE +32 COMP SYNC.         
000074   03 CHKP-MSG-IO-AREA           PIC X(32)   VALUE SPACE.                 
000075   03 CHKP-AREA-LENGTH           PIC S9(9)   VALUE +32 COMP SYNC.         
000076   03 CHKP-AREA                  PIC X(32)   VALUE SPACE.                 
000077   03 CHKP-ANT                   PIC S9(3)   VALUE +0.                    
000078   03 CHKP-MAX                   PIC S9(3)   VALUE +100.                  
000079                                                                          
000080 77  JA                          PIC X       VALUE 'J'.                   
000081 77  NEJ                         PIC X       VALUE 'N'.                   
000082                                                                          
000083 77  LNG-P-TO-P-PREFIX           PIC S9(4)  VALUE +17   COMP SYNC.        
000084 77  W-KVPOST                    PIC S9(7)   VALUE ZERO COMP-3.           
000085 77  W-ADLAGOMR                  PIC 9(2)    VALUE ZERO.                  
000086 77  W-ADINLOMR-PRT              PIC X(4)    VALUE SPACE.                 
000087 77  W-FS-IDFTG                  PIC  9(2)   VALUE ZERO.                  
000088 77  W-INL-IDLEVNR               PIC  X(5)   VALUE SPACE.                 
000089 77  W-INL-IDFS                  PIC  X(8)   VALUE SPACE.                 
000090 77  W-INL-TIAVIDAT              PIC S9(7)  COMP-3  VALUE ZERO.           
000091     SKIP2                                                                
000092 77  W4758B-EOF-SW               PIC X       VALUE 'N'.                   
000093     88  END-OF-W4758B                       VALUE 'J'.                   
000094                                                                          
000095 77  FOERSTA-6191-SW             PIC X       VALUE 'J'.                   
000096     88  FOERSTA-6191                        VALUE 'J'.                   
000097                                                                          
000098 77  FOERSTA-6196-SW             PIC X       VALUE 'J'.                   
000099     88  FOERSTA-6196                        VALUE 'J'.                   
000100                                                                          
000101 77  INLA-SW                     PIC X       VALUE 'J'.                   
000102     88  INLA-OK                             VALUE 'J'.                   
000103                                                                          
000104 77  AVI-SW                      PIC X       VALUE 'N'.                   
000105     88  NY-AVI                              VALUE 'J'.                   
000106                                                                          
000107 77  ART-SW                      PIC X       VALUE 'J'.                   
000108     88  ART-OK                              VALUE 'J'.                   
000109                                                                          
000110 77  OMSTART-SW                  PIC X       VALUE 'N'.                   
000111     88  OMSTART                             VALUE 'J'.                   
000112                                                                          
000113     EJECT                                                                
000114 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
000115 01  FILLER REDEFINES DAGENS-DATUM.                                       
000116     03  DAGENS-DATUM-AAR        PIC 9(2).                                
000117     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
000118     03  DAGENS-DATUM-DAG        PIC 9(2).                                
000119                                                                          
000120 01  DAGENS-TID                  PIC 9(8)    VALUE ZERO.                  
000121 01  FILLER REDEFINES DAGENS-TID.                                         
000122     03  DAGENS-TID-HHMMSS       PIC 9(6).                                
000123     03  DAGENS-TID-HD           PIC 9(2).                                
000124                                                                          
000125 01  SPAR-IDILIST                PIC S9(5)   VALUE +0 COMP-3.             
000126 01  SPAR-IDAVINR                PIC S9(7)   VALUE +0 COMP-3.             
000127 01  SPAR-KVRADER                PIC S9(5)   VALUE +0 COMP-3.             
000128 01  SPAR-IDRADNR-INL            PIC S9(5)   VALUE +0 COMP-3.             
000129     EJECT                                                                
000130 01  FLT-FOR-BER-AV-IDLOPNRM.                                             
000131     03  FLT-LGD                 PIC S9  COMP SYNC   VALUE +7.            
000132     03  VAEGNINGSTAL            PIC 9(7)        VALUE 2121212.           
000133     03  VAEGNTAL-LGD            PIC S9  COMP SYNC   VALUE +7.            
000134     03  MODUL-10-11             PIC 9(2)            VALUE 10.            
000135     03  ALT-A-B                 PIC X(1)            VALUE 'B'.           
000136                                                                          
000137 01  FILLER                      PIC X(16)   VALUE 'INDEX '.              
000138 01  EMB-IX                      PIC S9(4)   VALUE +0 COMP SYNC.          
000139                                                                          
000140 01  W-SPAR-AREA.                                                         
000141   03  W-SPAR-IDAVINR          PIC S9(7)      COMP-3 VALUE ZERO.          
000142   03  W-SPAR-IDLEVNR          PIC  X(5)             VALUE SPACE.         
000143   03  W-SPAR-TIAVSDAT         PIC S9(7)      COMP-3 VALUE ZERO.          
000144   03  W-SPAR-KDARTURS         PIC X(2)              VALUE SPACE.         
000145   03  W-SPAR-VLARTNTO         PIC S9(8)V9(1) COMP-3 VALUE ZERO.          
000146   03  W-SPAR-VKART            PIC S9(7)      COMP-3 VALUE ZERO.          
000147   03  W-SPAR-ADLAGOMR         PIC S9(3)      COMP-3 VALUE ZERO.          
000148   03  W-SPAR-ADGANG           PIC S9(3)      COMP-3 VALUE ZERO.          
000149   03  W-SPAR-ADPLATS          PIC S9(5)      COMP-3 VALUE ZERO.          
000150   03  W-SPAR-BEFT             PIC S9(3)      COMP-3 VALUE ZERO.          
000151   03  W-SPAR-IDFTG            PIC  9(2)             VALUE ZERO.          
000152   03  W-SPAR-PRARTSTD         PIC S9(7)V9(2) COMP-3 VALUE ZERO.          
000153   03  W-SPAR-IDFKNGRP         PIC S9(5)      COMP-3 VALUE ZERO.          
000154   03  W-SPAR-BEART            PIC X(25)             VALUE SPACE.         
000155   03  W-SPAR-CL1-KDLAGEMB     PIC X(4)              VALUE SPACE.         
000156   03  W-SPAR-KDFARLIG         PIC S9(1)      COMP-3 VALUE ZERO.          
000157   03  W-SPAR-KDSORT           PIC X(2)              VALUE SPACE.         
000158   03  W-SPAR-KVMP             PIC S9(7)      COMP-3 VALUE ZERO.          
000159   03  W-SPAR-IDARTNR-EMBQ3    PIC S9(9)      COMP-3 VALUE ZERO.          
000160   03  W-CL1-ART-EMBQ3         PIC  9(9)             VALUE ZERO.          
000161   03  W-SPAR-IDORDNST         PIC  9(5)             VALUE ZERO.          
000164                                                                          
000165   03  RKOD-ABEND-UTAN-DUMP    PIC S9(9) VALUE +33 COMP SYNC.             
000166                                                                          
000167   EJECT                                                                  
000168*      --- VALID IDDC CODES                                               
000169*                                                                         
000170*01    -COPY WWDCKONS                                                     
000171       EJECT                                                              
000172*      --- EMBALLAGE-KODER MED ÖVERSÄTTNINGAR                             
000173*01    -COPY W611EMB3                                                     
000174       EJECT                                                              
000175 01  DYNAMISKA-SUBPROGRAM.                                                
000176*                                                                         
000177     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000178     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000179     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
000180     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
000181     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
000182     03  CHECK                   PIC X(8)    VALUE 'CHECK   '.            
000183     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
000184     EJECT                                                                
000185*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
000186*01 -COPY WDATAREA                                                        
000187     EJECT                                                                
000188*    --- PARAMETRAR TILL POSTSUM                                          
000189*                                                                         
000190*01  -COPY W0005   -PRE  POSTSUM-                                         
000191     EJECT                                                                
000192 01  IN-AREA-START           PIC X(24)   VALUE                            
000193                                             'IN-AREA-START'.             
000194     SKIP2                                                                
000195*01  AREA -COPY W211R31    -PRE IN-                                       
000196*                                                                         
000197     EJECT                                                                
000198 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
000199     SKIP3                                                                
000200 01  KOM-MSG-IO-AREA.                                                     
000201*03  -COPY WMSGKOM                                                        
000202     EJECT                                                                
000203 01      P-TO-P-SW.                                                       
000204  02     P-TO-P-KVLL             PIC S9(4)           COMP SYNC.           
000205  02     P-TO-P-KDZ1             PIC X(1)  VALUE LOW-VALUE.               
000206  02     P-TO-P-KDZ2             PIC X(1)  VALUE LOW-VALUE.               
000207  02     P-TO-P-KDTRANS          PIC X(8).                                
000208  02     P-TO-P-IDTRANS          PIC X(4).                                
000209  02     P-TO-P-KDMFSFOR         PIC X(1).                                
000210  02     P-TO-P-DATA             PIC X(1920).                             
000211     EJECT                                                                
000212 01      FILLER                  PIC X(24)   VALUE                        
000213                                 'MOD6191-MID-W6I19101'.                  
000214     SKIP2                                                                
000215     -COPY W6I19101 -PRE MOD6191-                                         
000216     EJECT                                                                
000217 01      FILLER                  PIC X(24)   VALUE                        
000218                                 'MOD6192-MID-W6I19201'.                  
000219     SKIP2                                                                
000220     -COPY W6I19201 -PRE MOD6192-                                         
000221     EJECT                                                                
000222 01      FILLER                  PIC X(24)   VALUE                        
000223                                 'MOD6196-MID-W6I19602'.                  
000224     SKIP2                                                                
000225 01  -COPY W6I19602 -PRE MOD6196-                                         
000226     EJECT                                                                
000227 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000228     SKIP3                                                                
000229 01  NYCKLAR-TILL-DLI.                                                    
000230     03  W-W6D101KY-X.                                                    
000231         05  W-D101KY-IDDC       PIC X(2)    VALUE '11'.                  
000232         05  W-D101KY-IDLEVNR    PIC  X(5)   VALUE SPACE.                 
000233         05  W-D101KY-IDFS       PIC  X(8)   VALUE SPACE.                 
000234         05  W-D101KY-TIAVIDAT   PIC S9(7)   VALUE ZERO COMP-3.           
000235     03  W-W6GXKEY-6003-X.                                                
000236         05  W-6005-IDHTYP       PIC X(4)    VALUE '6003'.                
000237         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
000238     03  W-W6GXKEY-6005-X.                                                
000239         05  W-6005-IDHTYP       PIC X(4)    VALUE '6005'.                
000240         05  W-6005-IDDC         PIC X(2)    VALUE '11'.                  
000241         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
000242     03  W-W6GXKEY-6006-X.                                                
000243         05  W-6006-ADINLOMR     PIC X(4)    VALUE SPACE.                 
000244         05  FILLER              PIC X(1)    VALUE LOW-VALUE.             
000245     03  W-W6GXKEY-6017-X.                                                
000246         05  W-6017-IDHTYP       PIC X(4)    VALUE '6017'.                
000247         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
000248     03  W-W6GXKEY-6018-X.                                                
000249         05  W-6018-KDSEGKEY     PIC X(1)    VALUE '1'.                   
000250     03  W-IDRADNR-INL-X.                                                 
000251         05  W-IDRADNR-INL       PIC S9(5)   VALUE ZERO COMP-3.           
000252     03  W-IDARTNR-X.                                                     
000253         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
000254     03  W-KDCLAGER-X.                                                    
000255         05  W-KDCLAGER          PIC S9(1)   VALUE ZERO COMP-3.           
000256     03  W-IDRADNR-X.                                                     
000257         05  W-IDRADNR           PIC S9(5)   VALUE ZERO COMP-3.           
000258     03  W-IDSKYLT-X.                                                     
000259         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
000260     03  W-IDORDNST-X.                                                    
000261         05  W-IDORDNSB          PIC S9(5)   VALUE ZERO COMP-3.           
000262         05  W-IDORDNSS          PIC S9(1)   VALUE ZERO COMP-3.           
000263     03  W-WDD3BSEQ-X.                                                    
000264         05  W-IDARTNR-WDD3      PIC S9(9)   VALUE ZERO COMP-3.           
000265     03  W-KDSEGKEY-X.                                                    
000266         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
000267     03  W-W6GX-6025-KEY-X.                                               
000268         05  W-IDHTYP-6025       PIC X(04)   VALUE '6025'.                
000269         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
000270     03  W-W6GX-6026-KEY-X.                                               
000271         05  FILLER              PIC X       VALUE '1'.                   
000272     SKIP2                                                                
000273*    --- STATUS-KOD FRÅN IMS                                              
000274 01  STATUS-WS                   PIC XX.                                  
000275     88  SEGMENT-FINNS                       VALUE '  '.                  
000276     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
000277     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
000278     88  SEGMENT-SLUT                        VALUE 'GB'.                  
000279     88  IMS-EJ-OK                           VALUE 'XD'.                  
000280     SKIP2                                                                
000281 01  GODK-STATUSKODER.                                                    
000282     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000283     SKIP3                                                                
000284 01  SSA1                        PIC X(64).                               
000285 01  SSA2                        PIC X(64).                               
000286 01  SSA3                        PIC X(64).                               
000287     EJECT                                                                
000288*    --- IMS FUNKTIONSKODER                                               
000289*01  -COPY W0003                                                          
000290     EJECT                                                                
000291*    ---  DLI INPUT-OUTPUT AREA                                           
000292 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
000293     SKIP3                                                                
000294 01  DLI-IO-AREA.                                                         
000295     03  IO-AREA                 PIC X(900)  VALUE SPACE.                 
000296     SKIP3                                                                
000297     03  WLARTC01 REDEFINES IO-AREA.                                      
000298*        05  -COPY WDK601  -PRE ARTC-                                     
000299     EJECT                                                                
000300     03  WLARTC11 REDEFINES IO-AREA.                                      
000301*        05  -COPY WDK611  -PRE ARTC-                                     
000302     EJECT                                                                
000303     03  WLBENA11 REDEFINES IO-AREA.                                      
000304*        05  -COPY WDD311  -PRE BENA11-                                   
000305     EJECT                                                                
000306     03  W6CKPF11 REDEFINES IO-AREA.                                      
000307*        05  -COPY W6GX6026  -PRE CKPF-                                   
000308     EJECT                                                                
000309     03  W6PLAA11 REDEFINES IO-AREA.                                      
000310*        05  -COPY W6GX6006 -PRE PLAA-                                    
000311     EJECT                                                                
000312     03  WDJ201   REDEFINES IO-AREA.                                      
000313*        05  -COPY WDJ201   -PRE WDJ2-                                    
000314     EJECT                                                                
000315*    ---  DLI INPUT-OUTPUT AREA                                           
000316 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA-2'.         
000317     SKIP3                                                                
000318 01  DLI-IO-AREA2.                                                        
000319     03  IO-AREA2               PIC X(150)  VALUE SPACE.                  
000320     03  W6INLA01 REDEFINES IO-AREA2.                                     
000321*        05  -COPY W6D101                                                 
000322     EJECT                                                                
000323 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA-3'.         
000324     SKIP3                                                                
000325 01  DLI-IO-AREA3.                                                        
000326     03  IO-AREA3               PIC X(150)  VALUE SPACE.                  
000327     03  W6INLA11 REDEFINES IO-AREA3.                                     
000328*        05  -COPY W6D111                                                 
000329     EJECT                                                                
000330 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA-4'.         
000331     SKIP3                                                                
000332 01  DLI-IO-AREA4.                                                        
000333     03  IO-AREA4               PIC X(150)  VALUE SPACE.                  
000334     03  W6INLA21 REDEFINES IO-AREA4.                                     
000335*        05  -COPY W6D121                                                 
000336     EJECT                                                                
000337 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA-5'.         
000338     SKIP3                                                                
000339 01  DLI-IO-AREA5.                                                        
000340     03  IO-AREA5                PIC X(150)  VALUE SPACE.                 
000341     03  W6LOPA11 REDEFINES IO-AREA5.                                     
000342*        05  -COPY W6GX6018 -PRE LOPA-                                    
000343     EJECT                                                                
000344 LINKAGE SECTION.                                                         
000345                                                                          
000346*01  -COPY W0009   -PRE MSG-                                              
000347     EJECT                                                                
000348*01  -COPY W0009   -PRE DISP-                                             
000349     EJECT                                                                
000350*01  -COPY W0009   -PRE STAT-                                             
000351     EJECT                                                                
000352*01  -COPY W0009   -PRE AR-                                               
000353     EJECT                                                                
000354*01  -COPY W0008  -PRE ARTC-                                              
000355     05  FILLER                  PIC X.                                   
000356     EJECT                                                                
000357*01  -COPY W0008  -PRE BENA-                                              
000358     05  FILLER                  PIC X.                                   
000359     EJECT                                                                
000360*01  -COPY W0008  -PRE INLA-                                              
000361     05  FILLER                  PIC X.                                   
000362     EJECT                                                                
000363*01  -COPY W0008  -PRE PLAA-                                              
000364     05  FILLER                  PIC X.                                   
000365     EJECT                                                                
000366*01  -COPY W0008  -PRE LOPA-                                              
000367     05  FILLER                  PIC X.                                   
000368     EJECT                                                                
000369*01  -COPY W0008  -PRE CKPF-                                              
000370     05  FILLER                  PIC X.                                   
000371     EJECT                                                                
000372*01  -COPY W0008  -PRE WDJ2-                                              
000373     05  FILLER                  PIC X.                                   
000374     EJECT                                                                
000375 01 KOM-KOMA-PCB                 PIC X.                                   
000376     EJECT                                                                
000377                                                                          
000378 PROCEDURE DIVISION  USING MSG-PCB  DISP-PCB STAT-PCB AR-PCB              
000379                                    ARTC-PCB BENA-PCB                     
000380                           INLA-PCB PLAA-PCB LOPA-PCB CKPF-PCB            
000381                           WDJ2-PCB KOM-KOMA-PCB.                         
000382                                                                          
000383     ENTRY 'DLITCBL' USING MSG-PCB  DISP-PCB STAT-PCB AR-PCB              
000384                                    ARTC-PCB BENA-PCB                     
000385                           INLA-PCB PLAA-PCB LOPA-PCB CKPF-PCB            
000386                           WDJ2-PCB KOM-KOMA-PCB.                         
000387     SKIP2                                                                
000388     PERFORM A-INIT                                                       
000389                                                                          
000390     PERFORM B-BEHANDLA-SATSER                                            
000391                                                                          
000392     PERFORM Z-FINIT                                                      
000393                                                                          
000394     MOVE ZERO TO RETURN-CODE                                             
000395     GOBACK                                                               
000396     .                                                                    
000397     EJECT                                                                
000398 A-INIT SECTION.                                                          
000399     SKIP2                                                                
000400     OPEN INPUT  W4758B                                                   
000401          OUTPUT W61102                                                   
000402                                                                          
000403     ACCEPT DAGENS-DATUM       FROM DATE                                  
000404     ACCEPT DAGENS-TID         FROM TIME                                  
000405                                                                          
000406     MOVE ZERO                 TO W-SPAR-IDAVINR                          
000407                                  W-SPAR-TIAVSDAT                         
000408     MOVE SPACE                TO W-SPAR-IDLEVNR                          
000409     MOVE IDPGM                TO POSTSUM-PROGNAMN                        
000410     PERFORM IMS-RESTART                                                  
000411                                                                          
000412     PERFORM IMS-LAS-ATERSTART                                            
000413                                                                          
000414     IF SEGMENT-FINNS                                                     
000415       IF CKPF-6026-KVPOST     > ZERO                                     
000416         PERFORM AA-ATERSTART-EFTER-ABEND                                 
000417         MOVE JA               TO OMSTART-SW                              
000418        ELSE                                                              
000419         MOVE NEJ              TO OMSTART-SW                              
000420       END-IF                                                             
000421     END-IF                                                               
000422     MOVE ZERO                 TO CHKP-ANT                                
000423     MOVE JA                   TO FOERSTA-6191-SW                         
000424                                  FOERSTA-6196-SW                         
000425     MOVE NEJ                  TO AVI-SW                                  
000426                                                                          
000427     MOVE 'IDAG'               TO DAT-KDDATFORM                           
000428     CALL WDATKONV USING          DAT-KDDATFORM                           
000429                                  DAT-I-TIDATUM                           
000430                                  DAT-O-TIDATUM                           
000431                                  DAT-KDSVAR                              
000432     .                                                                    
000433     EJECT                                                                
000434 AA-ATERSTART-EFTER-ABEND SECTION.                                        
000435     SKIP2                                                                
000436     PERFORM UNTIL W-KVPOST    = CKPF-6026-KVPOST OR                      
000437                                 END-OF-W4758B                            
000438       PERFORM S01-LAES-W4758B                                            
000439       MOVE IN-IDAVINR         TO W-SPAR-IDAVINR                          
000440       MOVE IN-IDLEVNR-INL     TO W-SPAR-IDLEVNR                          
000441       MOVE IN-TIAVSDAT        TO W-SPAR-TIAVSDAT                         
000442     END-PERFORM                                                          
000443     .                                                                    
000444     EJECT                                                                
000445 B-BEHANDLA-SATSER  SECTION.                                              
000446                                                                          
000447     IF OMSTART                                                           
000448         MOVE IN-IDLEVNR-INL TO W-D101KY-IDLEVNR                          
000449         MOVE IN-IDAVINR     TO W-D101KY-IDFS                             
000450         MOVE IN-TIAVSDAT    TO W-D101KY-TIAVIDAT                         
000451         PERFORM IMS-GU-INLA01                                            
000452     ELSE                                                                 
000453         PERFORM S01-LAES-W4758B                                          
000454     END-IF                                                               
000455                                                                          
000456     IF END-OF-W4758B                                                     
000457        CONTINUE                                                          
000458     ELSE                                                                 
000459        IF NY-AVI                                                         
000460           PERFORM S05-KONTROLLERA-MOT-INLA                               
000461        END-IF                                                            
000462        PERFORM S10-HAEMTA-KONTR-ART-INFO                                 
000463     END-IF                                                               
000464     PERFORM IMS-GHU-LOPA11                                               
000465     MOVE LOPA-6018-IDLOPNRM   TO W-IDLOPNRM                              
000466     MOVE LOPA-6018-IDILIST    TO W-IDILIST                               
000467                                                                          
000468     MOVE +1                   TO 6191-IX                                 
000469                                  6196-IX                                 
000470     PERFORM UNTIL END-OF-W4758B                                          
000471        IF INLA-OK AND ART-OK                                             
000472           PERFORM BA-SKAPA-FOELJESEDEL                                   
000473           IF END-OF-W4758B OR NY-AVI                                     
000474              PERFORM BB-SKAPA-R6192-TRANS                                
000475           END-IF                                                         
000476           IF CHKP-ANT                >  CHKP-MAX                         
000477              MOVE W-IDLOPNRM         TO LOPA-6018-IDLOPNRM               
000478              PERFORM IMS-REPL-LOPA11                                     
000479              PERFORM X-TAG-CHECKPOINT                                    
000480              PERFORM IMS-GU-INLA01                                       
000481              PERFORM IMS-GHU-LOPA11                                      
000482              MOVE LOPA-6018-IDLOPNRM TO W-IDLOPNRM                       
000483              MOVE ZERO               TO CHKP-ANT                         
000484           END-IF                                                         
000485        ELSE                                                              
000486           PERFORM S02-SKRIV-FELFIL                                       
000487           PERFORM S01-LAES-W4758B                                        
000488           IF NY-AVI                                                      
000489              PERFORM S05-KONTROLLERA-MOT-INLA                            
000490           END-IF                                                         
000491           PERFORM S10-HAEMTA-KONTR-ART-INFO                              
000492        END-IF                                                            
000493     END-PERFORM                                                          
000494                                                                          
000495     IF 6191-IX > 1                                                       
000496         PERFORM S03-STARTA-6191-TRANS                                    
000497     END-IF                                                               
000498                                                                          
000499     IF 6196-IX > 1                                                       
000500         PERFORM S04-STARTA-6196-TRANS                                    
000501     END-IF                                                               
000502                                                                          
000503     MOVE W-IDLOPNRM           TO LOPA-6018-IDLOPNRM                      
000504     MOVE W-IDILIST            TO LOPA-6018-IDILIST                       
000505     PERFORM IMS-REPL-LOPA11                                              
000506     .                                                                    
000507     EJECT                                                                
000508 BA-SKAPA-FOELJESEDEL   SECTION.                                          
000509                                                                          
000510     IF NY-AVI  OR                                                        
000511     CHKP-ANT > CHKP-MAX                                                  
000512        PERFORM BAA-SKAPA-INLA01                                          
000513        MOVE NEJ              TO AVI-SW                                   
000514     ELSE                                                                 
000515        MOVE IN-IDLEVNR-INL   TO W-INL-IDLEVNR                            
000516        MOVE IN-IDAVINR       TO W-INL-IDFS                               
000517        MOVE IN-TIAVSDAT      TO W-INL-TIAVIDAT                           
000518     END-IF                                                               
000519                                                                          
000520     PERFORM UNTIL END-OF-W4758B OR NY-AVI                                
000521        IF ART-OK                                                         
000522           PERFORM BAB-SKAPA-INLA11                                       
000523           PERFORM BAC-SKAPA-INLA21                                       
000524           PERFORM BAD-SKAPA-R6191-TRANSAR                                
000525           IF IN-KDRT = +3                                                
000526              PERFORM BAE-SKAPA-R6196-TRANS                               
000527           END-IF                                                         
000528        ELSE                                                              
000529           PERFORM S02-SKRIV-FELFIL                                       
000530        END-IF                                                            
000531        IF CHKP-ANT > CHKP-MAX                                            
000532           MOVE W-IDLOPNRM         TO LOPA-6018-IDLOPNRM                  
000533           PERFORM IMS-REPL-LOPA11                                        
000534           PERFORM X-TAG-CHECKPOINT                                       
000535           PERFORM IMS-GU-INLA01                                          
000536           PERFORM IMS-GHU-LOPA11                                         
000537           MOVE LOPA-6018-IDLOPNRM TO W-IDLOPNRM                          
000538           MOVE ZERO               TO CHKP-ANT                            
000539        END-IF                                                            
000540        PERFORM S01-LAES-W4758B                                           
000541        PERFORM S10-HAEMTA-KONTR-ART-INFO                                 
000542        IF NY-AVI                                                         
000543           PERFORM S05-KONTROLLERA-MOT-INLA                               
000544        END-IF                                                            
000545     END-PERFORM                                                          
000546     .                                                                    
000547     EJECT                                                                
000548 BAA-SKAPA-INLA01        SECTION.                                         
000549                                                                          
000550     MOVE WC-CDC-SE            TO  INL-IDDC                               
000551     MOVE IN-IDLEVNR-INL       TO  INL-IDLEVNR                            
000552                                   W-SPAR-IDLEVNR                         
000553     MOVE IN-IDAVINR           TO  INL-IDFS                               
000554                                   W-SPAR-IDAVINR                         
000555     MOVE IN-TIAVSDAT          TO  INL-TIAVIDAT                           
000556                                   W-SPAR-TIAVSDAT                        
000557     MOVE IN-IDKONTO           TO  INL-IDKONTO                            
000558     MOVE 'N'                  TO  INL-FLFEL                              
000559     MOVE ZERO                 TO  INL-IDARTNR                            
000560     MOVE IN-IDKST             TO  INL-IDKST                              
000561     MOVE SPACE                TO  INL-IDLBBET                            
000562     MOVE W-SPAR-IDFTG         TO  INL-IDFTG                              
000563     MOVE IN-IDANALYS          TO  INL-IDANALYS                           
000564     MOVE '310'                TO  INL-KDINL                              
000565     MOVE DAGENS-DATUM         TO  INL-TIANKDAG                           
000566                                   INL-TIINLMOT                           
000567     MOVE ZERO                 TO  INL-IDSHIPM                            
000568     PERFORM IMS-ISRT-INLA01                                              
000569                                                                          
000570     MOVE INL-IDLEVNR          TO W-INL-IDLEVNR                           
000571     MOVE INL-IDFS             TO W-INL-IDFS                              
000572     MOVE INL-TIAVIDAT         TO W-INL-TIAVIDAT                          
000573                                                                          
000574     .                                                                    
000575     EJECT                                                                
000576 BAB-SKAPA-INLA11        SECTION.                                         
000577                                                                          
000578     PERFORM BABA-HAEMTA-KDLAGEMB                                         
000579     PERFORM BABB-TA-UT-IDLOPNRM                                          
000580     ADD +1                    TO SPAR-IDRADNR-INL                        
000581     MOVE SPAR-IDRADNR-INL     TO  ART-IDRADNR-INL                        
000582                                     W-IDRADNR-INL                        
000583     MOVE IN-IDARTNR           TO  ART-IDARTNR                            
000584     MOVE WC-CDC-SE            TO  ART-IDDC                               
000585     MOVE W-SPAR-ADLAGOMR      TO  ART-ADLAGOMR                           
000586     MOVE W-SPAR-ADGANG        TO  ART-ADGANG                             
000587     MOVE W-SPAR-ADPLATS       TO  ART-ADPLATS                            
000588     MOVE W-SPAR-BEART         TO  ART-BEART                              
000589     MOVE W-SPAR-BEFT          TO  ART-BEFT                               
000590     MOVE SPACE                TO  ART-ADTRDEST-KIT                       
000591     MOVE NEJ                  TO  ART-FLFEL                              
000592                                   ART-FLETIKETT                          
000593                                   ART-FLKVAFEL                           
000594                                   ART-FLKVAKAR                           
000595                                   ART-FLKLAR                             
000596                                   ART-FLANNULL                           
000597     MOVE W-SPAR-IDFKNGRP      TO  ART-IDFKNGRP                           
000598     MOVE W-IDLOPNRM           TO  ART-IDLOPNRM                           
000599     MOVE W-SPAR-KDARTURS      TO  ART-KDARTURS                           
000600     MOVE W-SPAR-KDFARLIG      TO  ART-KDFARLIG                           
000601     MOVE ZERO                 TO  ART-KDINLPRIO                          
000602                                   ART-KDKVAANT                           
000603     MOVE W-SPAR-CL1-KDLAGEMB  TO  ART-KDLAGEMB                           
000604     MOVE W-SPAR-KDSORT        TO  ART-KDSORT                             
000605     MOVE IN-KDRT              TO  ART-KDRT                               
000606     MOVE IN-KVAVIS            TO  ART-KVAVIS                             
000607     MOVE ZERO                 TO  ART-KVAVIS-KIT                         
000608                                   ART-KVAVIS-PRIO                        
000609                                   ART-KVKVASEK-BER                       
000610                                   ART-KVKVASEK-VER                       
000611                                   ART-KVKVAPRIM-BER                      
000612                                   ART-KVKVAPRIM-VER                      
000613                                   ART-TIUPPDAT                           
000614     MOVE W-SPAR-KVMP          TO  ART-KVMP                               
000615     MOVE W-SPAR-PRARTSTD      TO  ART-PRARTSTD                           
000616     MOVE W-SPAR-VKART         TO  ART-VKART                              
000617     MOVE W-SPAR-VLARTNTO      TO  ART-VLARTNTO                           
000618     MOVE NEJ                  TO  ART-FLSPLPART                          
000619     MOVE SPACE                TO  ART-ADTRDEST                           
000620     MOVE SPACE                TO  ART-KDKVAINL                           
000621     PERFORM IMS-ISRT-INLA11                                              
000622     .                                                                    
000623     EJECT                                                                
000624 BABA-HAEMTA-KDLAGEMB  SECTION.                                           
000625                                                                          
000626     MOVE W-SPAR-IDARTNR-EMBQ3  TO W-CL1-ART-EMBQ3                        
000627                                                                          
000628                                                                          
000629     MOVE SPACE                 TO W-SPAR-CL1-KDLAGEMB                    
000630                                                                          
000631     MOVE 1                     TO EMB-IX                                 
000632     PERFORM UNTIL EMB-IX       >  TAB-EMBQ3-MAX OR                       
000633          TAB-KOD (EMB-IX)      = W-CL1-ART-EMBQ3                         
000634       ADD 1                    TO EMB-IX                                 
000635     END-PERFORM                                                          
000636                                                                          
000637     IF EMB-IX                  > TAB-EMBQ3-MAX                           
000638         CONTINUE                                                         
000639      ELSE                                                                
000640         IF TAB-KOD (EMB-IX)        =  W-CL1-ART-EMBQ3                    
000641             MOVE TAB-TEXT (EMB-IX) TO W-SPAR-CL1-KDLAGEMB                
000642         END-IF                                                           
000643     END-IF                                                               
000644     .                                                                    
000645     EJECT                                                                
000646 BABB-TA-UT-IDLOPNRM      SECTION.                                        
000647                                                                          
000648     IF DAT-TIAAVVD-GRP (3:3)  =  W-VVD                                   
000649         ADD +1                TO W-LLLL                                  
000650      ELSE                                                                
000651         MOVE DAT-TIAAVVD-GRP (3:3) TO W-VVD                              
000652         MOVE +1                    TO W-LLLL                             
000653     END-IF                                                               
000654     CALL CHECK USING W-IDLOPNRM (2:7) FLT-LGD                            
000655          VAEGNINGSTAL VAEGNTAL-LGD W-K MODUL-10-11 ALT-A-B               
000656     .                                                                    
000657     EJECT                                                                
000658 BAC-SKAPA-INLA21        SECTION.                                         
000659                                                                          
000660     MOVE +1                   TO  RAD-IDRADNR                            
000661     MOVE IN-KVAVIS            TO  RAD-KVINLART                           
000662     IF W-SPAR-BEFT = 15                                                  
000663* TMO-ARTIKEL                                                             
000664       MOVE '    '           TO  RAD-ADINLOMR                             
000665     ELSE                                                                 
000666       MOVE 'SAT '           TO  RAD-ADINLOMR                             
000667     END-IF                                                               
000668     MOVE +0                   TO  RAD-IDILIST                            
000669                                   RAD-IDILIRAD                           
000670     MOVE SPACE                TO  RAD-ADINLOMR-NXT                       
000671                                   RAD-KDINLSTA                           
000672                                   RAD-IDLEVNR-KOLLI                      
000673     MOVE NEJ                  TO  RAD-FLDIVKLI                           
000674                                   RAD-FLKVAANT                           
000675                                   RAD-FLPRIO                             
000676                                   RAD-FLSATS                             
000677                                   RAD-FLINLFB                            
000678                                   RAD-FLINLFP                            
000679                                   RAD-FLSVSLS                            
000680     MOVE ZERO                 TO  RAD-IDANSTNR                           
000681                                   RAD-IDINLVGN                           
000682                                   RAD-IDOKOLLI                           
000683                                   RAD-TIUPPDAT                           
000684     MOVE 39                   TO  RAD-KDINLPRIO                          
000685                                                                          
000686     PERFORM IMS-ISRT-INLA21                                              
000687     .                                                                    
000688     EJECT                                                                
000689 BAD-SKAPA-R6191-TRANSAR  SECTION.                                        
000690                                                                          
000691     PERFORM BADA-SKAPA-R31-TRANS                                         
000692     PERFORM BADB-SKAPA-F2-R77-TRANS                                      
000693     .                                                                    
000694     EJECT                                                                
000695 BADA-SKAPA-R31-TRANS  SECTION.                                           
000696                                                                          
000697     MOVE 'W6110200'           TO MOD6191-MID-IDPGM                       
000698     MOVE WC-CDC-SE            TO MOD6191-MID-IDDC                        
000699     MOVE ART-IDLOPNRM         TO MOD6191-MID-IDLOPNRM (6191-IX)          
000700     MOVE RAD-IDRADNR          TO MOD6191-MID-IDRADNR  (6191-IX)          
000701     MOVE ART-PRARTSTD         TO MOD6191-MID-PRARTSTD (6191-IX)          
000702     MOVE ZERO                 TO MOD6191-MID-KDINLPRIO(6191-IX)          
000703     MOVE +0                   TO MOD6191-MID-KVKOLLI  (6191-IX)          
000704     MOVE 'N'                  TO MOD6191-MID-FLINLI   (6191-IX)          
000705                                                                          
000706     MOVE SPACE                TO MOD6191-MID-ADINLOMR-OLD                
000707                                                   (6191-IX)              
000708     MOVE SPACE                TO MOD6191-MID-ADINLOMR-NXT-OLD            
000709                                                   (6191-IX)              
000710     MOVE RAD-KDINLSTA         TO MOD6191-MID-KDINLSTA-OLD                
000711                                                   (6191-IX)              
000712     MOVE RAD-KVINLART         TO MOD6191-MID-KVINLART-OLD                
000713                                                   (6191-IX)              
000714                                                                          
000715     MOVE SPACE                TO MOD6191-MID-ADINLOMR-NEW                
000716                                                   (6191-IX)              
000717     MOVE SPACE                TO MOD6191-MID-ADINLOMR-NXT-NEW            
000718                                                   (6191-IX)              
000719     MOVE SPACE                TO MOD6191-MID-KDINLSTA-NEW                
000720                                                   (6191-IX)              
000721     MOVE RAD-KVINLART         TO MOD6191-MID-KVINLART-NEW                
000722                                                   (6191-IX)              
000723     ADD +1                    TO 6191-IX                                 
000724                                                                          
000725     IF 6191-IX >  MAX-6191-IX                                            
000726         PERFORM S03-STARTA-6191-TRANS                                    
000727     END-IF                                                               
000728     .                                                                    
000729     EJECT                                                                
000730 BADB-SKAPA-F2-R77-TRANS  SECTION.                                        
000731                                                                          
000732     MOVE 'W6110200'           TO MOD6191-MID-IDPGM                       
000733     MOVE WC-CDC-SE            TO MOD6191-MID-IDDC                        
000734     MOVE ART-IDLOPNRM         TO MOD6191-MID-IDLOPNRM (6191-IX)          
000735     MOVE RAD-IDRADNR          TO MOD6191-MID-IDRADNR  (6191-IX)          
000736     MOVE ART-PRARTSTD         TO MOD6191-MID-PRARTSTD (6191-IX)          
000737     MOVE RAD-KDINLPRIO        TO MOD6191-MID-KDINLPRIO(6191-IX)          
000738     MOVE +0                   TO MOD6191-MID-KVKOLLI  (6191-IX)          
000739     MOVE 'N'                  TO MOD6191-MID-FLINLI   (6191-IX)          
000740                                                                          
000741     MOVE SPACE                TO MOD6191-MID-ADINLOMR-OLD                
000742                                                   (6191-IX)              
000743     MOVE SPACE                TO MOD6191-MID-ADINLOMR-NXT-OLD            
000744                                                   (6191-IX)              
000745     MOVE RAD-KDINLSTA         TO MOD6191-MID-KDINLSTA-OLD                
000746                                                   (6191-IX)              
000747     MOVE RAD-KVINLART         TO MOD6191-MID-KVINLART-OLD                
000748                                                   (6191-IX)              
000749                                                                          
000750     MOVE RAD-ADINLOMR         TO MOD6191-MID-ADINLOMR-NEW                
000751                                                   (6191-IX)              
000752     MOVE SPACE                TO MOD6191-MID-ADINLOMR-NXT-NEW            
000753                                                   (6191-IX)              
000754     MOVE SPACE                TO MOD6191-MID-KDINLSTA-NEW                
000755                                                   (6191-IX)              
000756     MOVE RAD-KVINLART         TO MOD6191-MID-KVINLART-NEW                
000757                                                   (6191-IX)              
000758     ADD +1                    TO 6191-IX                                 
000759                                                                          
000760     IF 6191-IX                >  MAX-6191-IX                             
000761         PERFORM S03-STARTA-6191-TRANS                                    
000762     END-IF                                                               
000763     .                                                                    
000764     EJECT                                                                
000765 BAE-SKAPA-R6196-TRANS  SECTION.                                          
000766                                                                          
000767     MOVE ART-ADLAGOMR         TO W-ADLAGOMR                              
000768     MOVE W-ADLAGOMR           TO W-6006-ADINLOMR                         
000769     PERFORM IMS-GU-PLAA-PLAA11                                           
000770     IF (SEGMENT-FINNS AND PLAA-6006-KDLORAPP = 2) OR                     
000771        RAD-IDOKOLLI           =  ZERO                                    
000772         IF W-SPAR-BEFT = 15                                              
000773           CONTINUE                                                       
000774         ELSE                                                             
000775           MOVE IN-IDAVINR TO W-SPAR-IDORDNST                             
000776           MOVE W-SPAR-IDORDNST (1:4) TO W-IDORDNSB                       
000777           MOVE W-SPAR-IDORDNST (5:1) TO W-IDORDNSS                       
000778           PERFORM IMS-GU-WDJ201                                          
000779           IF SEGMENT-FINNS                                               
000780              IF WDJ2-SHUV-IDPRC = '998E'                                 
000781                MOVE 'GRP1'    TO W-ADINLOMR-PRT                          
000782              ELSE                                                        
000783                MOVE 'LTRL'    TO W-ADINLOMR-PRT                          
000784              END-IF                                                      
000785           ELSE                                                           
000786             MOVE 'LTRL'       TO W-ADINLOMR-PRT                          
000787           END-IF                                                         
000788           IF 6196-IX > +1                                                
000789             IF W-ADINLOMR-PRT = MOD6196-MID-ADINLOMR-PRT                 
000790               CONTINUE                                                   
000791             ELSE                                                         
000792               PERFORM S04-STARTA-6196-TRANS                              
000793             END-IF                                                       
000794           END-IF                                                         
000795           MOVE ART-IDDC         TO MOD6196-MID-IDDC                      
000796           MOVE ART-IDLOPNRM     TO MOD6196-MID-IDLOPNRM (6196-IX)        
000797           ADD +1                TO 6196-IX                               
000798           MOVE W-ADINLOMR-PRT   TO MOD6196-MID-ADINLOMR-PRT              
000799           IF 6196-IX                    > MAX-6196-IX                    
000800             PERFORM S04-STARTA-6196-TRANS                                
000801           END-IF                                                         
000802         END-IF                                                           
000803     END-IF                                                               
000804     .                                                                    
000805     EJECT                                                                
000806 BB-SKAPA-R6192-TRANS  SECTION.                                           
000807                                                                          
000808     MOVE WC-CDC-SE            TO MOD6192-MID-IDDC                        
000809     MOVE W-INL-IDLEVNR        TO MOD6192-MID-IDLEVNR                     
000810     MOVE W-INL-IDFS           TO MOD6192-MID-IDFS                        
000811     MOVE W-INL-TIAVIDAT       TO MOD6192-MID-TIAVIDAT                    
000812     MOVE ZERO                 TO MOD6192-MID-IDRADNR-INL                 
000813                                                                          
000814     MOVE SPACE                TO MSG-KOM-WMSGKOM                         
000815     MOVE +53                  TO MSG-KOM-KVLL                            
000816     MOVE LOW-VALUE            TO MSG-KOM-KDZ1                            
000817     MOVE LOW-VALUE            TO MSG-KOM-KDZ2                            
000818     MOVE SPACE                TO MSG-KOM-KDTRANS                         
000819     MOVE 'W6I19201'           TO MSG-KOM-IDCPYTXT                        
000820     MOVE 'INLEV   '           TO MSG-KOM-IDSNDNOD                        
000821     MOVE 'W6110200'           TO MSG-KOM-IDSNDJOB                        
000822     MOVE DAGENS-DATUM         TO MSG-KOM-TIREGDAT                        
000823     MOVE DAGENS-TID           TO MSG-KOM-TIKLOCK                         
000824     MOVE SPACE                TO MSG-KOM-IDMFSMED                        
000825                                                                          
000826     COMPUTE P-TO-P-KVLL       =  LNG-P-TO-P-PREFIX + 29                  
000827     MOVE 'W6T192X '           TO P-TO-P-KDTRANS                          
000828     MOVE '6100'               TO P-TO-P-IDTRANS                          
000829     MOVE '1'                  TO P-TO-P-KDMFSFOR                         
000830     MOVE MOD6192-MID-W6I19201 TO P-TO-P-DATA                             
000831                                                                          
000832     CALL W006KOM USING MSG-PCB                                           
000833                        DISP-PCB                                          
000834                        KOM-KOMA-PCB                                      
000835                        MSG-KOM-WMSGKOM                                   
000836                        P-TO-P-SW                                         
000837     .                                                                    
000838     EJECT                                                                
000839 Z-FINIT SECTION.                                                         
000840                                                                          
000841     PERFORM IMS-LAS-ATERSTART                                            
000842     MOVE ZERO                 TO CKPF-6026-KVPOST                        
000843     ACCEPT CKPF-6026-TIUPPDAT FROM DATE                                  
000844     ACCEPT CKPF-6026-TIUPPTID FROM TIME                                  
000845     IF SEGMENT-SAKNAS                                                    
000846       MOVE '1'                TO CKPF-6026-KDSEGKEY                      
000847       PERFORM IMS-ISRT-ATERSTART                                         
000848     ELSE                                                                 
000849       PERFORM IMS-REPL-ATERSTART                                         
000850     END-IF                                                               
000851                                                                          
000852     CLOSE W4758B                                                         
000853           W61102                                                         
000854                                                                          
000855     MOVE 'S' TO POSTSUM-OPKOD                                            
000856     CALL POSTSUM USING POSTSUM-PARM                                      
000857     .                                                                    
000858     EJECT                                                                
000859 S01-LAES-W4758B  SECTION.                                                
000860     SKIP2                                                                
000861     READ W4758B INTO          IN-AREA                                    
000862     AT END                                                               
000863        SET END-OF-W4758B      TO TRUE                                    
000864                                                                          
000865     NOT AT END                                                           
000866        MOVE 'W4758B'          TO POSTSUM-FDNAMN                          
000867        MOVE 'W61102D1'        TO POSTSUM-DDNAMN2                         
000868        CALL POSTSUM USING POSTSUM-PARM                                   
000869                                                                          
000870        ADD +1                 TO W-KVPOST                                
000871                                  CHKP-ANT                                
000872        IF IN-IDAVINR          =  W-SPAR-IDAVINR AND                      
000873           IN-IDLEVNR-INL      =  W-SPAR-IDLEVNR AND                      
000874           IN-TIAVSDAT         =  W-SPAR-TIAVSDAT                         
000875            MOVE NEJ           TO AVI-SW                                  
000876         ELSE                                                             
000877            MOVE JA            TO AVI-SW                                  
000878        END-IF                                                            
000879     END-READ                                                             
000880     .                                                                    
000881     EJECT                                                                
000882 S02-SKRIV-FELFIL SECTION.                                                
000883     SKIP2                                                                
000884     WRITE UT-POST FROM IN-AREA                                           
000885                                                                          
000886     MOVE 'W61102'             TO POSTSUM-FDNAMN                          
000887     MOVE 'W61102D2'           TO POSTSUM-DDNAMN2                         
000888     CALL POSTSUM USING POSTSUM-PARM                                      
000889     .                                                                    
000890     EJECT                                                                
000891 S03-STARTA-6191-TRANS         SECTION.                                   
000892                                                                          
000893     COMPUTE MOD6191-MID-KVPOST = 6191-IX - 1                             
000894     COMPUTE P-TO-P-KVLL        =  LNG-P-TO-P-PREFIX +                    
000895                                   17 + (MOD6191-MID-KVPOST * 64)         
000896     MOVE 'W6T191X '            TO P-TO-P-KDTRANS                         
000897     MOVE '6100'                TO P-TO-P-IDTRANS                         
000898     MOVE '1'                   TO P-TO-P-KDMFSFOR                        
000899                                                                          
000900     MOVE MOD6191-MID-W6I19101  TO P-TO-P-DATA                            
000901                                                                          
000902     IF FOERSTA-6191                                                      
000903         PERFORM IMS-ISRT-ALT-MSG-6191                                    
000904         MOVE NEJ              TO FOERSTA-6191-SW                         
000905      ELSE                                                                
000906         PERFORM IMS-PURG-ALT-MSG-6191                                    
000907     END-IF                                                               
000908     MOVE +1                   TO 6191-IX                                 
000909     .                                                                    
000910     EJECT                                                                
000911 S04-STARTA-6196-TRANS  SECTION.                                          
000912                                                                          
000913     COMPUTE MOD6196-MID-KVPOST = 6196-IX - 1                             
000914     COMPUTE P-TO-P-KVLL        = LNG-P-TO-P-PREFIX +                     
000915                                  13 + (MOD6196-MID-KVPOST * 9)           
000916     MOVE 'W6T196X '           TO P-TO-P-KDTRANS                          
000917     MOVE '6100'               TO P-TO-P-IDTRANS                          
000918     MOVE '1'                  TO P-TO-P-KDMFSFOR                         
000919     MOVE MOD6196-MID-W6I19602 TO P-TO-P-DATA                             
000920     IF FOERSTA-6196                                                      
000921         PERFORM IMS-ISRT-ALT-MSG-6196                                    
000922         MOVE NEJ              TO FOERSTA-6196-SW                         
000923      ELSE                                                                
000924         PERFORM IMS-PURG-ALT-MSG-6196                                    
000925     END-IF                                                               
000926     MOVE +1                   TO 6196-IX                                 
000927     .                                                                    
000928     EJECT                                                                
000929 S05-KONTROLLERA-MOT-INLA    SECTION.                                     
000930                                                                          
000931     MOVE ZERO                 TO W-FS-IDFTG                              
000932     MOVE JA                   TO INLA-SW                                 
000933     MOVE IN-IDLEVNR-INL       TO W-D101KY-IDLEVNR                        
000934     MOVE IN-IDAVINR           TO W-D101KY-IDFS                           
000935     MOVE IN-TIAVSDAT          TO W-D101KY-TIAVIDAT                       
000936                                                                          
000937     PERFORM IMS-GU-INLA01                                                
000938     IF SEGMENT-FINNS                                                     
000939         MOVE NEJ              TO INLA-SW                                 
000940     END-IF                                                               
000941     .                                                                    
000942     EJECT                                                                
000943 S10-HAEMTA-KONTR-ART-INFO   SECTION.                                     
000944                                                                          
000945     MOVE JA                   TO ART-SW                                  
000946     MOVE IN-IDARTNR           TO W-IDARTNR                               
000947     MOVE 1                    TO W-KDCLAGER                              
000948     PERFORM S10B-HAEMTA-ARTC-INFO                                        
000949     PERFORM S10C-HAEMTA-BENA-INFO                                        
000950     PERFORM S10D-KONTROLLERA-ART-INFO                                    
000951     .                                                                    
000952     EJECT                                                                
000953 S10B-HAEMTA-ARTC-INFO  SECTION.                                          
000954                                                                          
000955     PERFORM IMS-GU-ARTC01                                                
000956     IF SEGMENT-FINNS                                                     
000957         MOVE ARTC-ART-IDFTG     TO W-SPAR-IDFTG                          
000958         MOVE ARTC-ART-IDFKNGRP  TO W-SPAR-IDFKNGRP                       
000959         MOVE ARTC-ART-KDSORT    TO W-SPAR-KDSORT                         
000960                                                                          
000961         PERFORM IMS-GNP-ARTC11                                           
000962         IF SEGMENT-FINNS                                                 
000963           MOVE ARTC-CLAG-KVMP  TO W-SPAR-KVMP                            
000964           MOVE ARTC-CLAG-PRARTSTD  TO W-SPAR-PRARTSTD                    
000965           MOVE ARTC-CLAG-KDFARLIG      TO W-SPAR-KDFARLIG                
000966           MOVE ARTC-CLAG-VLARTNTO      TO W-SPAR-VLARTNTO                
000967           MOVE ARTC-CLAG-VKART         TO W-SPAR-VKART                   
000968           MOVE ARTC-CLAG-BEFT          TO W-SPAR-BEFT                    
000969           MOVE ARTC-CLAG-IDARTNR-EMBQ3 TO W-SPAR-IDARTNR-EMBQ3           
000970           MOVE ARTC-CLAG-ADLAGOMR      TO W-SPAR-ADLAGOMR                
000971           MOVE ARTC-CLAG-ADGANG        TO W-SPAR-ADGANG                  
000972           MOVE ARTC-CLAG-ADPLATS       TO W-SPAR-ADPLATS                 
000973           MOVE ARTC-CLAG-KDARTURS      TO W-SPAR-KDARTURS                
000974          ELSE                                                            
000975             MOVE ZERO         TO W-SPAR-KVMP                             
000976             MOVE NEJ              TO ART-SW                              
000977         END-IF                                                           
000978      ELSE                                                                
000979         MOVE NEJ                  TO ART-SW                              
000980     END-IF                                                               
000981     .                                                                    
000982     EJECT                                                                
000983 S10C-HAEMTA-BENA-INFO  SECTION.                                          
000984                                                                          
000985     MOVE IN-IDARTNR            TO W-IDARTNR-WDD3                         
000986     MOVE 'S  '                 TO W-IDSKYLT                              
000987     PERFORM IMS-GU-BENA11                                                
000988     IF SEGMENT-FINNS                                                     
000989         MOVE BENA11-TEXT-BEART TO W-SPAR-BEART                           
000990      ELSE                                                                
000991         MOVE SPACE             TO W-SPAR-BEART                           
000992     END-IF                                                               
000993                                                                          
000994     .                                                                    
000995     EJECT                                                                
000996 S10D-KONTROLLERA-ART-INFO SECTION.                                       
000997                                                                          
000998     MOVE W-SPAR-IDFTG     TO W-FS-IDFTG                                  
000999     .                                                                    
001000     EJECT                                                                
001001 X-TAG-CHECKPOINT   SECTION.                                              
001002                                                                          
001003     PERFORM IMS-LAS-ATERSTART                                            
001004     MOVE W-KVPOST             TO CKPF-6026-KVPOST                        
001005     ACCEPT CKPF-6026-TIUPPDAT FROM DATE                                  
001006     ACCEPT CKPF-6026-TIUPPTID FROM TIME                                  
001007     IF SEGMENT-SAKNAS                                                    
001008       MOVE '1'                TO CKPF-6026-KDSEGKEY                      
001009       PERFORM IMS-ISRT-ATERSTART                                         
001010     ELSE                                                                 
001011       PERFORM IMS-REPL-ATERSTART                                         
001012     END-IF                                                               
001013     IF 6191-IX                      > 1                                  
001014       PERFORM S03-STARTA-6191-TRANS                                      
001015     END-IF                                                               
001016     IF 6196-IX                      > 1                                  
001017       PERFORM S04-STARTA-6196-TRANS                                      
001018     END-IF                                                               
001019     PERFORM IMS-CHECKPOINT                                               
001020     ADD +1                   TO DAGENS-TID-HD                            
001021     .                                                                    
001022     EJECT                                                                
001023* --- IMS SEKTIONER ---                                                   
001024     SKIP3                                                                
001025 IMS-ISRT-ALT-MSG-6191  SECTION.                                          
001026     MOVE SPACE TO GODK-STATUSKODER                                       
001027     CALL  CBLTDLI  USING ISRT STAT-PCB P-TO-P-SW                         
001028     MOVE STAT-STATUS-CODE TO STATUS-WS                                   
001029     PERFORM IMS-STATUSKONTROLL                                           
001030     .                                                                    
001031     SKIP3                                                                
001032 IMS-PURG-ALT-MSG-6191  SECTION.                                          
001033     MOVE SPACE TO GODK-STATUSKODER                                       
001034     CALL  CBLTDLI  USING PURG STAT-PCB P-TO-P-SW                         
001035     MOVE STAT-STATUS-CODE TO STATUS-WS                                   
001036     PERFORM IMS-STATUSKONTROLL                                           
001037     .                                                                    
001038     SKIP3                                                                
001039 IMS-ISRT-ALT-MSG-6196  SECTION.                                          
001040     MOVE SPACE TO GODK-STATUSKODER                                       
001041     CALL  CBLTDLI  USING ISRT AR-PCB P-TO-P-SW                           
001042     MOVE AR-STATUS-CODE TO STATUS-WS                                     
001043     PERFORM IMS-STATUSKONTROLL                                           
001044     .                                                                    
001045     SKIP3                                                                
001046 IMS-PURG-ALT-MSG-6196  SECTION.                                          
001047     MOVE SPACE TO GODK-STATUSKODER                                       
001048     CALL  CBLTDLI  USING PURG AR-PCB P-TO-P-SW                           
001049     MOVE AR-STATUS-CODE TO STATUS-WS                                     
001050     PERFORM IMS-STATUSKONTROLL                                           
001051     .                                                                    
001052     EJECT                                                                
001053 IMS-RESTART SECTION.                                                     
001054     SKIP2                                                                
001055     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
001056     MOVE '  ' TO GODK-STATUSKODER                                        
001057     CALL CBLTDLI USING XRST MSG-PCB                                      
001058                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
001059                        CHKP-AREA-LENGTH CHKP-AREA                        
001060     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
001061     PERFORM IMS-STATUSKONTROLL                                           
001062     .                                                                    
001063     SKIP3                                                                
001064 IMS-REPL-ATERSTART SECTION.                                              
001065     SKIP2                                                                
001066     MOVE SPACE TO GODK-STATUSKODER                                       
001067     CALL CBLTDLI USING REPL CKPF-PCB DLI-IO-AREA                         
001068     MOVE CKPF-STATUS-CODE TO STATUS-WS                                   
001069     PERFORM IMS-STATUSKONTROLL                                           
001070     .                                                                    
001071     EJECT                                                                
001072 IMS-CHECKPOINT SECTION.                                                  
001073     SKIP2                                                                
001074     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
001075     MOVE '  XD' TO GODK-STATUSKODER                                      
001076     CALL CBLTDLI USING CHKP MSG-PCB                                      
001077                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
001078                        CHKP-AREA-LENGTH CHKP-AREA                        
001079     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
001080     PERFORM IMS-STATUSKONTROLL                                           
001081                                                                          
001082     IF IMS-EJ-OK                                                         
001083       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
001084       DISPLAY FELTEXT                                                    
001085       CALL FELLOG                                                        
001086     END-IF                                                               
001087     .                                                                    
001088     EJECT                                                                
001089 IMS-LAS-ATERSTART SECTION.                                               
001090     SKIP2                                                                
001091     STRING 'W6CKPF01(W6GXKEY  =' W-W6GX-6025-KEY-X ')'                   
001092            DELIMITED BY SIZE INTO SSA1                                   
001093     STRING 'W6CKPF11(KDSEGKEY =' W-W6GX-6026-KEY-X ')'                   
001094            DELIMITED BY SIZE INTO SSA2                                   
001095     MOVE '  GE' TO GODK-STATUSKODER                                      
001096     CALL CBLTDLI USING GHU CKPF-PCB DLI-IO-AREA SSA1 SSA2                
001097     MOVE CKPF-STATUS-CODE TO STATUS-WS                                   
001098     PERFORM IMS-STATUSKONTROLL                                           
001099     .                                                                    
001100     SKIP3                                                                
001101 IMS-ISRT-ATERSTART SECTION.                                              
001102     SKIP2                                                                
001103     STRING 'W6CKPF01(W6GXKEY  =' W-W6GX-6025-KEY-X ')'                   
001104          DELIMITED BY SIZE INTO SSA1                                     
001105     MOVE 'W6CKPF11' TO SSA2                                              
001106     MOVE '  ' TO GODK-STATUSKODER                                        
001107     CALL CBLTDLI USING ISRT CKPF-PCB DLI-IO-AREA SSA1 SSA2               
001108     MOVE CKPF-STATUS-CODE TO STATUS-WS                                   
001109     PERFORM IMS-STATUSKONTROLL                                           
001110     .                                                                    
001111     EJECT                                                                
001112 IMS-GU-ARTC01 SECTION.                                                   
001113     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
001114          DELIMITED BY SIZE INTO SSA1                                     
001115     MOVE '  GE' TO GODK-STATUSKODER                                      
001116     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
001117     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
001118     PERFORM IMS-STATUSKONTROLL                                           
001119     .                                                                    
001120     SKIP3                                                                
001121 IMS-GNP-ARTC11 SECTION.                                                  
001122     MOVE 'WLARTC11' TO SSA1                                              
001123     MOVE '  GE' TO GODK-STATUSKODER                                      
001124     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1                     
001125     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
001126     PERFORM IMS-STATUSKONTROLL                                           
001127     .                                                                    
001128     SKIP3                                                                
001129 IMS-GU-BENA11 SECTION.                                                   
001130     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
001131          DELIMITED BY SIZE INTO SSA1                                     
001132     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
001133          DELIMITED BY SIZE INTO SSA2                                     
001134     MOVE '  GE' TO GODK-STATUSKODER                                      
001135     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA SSA1 SSA2                 
001136     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
001137     PERFORM IMS-STATUSKONTROLL                                           
001138     .                                                                    
001139     EJECT                                                                
001140 IMS-ISRT-INLA01 SECTION.                                                 
001141                                                                          
001142     MOVE 'W6INLA01 ' TO SSA1                                             
001143     MOVE '  II' TO GODK-STATUSKODER                                      
001144     CALL CBLTDLI USING ISRT INLA-PCB DLI-IO-AREA2 SSA1                   
001145     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
001146     PERFORM IMS-STATUSKONTROLL                                           
001147     .                                                                    
001148     SKIP3                                                                
001149 IMS-GU-INLA01 SECTION.                                                   
001150                                                                          
001151     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
001152          DELIMITED BY SIZE INTO SSA1                                     
001153     MOVE '  GE' TO GODK-STATUSKODER                                      
001154     CALL CBLTDLI USING GU INLA-PCB DLI-IO-AREA2 SSA1                     
001155     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
001156     PERFORM IMS-STATUSKONTROLL                                           
001157     .                                                                    
001158     SKIP3                                                                
001159 IMS-ISRT-INLA11 SECTION.                                                 
001160                                                                          
001161     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
001162          DELIMITED BY SIZE INTO SSA1                                     
001163     MOVE 'W6INLA11 ' TO SSA2                                             
001164     MOVE '  II' TO GODK-STATUSKODER                                      
001165     CALL CBLTDLI USING ISRT INLA-PCB DLI-IO-AREA3 SSA1 SSA2              
001166     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
001167     PERFORM IMS-STATUSKONTROLL                                           
001168     .                                                                    
001169     EJECT                                                                
001170 IMS-ISRT-INLA21 SECTION.                                                 
001171                                                                          
001172     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
001173          DELIMITED BY SIZE INTO SSA1                                     
001174     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INL-X ')'                     
001175          DELIMITED BY SIZE INTO SSA2                                     
001176     MOVE 'W6INLA21 ' TO SSA3                                             
001177     MOVE '    ' TO GODK-STATUSKODER                                      
001178     CALL CBLTDLI USING ISRT INLA-PCB DLI-IO-AREA4 SSA1 SSA2 SSA3         
001179     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
001180     PERFORM IMS-STATUSKONTROLL                                           
001181     .                                                                    
001182     SKIP3                                                                
001183 IMS-GHU-LOPA11 SECTION.                                                  
001184     STRING 'W6LOPA01(W6GXKEY  =' W-W6GXKEY-6017-X ')'                    
001185          DELIMITED BY SIZE INTO SSA1                                     
001186     STRING 'W6LOPA11(KDSEGKEY =' W-W6GXKEY-6018-X ')'                    
001187          DELIMITED BY SIZE INTO SSA2                                     
001188     MOVE '    ' TO GODK-STATUSKODER                                      
001189     CALL CBLTDLI USING GHU LOPA-PCB DLI-IO-AREA5 SSA1 SSA2               
001190     MOVE LOPA-STATUS-CODE TO STATUS-WS                                   
001191     PERFORM IMS-STATUSKONTROLL                                           
001192     .                                                                    
001193     SKIP3                                                                
001194 IMS-REPL-LOPA11 SECTION.                                                 
001195     MOVE '    ' TO GODK-STATUSKODER                                      
001196     CALL CBLTDLI USING REPL LOPA-PCB DLI-IO-AREA5                        
001197     MOVE LOPA-STATUS-CODE TO STATUS-WS                                   
001198     PERFORM IMS-STATUSKONTROLL                                           
001199     .                                                                    
001200     EJECT                                                                
001201 IMS-GU-PLAA-PLAA11 SECTION.                                              
001202     STRING 'W6PLAA01(W6GXKEY  =' W-W6GXKEY-6005-X ')'                    
001203          DELIMITED BY SIZE INTO SSA1                                     
001204     STRING 'W6PLAA11(W6GXKEY  =' W-W6GXKEY-6006-X ')'                    
001205          DELIMITED BY SIZE INTO SSA2                                     
001206     MOVE '  GE' TO GODK-STATUSKODER                                      
001207     CALL CBLTDLI USING GU PLAA-PCB DLI-IO-AREA SSA1 SSA2                 
001208     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
001209     PERFORM IMS-STATUSKONTROLL                                           
001210     .                                                                    
001211     EJECT                                                                
001212 IMS-GU-WDJ201 SECTION.                                                   
001213     STRING 'WDJ201  (IDORDNST =' W-IDORDNST-X ')'                        
001214          DELIMITED BY SIZE INTO SSA1                                     
001215     MOVE '  GE' TO GODK-STATUSKODER                                      
001216     CALL CBLTDLI USING GU WDJ2-PCB DLI-IO-AREA SSA1                      
001217     MOVE WDJ2-STATUS-CODE TO STATUS-WS                                   
001218     PERFORM IMS-STATUSKONTROLL                                           
001219     .                                                                    
001220     SKIP3                                                                
001221 IMS-STATUSKONTROLL SECTION.                                              
001222     SKIP2                                                                
001223     SET STATUS-IX TO 1                                                   
001224     SEARCH GODK-STATUS                                                   
001225       AT END                                                             
001226         MOVE 'FEL VID DL1 ANROP' TO FELTEXT-STR                          
001227         DISPLAY FELTEXT                                                  
001228         CALL FELLOG                                                      
001229       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
001230         CONTINUE                                                         
001240     END-SEARCH                                                           
001300     .                                                                    
