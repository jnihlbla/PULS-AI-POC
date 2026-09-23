000001 ID DIVISION.                                                             
000002     SKIP2                                                                
000003 PROGRAM-ID.     W6013310.                                                
000004*AUTHOR.         KATARINA KYMMER / ARCHANA BHAT.                          
000005*DATE-WRITTEN.   92/08/14 / JUNE 2012.                                    
000006                                                                          
000007**   REMARKS.                                                             
000008*                                                                         
000009*    FUNKTION:                                                            
000010*        PROGRAMMET ÄR EN MPP SOM VISAR VILKA ÅTGÄRDER SOM                
000011*        FINNS PÅ ETT VISST PARTI,VAD SOM HITTILLS ÄR GJORT               
000012*        PÅ ETT PARTI,SAMT TILLÅTER REGISTRERING AV UTFÖRDA               
000013*        ÅTGÄRDER.BILDEN ANVÄNDS VID FÖRBEHANDLING AV IN-                 
000014*        LEVERANSER SOM HUVUDSAKLIG ARBETSBILD.                           
000015*                                                                         
000016*        PROGRAMMET UPPDATERAR W6INLA (W6D1)                              
000017*                   UPPDATERAR W6LOPA (W6G1)                              
000018*                   UPPDATERAR W6KVAE (W6H7)                              
000019*        W006KOM    UPPDATERAR WLKOMA (WDP8)                              
000020*                                                                         
000021*        PROGRAMMET LÄSER      W6PLAA (W6G1)                              
000022*                              W6UPFA (W6L1)                              
000023*                              WDK6                                       
000024*                              WDB6                                       
000025*        W611ADR    LÄSER      W6HANA (W6G1)                              
000026*                              W6PLAA (W6G1)                              
000027*                                                                         
000028*    INDATA.                                                              
000029*        REQU:   W60133I1                                                 
000030*                                                                         
000031*    UTDATA.                                                              
000032*        RESP:   W60133O1                                                 
000033                                                                          
000034     SKIP3                                                                
000035 ENVIRONMENT DIVISION.                                                    
000036     EJECT                                                                
000037 DATA DIVISION.                                                           
000038 WORKING-STORAGE SECTION.                                                 
000039                                                                          
000040*    -- CHECKED BY WY2000                                                 
000041 77  IDPGM                       PIC X(08)  VALUE 'W6013310'.             
000042 77  FELTEXT                     PIC X(80)  VALUE SPACE.                  
000043                                                                          
000044 77  WS-IDLEVNR-KOLLI            PIC X(5).                                
000045 77  W-IDLEVNR-KOLLI             PIC X(5).                                
000046 77  W-KVKRBEH                   PIC X(4)   VALUE '00.5'.                 
000047 77  WS-IDOKOLLI                 PIC X(9).                                
000048 77  W-IDOKOLLI                  PIC 9(9).                                
000049 77  WS-IDLOPNRM-ALFA            PIC X(9).                                
000050 77  WS-IDLOPNRM-NUM             PIC 9(9).                                
000051 77  WS-IDINLVGN                 PIC 9(3).                                
000052 77  WS-ADINLOMR                 PIC X(4).                                
000053 77  WS-ADINLOMR-NXT             PIC X(4).                                
000054 77  WS-KDINLQ                   PIC X.                                   
000055 77  WS-FLINLFB                  PIC X.                                   
000056 77  WS-FLADM-SKAPAD             PIC X      VALUE 'N'.                    
000057 77  WW-IDRADNR                  PIC S9(5)  COMP-3.                       
000058 77  WS-KVINLART-KVARET          PIC S9(6)  VALUE ZERO.                   
000059 77  WS-KVINLART-TOT-KVARET      PIC S9(6)  VALUE ZERO.                   
000060 77  WS-KVINLART                 PIC S9(6)  VALUE ZERO.                   
000061 77  WS-KVROS                    PIC S9(6)  VALUE ZERO.                   
000062 77  WW-KVKOLLI-TOT              PIC 9(4)   VALUE ZERO.                   
000063 77  WW-KVAVIS-KVAR              PIC S9(7)  VALUE ZERO.                   
000064 77  W-KVAVIS-KVAR               PIC S9(7)  VALUE ZERO.                   
000065 77  WW-KVAVIS-PRIO-KVAR         PIC S9(7)  VALUE ZERO.                   
000066 77  W-KVAVIS-PRIO-KVAR          PIC S9(7)  VALUE ZERO.                   
000067 77  WW-KVKVAPRIM-KVAR           PIC S9(6)  VALUE ZERO.                   
000068 77  WW-KVAVIS-KIT-KVAR          PIC S9(6)  VALUE ZERO.                   
000069 77  W-KVAVIS-KIT-KVAR           PIC S9(6)  VALUE ZERO.                   
000070 77  W-KDSORT                    PIC S9(3)  VALUE ZERO COMP-3.            
000071 77  W-ADINLOMR                  PIC  X(4)  VALUE SPACE.                  
000072 77  W-ADINLOMR-NXT              PIC  X(4)  VALUE SPACE.                  
000073 77  W-KVINLART                  PIC 9(7)   VALUE ZERO.                   
000074 77  W-TOT-KVINLART              PIC 9(7)   VALUE ZERO.                   
000075 77  W-KVAANT                    PIC S9(7)  VALUE ZERO.                   
000076 77  W-KVAVIS-VERKL              PIC 9(7)   VALUE ZERO.                   
000077 77  FL-KR                       PIC X(1)   VALUE 'J'.                    
000078 77  6191-IX                     PIC S9(4)  VALUE +0 COMP SYNC.           
000079 77  6195-IX                     PIC S9(4)  VALUE +0 COMP SYNC.           
000080 77  INDX                        PIC S9(4)  VALUE +0 COMP SYNC.           
000081 77  MAX-TAB-IX                  PIC S9(4)  VALUE +30 COMP SYNC.          
000082 77  FYLLD-IX                    PIC S9(4)  VALUE +0 COMP SYNC.           
000083 77  6197-IX                     PIC S9(9)   VALUE +0   COMP SYNC.        
000084 77  MAX-6197-IX                 PIC S9(9)   VALUE +15  COMP SYNC.        
000085 77  W-6193-IDRADNR              PIC S9(5)  VALUE ZERO COMP-3.            
000086 77  W-SPAR-IDRADNR              PIC S9(5)  VALUE ZERO COMP-3.            
000087 77  W-SPAR-IDLEVNR              PIC X(5)   VALUE SPACE.                  
000088 77  W-SPAR-IDLOPNRM             PIC S9(9)  VALUE ZERO COMP-3.            
000089 77  W-KVINLART-UPD              PIC S9(7)  VALUE ZERO COMP-3.            
000090 77  W-NUM-IDLOPNRM              PIC S9(9)  VALUE ZERO COMP-3.            
000091 77  W-VKKOLLIN                  PIC S9(5)V9(1) VALUE ZERO COMP-3.        
000092 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
000093 77  DAGENS-TID                  PIC 9(8)    VALUE ZERO.                  
000094 77  LNG-P-TO-P-PREFIX           PIC S9(4)  VALUE +17 COMP SYNC.          
000095 77  W-BEFT                PIC S9(3)   VALUE ZERO COMP-3.                 
000096 77  WS-IDARTNR            PIC S9(9)   VALUE ZERO.                        
000097 77  P-TO-P-PREFIX-LNG     PIC S9(4)   VALUE +17 COMP SYNC.               
000098                                                                          
000099*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
000100 77  JA                          PIC X       VALUE 'J'.                   
000101 77  YES                         PIC X       VALUE 'Y'.                   
000102 77  NEJ                         PIC X       VALUE 'N'.                   
000103                                                                          
000104 77  WS-NYCKLAR                  PIC 9.                                   
000105     88 IDLEVNWKOLLI-NYCKEL                VALUE 1.                       
000106     88 IDLOPNRM-NYCKEL                    VALUE 2.                       
000107                                                                          
000108 77  SPRAK-IX                    PIC S9(9)  VALUE +0   COMP SYNC.         
000109                                                                          
000110*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
000111                                                                          
000112 77  INDATA-SW                   PIC X       VALUE 'J'.                   
000113     88  INDATA-OK                           VALUE 'J'.                   
000114     88  INDATA-FEL                          VALUE 'N'.                   
000115                                                                          
000116 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
000117     88  NYCKLAR-OK                          VALUE 'J'.                   
000118     88  NYCKLAR-FEL                         VALUE 'N'.                   
000119                                                                          
000120 77  PRIM-CONTROL-SW             PIC X       VALUE 'N'.                   
000121     88  PRIM-CONTROL-YES                    VALUE 'J'.                   
000122     88  PRIM-CONTROL-NO                     VALUE 'N'.                   
000123                                                                          
000124 77  KOLLI-SW                    PIC X       VALUE 'N'.                   
000125     88  KOLLI                               VALUE 'J'.                   
000126                                                                          
000127 77  TRAFF-SW                    PIC X       VALUE 'N'.                   
000128     88  TRAFF                               VALUE 'J'.                   
000129                                                                          
000130 77  DIVKLI-SW                   PIC X       VALUE 'N'.                   
000131     88  DIVKLI                              VALUE 'J'.                   
000132                                                                          
000133 77  FBRAPP-SW                   PIC X       VALUE 'J'.                   
000134     88  FBRAPP                              VALUE 'J'.                   
000135     88  FBRAPP-FINNS                        VALUE 'N'.                   
000136                                                                          
000137 77  PRIOGODS-SW                 PIC X       VALUE 'N'.                   
000138     88  PRIOGODS                            VALUE 'J'.                   
000139                                                                          
000140 77  FP-SW                       PIC X       VALUE 'N'.                   
000141     88  FP                                  VALUE 'J'.                   
000142                                                                          
000143 77  BLAEDDRING-SW               PIC X       VALUE 'N'.                   
000144     88  BLAEDDRING                          VALUE 'J'.                   
000145                                                                          
000146 77  ANTALSKONTR-SW              PIC X       VALUE 'N'.                   
000147     88  ANTALSKONTR-KLAR                    VALUE 'J'.                   
000148                                                                          
000149 77  NYTT-KOLLIID-SW             PIC X       VALUE 'N'.                   
000150     88  NYTT-KOLLIID                        VALUE 'J'.                   
000151                                                                          
000152 77  6191-SW                     PIC X       VALUE 'N'.                   
000153     88  6191-TRANS                          VALUE 'J'.                   
000154                                                                          
000155 77  6202-SW                     PIC X       VALUE 'N'.                   
000156     88  6202-TRANS                          VALUE 'J'.                   
000157                                                                          
000158 77  FOERSTA-6197-SW             PIC X       VALUE 'J'.                   
000159     88  FOERSTA-6197                        VALUE 'J'.                   
000160                                                                          
000161 77  VAGN-SW                     PIC X       VALUE 'N'.                   
000162     88  VAGN-FINNS                          VALUE 'J'.                   
000163                                                                          
000164 77  KVARET-SW                   PIC X       VALUE 'N'.                   
000165     88  KVARET-FINNS                        VALUE 'J'.                   
000166     88  KVARET-SAKNAS                       VALUE 'N'.                   
000167                                                                          
000168 77  WS-KDMFSFOR                 PIC X       VALUE SPACE.                 
000169     88  WS-SWEDISH-TEXT                     VALUE '1'.                   
000170     88  WS-ENGLISH-TEXT                     VALUE '2'.                   
000180     EJECT                                                                
000181*    01  -COPY W6D111  -PRE SPAR-                                         
000182     EJECT                                                                
000183*    01  -COPY W6D121  -PRE SPAR-                                         
000184     EJECT                                                                
000185*     -- FÖR REDIGERING AV IDANSTNR FRÅN USERID                           
000186  01     WS-IDANSTNR             PIC 9(5)    VALUE ZERO.                  
000187  01     FILLER                  REDEFINES WS-IDANSTNR.                   
000188   03    WS-IDANSTNR-TKN         OCCURS 5                                 
000189                                 PIC 9(1).                                
000190  01     WS-USERID               PIC X(8)    VALUE SPACE.                 
000191  01     FILLER                  REDEFINES WS-USERID.                     
000192   03    WS-USERID-TKN           OCCURS 8                                 
000193                                 PIC 9(1).                                
000194  01     USER-X.                                                          
000195    03   FILLER                  PIC X(2).                                
000196    03   USER                    PIC X(5).                                
000197    03   FILLER                  PIC X(1).                                
000198                                                                          
000199 01      K-KONSTANTER.                                                    
000200                                                                          
000201*     -- FÄLTLÄNGD USERID                                                 
000202  02     K-USERID-LNG            PIC S9(9)   VALUE +8   COMP SYNC.        
000203*     -- FÄLTLÄNGD IDANSTNR                                               
000204  02     K-IDANSTNR-LNG          PIC S9(9)   VALUE +5   COMP SYNC.        
000205*     -- TECKEN I WS-USERID                                               
000206  02     IX-USERID               PIC S9(9)   VALUE ZERO COMP SYNC.        
000207*     -- TECKEN I WS-IDANSTNR                                             
000208  02     IX-IDANSTNR             PIC S9(9)   VALUE ZERO COMP SYNC.        
000209     EJECT                                                                
000210*                                                                         
000211 01  ALL-UTF8-SPACE.                                                      
000212     03 FILLER                   PIC X(50)   VALUE ALL X'20'.             
000213*                                                                         
000214 01  ALL-SPACE.                                                           
000215     03 FILLER                   PIC X(50)   VALUE SPACE.                 
000216*                                                                         
000217 01  ALL-PLUS.                                                            
000218     03 FILLER                   PIC X(50)   VALUE ALL '+'.               
000219*                                                                         
000220 01  ALL-UTF8-PLUS.                                                       
000221     03 FILLER                   PIC X(50)   VALUE ALL X'2B'.             
000222*                                                                         
000223 01  RAD-TABELL.                                                          
000224   03 TABELL OCCURS 30 INDEXED BY TAB-IX.                                 
000225      05 TAB-KVINLART        PIC 9(6).                                    
000226      05 TAB-KVKOLLI         PIC 9(4).                                    
000227      05 TAB-ADINLOMR        PIC X(4).                                    
000228      05 TAB-KDINLSTA        PIC X(3).                                    
000229      05 TAB-IDINLVGN        PIC 9(3).                                    
000230      05 TAB-ADINLOMR-NXT    PIC X(4).                                    
000231      05 TAB-KDKLIPRI        PIC X.                                       
000232      05 TAB-FLSATS          PIC X.                                       
000233      05 TAB-FLKVAANT        PIC X.                                       
000234      05 TAB-PLAC-KDINLOMR   PIC X(3).                                    
000235      05 TAB-ADR-KDINLOMR    PIC X(3).                                    
000236                                                                          
000237*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
000238 01  GENERELLA-SUBPROGRAM.                                                
000239     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000240     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
000241     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000242     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
000243     03  W611PMRK                PIC X(8)    VALUE 'W611PMRK'.            
000244     03  W611ADR                 PIC X(8)    VALUE 'W611ADR'.             
000245     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
000246     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
000247     03  W611STYR                PIC X(8)    VALUE 'W611STYR'.            
000248     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
000249                                                                          
000250 77  RKOD-ABEND                  PIC S9(4)   VALUE +33 COMP SYNC.         
000251     EJECT                                                                
000252*01 -COPY WMSGINIT                                                        
000253     SKIP3                                                                
000254*01 -COPY WWDC99                                                          
000255     SKIP3                                                                
000256*    --- PARAMETRAR TILL SUBPROGRAM W611STYR                              
000257*   -COPY W611STYR                                                        
000258 01  MESSAGE-CODES.                                                       
000259     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '020'.                 
000260     03  ERR-UPD-NOT-ALLOWED     PIC X(3)    VALUE '007'.                 
000261     03  INF-PRESS-PF11          PIC X(3)    VALUE '013'.                 
000262     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '014'.                 
000263     03  INF-UPDATE-DONE         PIC X(3)    VALUE '001'.                 
000264     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
000265     03  ERR-FINNS-EJ            PIC X(3)    VALUE '025'.                 
000266     03  SPLITT-PRESS-PF23       PIC X(3)    VALUE '353'.                 
000267     03  NO-IR-CREATED           PIC X(3)    VALUE '364'.                 
000268     03  INF-PRINTING-REQUEST    PIC X(3)    VALUE '376'.                 
000269     03  ERR-WRONG-PRINTER       PIC X(3)    VALUE '347'.                 
000270     03  ERR-QUALITY-ERROR       PIC X(3)    VALUE '357'.                 
000271     03  ERR-BATCH-ON-CASE-LVL   PIC X(3)    VALUE '367'.                 
000272     03  ERR-IR-MANUAL-CHG       PIC X(3)    VALUE '335'.                 
000273     03  ERR-IR-ALREADY-REG      PIC X(3)    VALUE '358'.                 
000274     03  ERR-KIT-PRIO-NOT-ALLOWED PIC X(3)   VALUE '370'.                 
000275     03  ERR-MISC-CASE           PIC X(3)    VALUE '354'.                 
000276     03  ERR-MISS-REGISTER       PIC X(3)    VALUE '025'.                 
000277     03  ERR-HILIGHT-FLDS-WRONG  PIC X(3)    VALUE '020'.                 
000278     03  ERR-SURPLUS-DLVRY       PIC X(3)    VALUE '371'.                 
000279     03  INF-PF8-NXT-BTCH        PIC X(3)    VALUE '372'.                 
000280     03  ERR-NO-DEVN-3-77        PIC X(3)    VALUE '373'.                 
000281     03  ERR-USE-6122-SCR        PIC X(3)    VALUE '374'.                 
000282     03  ERR-LOT-NOT-ON-CASE-LVL PIC X(3)    VALUE '368'.                 
000283     03  ERR-WRONG-QTY           PIC X(3)    VALUE '330'.                 
000284     03  INF-MORE-LINES          PIC X(3)    VALUE '011'.                 
000285     03  ERR-TROLLEY-HAS-LOC-ADD PIC X(3)    VALUE '375'.                 
000286     03  ERR-PRI-SPL-CTL-NOT-DONE PIC X(3)   VALUE '377'.                 
000287     03  INF-ADM-REP             PIC X(3)    VALUE '378'.                 
000288     EJECT                                                                
000289*01  -COPY W611PMRK                                                       
000290     EJECT                                                                
000291*01  -COPY W611ADR                                                        
000292     EJECT                                                                
000293*01  -COPY W006PRT                                                        
000294     EJECT                                                                
000295*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
000296*                                                                         
000297 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
000298     SKIP3                                                                
000299*01  MID -COPY W6I13301                                                   
000300     EJECT                                                                
000301 01  FILLER                      PIC X(16)  VALUE 'MSG/RESP-AREA'.        
000302     SKIP3                                                                
000303*01  -COPY WMSGAREA                                                       
000304     EJECT                                                                
000305     03  MOD REDEFINES MSG-AREA.                                          
000306*      05  -COPY W6O13301                                                 
000307     EJECT                                                                
000308 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
000309     SKIP3                                                                
000310*01  -COPY WMFSAREA                                                       
000311     EJECT                                                                
000312 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
000313     SKIP3                                                                
000314 01  KOM-MSG-IO-AREA.                                                     
000315*03  -COPY WMSGKOM                                                        
000316     EJECT                                                                
000317 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-AREA'.         
000318                                                                          
000319*01  -COPY WMSGSNUF   -PRE  P-TO-P-                                       
000320     EJECT                                                                
000321 01      FILLER                  PIC X(24)   VALUE                        
000322                                 'MOD6191-MID-W6I19101'.                  
000323     SKIP2                                                                
000324     -COPY W6I19101 -PRE MOD6191-                                         
000325     EJECT                                                                
000326 01      FILLER                  PIC X(24)   VALUE                        
000327                                 'MOD6193-MID-W6I19301'.                  
000328     SKIP2                                                                
000329     -COPY W6I19301 -PRE MOD6193-                                         
000330     EJECT                                                                
000331 01      FILLER                  PIC X(24)   VALUE                        
000332                                 'MOD6194-MID-W6I19401'.                  
000333     SKIP2                                                                
000334     -COPY W6I19401 -PRE MOD6194-                                         
000335     EJECT                                                                
000336 01      FILLER                  PIC X(24)   VALUE                        
000337                                 'MOD6195-MID-W6I19501'.                  
000338     SKIP2                                                                
000339     -COPY W6I19501 -PRE MOD6195-                                         
000340     EJECT                                                                
000341                                                                          
000342 01      FILLER                  PIC X(24)   VALUE                        
000343                                 'MOD6197-MID-W6I19701'.                  
000344*01  -COPY W6I19701   -PRE 6197-                                          
000345     EJECT                                                                
000346                                                                          
000347 01  FILLER                      PIC X(24)   VALUE                        
000348                                 '6202-MID-W60202I1'.                     
000349 01  6202-AREA.                                                           
000350     SKIP2                                                                
000351     03 -COPY WREQUPRE -PRE 6202-                                         
000352     03 -COPY WZ01REQU -PRE 6202-                                         
000353     03 -COPY W60202I1 -PRE 6202-                                         
000354     EJECT                                                                
000355                                                                          
000356*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
000357*                                                                         
000358 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000359     SKIP3                                                                
000360 01  FILLER                  PIC X(16)  VALUE 'WTRAUTF8-AREA   '.         
000361*01  -COPY WTRAUTF8                                                       
000362                                                                          
000363 01  WS-IDSKYLT-SE               PIC X(3) VALUE 'S  '.                    
000364 01  WS-IDSKYLT-GB               PIC X(3) VALUE 'GB '.                    
000365 01  WS-IDSKYLT-CN               PIC X(3) VALUE 'RCN'.                    
000366                                                                          
000367 01  WS-CP-UNICODE               PIC X(4)  VALUE 'UTF8'.                  
000368 01  WS-CP-EBCDIC                PIC X(3)  VALUE '278'.                   
000369                                                                          
000370 01  NYCKLAR-TILL-DLI.                                                    
000371     03  W-W6D101KY-X.                                                    
000372         05  W-D101KY-IDDC       PIC X(2)     VALUE SPACE.                
000373         05  W-D101KY-IDLEVNR    PIC X(5)     VALUE SPACE.                
000374         05  W-D101KY-IDFS       PIC X(8)     VALUE SPACE.                
000375         05  W-D101KY-TIAVIDAT   PIC S9(7)    COMP-3 VALUE ZERO.          
000376                                                                          
000377     03  W-W6D1CSEQ-X.                                                    
000378         05  W-IDLEVNRK          PIC X(5).                                
000379         05  W-IDOKOLLIK         PIC  9(9).                               
000380     03  W-W6D1BSEQ-X.                                                    
000381         05  W-IDLOPNR           PIC S9(9)   COMP-3.                      
000382     03  W-IDLOPNRM-X.                                                    
000383         05  W-IDLOPNRM-BSEQ     PIC S9(9)   COMP-3 VALUE ZERO.           
000384     03  W-IDLEVNRK-X.                                                    
000385         05  W-IDLEVNR           PIC X(5).                                
000386     03  W-IDOKOLLI-X.                                                    
000387         05  W-IDOKOLLINR        PIC  9(9).                               
000388     03  W-W6GXKEY-6005-X.                                                
000389         05  W-6005-IDHTYP       PIC X(4)    VALUE '6005'.                
000390         05  W-6005-IDDC         PIC X(2)    VALUE SPACE.                 
000391         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
000392     03  W-W6GXKEY-6006-X.                                                
000393         05  W-6006-ADINLOMR     PIC X(4)    VALUE SPACE.                 
000394         05  FILLER              PIC X(1)    VALUE LOW-VALUE.             
000395     03  W-W6GXKEY-6017-X.                                                
000396         05  W-6017-IDHTYP       PIC X(4)    VALUE '6017'.                
000397         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
000398     03  W-W6GXKEY-6018-X.                                                
000399         05  W-6018-KDSEGKEY     PIC X(1)    VALUE '1'.                   
000400     03  W-W6D1FSEQ-X.                                                    
000401         05  W-D1FSEQ-IDINLVGN   PIC 9(3)    VALUE ZERO.                  
000402     03  W-IDINLVGN-X.                                                    
000403         05  W-IDINLVGN          PIC 9(3)    VALUE ZERO.                  
000404     03  W-IDDC-X.                                                        
000405         05  W-IDDC              PIC X(2).                                
000406     03  W-IDRADNR-X.                                                     
000407         05  W-IDRADNR           PIC S9(5)   VALUE ZERO  COMP-3.          
000408     03  W-W6H7CSEQ-X.                                                    
000409         05  W-IDLOPNRM-H7       PIC S9(9)    VALUE ZERO COMP-3.          
000410         05  W-DAAVSDAT-H7       PIC  9(8)    VALUE ZERO.                 
000411     03  W-W6H7BSEQ-MIN-X.                                                
000412         05  W-IDARTNR-H7-MIN    PIC S9(9)    VALUE ZERO COMP-3.          
000413         05  W-DAREGDAT-9KOMPL-MIN                                        
000414                                 PIC 9(8)     VALUE ZERO.                 
000415         05  W-IDLEVNR-H7-MIN-X.                                          
000416            07  W-IDLEVNR-H7-MIN PIC X(5)     VALUE SPACE.                
000417         05  W-KVKRKNTR-H7-MIN-X.                                         
000418            07 W-KVKRKNTR-H7-MIN PIC S9       VALUE ZERO COMP-3.          
000419     03  W-W6H7BSEQ-MAX-X.                                                
000420         05  W-IDARTNR-H7-MAX    PIC S9(9)    VALUE ZERO COMP-3.          
000421         05  W-DAREGDAT-9KOMPL-MAX                                        
000422                                 PIC 9(8)     VALUE ZERO.                 
000423         05  W-IDLEVNR-H7-MAX-X.                                          
000424            07 W-IDLEVNR-H7-MAX  PIC X(5)     VALUE SPACE.                
000425         05  W-KVKRKNTR-H7-MAX-X.                                         
000426            07 W-KVKRKNTR-H7-MAX PIC S9       VALUE ZERO COMP-3.          
000427     03  W-IDARTNR-X.                                                     
000428         05  W-IDARTNR           PIC S9(9)   COMP-3 VALUE ZERO.           
000429     03  W-IDSKYLT-X.                                                     
000430         05  W-IDSKYLT           PIC X(3)    VALUE 'GB '.                 
000431     SKIP2                                                                
000432     03  W-KDSEGKEY-X.                                                    
000433         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
000434                                                                          
000435     03  W-IDDC-B6-X.                                                     
000436         05 W-IDDC-B6            PIC X(2).                                
000437                                                                          
000438     SKIP2                                                                
000439*    --- STATUS-KOD FRÅN IMS                                              
000440 01  STATUS-WS                   PIC XX.                                  
000441     88  SEGMENT-FINNS                       VALUE '  '.                  
000442     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
000443     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
000444     88  SEGMENT-SLUT                        VALUE 'GB'.                  
000445     SKIP2                                                                
000446 01  GODK-STATUSKODER.                                                    
000447     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000448     SKIP3                                                                
000449 01  SSA1                        PIC X(128).                              
000450 01  SSA2                        PIC X(64).                               
000451     EJECT                                                                
000452*    --- IMS FUNKTIONSKODER                                               
000453*01  -COPY W0003                                                          
000454     EJECT                                                                
000455*    ---  DLI INPUT-OUTPUT AREA                                           
000456 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
000457     SKIP3                                                                
000458 01  DLI-IO-AREA.                                                         
000459     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
000460     SKIP3                                                                
000461     03  W6LOPA11 REDEFINES IO-AREA.                                      
000462*        05  -COPY W6GX6018     -PRE LOPA-                                
000463     EJECT                                                                
000464 01  DLI-IO-AREA-INL01.                                                   
000465     03 W6INLA01.                                                         
000466*       05  -COPY W6D101                                                  
000467     EJECT                                                                
000468 01  DLI-IO-AREA-INL11.                                                   
000469     03 W6INLD11.                                                         
000470*       05  -COPY W6D111                                                  
000471     EJECT                                                                
000472 01  DLI-IO-AREA-INL21.                                                   
000473     03 W6INLA21.                                                         
000474*       05  -COPY W6D121                                                  
000475     EJECT                                                                
000476 01  DLI-IO-AREA-INL21-KVARET.                                            
000477     03 W6INLA21.                                                         
000478*       05  -COPY W6D121 -PRE KVARET-                                     
000479     EJECT                                                                
000480 01  DLI-IO-AREA-INL21-FB.                                                
000481     03 W6INLA21.                                                         
000482*       05  -COPY W6D121 -PRE FB-                                         
000483     EJECT                                                                
000484 01  DLI-IO-AREA-INLC01.                                                  
000485     03 W6D1B1.                                                           
000486*       05  -COPY W6D1B1                                                  
000487     EJECT                                                                
000488 01  DLI-IO-AREA-KVAE01.                                                  
000489     03 W6KVAE01.                                                         
000490*       05  -COPY W6H701  -PRE KVAE-                                      
000491     EJECT                                                                
000492 01  DLI-IO-AREA-PLAA01.                                                  
000493     03 W6PLAA11.                                                         
000494*       05  -COPY W6GX6006 -PRE PLAA-                                     
000495     EJECT                                                                
000496 01  DLI-IO-AREA-UPFA01.                                                  
000497     03  W6UPFA01.                                                        
000498*        05  -COPY W6L101                                                 
000499     SKIP3                                                                
000500 01  DLI-IO-AREA-UPFA11.                                                  
000501     03  W6UPFA11.                                                        
000502*        05  -COPY W6L111                                                 
000503     SKIP3                                                                
000504 01  DLI-IO-AREA-UPFA12.                                                  
000505     03  W6UPFA12.                                                        
000506*        05  -COPY W6L112                                                 
000507     SKIP3                                                                
000508 01  DLI-IO-AREA-WDK6.                                                    
000509     03  WDK611.                                                          
000510*        05  -COPY WDK611                                                 
000511                                                                          
000512 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
000513 01   DLI-IO-AREA-B601.                                                   
000514*     03  -COPY WDB601                                                    
000515                                                                          
000516     SKIP3                                                                
000517 01  FILLER                PIC X(16) VALUE 'DLI-IO-MDD311'.               
000518 01  DLI-IO-WDD311.                                                       
000519*    03  -COPY WDD311                                                     
000520                                                                          
000521     EJECT                                                                
000522 LINKAGE SECTION.                                                         
000523                                                                          
000524 01  REQU-AREA.                                                           
000525*    03 -COPY WZ01REQU                                                    
000526*    03 -COPY W60133I1                                                    
000527     EJECT                                                                
000528 01  RESP-AREA.                                                           
000529*    03 -COPY WZ01RESP                                                    
000530*    03 -COPY W60133O1                                                    
000531     EJECT                                                                
000532 01  MAX-KVRADER                 PIC S9(4) COMP.                          
000533*                                                                         
000534*01  -COPY W0009   -PRE MSG-                                              
000535     EJECT                                                                
000536*01  -COPY W0009   -PRE ALT1-                                             
000537     EJECT                                                                
000538*01  -COPY W0009   -PRE ALT2-                                             
000539     EJECT                                                                
000540*01  -COPY W0009   -PRE ALT3-                                             
000541     EJECT                                                                
000542*01  -COPY W0009   -PRE ALT4-                                             
000543     EJECT                                                                
000544*01  -COPY W0009   -PRE 6197-                                             
000545     EJECT                                                                
000546*01  -COPY W0009   -PRE DISP-                                             
000547     EJECT                                                                
000548*01  -COPY W0008  -PRE USEA-                                              
000549     05  FILLER                  PIC X.                                   
000550     EJECT                                                                
000551*01  -COPY W0008  -PRE PLAA-                                              
000552     05  FILLER                  PIC X.                                   
000553     EJECT                                                                
000554*01  -COPY W0008  -PRE LOPA-                                              
000555     05  FILLER                  PIC X.                                   
000556     EJECT                                                                
000557*01  -COPY W0008  -PRE INLA-                                              
000558     05  FILLER                  PIC X.                                   
000559     EJECT                                                                
000560*01  -COPY W0008  -PRE INLD-                                              
000561     05  FILLER                  PIC X.                                   
000562     EJECT                                                                
000563*01  -COPY W0008  -PRE INLC-                                              
000564     05  FILLER                  PIC X.                                   
000565     EJECT                                                                
000566*01  -COPY W0008  -PRE ALT-INLC-                                          
000567     05  FILLER                  PIC X.                                   
000568     EJECT                                                                
000569*01  -COPY W0008  -PRE INLB1-                                             
000570     05  FILLER                  PIC X.                                   
000571     EJECT                                                                
000572*01  -COPY W0008  -PRE INLG-                                              
000573     05  FILLER                  PIC X.                                   
000574     EJECT                                                                
000575*01  -COPY W0008  -PRE KVAE-                                              
000576     05  FILLER                  PIC X.                                   
000577     EJECT                                                                
000578*01  -COPY W0008  -PRE KVABSEQ-                                           
000579     05  FILLER                  PIC X.                                   
000580     EJECT                                                                
000581*01  -COPY W0008  -PRE WDK6-                                              
000582     05  FILLER                  PIC X.                                   
000583     EJECT                                                                
000584*01  -COPY W0008  -PRE WDB6-                                              
000585     05  FILLER                  PIC X.                                   
000586     EJECT                                                                
000587*01  -COPY W0008  -PRE WDD3-                                              
000588     05  FILLER                  PIC X.                                   
000589     EJECT                                                                
000590**  PCB'ER FÖR SUBPGM                                                     
000591 01  ADR-INLA-PCB                PIC X.                                   
000592                                                                          
000593 01  ADR-INLC-PCB                PIC X.                                   
000594                                                                          
000595 01  ADR-PLAA-PCB                PIC X.                                   
000596                                                                          
000597 01  ADR-WDK6-PCB                PIC X.                                   
000598                                                                          
000599 01  ADR-STYR-HANA-PCB           PIC X.                                   
000600                                                                          
000601 01  ADR-STYR-PLAA-PCB           PIC X.                                   
000602                                                                          
000603 01  KOM-KOMA-PCB                PIC X.                                   
000604                                                                          
000605 01  PMRK-INLB-PCB               PIC X.                                   
000606                                                                          
000607 01  PMRK-INLC-PCB               PIC X.                                   
000608                                                                          
000609 01  PMRK-PLAA-PCB               PIC X.                                   
000610                                                                          
000611 01  STYR-HANA-PCB               PIC X.                                   
000612                                                                          
000613 01  STYR-PLAA-PCB               PIC X.                                   
000614     EJECT                                                                
000615*01  -COPY W0008  -PRE UPFA-                                              
000616     05  FILLER                  PIC X.                                   
000617     EJECT                                                                
000618 PROCEDURE DIVISION  USING REQU-AREA RESP-AREA MAX-KVRADER                
000619                           MSG-PCB ALT1-PCB ALT2-PCB ALT3-PCB             
000620                           ALT4-PCB 6197-PCB DISP-PCB USEA-PCB            
000621                           PLAA-PCB LOPA-PCB INLA-PCB                     
000622                           INLD-PCB                                       
000623                           INLC-PCB                                       
000624                           ALT-INLC-PCB                                   
000625                           INLB1-PCB                                      
000626                           INLG-PCB KVAE-PCB                              
000627                           KVABSEQ-PCB                                    
000628                           WDK6-PCB WDB6-PCB WDD3-PCB                     
000629                           ADR-INLA-PCB                                   
000630                           ADR-INLC-PCB                                   
000631                           ADR-PLAA-PCB                                   
000632                           ADR-WDK6-PCB                                   
000633                           ADR-STYR-HANA-PCB                              
000634                           ADR-STYR-PLAA-PCB                              
000635                           KOM-KOMA-PCB                                   
000636                           PMRK-INLB-PCB PMRK-INLC-PCB                    
000637                           PMRK-PLAA-PCB                                  
000638                           STYR-HANA-PCB                                  
000639                           STYR-PLAA-PCB                                  
000640                           UPFA-PCB.                                      
000641                                                                          
000642     PERFORM A-INIT                                                       
000643     PERFORM B-KOLLA-NYCKLAR                                              
000644     IF NYCKLAR-OK                                                        
000645       IF REQU-UPDATE                                                     
000646         PERFORM G-KOLLA-INPUT                                            
000647         IF INDATA-OK                                                     
000648           PERFORM H-UPPDATERA                                            
000649         END-IF                                                           
000650       ELSE                                                               
000651         IF REQU-FIRST                                                    
000652          PERFORM C-FOERSTA-SIDA                                          
000653         ELSE                                                             
000654          IF REQU-NEXT                                                    
000655           PERFORM D-NAESTA-SIDA                                          
000656          ELSE                                                            
000657           PERFORM E-SAMMA-SIDA                                           
000658          END-IF                                                          
000659         END-IF                                                           
000660       END-IF                                                             
000661       IF INDATA-OK                                                       
000662         PERFORM F-LAES-VISA-INFO                                         
000663       END-IF                                                             
000664     END-IF                                                               
000665*    COMPUTE RESP-KVRADER = RESP-KVRADER - 1                              
000666                                                                          
000667     GOBACK                                                               
000668     .                                                                    
000669     EJECT                                                                
000670 A-INIT SECTION.                                                          
000671                                                                          
000672     MOVE ALL-PLUS      TO RESP-W60133O1                                  
000673     MOVE ALL-UTF8-PLUS TO RESP-BEART                                     
000674     PERFORM MFS-FORM-ATTR                                                
000675                                                                          
000676     MOVE 001          TO RESP-IDMSGVER                                   
000677     MOVE SPACE        TO RESP-IDMSG-ERROR                                
000678                          RESP-IDMSG-INFO                                 
000679                          RESP-IDELMT-ERROR                               
000680     MOVE REQU-KVRADER TO RESP-KVRADER                                    
000681     .                                                                    
000682     EJECT                                                                
000683 B-KOLLA-NYCKLAR SECTION.                                                 
000684                                                                          
000685     MOVE JA                     TO INDATA-SW                             
000686     MOVE NEJ                    TO VAGN-SW                               
000687                                                                          
000688     MOVE REQU-IDLEVNR-KOLLI-KEY TO WS-IDLEVNR-KOLLI                      
000689     MOVE REQU-IDOKOLLI-KEY      TO WS-IDOKOLLI                           
000690     MOVE REQU-IDLOPNRM-KEY      TO WS-IDLOPNRM-ALFA                      
000700     IF REQU-IDSPRAK = 'SV'                                               
000710        MOVE +1                  TO WS-KDMFSFOR                           
000720     ELSE                                                                 
000730        MOVE +2                  TO WS-KDMFSFOR                           
000740     END-IF                                                               
000741                                                                          
000742     INSPECT WS-IDOKOLLI REPLACING LEADING SPACE BY ZERO                  
000743     INSPECT WS-IDLOPNRM-ALFA REPLACING LEADING SPACE BY ZERO             
000744                                                                          
000745     MOVE REQU-IDDC-KEY          TO W-IDDC-B6                             
000746                                    WS-IDDC                               
000747     PERFORM IMS-GU-WDB601                                                
000748                                                                          
000749     IF DCS-KDDC = SPACE OR DCS-DDC                                       
000750        MOVE NEJ       TO NYCKLAR-SW                                      
000751     ELSE                                                                 
000752        MOVE DCS-IDDC  TO W-IDDC                                          
000753                          W-6005-IDDC                                     
000754     END-IF                                                               
000755                                                                          
000756     PERFORM BB-BESTAEM-NYCKEL                                            
000757                                                                          
000758     IF IDLEVNWKOLLI-NYCKEL                                               
000759       IF WS-IDLEVNR-KOLLI  NOT = SPACE                                   
000760          MOVE WS-IDLEVNR-KOLLI TO W-IDLEVNR-KOLLI                        
000761       ELSE                                                               
000762          MOVE NEJ TO NYCKLAR-SW                                          
000763       END-IF                                                             
000764                                                                          
000765       IF WS-IDOKOLLI  NUMERIC  AND WS-IDOKOLLI > ZERO                    
000766          MOVE WS-IDOKOLLI TO W-IDOKOLLI                                  
000767       ELSE                                                               
000768          MOVE NEJ TO NYCKLAR-SW                                          
000769       END-IF                                                             
000770     ELSE                                                                 
000771       IF WS-IDLOPNRM-ALFA NUMERIC AND WS-IDLOPNRM-ALFA > ZERO            
000772          MOVE WS-IDLOPNRM-ALFA TO WS-IDLOPNRM-NUM                        
000773       ELSE                                                               
000774          MOVE NEJ TO NYCKLAR-SW                                          
000775       END-IF                                                             
000776     END-IF                                                               
000777                                                                          
000778     IF NYCKLAR-OK                                                        
000779        MOVE WS-IDLEVNR-KOLLI TO RESP-IDLEVNR-KOLLI                       
000780        MOVE WS-IDOKOLLI      TO RESP-IDOKOLLI                            
000781        MOVE WS-IDLOPNRM-ALFA TO RESP-IDLOPNRM                            
000782        MOVE DCS-IDDC         TO RESP-IDDC                                
000783        INSPECT RESP-IDLOPNRM   REPLACING LEADING ZERO BY SPACE           
000784        INSPECT RESP-IDOKOLLI   REPLACING LEADING ZERO BY SPACE           
000785        INSPECT RESP-IDDC       REPLACING LEADING ZERO BY SPACE           
000786     ELSE                                                                 
000787        MOVE MFS-RENSA-FAELT  TO RESP-IDLEVNR-KOLLI                       
000788                                 RESP-IDOKOLLI                            
000789                                 RESP-IDLOPNRM                            
000790                                 RESP-IDDC                                
000791     END-IF                                                               
000792                                                                          
000793     IF NYCKLAR-FEL                                                       
000794       MOVE ERR-WRONG-KEY    TO RESP-IDMSG-ERROR                          
000795       PERFORM MFS-RENSA-FAELT-UT                                         
000796       PERFORM MFS-RENSA-FAELT-IN                                         
000797     END-IF                                                               
000798                                                                          
000799     IF REQU-ADINLOMR-PRT = ALL '+'                                       
000800       MOVE SPACE             TO RESP-ADINLOMR-PRT                        
000810     ELSE                                                                 
000811       MOVE REQU-ADINLOMR-PRT TO RESP-ADINLOMR-PRT                        
000812     END-IF                                                               
000813     .                                                                    
000814     EJECT                                                                
000815 BB-BESTAEM-NYCKEL SECTION.                                               
000816                                                                          
000817***NYA NYCKLAR                                                            
000818*    IF REQU-IDLEVNR-KOLLI-KEY NOT = ALL '+' OR                           
000819*       REQU-IDOKOLLI-KEY NOT = ALL '+'                                   
000820*       MOVE 1 TO WS-NYCKLAR                                              
000830*    ELSE                                                                 
000840*      IF REQU-IDLOPNRM-KEY NOT = ALL '+'                                 
000850*         MOVE 2 TO WS-NYCKLAR                                            
000860*      ELSE                                                               
000870***GAMLA NYCKLAR                                                          
000880         IF WS-IDLEVNR-KOLLI  NOT = SPACE AND                             
000890            WS-IDOKOLLI NOT = ZERO                                        
000900            MOVE 1 TO WS-NYCKLAR                                          
000901         ELSE                                                             
000902           IF WS-IDLOPNRM-ALFA NOT = ALL '+'                              
000903              MOVE 2 TO WS-NYCKLAR                                        
000904           END-IF                                                         
000905         END-IF                                                           
000906*      END-IF                                                             
000907*    END-IF                                                               
000908     .                                                                    
000909     EJECT                                                                
000910 C-FOERSTA-SIDA SECTION.                                                  
000911                                                                          
000912     PERFORM MFS-RENSA-FAELT-IN                                           
000913     .                                                                    
000914     EJECT                                                                
000915 D-NAESTA-SIDA SECTION.                                                   
000916                                                                          
000917     MOVE W-IDLEVNR-KOLLI      TO W-IDLEVNRK                              
000918     MOVE W-IDOKOLLI           TO W-IDOKOLLIK                             
000919     MOVE REQU-IDLOPNRM-START  TO W-NUM-IDLOPNRM                          
000920     PERFORM IMS-GU-W6INLA11-C                                            
000921     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT OR                      
000922                   ART-IDLOPNRM = W-NUM-IDLOPNRM                          
000923         PERFORM IMS-GN-W6INLA11-C                                        
000924     END-PERFORM                                                          
000925     PERFORM MFS-RENSA-FAELT-IN                                           
000926     .                                                                    
000927     EJECT                                                                
000928 E-SAMMA-SIDA SECTION.                                                    
000929                                                                          
000930     IF IDLEVNWKOLLI-NYCKEL                                               
000931         MOVE W-IDLEVNR-KOLLI    TO W-IDLEVNRK                            
000932         MOVE W-IDOKOLLI         TO W-IDOKOLLIK                           
000933         MOVE REQU-IDLOPNRM-START TO W-NUM-IDLOPNRM                       
000934         PERFORM IMS-GU-W6INLA11-C                                        
000935         PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT OR                  
000936                       ART-IDLOPNRM = W-NUM-IDLOPNRM                      
000937             PERFORM IMS-GN-W6INLA11-C                                    
000938         END-PERFORM                                                      
000939     ELSE                                                                 
000940         MOVE REQU-IDLOPNRM-START TO W-IDLOPNR                            
000941         PERFORM IMS-GU-W6INLA11-B                                        
000942     END-IF                                                               
000943     IF REQU-INPUT              = ALL '+'                                 
000944       PERFORM MFS-RENSA-FAELT-IN                                         
000945     ELSE                                                                 
000946       MOVE INF-PRESS-PF11   TO RESP-IDMSG-INFO                           
000947       PERFORM EA-MID-INDATA-TILL-MOD                                     
000948     END-IF                                                               
000949     .                                                                    
000950     EJECT                                                                
000951 EA-MID-INDATA-TILL-MOD SECTION.                                          
000952                                                                          
000953     IF REQU-KDCMDVAL          =  ALL '+'                                 
000954         MOVE ALL-SPACE        TO RESP-KDCMDVAL-INM                       
000955     ELSE                                                                 
000956         MOVE REQU-KDCMDVAL    TO RESP-KDCMDVAL-INM                       
000957         MOVE MFS-ADD-LAES-IN-FAELT                                       
000958                               TO RESP-KDCMDVAL-INM-ATTR                  
000959     END-IF                                                               
000960                                                                          
000961     IF REQU-KVINLART          =  ALL '+'                                 
000962         MOVE ALL-SPACE        TO RESP-KVINLART-INM                       
000963     ELSE                                                                 
000964         MOVE REQU-KVINLART    TO RESP-KVINLART-INM                       
000965         MOVE MFS-ADD-LAES-IN-FAELT                                       
000966                               TO RESP-KVINLART-INM-ATTR                  
000967     END-IF                                                               
000968                                                                          
000969     IF REQU-ADINLOMR          =  ALL '+'                                 
000970         MOVE ALL-SPACE        TO RESP-ADINLOMR-INM                       
000971     ELSE                                                                 
000972         MOVE REQU-ADINLOMR    TO RESP-ADINLOMR-INM                       
000973         MOVE MFS-ADD-LAES-IN-FAELT                                       
000974                               TO RESP-ADINLOMR-INM-ATTR                  
000975     END-IF                                                               
000976                                                                          
000977     IF REQU-IDINLVGN          =  ALL '+'                                 
000978         MOVE ALL-SPACE        TO RESP-IDINLVGN-INM                       
000979     ELSE                                                                 
000980         MOVE REQU-IDINLVGN    TO RESP-IDINLVGN-INM                       
000981         MOVE MFS-ADD-LAES-IN-FAELT                                       
000982                               TO RESP-IDINLVGN-INM-ATTR                  
000983     END-IF                                                               
000984                                                                          
000985     IF REQU-ADINLOMR-NXT      =  ALL '+'                                 
000986         MOVE ALL-SPACE        TO RESP-ADINLOMR-NXT-INM                   
000987     ELSE                                                                 
000988         MOVE REQU-ADINLOMR-NXT TO RESP-ADINLOMR-NXT-INM                  
000989         MOVE MFS-ADD-LAES-IN-FAELT                                       
000990                               TO RESP-ADINLOMR-NXT-INM-ATTR              
000991     END-IF                                                               
000992                                                                          
000993     IF REQU-KDKLIPRI          =  ALL '+'                                 
000994         MOVE ALL-SPACE        TO RESP-KDKLIPRI-INM                       
000995     ELSE                                                                 
000996         MOVE REQU-KDKLIPRI    TO RESP-KDKLIPRI-INM                       
000997         MOVE MFS-ADD-LAES-IN-FAELT                                       
000998                               TO RESP-KDKLIPRI-INM-ATTR                  
000999     END-IF                                                               
001000                                                                          
001001     IF REQU-FLSATS            =  ALL '+'                                 
001002         MOVE ALL-SPACE        TO RESP-FLSATS-INM                         
001003     ELSE                                                                 
001004         MOVE REQU-FLSATS      TO RESP-FLSATS-INM                         
001005         MOVE MFS-ADD-LAES-IN-FAELT                                       
001006                               TO RESP-FLSATS-INM-ATTR                    
001007     END-IF                                                               
001008                                                                          
001009     IF REQU-KDPRTVAL          = ALL '+'                                  
001010         MOVE ALL-SPACE       TO RESP-KDPRTVAL                            
001011     ELSE                                                                 
001012       MOVE REQU-KDPRTVAL      TO RESP-KDPRTVAL                           
001013       MOVE MFS-ADD-LAES-IN-FAELT                                         
001014                               TO RESP-KDPRTVAL-ATTR                      
001015     END-IF                                                               
001016     .                                                                    
001017     EJECT                                                                
001018 F-LAES-VISA-INFO SECTION.                                                
001019                                                                          
001020     MOVE NEJ                  TO KOLLI-SW                                
001021                                  DIVKLI-SW                               
001022                                  BLAEDDRING-SW                           
001023     MOVE ZERO                 TO RESP-IDLOPNRM-NEXT                      
001024                                  RESP-KVRADER                            
001025     EVALUATE TRUE                                                        
001026                                                                          
001027       WHEN IDLEVNWKOLLI-NYCKEL                                           
001028         MOVE W-IDLEVNR-KOLLI TO W-IDLEVNRK                               
001029         MOVE W-IDOKOLLI      TO W-IDOKOLLIK                              
001030         IF (REQU-NEXT OR REQU-QUERY) AND (NOT REQU-UPDATE)               
001031             CONTINUE                                                     
001032         ELSE                                                             
001033           IF REQU-UPDATE                                                 
001034             MOVE REQU-IDLOPNRM-START   TO W-NUM-IDLOPNRM                 
001035             PERFORM IMS-GU-W6INLA11-C                                    
001036             PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT OR              
001037                           ART-IDLOPNRM = W-NUM-IDLOPNRM                  
001038               PERFORM IMS-GN-W6INLA11-C                                  
001039             END-PERFORM                                                  
001040           ELSE                                                           
001041             PERFORM IMS-GU-W6INLA11-C                                    
001042           END-IF                                                         
001043         END-IF                                                           
001044         MOVE ART-IDLOPNRM TO RESP-IDLOPNRM                               
001045                                                                          
001046       WHEN IDLOPNRM-NYCKEL                                               
001047         MOVE WS-IDLOPNRM-NUM TO W-IDLOPNR                                
001048         IF REQU-QUERY AND (NOT REQU-UPDATE)                              
001049             CONTINUE                                                     
001050         ELSE                                                             
001051             PERFORM IMS-GU-W6INLA11-B                                    
001052         END-IF                                                           
001053         MOVE ALL-SPACE       TO RESP-IDLEVNR-KOLLI                       
001054                                 RESP-IDOKOLLI                            
001055     END-EVALUATE                                                         
001056                                                                          
001057     IF SEGMENT-FINNS                                                     
001058       IF ART-IDLOPNRM > ZERO                                             
001059         MOVE ART-IDLOPNRM     TO RESP-IDLOPNRM-START                     
001060                                  W-SPAR-IDLOPNRM                         
001061         PERFORM FA-PARTIINFO                                             
001062         PERFORM FB-KOLLIINFO                                             
001063       ELSE                                                               
001064         MOVE ERR-FINNS-EJ     TO RESP-IDMSG-ERROR                        
001065         PERFORM MFS-RENSA-FAELT-UT                                       
001066         PERFORM MFS-RENSA-FAELT-IN                                       
001067       END-IF                                                             
001068     ELSE                                                                 
001069         MOVE ERR-FINNS-EJ     TO RESP-IDMSG-ERROR                        
001070         PERFORM MFS-RENSA-FAELT-UT                                       
001080         PERFORM MFS-RENSA-FAELT-IN                                       
001081     END-IF                                                               
001082                                                                          
001083     IF FL-KR = NEJ                                                       
001084         MOVE NO-IR-CREATED    TO RESP-IDMSG-INFO                         
001085     END-IF                                                               
001086                                                                          
001087     IF FYLLD-IX               >  MAX-TAB-IX                              
001088         MOVE INF-MORE-LINES   TO RESP-IDMSG-INFO                         
001089     END-IF                                                               
001090                                                                          
001091     IF BLAEDDRING AND RESP-IDMSG-INFO = SPACE                            
001092         MOVE INF-PF8-NXT-BTCH TO RESP-IDMSG-INFO                         
001093     END-IF                                                               
001094     .                                                                    
001095     EJECT                                                                
001096 FA-PARTIINFO  SECTION.                                                   
001097                                                                          
001098     MOVE ART-IDARTNR TO RESP-IDARTNR                                     
001099                         WS-IDARTNR                                       
001100                         W-IDARTNR                                        
001101                                                                          
001102     MOVE ALL '+' TO MSGI-WMSGINIT                                        
001103     MOVE '001'                  TO MSGI-KDCALL                           
001104     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
001105     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
001106     MOVE '6133'                 TO MSGI-IDTRANS                          
001107     MOVE WS-IDARTNR             TO MSGI-IDARTNR                          
001108     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
001109                                                                          
001110     MOVE ART-KVAVIS  TO RESP-KVAVIS                                      
001111     MOVE ART-BEART   TO RESP-BEART                                       
001112     IF REQU-IDMSGVER = 1                                                 
001113       PERFORM FAC-GET-BEART                                              
001114     END-IF                                                               
001115     MOVE ART-KDSORT  TO RESP-KDSORT                                      
001116     MOVE ART-BEFT    TO RESP-BEFT                                        
001117                                                                          
001118     IF ART-FLKVAFEL = JA OR ART-FLKVAKAR = JA                            
001119         MOVE ERR-QUALITY-ERROR TO RESP-IDMSG-ERROR                       
001120     END-IF                                                               
001121                                                                          
001122     EVALUATE TRUE                                                        
001123       WHEN ART-KDFARLIG       = 4                                        
001124         IF WS-ENGLISH-TEXT                                               
001125            MOVE YES           TO RESP-BEFARLIG                           
001126         ELSE                                                             
001127            MOVE JA            TO RESP-BEFARLIG                           
001128         END-IF                                                           
001129       WHEN ART-KDFARLIG       = 5                                        
001130         MOVE 'ASBEST'         TO RESP-BEFARLIG                           
001131       WHEN ART-KDFARLIG       = 6                                        
001132         IF WS-ENGLISH-TEXT                                               
001133           MOVE 'CHEMICALS '   TO RESP-BEFARLIG                           
001134         ELSE                                                             
001135           MOVE 'KEMIKALIER'   TO RESP-BEFARLIG                           
001136         END-IF                                                           
001137       WHEN ART-KDFARLIG       = 7                                        
001138         IF WS-ENGLISH-TEXT                                               
001139            MOVE YES           TO RESP-BEFARLIG                           
001140         ELSE                                                             
001141            MOVE JA            TO RESP-BEFARLIG                           
001142         END-IF                                                           
001143       WHEN OTHER                                                         
001144         IF WS-ENGLISH-TEXT                                               
001145           MOVE 'NO '            TO RESP-BEFARLIG                         
001146         ELSE                                                             
001147           MOVE 'NEJ'            TO RESP-BEFARLIG                         
001148         END-IF                                                           
001149     END-EVALUATE                                                         
001150                                                                          
001151     MOVE ART-KDLAGEMB         TO RESP-KDLAGEMB                           
001152     MOVE ART-ADLAGOMR         TO RESP-ADLAGOMR                           
001153     MOVE ART-ADGANG           TO RESP-ADGANG                             
001154     MOVE ART-ADPLATS          TO RESP-ADPLATS                            
001155     MOVE ART-ADTRDEST-KIT     TO RESP-ADTRDEST-KIT                       
001156                                                                          
001157     IF ART-KDKVAANT > ZERO                                               
001158       IF WS-ENGLISH-TEXT                                                 
001159          MOVE YES TO RESP-FLKVAANT-TOT                                   
001160       ELSE                                                               
001161          MOVE JA TO RESP-FLKVAANT-TOT                                    
001162       END-IF                                                             
001163     ELSE                                                                 
001164       MOVE NEJ TO RESP-FLKVAANT-TOT                                      
001165     END-IF                                                               
001166                                                                          
001167     COMPUTE WW-KVKVAPRIM-KVAR =                                          
001168                       ART-KVKVAPRIM-BER - ART-KVKVAPRIM-VER              
001169                                                                          
001170     IF WW-KVKVAPRIM-KVAR < ZERO                                          
001171        MOVE ZERO TO RESP-KVKVAPRIM-KVAR                                  
001172     ELSE                                                                 
001173        MOVE WW-KVKVAPRIM-KVAR TO RESP-KVKVAPRIM-KVAR                     
001174     END-IF                                                               
001175                                                                          
001176     MOVE ZERO        TO WS-KVROS                                         
001177     IF DCS-CDC                                                           
001178       PERFORM IMS-GU-WDK611                                              
001179       IF SEGMENT-FINNS                                                   
001180         MOVE CLAG-KVROS TO WS-KVROS                                      
001181       END-IF                                                             
001182     END-IF                                                               
001183                                                                          
001184     MOVE WS-KVROS TO RESP-KVROS                                          
001185                                                                          
001186     MOVE ART-IDLOPNRM         TO ADR-IDLOPNRM                            
001187                                                                          
001188     CALL W611ADR USING ADR-W611ADR ADR-INLA-PCB ADR-INLC-PCB             
001189                        ADR-PLAA-PCB ADR-WDK6-PCB                         
001190                        ADR-STYR-HANA-PCB                                 
001191                        ADR-STYR-PLAA-PCB                                 
001192                                                                          
001193     MOVE ADR-ADINLOMR-NXT1    TO RESP-ADINLOMR-NXT1                      
001194     MOVE ADR-ADINLOMR-NXT2    TO RESP-ADINLOMR-NXT2                      
001195     MOVE ADR-ADINLOMR-NXT3    TO RESP-ADINLOMR-NXT3                      
001196     MOVE ADR-ADINLOMR-NXT4    TO RESP-ADINLOMR-NXT4                      
001197     MOVE ADR-ADINLOMR-NXT5    TO RESP-ADINLOMR-NXT5                      
001198     .                                                                    
001199     EJECT                                                                
001200                                                                          
001201 FAC-GET-BEART         SECTION.                                           
001202*    -- SELECT LANGUAGE TO FETCH AND CORRESPONDING CODE-PAGE              
001203                                                                          
001204     MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                              
001205     IF DCS-UNICODE-IDSKYLT                                               
001206        MOVE 'UTF8'             TO TRAUTF8-KDCP                           
001207     ELSE                                                                 
001208        MOVE '278 '             TO TRAUTF8-KDCP                           
001209     END-IF                                                               
001210                                                                          
001211     PERFORM IMS-GU-WDD311                                                
001212     IF SEGMENT-FINNS                                                     
001213       MOVE TEXT-BEART    TO TRAUTF8-TECONV-FROM                          
001214     ELSE                                                                 
001215       MOVE SPACE         TO TRAUTF8-TECONV-FROM                          
001216                             TEXT-BEART                                   
001217       MOVE WS-CP-EBCDIC  TO TRAUTF8-KDCP                                 
001218     END-IF                                                               
001219     IF TRAUTF8-TECONV-FROM = SPACES                                      
001220      MOVE 'GB'  TO W-IDSKYLT                                             
001221      MOVE '278' TO TRAUTF8-KDCP                                          
001222      PERFORM IMS-GU-WDD311                                               
001223      MOVE TEXT-BEART    TO TRAUTF8-TECONV-FROM                           
001224     END-IF                                                               
001225                                                                          
001226*    CALL FROM WEB                                                        
001227*    CONVERT TO UNICODE IF NOT ALREADY SO, STRIP TRAILING SPACE           
001228     CALL WTRAUTF8 USING TRAUTF8-AREA                                     
001229     MOVE TRAUTF8-TECONV-TO TO RESP-BEART                                 
001230     .                                                                    
001231     EJECT                                                                
001232 FB-KOLLIINFO  SECTION.                                                   
001233                                                                          
001234     MOVE +0                   TO FYLLD-IX                                
001235     EVALUATE TRUE                                                        
001236       WHEN IDLEVNWKOLLI-NYCKEL                                           
001237         PERFORM FBA-FYLLA-TAB-IDLEVNWKOLLI                               
001238                                                                          
001239       WHEN IDLOPNRM-NYCKEL                                               
001240         PERFORM FBB-FYLLA-TAB-IDLOPNRM                                   
001241                                                                          
001242     END-EVALUATE                                                         
001243                                                                          
001244     MOVE ART-W6D111     TO SPAR-ART-W6D111                               
001245                                                                          
001246     IF DIVKLI                                                            
001247         PERFORM FBC-BLAEDDRINGS-NYCKEL                                   
001248     END-IF                                                               
001249                                                                          
001250     IF KOLLI                                                             
001251         IF REQU-UPDATE                                                   
001252             CONTINUE                                                     
001253         ELSE                                                             
001254             MOVE ERR-BATCH-ON-CASE-LVL TO RESP-IDMSG-ERROR               
001255         END-IF                                                           
001256         PERFORM MFS-RENSA-FAELT-IN                                       
001257         PERFORM MFS-RENSA-FAELT-UT                                       
001258         MOVE SPAR-ART-IDLOPNRM     TO RESP-IDLOPNRM-START                
001259     ELSE                                                                 
001260         PERFORM FBD-LAEGGA-UT-MOD                                        
001261     END-IF                                                               
001262     .                                                                    
001263     EJECT                                                                
001264 FBA-FYLLA-TAB-IDLEVNWKOLLI SECTION.                                      
001265                                                                          
001266     MOVE W-IDLEVNR-KOLLI      TO W-IDLEVNR                               
001267     MOVE W-IDOKOLLI           TO W-IDOKOLLINR                            
001268     PERFORM IMS-GNP-W6INLA21                                             
001269     IF RAD-FLDIVKLI           =  JA                                      
001270         MOVE JA               TO DIVKLI-SW                               
001271     END-IF                                                               
001272     MOVE RAD-IDRADNR          TO WW-IDRADNR                              
001273     IF RAD-KDINLSTA           = SPACE OR 'FPK' OR 'SAK'                  
001274         PERFORM S01-BERAKNA                                              
001275         PERFORM S02-FYLL-TAB                                             
001276         PERFORM IMS-GNP-W6INLA21-C-FIRST                                 
001277     ELSE                                                                 
001278         IF RAD-FLDIVKLI = JA                                             
001279           PERFORM UNTIL SEGMENT-SLUT OR SEGMENT-SAKNAS OR                
001280                         (RAD-KDINLSTA = SPACE OR 'FPK' OR 'SAK')         
001281             PERFORM IMS-GNP-W6INLA21                                     
001282           END-PERFORM                                                    
001283           IF SEGMENT-SLUT OR SEGMENT-SAKNAS                              
001284             PERFORM IMS-GN-W6INLA11-C                                    
001285             IF SEGMENT-FINNS                                             
001286               PERFORM IMS-GNP-W6INLA21                                   
001287             END-IF                                                       
001288             PERFORM UNTIL SEGMENT-SLUT OR SEGMENT-SAKNAS OR              
001289                         (RAD-KDINLSTA = SPACE OR 'FPK' OR 'SAK')         
001290               PERFORM IMS-GN-W6INLA11-C                                  
001291               IF SEGMENT-FINNS                                           
001292                 PERFORM IMS-GNP-W6INLA21                                 
001293               END-IF                                                     
001294             END-PERFORM                                                  
001295             IF SEGMENT-FINNS                                             
001296               MOVE ART-IDLOPNRM TO RESP-IDLOPNRM                         
001297               PERFORM S40-BYT-ARTIKEL-UPPGIFTER                          
001298               PERFORM S01-BERAKNA                                        
001299               PERFORM S02-FYLL-TAB                                       
001300               PERFORM IMS-GNP-W6INLA21-C-FIRST                           
001301             END-IF                                                       
001302           END-IF                                                         
001303         ELSE                                                             
001304           MOVE 'GE'             TO STATUS-WS                             
001305         END-IF                                                           
001306     END-IF                                                               
001307     .                                                                    
001308     EJECT                                                                
001309 FBB-FYLLA-TAB-IDLOPNRM SECTION.                                          
001310                                                                          
001311     PERFORM IMS-GNP-W6INLA21-OKVAL-B                                     
001312     IF SEGMENT-FINNS AND RAD-IDRADNR  = 1                                
001313         PERFORM UNTIL SEGMENT-SLUT OR                                    
001314                       SEGMENT-SAKNAS OR                                  
001315                       TAB-IX > 30                                        
001316             IF RAD-KDINLSTA   = SPACE OR 'FPK' OR 'SAK'                  
001317                 PERFORM S02-FYLL-TAB                                     
001318             END-IF                                                       
001319             PERFORM S01-BERAKNA                                          
001320             PERFORM IMS-GNP-W6INLA21-OKVAL-B                             
001321         END-PERFORM                                                      
001322     ELSE                                                                 
001323         MOVE JA               TO KOLLI-SW                                
001324     END-IF                                                               
001325     .                                                                    
001326     EJECT                                                                
001327 FBC-BLAEDDRINGS-NYCKEL     SECTION.                                      
001328                                                                          
001329     PERFORM IMS-GN-W6INLA11-C                                            
001330     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT OR                      
001331                   ART-IDLOPNRM   NOT = W-SPAR-IDLOPNRM                   
001332         PERFORM IMS-GN-W6INLA11-C                                        
001333     END-PERFORM                                                          
001334                                                                          
001335     IF SEGMENT-FINNS                                                     
001336       PERFORM IMS-GNP-W6INLA21                                           
001337       IF RAD-KDINLSTA = SPACE OR 'SAK' OR 'FPK'                          
001338         MOVE ART-IDLOPNRM     TO RESP-IDLOPNRM-NEXT                      
001339         MOVE JA               TO BLAEDDRING-SW                           
001340       ELSE                                                               
001341         PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT OR                  
001342                      (RAD-KDINLSTA = SPACE OR 'FPK' OR 'SAK')            
001343           PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT OR                
001344                        (RAD-KDINLSTA = SPACE OR 'FPK' OR 'SAK')          
001345             PERFORM IMS-GNP-W6INLA21                                     
001346           END-PERFORM                                                    
001347           IF SEGMENT-SAKNAS OR SEGMENT-SLUT                              
001348             PERFORM IMS-GN-W6INLA11-C                                    
001349           END-IF                                                         
001350         END-PERFORM                                                      
001351         IF SEGMENT-FINNS                                                 
001352           MOVE ART-IDLOPNRM     TO RESP-IDLOPNRM-NEXT                    
001353           MOVE JA               TO BLAEDDRING-SW                         
001354         END-IF                                                           
001355       END-IF                                                             
001356     END-IF                                                               
001357     .                                                                    
001358     EJECT                                                                
001359 FBD-LAEGGA-UT-MOD SECTION.                                               
001360                                                                          
001361     MOVE ALL-SPACE            TO RESP-KDKLIPRI                           
001362                                  RESP-FLSATS                             
001363                                  RESP-FLKVAANT                           
001364     PERFORM FBDA-LAEGG-UT-HUVUD                                          
001365     SET TAB-IX TO 1                                                      
001366     MOVE 1                    TO INDX                                    
001367     MOVE 0                    TO RESP-KVRADER                            
001368     IF FYLLD-IX               >  ZERO                                    
001369         PERFORM FBDB-LAEGG-UT-RAD1                                       
001370     ELSE                                                                 
001371         MOVE ERR-FINNS-EJ     TO RESP-IDMSG-ERROR                        
001372         PERFORM MFS-RENSA-FAELT-UT                                       
001373         PERFORM MFS-RENSA-FAELT-IN                                       
001374     END-IF                                                               
001375                                                                          
001376     ADD 1                     TO INDX                                    
001377     ADD 1                     TO RESP-KVRADER                            
001378     MOVE 1                    TO W-KDSORT                                
001379     PERFORM UNTIL INDX        > MAX-KVRADER OR                           
001380                   INDX        > FYLLD-IX                                 
001381         PERFORM FBDC-LAEGG-UT-OEVRIGA-RADER                              
001382                                                                          
001383     END-PERFORM                                                          
001384                                                                          
001385     IF INDX                     < REQU-KVRADER                           
001386         PERFORM UNTIL INDX      > REQU-KVRADER                           
001387            MOVE ALL-SPACE       TO RESP-KVINLART-LINE(INDX)              
001388                                    RESP-KVKOLLI-LINE(INDX)               
001389                                    RESP-ADINLOMR-LINE(INDX)              
001390                                    RESP-KDINLSTA-LINE(INDX)              
001391                                    RESP-IDINLVGN-LINE(INDX)              
001392                                    RESP-ADINLOMR-NXT-LINE(INDX)          
001393            ADD +1               TO INDX                                  
001394         END-PERFORM                                                      
001395     END-IF                                                               
001396     .                                                                    
001397     EJECT                                                                
001398 FBDA-LAEGG-UT-HUVUD SECTION.                                             
001399                                                                          
001400     MOVE WW-KVKOLLI-TOT       TO RESP-KVKOLLI-TOT                        
001401                                                                          
001402     COMPUTE W-KVAVIS-KVAR     =  SPAR-ART-KVAVIS - WW-KVAVIS-KVAR        
001403     IF W-KVAVIS-KVAR          <  ZERO                                    
001404        MOVE ZERO              TO RESP-KVAVIS-KVAR                        
001405     ELSE                                                                 
001406        MOVE W-KVAVIS-KVAR     TO RESP-KVAVIS-KVAR                        
001407     END-IF                                                               
001408                                                                          
001409     COMPUTE W-KVAVIS-PRIO-KVAR =                                         
001410                SPAR-ART-KVAVIS-PRIO - WW-KVAVIS-PRIO-KVAR                
001411     IF W-KVAVIS-PRIO-KVAR      < ZERO                                    
001412        MOVE ZERO               TO RESP-KVAVIS-PRIO-KVAR                  
001413     ELSE                                                                 
001414        MOVE W-KVAVIS-PRIO-KVAR TO RESP-KVAVIS-PRIO-KVAR                  
001415     END-IF                                                               
001416                                                                          
001417     COMPUTE W-KVAVIS-KIT-KVAR =                                          
001418             SPAR-ART-KVAVIS-KIT - WW-KVAVIS-KIT-KVAR                     
001419     IF W-KVAVIS-KIT-KVAR      < ZERO                                     
001420        MOVE ZERO              TO RESP-KVAVIS-KIT-KVAR                    
001421     ELSE                                                                 
001422        MOVE W-KVAVIS-KIT-KVAR TO RESP-KVAVIS-KIT-KVAR                    
001423     END-IF                                                               
001424     .                                                                    
001425     EJECT                                                                
001426 FBDB-LAEGG-UT-RAD1 SECTION.                                              
001427                                                                          
001428     MOVE TAB-KVINLART(TAB-IX)     TO RESP-KVINLART-LINE(INDX)            
001429     MOVE TAB-KVKOLLI(TAB-IX)      TO RESP-KVKOLLI-LINE(INDX)             
001430     MOVE TAB-ADINLOMR(TAB-IX)     TO RESP-ADINLOMR-LINE(INDX)            
001431     MOVE TAB-KDINLSTA(TAB-IX)     TO RESP-KDINLSTA-LINE(INDX)            
001432     IF WS-ENGLISH-TEXT                                                   
001433       EVALUATE RESP-KDINLSTA-LINE(INDX)                                  
001434         WHEN 'FPK'                                                       
001435            MOVE 'PP '         TO RESP-KDINLSTA-LINE(INDX)                
001436         WHEN 'INL'                                                       
001437            MOVE 'BIN'         TO RESP-KDINLSTA-LINE(INDX)                
001438         WHEN 'SAK'                                                       
001439            MOVE 'MIS'         TO RESP-KDINLSTA-LINE(INDX)                
001440         WHEN 'AVV'                                                       
001441            MOVE 'DEV'         TO RESP-KDINLSTA-LINE(INDX)                
001442         WHEN 'ANT'                                                       
001443            MOVE 'DEV'         TO RESP-KDINLSTA-LINE(INDX)                
001444         WHEN 'KVA'                                                       
001445            MOVE 'Q-D'         TO RESP-KDINLSTA-LINE(INDX)                
001446         WHEN 'RET'                                                       
001447            MOVE 'RET'         TO RESP-KDINLSTA-LINE(INDX)                
001448         WHEN 'FRD'                                                       
001449            MOVE 'TRP'         TO RESP-KDINLSTA-LINE(INDX)                
001450         WHEN 'MAK'                                                       
001451            MOVE 'CAN'         TO RESP-KDINLSTA-LINE(INDX)                
001452       END-EVALUATE                                                       
001453     END-IF                                                               
001454     MOVE TAB-IDINLVGN(TAB-IX)     TO RESP-IDINLVGN-LINE(INDX)            
001455     MOVE TAB-ADINLOMR-NXT(TAB-IX) TO RESP-ADINLOMR-NXT-LINE(INDX)        
001456     MOVE TAB-KDKLIPRI(TAB-IX)     TO RESP-KDKLIPRI                       
001457     MOVE TAB-FLSATS(TAB-IX)       TO RESP-FLSATS                         
001458     MOVE TAB-FLKVAANT(TAB-IX)     TO RESP-FLKVAANT                       
001459     .                                                                    
001460     EJECT                                                                
001461 FBDC-LAEGG-UT-OEVRIGA-RADER SECTION.                                     
001462                                                                          
001463     EVALUATE W-KDSORT                                                    
001464         WHEN 1                                                           
001465            PERFORM FBDCA-SAMMA-PLAC-ADR-STAT                             
001466            ADD +1             TO W-KDSORT                                
001467                                                                          
001468         WHEN 2                                                           
001469            PERFORM FBDCB-SAMMA-PLAC-ADR                                  
001470                                                                          
001471         WHEN 3                                                           
001472            PERFORM FBDCC-SAMMA-ADR-STAT                                  
001473                                                                          
001474         WHEN 4                                                           
001475            PERFORM FBDCD-SAMMA-ADR                                       
001476                                                                          
001477         WHEN 5                                                           
001478            PERFORM FBDCE-PLAC-LPL-STAT                                   
001479                                                                          
001480         WHEN 6                                                           
001481            PERFORM FBDCF-PLAC-LPL                                        
001482                                                                          
001483         WHEN 7                                                           
001484            PERFORM FBDCG-PLAC-FP-FBP-STAT                                
001485                                                                          
001486         WHEN 8                                                           
001487            PERFORM FBDCH-PLAC-FP-FBP                                     
001488                                                                          
001489         WHEN 9                                                           
001490            PERFORM FBDCI-ADR-FP-FBP-STAT                                 
001491                                                                          
001492         WHEN 10                                                          
001493            PERFORM FBDCJ-ADR-FP-FBP                                      
001494                                                                          
001495         WHEN 11                                                          
001496            PERFORM FBDCK-LAEGG-UT-RESTEN                                 
001497     END-EVALUATE                                                         
001498     .                                                                    
001499     EJECT                                                                
001500 FBDCA-SAMMA-PLAC-ADR-STAT  SECTION.                                      
001501                                                                          
001502     SET TAB-IX                TO 2                                       
001503     PERFORM UNTIL TAB-IX      >  FYLLD-IX OR                             
001504                  (TAB-ADINLOMR(TAB-IX) = TAB-ADINLOMR(1) AND             
001505                   TAB-ADINLOMR-NXT(TAB-IX) =                             
001506                   TAB-ADINLOMR-NXT(1)                    AND             
001507                   TAB-KDINLSTA(TAB-IX) = TAB-KDINLSTA(1))                
001508          SET TAB-IX UP BY 1                                              
001509     END-PERFORM                                                          
001510                                                                          
001511     IF TAB-IX                 > FYLLD-IX                                 
001512         CONTINUE                                                         
001513     ELSE                                                                 
001514         MOVE TAB-KVINLART(TAB-IX)     TO RESP-KVINLART-LINE(INDX)        
001515         MOVE TAB-KVKOLLI(TAB-IX)      TO RESP-KVKOLLI-LINE(INDX)         
001516         MOVE TAB-ADINLOMR(TAB-IX)     TO RESP-ADINLOMR-LINE(INDX)        
001517         MOVE TAB-KDINLSTA(TAB-IX)     TO RESP-KDINLSTA-LINE(INDX)        
001518         IF WS-ENGLISH-TEXT                                               
001519           EVALUATE RESP-KDINLSTA-LINE(INDX)                              
001520             WHEN 'FPK'                                                   
001521                MOVE 'PP '         TO RESP-KDINLSTA-LINE(INDX)            
001522             WHEN 'INL'                                                   
001523                MOVE 'BIN'         TO RESP-KDINLSTA-LINE(INDX)            
001524             WHEN 'SAK'                                                   
001525                MOVE 'MIS'         TO RESP-KDINLSTA-LINE(INDX)            
001526             WHEN 'AVV'                                                   
001527                MOVE 'DEV'         TO RESP-KDINLSTA-LINE(INDX)            
001528             WHEN 'ANT'                                                   
001529                MOVE 'DEV'         TO RESP-KDINLSTA-LINE(INDX)            
001530             WHEN 'KVA'                                                   
001531                MOVE 'Q-D'         TO RESP-KDINLSTA-LINE(INDX)            
001532             WHEN 'RET'                                                   
001533                MOVE 'RET'         TO RESP-KDINLSTA-LINE(INDX)            
001534             WHEN 'FRD'                                                   
001535                MOVE 'TRP'         TO RESP-KDINLSTA-LINE(INDX)            
001536             WHEN 'MAK'                                                   
001537                MOVE 'CAN'         TO RESP-KDINLSTA-LINE(INDX)            
001538           END-EVALUATE                                                   
001539         END-IF                                                           
001540         MOVE TAB-IDINLVGN(TAB-IX)     TO RESP-IDINLVGN-LINE(INDX)        
001541         MOVE TAB-ADINLOMR-NXT(TAB-IX) TO                                 
001542                                      RESP-ADINLOMR-NXT-LINE(INDX)        
001543         PERFORM S03-SUDDA-UT-TAB-RAD                                     
001544         ADD +1                        TO INDX                            
001545         ADD +1                        TO RESP-KVRADER                    
001546     END-IF                                                               
001547     .                                                                    
001548     EJECT                                                                
001549 FBDCB-SAMMA-PLAC-ADR       SECTION.                                      
001550                                                                          
001551     SET TAB-IX                TO 2                                       
001552     PERFORM UNTIL TAB-IX      >  FYLLD-IX OR                             
001553                  (TAB-ADINLOMR(TAB-IX) = TAB-ADINLOMR(1) AND             
001554                   TAB-ADINLOMR-NXT(TAB-IX) =                             
001555                   TAB-ADINLOMR-NXT(1))                                   
001556          SET TAB-IX UP BY 1                                              
001557     END-PERFORM                                                          
001558                                                                          
001559     IF TAB-IX                 > FYLLD-IX                                 
001560         ADD 1                 TO W-KDSORT                                
001561     ELSE                                                                 
001562         MOVE TAB-KVINLART(TAB-IX)     TO RESP-KVINLART-LINE(INDX)        
001563         MOVE TAB-KVKOLLI(TAB-IX)      TO RESP-KVKOLLI-LINE(INDX)         
001564         MOVE TAB-ADINLOMR(TAB-IX)     TO RESP-ADINLOMR-LINE(INDX)        
001565         MOVE TAB-KDINLSTA(TAB-IX)     TO RESP-KDINLSTA-LINE(INDX)        
001566         IF WS-ENGLISH-TEXT                                               
001567           EVALUATE RESP-KDINLSTA-LINE(INDX)                              
001568             WHEN 'FPK'                                                   
001569                MOVE 'PP '         TO RESP-KDINLSTA-LINE(INDX)            
001570             WHEN 'INL'                                                   
001571                MOVE 'BIN'         TO RESP-KDINLSTA-LINE(INDX)            
001572             WHEN 'SAK'                                                   
001573                MOVE 'MIS'         TO RESP-KDINLSTA-LINE(INDX)            
001574             WHEN 'AVV'                                                   
001575                MOVE 'DEV'         TO RESP-KDINLSTA-LINE(INDX)            
001576             WHEN 'ANT'                                                   
001577                MOVE 'DEV'         TO RESP-KDINLSTA-LINE(INDX)            
001578             WHEN 'KVA'                                                   
001579                MOVE 'Q-D'         TO RESP-KDINLSTA-LINE(INDX)            
001580             WHEN 'RET'                                                   
001581                MOVE 'RET'         TO RESP-KDINLSTA-LINE(INDX)            
001582             WHEN 'FRD'                                                   
001583                MOVE 'TRP'         TO RESP-KDINLSTA-LINE(INDX)            
001584             WHEN 'MAK'                                                   
001585                MOVE 'CAN'         TO RESP-KDINLSTA-LINE(INDX)            
001586           END-EVALUATE                                                   
001587         END-IF                                                           
001588         MOVE TAB-IDINLVGN(TAB-IX)     TO RESP-IDINLVGN-LINE(INDX)        
001589         MOVE TAB-ADINLOMR-NXT(TAB-IX) TO                                 
001590                                     RESP-ADINLOMR-NXT-LINE(INDX)         
001591         PERFORM S03-SUDDA-UT-TAB-RAD                                     
001592         ADD +1                        TO INDX                            
001593         ADD +1                        TO RESP-KVRADER                    
001594     END-IF                                                               
001595     .                                                                    
001596     EJECT                                                                
001597 FBDCC-SAMMA-ADR-STAT  SECTION.                                           
001598                                                                          
001599     SET TAB-IX                TO 2                                       
001600     PERFORM UNTIL TAB-IX      >  FYLLD-IX OR                             
001601                  (TAB-ADINLOMR-NXT(TAB-IX) =                             
001602                   TAB-ADINLOMR(1)               AND                      
001603                   TAB-KDINLSTA(TAB-IX) = TAB-KDINLSTA(1))                
001604          SET TAB-IX UP BY 1                                              
001605     END-PERFORM                                                          
001606                                                                          
001607     IF TAB-IX                 > FYLLD-IX                                 
001608         ADD 1                 TO W-KDSORT                                
001609     ELSE                                                                 
001610         MOVE TAB-KVINLART(TAB-IX)     TO RESP-KVINLART-LINE(INDX)        
001611         MOVE TAB-KVKOLLI(TAB-IX)      TO RESP-KVKOLLI-LINE(INDX)         
001612         MOVE TAB-ADINLOMR(TAB-IX)     TO RESP-ADINLOMR-LINE(INDX)        
001613         MOVE TAB-KDINLSTA(TAB-IX)     TO RESP-KDINLSTA-LINE(INDX)        
001614         IF WS-ENGLISH-TEXT                                               
001615           EVALUATE RESP-KDINLSTA-LINE(INDX)                              
001616             WHEN 'FPK'                                                   
001617                MOVE 'PP '         TO RESP-KDINLSTA-LINE(INDX)            
001618             WHEN 'INL'                                                   
001619                MOVE 'BIN'         TO RESP-KDINLSTA-LINE(INDX)            
001620             WHEN 'SAK'                                                   
001621                MOVE 'MIS'         TO RESP-KDINLSTA-LINE(INDX)            
001622             WHEN 'AVV'                                                   
001623                MOVE 'DEV'         TO RESP-KDINLSTA-LINE(INDX)            
001624             WHEN 'ANT'                                                   
001625                MOVE 'DEV'         TO RESP-KDINLSTA-LINE(INDX)            
001626             WHEN 'KVA'                                                   
001627                MOVE 'Q-D'         TO RESP-KDINLSTA-LINE(INDX)            
001628             WHEN 'RET'                                                   
001629                MOVE 'RET'         TO RESP-KDINLSTA-LINE(INDX)            
001630             WHEN 'FRD'                                                   
001631                MOVE 'TRP'         TO RESP-KDINLSTA-LINE(INDX)            
001632             WHEN 'MAK'                                                   
001633                MOVE 'CAN'         TO RESP-KDINLSTA-LINE(INDX)            
001634           END-EVALUATE                                                   
001635         END-IF                                                           
001636         MOVE TAB-IDINLVGN(TAB-IX)     TO RESP-IDINLVGN-LINE(INDX)        
001637         MOVE TAB-ADINLOMR-NXT(TAB-IX) TO                                 
001638                                      RESP-ADINLOMR-NXT-LINE(INDX)        
001639         PERFORM S03-SUDDA-UT-TAB-RAD                                     
001640         ADD +1                        TO INDX                            
001641         ADD +1                        TO RESP-KVRADER                    
001642     END-IF                                                               
001643     .                                                                    
001644     EJECT                                                                
001645 FBDCD-SAMMA-ADR       SECTION.                                           
001646                                                                          
001647     SET TAB-IX                TO 2                                       
001648     PERFORM UNTIL TAB-IX      >  FYLLD-IX OR                             
001649                  (TAB-ADINLOMR-NXT(TAB-IX) =                             
001650                   TAB-ADINLOMR(1))                                       
001651          SET TAB-IX UP BY 1                                              
001652     END-PERFORM                                                          
001653                                                                          
001654     IF TAB-IX                 > FYLLD-IX                                 
001655         ADD 1                 TO W-KDSORT                                
001656     ELSE                                                                 
001657         MOVE TAB-KVINLART(TAB-IX)     TO RESP-KVINLART-LINE(INDX)        
001658         MOVE TAB-KVKOLLI(TAB-IX)      TO RESP-KVKOLLI-LINE(INDX)         
001659         MOVE TAB-ADINLOMR(TAB-IX)     TO RESP-ADINLOMR-LINE(INDX)        
001660         MOVE TAB-KDINLSTA(TAB-IX)     TO RESP-KDINLSTA-LINE(INDX)        
001661         IF WS-ENGLISH-TEXT                                               
001662           EVALUATE RESP-KDINLSTA-LINE(INDX)                              
001663             WHEN 'FPK'                                                   
001664                MOVE 'PP '         TO RESP-KDINLSTA-LINE(INDX)            
001665             WHEN 'INL'                                                   
001666                MOVE 'BIN'         TO RESP-KDINLSTA-LINE(INDX)            
001667             WHEN 'SAK'                                                   
001668                MOVE 'MIS'         TO RESP-KDINLSTA-LINE(INDX)            
001669             WHEN 'AVV'                                                   
001670                MOVE 'DEV'         TO RESP-KDINLSTA-LINE(INDX)            
001671             WHEN 'ANT'                                                   
001672                MOVE 'DEV'         TO RESP-KDINLSTA-LINE(INDX)            
001673             WHEN 'KVA'                                                   
001674                MOVE 'Q-D'         TO RESP-KDINLSTA-LINE(INDX)            
001675             WHEN 'RET'                                                   
001676                MOVE 'RET'         TO RESP-KDINLSTA-LINE(INDX)            
001677             WHEN 'FRD'                                                   
001678                MOVE 'TRP'         TO RESP-KDINLSTA-LINE(INDX)            
001679             WHEN 'MAK'                                                   
001680                MOVE 'CAN'         TO RESP-KDINLSTA-LINE(INDX)            
001681           END-EVALUATE                                                   
001682         END-IF                                                           
001683         MOVE TAB-IDINLVGN(TAB-IX)     TO RESP-IDINLVGN-LINE(INDX)        
001684         MOVE TAB-ADINLOMR-NXT(TAB-IX) TO                                 
001685                                      RESP-ADINLOMR-NXT-LINE(INDX)        
001686         PERFORM S03-SUDDA-UT-TAB-RAD                                     
001687         ADD +1                        TO INDX                            
001688         ADD +1                        TO RESP-KVRADER                    
001689     END-IF                                                               
001690     .                                                                    
001691     EJECT                                                                
001692 FBDCE-PLAC-LPL-STAT   SECTION.                                           
001693                                                                          
001694     SET TAB-IX                TO 2                                       
001695     PERFORM UNTIL TAB-IX      >  FYLLD-IX OR                             
001696                  (TAB-PLAC-KDINLOMR(TAB-IX) = 'LPL'  AND                 
001697                   TAB-ADINLOMR-NXT(TAB-IX)  = SPACE  AND                 
001698                   TAB-KDINLSTA(TAB-IX) = TAB-KDINLSTA(1))                
001699          SET TAB-IX UP BY 1                                              
001700     END-PERFORM                                                          
001701                                                                          
001702     IF TAB-IX                 > FYLLD-IX                                 
001703         ADD 1                 TO W-KDSORT                                
001704     ELSE                                                                 
001705         MOVE TAB-KVINLART(TAB-IX)     TO RESP-KVINLART-LINE(INDX)        
001706         MOVE TAB-KVKOLLI(TAB-IX)      TO RESP-KVKOLLI-LINE(INDX)         
001707         MOVE TAB-ADINLOMR(TAB-IX)     TO RESP-ADINLOMR-LINE(INDX)        
001708         MOVE TAB-KDINLSTA(TAB-IX)     TO RESP-KDINLSTA-LINE(INDX)        
001709         IF WS-ENGLISH-TEXT                                               
001710           EVALUATE RESP-KDINLSTA-LINE(INDX)                              
001711             WHEN 'FPK'                                                   
001712                MOVE 'PP '         TO RESP-KDINLSTA-LINE(INDX)            
001713             WHEN 'INL'                                                   
001714                MOVE 'BIN'         TO RESP-KDINLSTA-LINE(INDX)            
001715             WHEN 'SAK'                                                   
001716                MOVE 'MIS'         TO RESP-KDINLSTA-LINE(INDX)            
001717             WHEN 'AVV'                                                   
001718                MOVE 'DEV'         TO RESP-KDINLSTA-LINE(INDX)            
001719             WHEN 'ANT'                                                   
001720                MOVE 'DEV'         TO RESP-KDINLSTA-LINE(INDX)            
001721             WHEN 'KVA'                                                   
001722                MOVE 'Q-D'         TO RESP-KDINLSTA-LINE(INDX)            
001723             WHEN 'RET'                                                   
001724                MOVE 'RET'         TO RESP-KDINLSTA-LINE(INDX)            
001725             WHEN 'FRD'                                                   
001726                MOVE 'TRP'         TO RESP-KDINLSTA-LINE(INDX)            
001727             WHEN 'MAK'                                                   
001728                MOVE 'CAN'         TO RESP-KDINLSTA-LINE(INDX)            
001729           END-EVALUATE                                                   
001730         END-IF                                                           
001731         MOVE TAB-IDINLVGN(TAB-IX)     TO RESP-IDINLVGN-LINE(INDX)        
001732         MOVE TAB-ADINLOMR-NXT(TAB-IX) TO                                 
001733                                    RESP-ADINLOMR-NXT-LINE(INDX)          
001734         PERFORM S03-SUDDA-UT-TAB-RAD                                     
001735         ADD +1                        TO INDX                            
001736         ADD +1                        TO RESP-KVRADER                    
001737     END-IF                                                               
001738     .                                                                    
001739     EJECT                                                                
001740 FBDCF-PLAC-LPL        SECTION.                                           
001741                                                                          
001742     SET TAB-IX                TO 2                                       
001743     PERFORM UNTIL TAB-IX      >  FYLLD-IX OR                             
001744                  (TAB-PLAC-KDINLOMR(TAB-IX) = 'LPL'  AND                 
001745                   TAB-ADINLOMR-NXT(TAB-IX)  = SPACE)                     
001746          SET TAB-IX UP BY 1                                              
001747     END-PERFORM                                                          
001748                                                                          
001749     IF TAB-IX                 > FYLLD-IX                                 
001750         ADD 1                 TO W-KDSORT                                
001751     ELSE                                                                 
001752         MOVE TAB-KVINLART(TAB-IX)     TO RESP-KVINLART-LINE(INDX)        
001753         MOVE TAB-KVKOLLI(TAB-IX)      TO RESP-KVKOLLI-LINE(INDX)         
001754         MOVE TAB-ADINLOMR(TAB-IX)     TO RESP-ADINLOMR-LINE(INDX)        
001755         MOVE TAB-KDINLSTA(TAB-IX)     TO RESP-KDINLSTA-LINE(INDX)        
001756         IF WS-ENGLISH-TEXT                                               
001757           EVALUATE RESP-KDINLSTA-LINE(INDX)                              
001758             WHEN 'FPK'                                                   
001759                MOVE 'PP '         TO RESP-KDINLSTA-LINE(INDX)            
001760             WHEN 'INL'                                                   
001761                MOVE 'BIN'         TO RESP-KDINLSTA-LINE(INDX)            
001762             WHEN 'SAK'                                                   
001763                MOVE 'MIS'         TO RESP-KDINLSTA-LINE(INDX)            
001764             WHEN 'AVV'                                                   
001765                MOVE 'DEV'         TO RESP-KDINLSTA-LINE(INDX)            
001766             WHEN 'ANT'                                                   
001767                MOVE 'DEV'         TO RESP-KDINLSTA-LINE(INDX)            
001768             WHEN 'KVA'                                                   
001769                MOVE 'Q-D'         TO RESP-KDINLSTA-LINE(INDX)            
001770             WHEN 'RET'                                                   
001771                MOVE 'RET'         TO RESP-KDINLSTA-LINE(INDX)            
001772             WHEN 'FRD'                                                   
001773                MOVE 'TRP'         TO RESP-KDINLSTA-LINE(INDX)            
001774             WHEN 'MAK'                                                   
001775                MOVE 'CAN'         TO RESP-KDINLSTA-LINE(INDX)            
001776           END-EVALUATE                                                   
001777         END-IF                                                           
001778         MOVE TAB-IDINLVGN(TAB-IX)     TO RESP-IDINLVGN-LINE(INDX)        
001779         MOVE TAB-ADINLOMR-NXT(TAB-IX) TO                                 
001780                                     RESP-ADINLOMR-NXT-LINE(INDX)         
001781         PERFORM S03-SUDDA-UT-TAB-RAD                                     
001782         ADD +1                        TO INDX                            
001783         ADD +1                        TO RESP-KVRADER                    
001784     END-IF                                                               
001785     .                                                                    
001786     EJECT                                                                
001787 FBDCG-PLAC-FP-FBP-STAT     SECTION.                                      
001788                                                                          
001789     SET TAB-IX                TO 2                                       
001790     PERFORM UNTIL TAB-IX      >  FYLLD-IX OR                             
001791                 ((TAB-PLAC-KDINLOMR(TAB-IX) = 'F  ' OR 'FBP') AND        
001792                   TAB-ADINLOMR-NXT(TAB-IX)  = SPACE         AND          
001793                   TAB-KDINLSTA(TAB-IX) = TAB-KDINLSTA(1))                
001794          SET TAB-IX UP BY 1                                              
001795     END-PERFORM                                                          
001796                                                                          
001797     IF TAB-IX                 > FYLLD-IX                                 
001798         ADD 1                 TO W-KDSORT                                
001799     ELSE                                                                 
001800         MOVE TAB-KVINLART(TAB-IX)     TO RESP-KVINLART-LINE(INDX)        
001801         MOVE TAB-KVKOLLI(TAB-IX)      TO RESP-KVKOLLI-LINE(INDX)         
001802         MOVE TAB-ADINLOMR(TAB-IX)     TO RESP-ADINLOMR-LINE(INDX)        
001803         MOVE TAB-KDINLSTA(TAB-IX)     TO RESP-KDINLSTA-LINE(INDX)        
001804         IF WS-ENGLISH-TEXT                                               
001805           EVALUATE RESP-KDINLSTA-LINE(INDX)                              
001806             WHEN 'FPK'                                                   
001807                MOVE 'PP '         TO RESP-KDINLSTA-LINE(INDX)            
001808             WHEN 'INL'                                                   
001809                MOVE 'BIN'         TO RESP-KDINLSTA-LINE(INDX)            
001810             WHEN 'SAK'                                                   
001811                MOVE 'MIS'         TO RESP-KDINLSTA-LINE(INDX)            
001812             WHEN 'AVV'                                                   
001813                MOVE 'DEV'         TO RESP-KDINLSTA-LINE(INDX)            
001814             WHEN 'ANT'                                                   
001815                MOVE 'DEV'         TO RESP-KDINLSTA-LINE(INDX)            
001816             WHEN 'KVA'                                                   
001817                MOVE 'Q-D'         TO RESP-KDINLSTA-LINE(INDX)            
001818             WHEN 'RET'                                                   
001819                MOVE 'RET'         TO RESP-KDINLSTA-LINE(INDX)            
001820             WHEN 'FRD'                                                   
001821                MOVE 'TRP'         TO RESP-KDINLSTA-LINE(INDX)            
001822             WHEN 'MAK'                                                   
001823                MOVE 'CAN'         TO RESP-KDINLSTA-LINE(INDX)            
001824           END-EVALUATE                                                   
001825         END-IF                                                           
001826         MOVE TAB-IDINLVGN(TAB-IX)     TO RESP-IDINLVGN-LINE(INDX)        
001827         MOVE TAB-ADINLOMR-NXT(TAB-IX) TO                                 
001828                                      RESP-ADINLOMR-NXT-LINE(INDX)        
001829         PERFORM S03-SUDDA-UT-TAB-RAD                                     
001830         ADD +1                        TO INDX                            
001831         ADD +1                        TO RESP-KVRADER                    
001832     END-IF                                                               
001833     .                                                                    
001834     EJECT                                                                
001835 FBDCH-PLAC-FP-FBP          SECTION.                                      
001836                                                                          
001837     SET TAB-IX                TO 2                                       
001838     PERFORM UNTIL TAB-IX      >  FYLLD-IX OR                             
001839                  (TAB-PLAC-KDINLOMR(TAB-IX) = 'F  ' OR 'FBP') AND        
001840                   TAB-ADINLOMR-NXT(TAB-IX)  = SPACE                      
001841          SET TAB-IX UP BY 1                                              
001842     END-PERFORM                                                          
001843                                                                          
001844     IF TAB-IX                 > FYLLD-IX                                 
001845         ADD 1                 TO W-KDSORT                                
001846     ELSE                                                                 
001847         MOVE TAB-KVINLART(TAB-IX)     TO RESP-KVINLART-LINE(INDX)        
001848         MOVE TAB-KVKOLLI(TAB-IX)      TO RESP-KVKOLLI-LINE(INDX)         
001849         MOVE TAB-ADINLOMR(TAB-IX)     TO RESP-ADINLOMR-LINE(INDX)        
001850         MOVE TAB-KDINLSTA(TAB-IX)     TO RESP-KDINLSTA-LINE(INDX)        
001851         IF WS-ENGLISH-TEXT                                               
001852           EVALUATE RESP-KDINLSTA-LINE(INDX)                              
001853             WHEN 'FPK'                                                   
001854                MOVE 'PP '         TO RESP-KDINLSTA-LINE(INDX)            
001855             WHEN 'INL'                                                   
001856                MOVE 'BIN'         TO RESP-KDINLSTA-LINE(INDX)            
001857             WHEN 'SAK'                                                   
001858                MOVE 'MIS'         TO RESP-KDINLSTA-LINE(INDX)            
001859             WHEN 'AVV'                                                   
001860                MOVE 'DEV'         TO RESP-KDINLSTA-LINE(INDX)            
001861             WHEN 'ANT'                                                   
001862                MOVE 'DEV'         TO RESP-KDINLSTA-LINE(INDX)            
001863             WHEN 'KVA'                                                   
001864                MOVE 'Q-D'         TO RESP-KDINLSTA-LINE(INDX)            
001865             WHEN 'RET'                                                   
001866                MOVE 'RET'         TO RESP-KDINLSTA-LINE(INDX)            
001867             WHEN 'FRD'                                                   
001868                MOVE 'TRP'         TO RESP-KDINLSTA-LINE(INDX)            
001869             WHEN 'MAK'                                                   
001870                MOVE 'CAN'         TO RESP-KDINLSTA-LINE(INDX)            
001871           END-EVALUATE                                                   
001872         END-IF                                                           
001873         MOVE TAB-IDINLVGN(TAB-IX)     TO RESP-IDINLVGN-LINE(INDX)        
001874         MOVE TAB-ADINLOMR-NXT(TAB-IX) TO                                 
001875                                      RESP-ADINLOMR-NXT-LINE(INDX)        
001876         PERFORM S03-SUDDA-UT-TAB-RAD                                     
001877         ADD +1                        TO INDX                            
001878         ADD +1                        TO RESP-KVRADER                    
001879     END-IF                                                               
001880     .                                                                    
001881     EJECT                                                                
001882 FBDCI-ADR-FP-FBP-STAT      SECTION.                                      
001883                                                                          
001884     SET TAB-IX                TO 2                                       
001885     PERFORM UNTIL TAB-IX      >  FYLLD-IX OR                             
001886              ((TAB-ADR-KDINLOMR(TAB-IX) = 'F  ' OR 'FBP') AND            
001887                   TAB-KDINLSTA(TAB-IX) = TAB-KDINLSTA(1))                
001888          SET TAB-IX UP BY 1                                              
001889     END-PERFORM                                                          
001890                                                                          
001891     IF TAB-IX                 > FYLLD-IX                                 
001892         ADD 1                 TO W-KDSORT                                
001893     ELSE                                                                 
001894         MOVE TAB-KVINLART(TAB-IX)     TO RESP-KVINLART-LINE(INDX)        
001895         MOVE TAB-KVKOLLI(TAB-IX)      TO RESP-KVKOLLI-LINE(INDX)         
001896         MOVE TAB-ADINLOMR(TAB-IX)     TO RESP-ADINLOMR-LINE(INDX)        
001897         MOVE TAB-KDINLSTA(TAB-IX)     TO RESP-KDINLSTA-LINE(INDX)        
001898         IF WS-ENGLISH-TEXT                                               
001899           EVALUATE RESP-KDINLSTA-LINE(INDX)                              
001900             WHEN 'FPK'                                                   
001901                MOVE 'PP '         TO RESP-KDINLSTA-LINE(INDX)            
001902             WHEN 'INL'                                                   
001903                MOVE 'BIN'         TO RESP-KDINLSTA-LINE(INDX)            
001904             WHEN 'SAK'                                                   
001905                MOVE 'MIS'         TO RESP-KDINLSTA-LINE(INDX)            
001906             WHEN 'AVV'                                                   
001907                MOVE 'DEV'         TO RESP-KDINLSTA-LINE(INDX)            
001908             WHEN 'ANT'                                                   
001909                MOVE 'DEV'         TO RESP-KDINLSTA-LINE(INDX)            
001910             WHEN 'KVA'                                                   
001911                MOVE 'Q-D'         TO RESP-KDINLSTA-LINE(INDX)            
001912             WHEN 'RET'                                                   
001913                MOVE 'RET'         TO RESP-KDINLSTA-LINE(INDX)            
001914             WHEN 'FRD'                                                   
001915                MOVE 'TRP'         TO RESP-KDINLSTA-LINE(INDX)            
001916             WHEN 'MAK'                                                   
001917                MOVE 'CAN'         TO RESP-KDINLSTA-LINE(INDX)            
001918           END-EVALUATE                                                   
001919         END-IF                                                           
001920         MOVE TAB-IDINLVGN(TAB-IX)     TO RESP-IDINLVGN-LINE(INDX)        
001921         MOVE TAB-ADINLOMR-NXT(TAB-IX) TO                                 
001922                                     RESP-ADINLOMR-NXT-LINE(INDX)         
001923         PERFORM S03-SUDDA-UT-TAB-RAD                                     
001924         ADD +1                        TO INDX                            
001925         ADD +1                        TO RESP-KVRADER                    
001926     END-IF                                                               
001927     .                                                                    
001928     EJECT                                                                
001929 FBDCJ-ADR-FP-FBP           SECTION.                                      
001930                                                                          
001931     SET TAB-IX                TO 2                                       
001932     PERFORM UNTIL TAB-IX      >  FYLLD-IX OR                             
001933                TAB-ADR-KDINLOMR(TAB-IX) = 'F  ' OR 'FBP'                 
001934          SET TAB-IX UP BY 1                                              
001935     END-PERFORM                                                          
001936                                                                          
001937     IF TAB-IX                 > FYLLD-IX                                 
001938         ADD 1                 TO W-KDSORT                                
001939     ELSE                                                                 
001940         MOVE TAB-KVINLART(TAB-IX)     TO RESP-KVINLART-LINE(INDX)        
001941         MOVE TAB-KVKOLLI(TAB-IX)      TO RESP-KVKOLLI-LINE(INDX)         
001942         MOVE TAB-ADINLOMR(TAB-IX)     TO RESP-ADINLOMR-LINE(INDX)        
001943         MOVE TAB-KDINLSTA(TAB-IX)     TO RESP-KDINLSTA-LINE(INDX)        
001944         IF WS-ENGLISH-TEXT                                               
001945           EVALUATE RESP-KDINLSTA-LINE(INDX)                              
001946             WHEN 'FPK'                                                   
001947                MOVE 'PP '         TO RESP-KDINLSTA-LINE(INDX)            
001948             WHEN 'INL'                                                   
001949                MOVE 'BIN'         TO RESP-KDINLSTA-LINE(INDX)            
001950             WHEN 'SAK'                                                   
001951                MOVE 'MIS'         TO RESP-KDINLSTA-LINE(INDX)            
001952             WHEN 'AVV'                                                   
001953                MOVE 'DEV'         TO RESP-KDINLSTA-LINE(INDX)            
001954             WHEN 'ANT'                                                   
001955                MOVE 'DEV'         TO RESP-KDINLSTA-LINE(INDX)            
001956             WHEN 'KVA'                                                   
001957                MOVE 'Q-D'         TO RESP-KDINLSTA-LINE(INDX)            
001958             WHEN 'RET'                                                   
001959                MOVE 'RET'         TO RESP-KDINLSTA-LINE(INDX)            
001960             WHEN 'FRD'                                                   
001961                MOVE 'TRP'         TO RESP-KDINLSTA-LINE(INDX)            
001962             WHEN 'MAK'                                                   
001963                MOVE 'CAN'         TO RESP-KDINLSTA-LINE(INDX)            
001964           END-EVALUATE                                                   
001965         END-IF                                                           
001966         MOVE TAB-IDINLVGN(TAB-IX)     TO RESP-IDINLVGN-LINE(INDX)        
001967         MOVE TAB-ADINLOMR-NXT(TAB-IX) TO                                 
001968                                     RESP-ADINLOMR-NXT-LINE(INDX)         
001969         PERFORM S03-SUDDA-UT-TAB-RAD                                     
001970         ADD +1                        TO INDX                            
001971         ADD +1                        TO RESP-KVRADER                    
001972     END-IF                                                               
001973     .                                                                    
001974     EJECT                                                                
001975 FBDCK-LAEGG-UT-RESTEN          SECTION.                                  
001976                                                                          
001977     SET TAB-IX                         TO 2                              
001978     PERFORM UNTIL TAB-IX               >  FYLLD-IX OR                    
001979                   TAB-KVINLART(TAB-IX) >  ZERO                           
001980          SET TAB-IX UP BY 1                                              
001981     END-PERFORM                                                          
001982                                                                          
001983     IF TAB-IX                 > FYLLD-IX                                 
001984         CONTINUE                                                         
001985     ELSE                                                                 
001986         MOVE TAB-KVINLART(TAB-IX)     TO RESP-KVINLART-LINE(INDX)        
001987         MOVE TAB-KVKOLLI(TAB-IX)      TO RESP-KVKOLLI-LINE(INDX)         
001988         MOVE TAB-ADINLOMR(TAB-IX)     TO RESP-ADINLOMR-LINE(INDX)        
001989         MOVE TAB-KDINLSTA(TAB-IX)     TO RESP-KDINLSTA-LINE(INDX)        
001990         IF WS-ENGLISH-TEXT                                               
001991           EVALUATE RESP-KDINLSTA-LINE(INDX)                              
001992             WHEN 'FPK'                                                   
001993                MOVE 'PP '         TO RESP-KDINLSTA-LINE(INDX)            
001994             WHEN 'INL'                                                   
001995                MOVE 'BIN'         TO RESP-KDINLSTA-LINE(INDX)            
001996             WHEN 'SAK'                                                   
001997                MOVE 'MIS'         TO RESP-KDINLSTA-LINE(INDX)            
001998             WHEN 'AVV'                                                   
001999                MOVE 'DEV'         TO RESP-KDINLSTA-LINE(INDX)            
002000             WHEN 'ANT'                                                   
002001                MOVE 'DEV'         TO RESP-KDINLSTA-LINE(INDX)            
002002             WHEN 'KVA'                                                   
002003                MOVE 'Q-D'         TO RESP-KDINLSTA-LINE(INDX)            
002004             WHEN 'RET'                                                   
002005                MOVE 'RET'         TO RESP-KDINLSTA-LINE(INDX)            
002006             WHEN 'FRD'                                                   
002007                MOVE 'TRP'         TO RESP-KDINLSTA-LINE(INDX)            
002008             WHEN 'MAK'                                                   
002009                MOVE 'CAN'         TO RESP-KDINLSTA-LINE(INDX)            
002010           END-EVALUATE                                                   
002011         END-IF                                                           
002012         MOVE TAB-IDINLVGN(TAB-IX)     TO RESP-IDINLVGN-LINE(INDX)        
002013         MOVE TAB-ADINLOMR-NXT(TAB-IX) TO                                 
002014                                     RESP-ADINLOMR-NXT-LINE(INDX)         
002015         PERFORM S03-SUDDA-UT-TAB-RAD                                     
002016         ADD +1                        TO INDX                            
002017         ADD +1                        TO RESP-KVRADER                    
002018     END-IF                                                               
002019     .                                                                    
002020     EJECT                                                                
002021 G-KOLLA-INPUT SECTION.                                                   
002022                                                                          
002023     MOVE SPACE                     TO RESP-IDMSG-ERROR                   
002024                                       PRT-IDPRTLST                       
002025     IF REQU-INPUT                   =  ALL '+'                           
002026         MOVE ERR-PF11-AND-NO-DATA  TO RESP-IDMSG-ERROR                   
002027         PERFORM MFS-ROER-EJ-FAELT-IN                                     
002028         PERFORM MFS-ROER-EJ-FAELT-UT                                     
002029         MOVE NEJ                   TO INDATA-SW                          
002030     ELSE                                                                 
002031         MOVE MFS-ALFA-FAELT-RAETT  TO RESP-KDCMDVAL-INM-ATTR             
002032                                       RESP-ADINLOMR-INM-ATTR             
002033                                       RESP-ADINLOMR-NXT-INM-ATTR         
002034                                       RESP-KDKLIPRI-INM-ATTR             
002035                                       RESP-FLSATS-INM-ATTR               
002036                                       RESP-ADINLOMR-PRT-ATTR             
002037                                       RESP-KDPRTVAL-ATTR                 
002038         MOVE MFS-NUM-FAELT-RAETT   TO RESP-KVINLART-INM-ATTR             
002039                                                                          
002040         PERFORM GA-KOLLA-KDCMDVAL                                        
002041         PERFORM GB-LAES-KOLLISEG                                         
002042         IF SEGMENT-FINNS                                                 
002043           IF INDATA-FEL                                                  
002044             CONTINUE                                                     
002045           ELSE                                                           
002046             EVALUATE TRUE                                                
002047               WHEN REQU-KDCMDVAL = 'AKU' OR 'A  ' OR 'QTY'               
002048                  PERFORM GC-KOLLA-ANTALSKONTROLL                         
002049                                                                          
002050              WHEN REQU-KDCMDVAL = 'MOT' OR 'REC' OR 'KR ' OR 'IR'        
002051                  PERFORM GD-KOLLA-ANTALSAVVIKELSE                        
002052                                                                          
002053               WHEN REQU-KDCMDVAL = 'SEK'                                 
002054                  PERFORM GE-KOLLA-SEKUNDAERUTPLOCK                       
002055                                                                          
002056               WHEN REQU-KDCMDVAL = 'FL ' OR 'LA '                        
002057                  PERFORM GF-KOLLA-FLAGGA                                 
002058                                                                          
002059               WHEN REQU-KDCMDVAL = 'ET ' OR 'ML '                        
002060                  PERFORM GG-KOLLA-ETIKETT                                
002061                                                                          
002062               WHEN REQU-KDCMDVAL = 'KVA' OR 'QD'                         
002063                  PERFORM GH-KOLLA-KVALITETSAVVIKELSE                     
002064                                                                          
002065               WHEN REQU-KDCMDVAL = 'RET'                                 
002066                  PERFORM GL-KOLLA-RETUR                                  
002067                                                                          
002068               WHEN REQU-KDCMDVAL = 'AKP' OR 'QB'                         
002069                  PERFORM GM-KOLLA-ANTALSKONTROLL-PARTI                   
002070                                                                          
002071             END-EVALUATE                                                 
002072                                                                          
002073             IF REQU-KDKLIPRI  = ALL '+' OR SPACE                         
002074                 CONTINUE                                                 
002075             ELSE                                                         
002076                 PERFORM GI-KOLLA-PRIOMAERKNING                           
002077             END-IF                                                       
002078                                                                          
002079             IF REQU-FLSATS    = ALL '+' OR SPACE                         
002080                 CONTINUE                                                 
002081             ELSE                                                         
002082                 PERFORM GJ-KOLLA-OFOERAEDLAT                             
002083             END-IF                                                       
002084                                                                          
002085             IF REQU-ADINLOMR  = ALL '+' AND                              
002086                REQU-ADINLOMR-NXT = ALL '+' AND                           
002087                REQU-IDINLVGN  = ALL '+'                                  
002088                 CONTINUE                                                 
002089             ELSE                                                         
002090                 PERFORM GK-KOLLA-KOLLIFLYTT                              
002091             END-IF                                                       
002092           END-IF                                                         
002093         END-IF                                                           
002094                                                                          
002095         IF INDATA-FEL                                                    
002096             IF RESP-IDMSG-ERROR       =  SPACE                           
002097                 MOVE ERR-CORR-HILITE-FLDS TO RESP-IDMSG-ERROR            
002098             END-IF                                                       
002099             PERFORM MFS-ROER-EJ-FAELT-UT                                 
002100             PERFORM MFS-ROER-EJ-FAELT-IN                                 
002101         END-IF                                                           
002102     END-IF                                                               
002103     .                                                                    
002104     EJECT                                                                
002105 GA-KOLLA-KDCMDVAL  SECTION.                                              
002106                                                                          
002107     IF REQU-KDCMDVAL          = ALL '+' OR SPACE OR 'AKU' OR             
002108                                 'A  ' OR 'AKP' OR 'KR ' OR 'IR '         
002109                                 OR 'MOT' OR 'FL ' OR 'ET ' OR            
002110                                 'KVA' OR 'SEK' OR 'RET' OR               
002111                                 'QTY' OR 'QB ' OR 'REC' OR               
002112                                 'LA ' OR 'ML ' OR 'QD '                  
002113         MOVE MFS-ALFA-FAELT-RAETT TO RESP-KDCMDVAL-INM-ATTR              
002114     ELSE                                                                 
002115         MOVE MFS-ALFA-FAELT-FEL   TO RESP-KDCMDVAL-INM-ATTR              
002116         MOVE NEJ                  TO INDATA-SW                           
002117     END-IF                                                               
002118                                                                          
002119     IF INDATA-OK                                                         
002120       IF REQU-KDCMDVAL = 'REC'                                           
002121         IF REQU-IDUSER-IN = ALL '+'                                      
002122           MOVE MFS-ALFA-FAELT-FEL TO RESP-IDUSER-IN-ATTR                 
002123           MOVE NEJ TO INDATA-SW                                          
002124         END-IF                                                           
002125       END-IF                                                             
002126     END-IF                                                               
002127     .                                                                    
002128     EJECT                                                                
002129 GB-LAES-KOLLISEG          SECTION.                                       
002130                                                                          
002131     EVALUATE TRUE                                                        
002132                                                                          
002133       WHEN IDLEVNWKOLLI-NYCKEL                                           
002134         MOVE W-IDLEVNR-KOLLI  TO W-IDLEVNRK                              
002135                                  W-IDLEVNR                               
002136         MOVE W-IDOKOLLI       TO W-IDOKOLLIK                             
002137                                  W-IDOKOLLINR                            
002138                                                                          
002139           MOVE REQU-IDLOPNRM-START TO W-NUM-IDLOPNRM                     
002140                                      WS-IDLOPNRM-NUM                     
002141           PERFORM IMS-GU-W6INLA11-C                                      
002142           PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT OR                
002143                         ART-IDLOPNRM = W-NUM-IDLOPNRM                    
002144               PERFORM IMS-GN-W6INLA11-C                                  
002145           END-PERFORM                                                    
002146           IF SEGMENT-FINNS                                               
002147             PERFORM IMS-GNP-W6INLA21                                     
002148           END-IF                                                         
002149                                                                          
002150       WHEN IDLOPNRM-NYCKEL                                               
002151         MOVE WS-IDLOPNRM-NUM  TO W-IDLOPNR                               
002152         MOVE 1                TO W-IDRADNR                               
002153         PERFORM IMS-GU-W6INLA21-B                                        
002154                                                                          
002155     END-EVALUATE                                                         
002156                                                                          
002157     IF SEGMENT-SAKNAS                                                    
002158         MOVE NEJ                 TO INDATA-SW                            
002159         MOVE ERR-UPD-NOT-ALLOWED TO RESP-IDMSG-ERROR                     
002160     ELSE                                                                 
002161         IF RAD-IDINLVGN > 0                                              
002162           IF REQU-KDCMDVAL = ALL '+' OR SPACE                            
002163             CONTINUE                                                     
002164           ELSE                                                           
002165             MOVE ERR-UPD-NOT-ALLOWED TO RESP-IDMSG-ERROR                 
002166             MOVE NEJ                 TO INDATA-SW                        
002167             MOVE MFS-ALFA-FAELT-FEL  TO RESP-KDCMDVAL-INM-ATTR           
002168           END-IF                                                         
002169           IF REQU-KVINLART = ALL '+'                                     
002170             CONTINUE                                                     
002171           ELSE                                                           
002172             MOVE ERR-UPD-NOT-ALLOWED TO RESP-IDMSG-ERROR                 
002173             MOVE NEJ                 TO INDATA-SW                        
002174             MOVE MFS-NUM-FAELT-FEL   TO RESP-KVINLART-INM-ATTR           
002175           END-IF                                                         
002176           IF REQU-KDKLIPRI = ALL '+' OR SPACE                            
002177             CONTINUE                                                     
002178           ELSE                                                           
002179             MOVE ERR-UPD-NOT-ALLOWED TO RESP-IDMSG-ERROR                 
002180             MOVE NEJ                 TO INDATA-SW                        
002181             MOVE MFS-ALFA-FAELT-FEL  TO RESP-KDKLIPRI-INM-ATTR           
002182           END-IF                                                         
002183           IF REQU-FLSATS = ALL '+' OR SPACE                              
002184             CONTINUE                                                     
002185           ELSE                                                           
002186             MOVE ERR-UPD-NOT-ALLOWED TO RESP-IDMSG-ERROR                 
002187             MOVE NEJ                 TO INDATA-SW                        
002188             MOVE MFS-ALFA-FAELT-FEL  TO RESP-FLSATS-INM-ATTR             
002189           END-IF                                                         
002190         END-IF                                                           
002191     END-IF                                                               
002192     .                                                                    
002193     EJECT                                                                
002194 GC-KOLLA-ANTALSKONTROLL   SECTION.                                       
002195                                                                          
002196     IF RAD-FLKVAANT              =  JA                                   
002197         MOVE ERR-UPD-NOT-ALLOWED TO RESP-IDMSG-ERROR                     
002198         MOVE NEJ                 TO INDATA-SW                            
002199         MOVE MFS-ALFA-FAELT-FEL  TO RESP-KDCMDVAL-INM-ATTR               
002200     END-IF                                                               
002201                                                                          
002202     PERFORM S04-KOLLA-KDINLSTA                                           
002203                                                                          
002204     IF REQU-KVINLART NOT = ALL '+'                                       
002205       MOVE ERR-CORR-HILITE-FLDS TO RESP-IDMSG-ERROR                      
002206       MOVE NEJ               TO INDATA-SW                                
002207       MOVE MFS-NUM-FAELT-FEL TO RESP-KVINLART-INM-ATTR                   
002208     END-IF                                                               
002209     .                                                                    
002210     EJECT                                                                
002211 GD-KOLLA-ANTALSAVVIKELSE SECTION.                                        
002212                                                                          
002213     IF IDLEVNWKOLLI-NYCKEL                                               
002214         PERFORM IMS-GU-W6INLA11-C                                        
002215     ELSE                                                                 
002216         PERFORM IMS-GU-W6INLA11-B                                        
002217     END-IF                                                               
002218                                                                          
002219     IF ART-KDRT                  =  3 OR 77                              
002220         MOVE ERR-NO-DEVN-3-77    TO RESP-IDMSG-ERROR                     
002221         MOVE NEJ                 TO INDATA-SW                            
002222         MOVE MFS-ALFA-FAELT-FEL  TO RESP-KDCMDVAL-INM-ATTR               
002223     END-IF                                                               
002224                                                                          
002225     IF ART-FLSPLPART = JA AND REQU-UPDATE                                
002226         MOVE SPLITT-PRESS-PF23   TO RESP-IDMSG-ERROR                     
002227         MOVE NEJ                 TO INDATA-SW                            
002228         MOVE MFS-ALFA-FAELT-FEL  TO RESP-KDCMDVAL-INM-ATTR               
002229     END-IF                                                               
002230                                                                          
002231     IF REQU-KDCMDVAL NOT = 'KR ' AND 'IR '                               
002232       PERFORM S04-KOLLA-KDINLSTA                                         
002233       IF RAD-FLDIVKLI           =  JA                                    
002234           MOVE '6E'             TO PRT-IDPRTLST(1:2)                     
002235       ELSE                                                               
002236           MOVE '6F'             TO PRT-IDPRTLST(1:2)                     
002237       END-IF                                                             
002238       IF RAD-IDRADNR = 1 AND RAD-IDOKOLLI = ZERO                         
002239         CONTINUE                                                         
002240       ELSE                                                               
002241         IF REQU-IDMSGVER = 1                                             
002242           CONTINUE                                                       
002243         ELSE                                                             
002244           PERFORM S05-KOLLA-ADINLOMR-PRT                                 
002245         END-IF                                                           
002246       END-IF                                                             
002247       PERFORM S06-KOLLA-KVINLART                                         
002248     END-IF                                                               
002249     IF INDATA-OK                                                         
002250       PERFORM GDA-KOLLA-KONTROLLRAPPORT                                  
002251     END-IF                                                               
002252     .                                                                    
002253     EJECT                                                                
002254 GDA-KOLLA-KONTROLLRAPPORT  SECTION.                                      
002255                                                                          
002256     MOVE ART-IDLOPNRM TO W-IDLOPNRM-BSEQ                                 
002257                                                                          
002258     PERFORM IMS-GU-INLC-W6D1B                                            
002259                                                                          
002260     MOVE SEQB-IDLOPNRM TO W-IDLOPNRM-H7                                  
002261     MOVE SEQB-TIAVIDAT TO W-DAAVSDAT-H7                                  
002262     IF SEQB-TIAVIDAT NOT = ZERO                                          
002263       IF SEQB-TIAVIDAT < 500000                                          
002264         MOVE 20        TO W-DAAVSDAT-H7 (1:2)                            
002265       ELSE                                                               
002266         IF SEQB-TIAVIDAT < 999999                                        
002267           MOVE 19      TO W-DAAVSDAT-H7 (1:2)                            
002268         ELSE                                                             
002269           MOVE 99999999 TO W-DAAVSDAT-H7                                 
002270         END-IF                                                           
002271       END-IF                                                             
002272     END-IF                                                               
002273     PERFORM IMS-GHU-KVAE-W6H701                                          
002274     IF SEGMENT-FINNS                                                     
002275       IF (REQU-KDCMDVAL = 'KR ' OR 'IR ') OR                             
002276          (KVAE-KR-KDKRUTF = '4')                                         
002277         MOVE ERR-IR-ALREADY-REG TO RESP-IDMSG-ERROR                      
002278         MOVE NEJ                TO INDATA-SW                             
002279         MOVE MFS-ALFA-FAELT-FEL TO RESP-KDCMDVAL-INM-ATTR                
002280       ELSE                                                               
002281         IF KVAE-KR-IDKRFEL          = 'PA' OR 'PB' OR 'K '               
002282           IF (KVAE-KR-KVANTMOT + W-KVINLART-UPD - RAD-KVINLART) =        
002283               KVAE-KR-KVAVIS                                             
002284             IF (KVAE-KR-KDKRSTA = '0' OR '1')                            
002285                IF KVAE-KR-FLANNULL = NEJ                                 
002286** OM ANTALSAVVIKELSE TAS TILLBAKA FÅR KR ENDAST ANNULERAS OM             
002287** INTE ADMINISTRATIVT FEL ÄR ANGIVET PÅ BILD 6139                        
002288                  PERFORM IMS-GU-UPFA01                                   
002289                  IF SEGMENT-FINNS                                        
002290                    IF UPPF-KDKVASTA-ADM = '3'                            
002291                      MOVE 'K ' TO KVAE-KR-IDKRFEL                        
002292                      MOVE ZERO TO KVAE-KR-KVANTMOT                       
002293                                   KVAE-KR-KVART-AAVV                     
002294                                   KVAE-KR-KVART-EJ-GODK                  
002295                                   KVAE-KR-KVART-KJUST                    
002296                                   KVAE-KR-KVART-KONTR                    
002297                                   KVAE-KR-KVART-RET                      
002298                                   KVAE-KR-KVART-SJUST                    
002299                                   KVAE-KR-KVART-SKROT                    
002300                                   KVAE-KR-KDDISP                         
002301                                   KVAE-KR-KDHANDCO                       
002302                      MOVE JA   TO WS-FLADM-SKAPAD                        
002303                    ELSE                                                  
002304                      MOVE JA TO KVAE-KR-FLANNULL                         
002305                      MOVE MSG-SIGNON-USERID TO USER-X                    
002306                      MOVE USER TO KVAE-KR-BEKRANS                        
002307                      MOVE SPACE TO KVAE-KR-IDKRATLF                      
002308                    END-IF                                                
002309                  ELSE                                                    
002310                    MOVE JA TO KVAE-KR-FLANNULL                           
002311                    MOVE MSG-SIGNON-USERID TO USER-X                      
002312                    MOVE USER TO KVAE-KR-BEKRANS                          
002313                    MOVE SPACE TO KVAE-KR-IDKRATLF                        
002314                  END-IF                                                  
002315                  PERFORM IMS-REPL-KVAE-W6H7                              
002316                END-IF                                                    
002317             ELSE                                                         
002318               MOVE ERR-IR-MANUAL-CHG  TO RESP-IDMSG-ERROR                
002319             END-IF                                                       
002320           ELSE                                                           
002321             IF (KVAE-KR-KDKRSTA = '0' OR '1') OR                         
002322                (KVAE-KR-KDKRSTA > '1'        AND                         
002323                 KVAE-KR-FLKRGODK = NEJ )                                 
002324                IF KVAE-KR-FLANNULL = JA                                  
002325                  MOVE NEJ TO KVAE-KR-FLANNULL                            
002326                  PERFORM IMS-REPL-KVAE-W6H7                              
002327                END-IF                                                    
002328             ELSE                                                         
002329               MOVE ERR-IR-ALREADY-REG TO RESP-IDMSG-ERROR                
002330               MOVE NEJ                TO INDATA-SW                       
002331               MOVE MFS-ALFA-FAELT-FEL TO RESP-KDCMDVAL-INM-ATTR          
002332             END-IF                                                       
002333           END-IF                                                         
002334         ELSE                                                             
002335             PERFORM IMS-GHN-KVAE-W6H701                                  
002336             IF SEGMENT-FINNS                                             
002337               IF KVAE-KR-IDKRFEL          = 'PA' OR 'PB' OR 'K '         
002338                 IF (KVAE-KR-KVANTMOT + W-KVINLART-UPD -                  
002339                                        RAD-KVINLART) =                   
002340                     KVAE-KR-KVAVIS                                       
002341                   IF (KVAE-KR-KDKRSTA = '0' OR '1')                      
002342                      IF KVAE-KR-FLANNULL = NEJ                           
002343                        MOVE JA TO KVAE-KR-FLANNULL                       
002344                        MOVE MSG-SIGNON-USERID TO USER-X                  
002345                        MOVE USER TO KVAE-KR-BEKRANS                      
002346                        MOVE SPACE TO KVAE-KR-IDKRATLF                    
002347                        PERFORM IMS-REPL-KVAE-W6H7                        
002348                      END-IF                                              
002349                   ELSE                                                   
002350                     MOVE ERR-IR-MANUAL-CHG  TO RESP-IDMSG-ERROR          
002351                   END-IF                                                 
002352                 ELSE                                                     
002353                   IF (KVAE-KR-KDKRSTA = '0' OR '1') OR                   
002354                      (KVAE-KR-KDKRSTA > '1'        AND                   
002355                       KVAE-KR-FLKRGODK = NEJ )                           
002356                     IF KVAE-KR-FLANNULL = JA                             
002357                       MOVE NEJ TO KVAE-KR-FLANNULL                       
002358                       PERFORM IMS-REPL-KVAE-W6H7                         
002359                     END-IF                                               
002360                   ELSE                                                   
002361                     MOVE ERR-IR-ALREADY-REG TO RESP-IDMSG-ERROR          
002362                     MOVE NEJ                TO INDATA-SW                 
002363                     MOVE MFS-ALFA-FAELT-FEL TO                           
002364                                          RESP-KDCMDVAL-INM-ATTR          
002365                   END-IF                                                 
002366                 END-IF                                                   
002367               END-IF                                                     
002368           END-IF                                                         
002369         END-IF                                                           
002370       END-IF                                                             
002371     END-IF                                                               
002372     .                                                                    
002373     EJECT                                                                
002374 GE-KOLLA-SEKUNDAERUTPLOCK SECTION.                                       
002375                                                                          
002376     PERFORM S04-KOLLA-KDINLSTA                                           
002377     MOVE '6E'                 TO PRT-IDPRTLST(1:2)                       
002378     PERFORM S05-KOLLA-ADINLOMR-PRT                                       
002379     IF RAD-FLDIVKLI = NEJ AND RAD-IDRADNR > +1                           
002380       MOVE '6F'                 TO PRT-IDPRTLST(1:2)                     
002381       PERFORM S05-KOLLA-ADINLOMR-PRT                                     
002382     END-IF                                                               
002383     PERFORM S06-KOLLA-KVINLART                                           
002384     .                                                                    
002385     EJECT                                                                
002386 GF-KOLLA-FLAGGA SECTION.                                                 
002387                                                                          
002388     PERFORM S04-KOLLA-KDINLSTA                                           
002389     MOVE '6F'                 TO PRT-IDPRTLST(1:2)                       
002390     PERFORM S05-KOLLA-ADINLOMR-PRT                                       
002391                                                                          
002392     IF RAD-IDRADNR               =  1 OR                                 
002393        RAD-IDOKOLLI              =  ZERO                                 
002394         MOVE ERR-USE-6122-SCR    TO RESP-IDMSG-ERROR                     
002395         MOVE NEJ                 TO INDATA-SW                            
002396         MOVE MFS-ALFA-FAELT-FEL  TO RESP-KDCMDVAL-INM-ATTR               
002397     END-IF                                                               
002398                                                                          
002399     IF REQU-KVINLART NOT = ALL '+'                                       
002400       MOVE ERR-CORR-HILITE-FLDS TO RESP-IDMSG-ERROR                      
002401       MOVE MFS-NUM-FAELT-FEL TO RESP-KVINLART-INM-ATTR                   
002402     END-IF                                                               
002403                                                                          
002404     PERFORM S10-KOLLA-KDPRTVAL                                           
002405     .                                                                    
002406     EJECT                                                                
002407 GG-KOLLA-ETIKETT            SECTION.                                     
002408                                                                          
002409     PERFORM S04-KOLLA-KDINLSTA                                           
002410     MOVE '6E'                 TO PRT-IDPRTLST(1:2)                       
002411     PERFORM S05-KOLLA-ADINLOMR-PRT                                       
002412                                                                          
002413     IF RAD-FLSATS                =  NEJ                                  
002414         CONTINUE                                                         
002415     ELSE                                                                 
002416         MOVE ERR-UPD-NOT-ALLOWED TO RESP-IDMSG-ERROR                     
002417         MOVE NEJ                 TO INDATA-SW                            
002418     END-IF                                                               
002419                                                                          
002420     IF REQU-KVINLART NOT = ALL '+'                                       
002421       MOVE ERR-CORR-HILITE-FLDS TO RESP-IDMSG-ERROR                      
002422       MOVE NEJ               TO INDATA-SW                                
002423       MOVE MFS-NUM-FAELT-FEL TO RESP-KVINLART-INM-ATTR                   
002424     END-IF                                                               
002425                                                                          
002426     PERFORM S10-KOLLA-KDPRTVAL                                           
002427     .                                                                    
002428     EJECT                                                                
002429 GH-KOLLA-KVALITETSAVVIKELSE   SECTION.                                   
002430                                                                          
002431     PERFORM S04-KOLLA-KDINLSTA                                           
002432     PERFORM S06-KOLLA-KVINLART                                           
002433     IF INDATA-OK                                                         
002434       PERFORM S22A-KOLLA-KVA                                             
002435       IF INDATA-OK                                                       
002436         IF WS-KVINLART-TOT-KVARET = WS-KVINLART  OR                      
002437            RAD-IDRADNR = +1                                              
002438           CONTINUE                                                       
002439         ELSE                                                             
002440           IF RAD-FLDIVKLI = NEJ                                          
002441             MOVE '6F'                 TO PRT-IDPRTLST(1:2)               
002442           ELSE                                                           
002443             MOVE '6E'                 TO PRT-IDPRTLST(1:2)               
002444           END-IF                                                         
002445           PERFORM S05-KOLLA-ADINLOMR-PRT                                 
002446         END-IF                                                           
002447       END-IF                                                             
002448     END-IF                                                               
002449     IF IDLEVNWKOLLI-NYCKEL                                               
002450         PERFORM IMS-GU-W6INLA11-C                                        
002451     ELSE                                                                 
002452         PERFORM IMS-GU-W6INLA11-B                                        
002453     END-IF                                                               
002454                                                                          
002455     IF ART-KDRT                  =  3 OR 77 OR 8                         
002456         MOVE ERR-NO-DEVN-3-77    TO RESP-IDMSG-ERROR                     
002457         MOVE NEJ                 TO INDATA-SW                            
002458         MOVE MFS-ALFA-FAELT-FEL  TO RESP-KDCMDVAL-INM-ATTR               
002459     END-IF                                                               
002460     .                                                                    
002461     EJECT                                                                
002462 GI-KOLLA-PRIOMAERKNING        SECTION.                                   
002463                                                                          
002464     IF REQU-KDCMDVAL             =  ALL '+' OR SPACE OR                  
002465                                     'AKU' OR 'A  ' OR 'FL ' OR           
002466                                     'QTY' OR 'FL '                       
002467         CONTINUE                                                         
002468     ELSE                                                                 
002469         MOVE ERR-CORR-HILITE-FLDS TO RESP-IDMSG-ERROR                    
002470         MOVE NEJ                 TO INDATA-SW                            
002471         MOVE MFS-ALFA-FAELT-FEL  TO RESP-KDCMDVAL-INM-ATTR               
002472                                     RESP-KDKLIPRI-INM-ATTR               
002473     END-IF                                                               
002474                                                                          
002475     IF REQU-KDKLIPRI             =  'J' OR 'N' OR 'P' OR 'Y'             
002476         CONTINUE                                                         
002477     ELSE                                                                 
002478         MOVE ERR-CORR-HILITE-FLDS TO RESP-IDMSG-ERROR                    
002479         MOVE NEJ                 TO INDATA-SW                            
002480         MOVE MFS-ALFA-FAELT-FEL  TO RESP-KDKLIPRI-INM-ATTR               
002481     END-IF                                                               
002482                                                                          
002483     IF REQU-KDKLIPRI            = 'J' OR 'P' OR 'Y'                      
002484        IF (RAD-FLSATS           = JA   AND                               
002485            REQU-FLSATS      NOT = NEJ) OR                                
002486           (REQU-FLSATS          = JA OR YES)                             
002487           MOVE ERR-KIT-PRIO-NOT-ALLOWED TO RESP-IDMSG-ERROR              
002488           MOVE NEJ                 TO INDATA-SW                          
002489           MOVE MFS-ALFA-FAELT-FEL  TO RESP-KDKLIPRI-INM-ATTR             
002490        END-IF                                                            
002491     END-IF                                                               
002492                                                                          
002493     IF RAD-IDRADNR = +1 AND RAD-FLDIVKLI = NEJ                           
002494           MOVE ERR-UPD-NOT-ALLOWED TO RESP-IDMSG-ERROR                   
002495           MOVE NEJ                 TO INDATA-SW                          
002496           MOVE MFS-ALFA-FAELT-FEL  TO RESP-KDKLIPRI-INM-ATTR             
002497     END-IF                                                               
002498                                                                          
002499     PERFORM S04-KOLLA-KDINLSTA                                           
002500     .                                                                    
002501     EJECT                                                                
002502 GJ-KOLLA-OFOERAEDLAT          SECTION.                                   
002503                                                                          
002504     IF REQU-KDCMDVAL             =  ALL '+' OR SPACE OR                  
002505                                     'AKU' OR 'A  ' OR 'FL ' OR           
002506                                     'QTY' OR 'LA '                       
002507         CONTINUE                                                         
002508     ELSE                                                                 
002509         MOVE ERR-CORR-HILITE-FLDS TO RESP-IDMSG-ERROR                    
002510         MOVE NEJ                 TO INDATA-SW                            
002511         MOVE MFS-ALFA-FAELT-FEL  TO RESP-KDCMDVAL-INM-ATTR               
002512                                     RESP-FLSATS-INM-ATTR                 
002513     END-IF                                                               
002514                                                                          
002515     IF REQU-FLSATS               =  JA OR YES OR NEJ                     
002516         CONTINUE                                                         
002517     ELSE                                                                 
002518         MOVE ERR-CORR-HILITE-FLDS TO RESP-IDMSG-ERROR                    
002519         MOVE NEJ                 TO INDATA-SW                            
002520         MOVE MFS-ALFA-FAELT-FEL  TO RESP-FLSATS-INM-ATTR                 
002521     END-IF                                                               
002522                                                                          
002523     IF REQU-FLSATS               =  JA OR YES                            
002524         IF (RAD-FLPRIO           =  JA    AND                            
002525            (REQU-KDKLIPRI    NOT =  NEJ)) OR                             
002526            (REQU-KDKLIPRI        =  JA OR YES)                           
002527             MOVE ERR-KIT-PRIO-NOT-ALLOWED TO RESP-IDMSG-ERROR            
002528             MOVE NEJ             TO INDATA-SW                            
002529             MOVE MFS-ALFA-FAELT-FEL TO RESP-FLSATS-INM-ATTR              
002530         END-IF                                                           
002531                                                                          
002532         IF RAD-IDRADNR           =  1                                    
002533             MOVE ERR-LOT-NOT-ON-CASE-LVL  TO RESP-IDMSG-ERROR            
002534             MOVE NEJ                      TO INDATA-SW                   
002535             MOVE MFS-ALFA-FAELT-FEL       TO RESP-FLSATS-INM-ATTR        
002536         END-IF                                                           
002537     END-IF                                                               
002538                                                                          
002539     PERFORM S04-KOLLA-KDINLSTA                                           
002540     .                                                                    
002541     EJECT                                                                
002542 GK-KOLLA-KOLLIFLYTT           SECTION.                                   
002543                                                                          
002544     PERFORM S04-KOLLA-KDINLSTA                                           
002545                                                                          
002546     IF REQU-ADINLOMR      NOT = ALL '+'                                  
002547         PERFORM GKA-KOLLA-PLACAENDRING                                   
002548         PERFORM S50-PRIM-CONTROL                                         
002549     END-IF                                                               
002550                                                                          
002551     IF REQU-ADINLOMR-NXT  NOT = ALL '+'                                  
002552         PERFORM GKB-KOLLA-ADRAENDRING                                    
002553     END-IF                                                               
002554                                                                          
002555     IF REQU-IDINLVGN        NOT = ALL '+'                                
002556         PERFORM GKC-KOLLA-VAGNAENDRING                                   
002557     END-IF                                                               
002558     .                                                                    
002559     EJECT                                                                
002560 GKA-KOLLA-PLACAENDRING  SECTION.                                         
002561                                                                          
002562     IF REQU-KDCMDVAL             =  ALL '+' OR SPACE OR                  
002563                                     'AKU' OR 'A  ' OR 'FL ' OR           
002564                                     'QTY' OR 'LA '                       
002565         CONTINUE                                                         
002566     ELSE                                                                 
002567         MOVE ERR-CORR-HILITE-FLDS TO RESP-IDMSG-ERROR                    
002568         MOVE NEJ                 TO INDATA-SW                            
002569         MOVE MFS-ALFA-FAELT-FEL  TO RESP-KDCMDVAL-INM-ATTR               
002570                                     RESP-ADINLOMR-INM-ATTR               
002571     END-IF                                                               
002572                                                                          
002573     IF RAD-FLDIVKLI              =  JA                                   
002574         MOVE ERR-MISC-CASE       TO RESP-IDMSG-ERROR                     
002575         MOVE NEJ                 TO INDATA-SW                            
002576         MOVE MFS-ALFA-FAELT-FEL  TO RESP-ADINLOMR-INM-ATTR               
002577     END-IF                                                               
002578                                                                          
002579     MOVE REQU-ADINLOMR           TO W-6006-ADINLOMR                      
002580     PERFORM IMS-GU-PLAA-PLAA11                                           
002581     IF SEGMENT-SAKNAS                                                    
002582         MOVE 'ADINLOMR'          TO RESP-IDELMT-ERROR                    
002583         MOVE ERR-MISS-REGISTER   TO RESP-IDMSG-ERROR                     
002584         MOVE NEJ                 TO INDATA-SW                            
002585         MOVE MFS-ALFA-FAELT-FEL  TO RESP-ADINLOMR-INM-ATTR               
002586     ELSE                                                                 
002587         IF PLAA-6006-KDINLOMR = 'F' OR 'FBP'                             
002588           MOVE JA TO FP-SW                                               
002589         END-IF                                                           
002590     END-IF                                                               
002591                                                                          
002592     IF REQU-ADINLOMR-NXT         =  ALL '+'                              
002593         CONTINUE                                                         
002594     ELSE                                                                 
002595         IF REQU-ADINLOMR         =  REQU-ADINLOMR-NXT                    
002596             MOVE ERR-UPD-NOT-ALLOWED TO RESP-IDMSG-ERROR                 
002597             MOVE NEJ                 TO INDATA-SW                        
002598             MOVE MFS-ALFA-FAELT-FEL  TO RESP-ADINLOMR-INM-ATTR           
002599         END-IF                                                           
002600     END-IF                                                               
002601     .                                                                    
002602     EJECT                                                                
002603 GKB-KOLLA-ADRAENDRING  SECTION.                                          
002604                                                                          
002605     IF REQU-KDCMDVAL             =  ALL '+' OR SPACE OR                  
002606                                     'AKU' OR 'A  ' OR 'FL ' OR           
002607                                     'QTY' OR 'LA '                       
002608         CONTINUE                                                         
002609     ELSE                                                                 
002610         MOVE ERR-CORR-HILITE-FLDS TO RESP-IDMSG-ERROR                    
002611         MOVE NEJ                 TO INDATA-SW                            
002612         MOVE MFS-ALFA-FAELT-FEL  TO RESP-KDCMDVAL-INM-ATTR               
002613                                     RESP-ADINLOMR-NXT-INM-ATTR           
002614     END-IF                                                               
002615                                                                          
002616     IF RAD-FLDIVKLI              =  JA                                   
002617         MOVE ERR-MISC-CASE       TO RESP-IDMSG-ERROR                     
002618         MOVE NEJ                 TO INDATA-SW                            
002619         MOVE MFS-ALFA-FAELT-FEL  TO RESP-ADINLOMR-NXT-INM-ATTR           
002620     END-IF                                                               
002621                                                                          
002622     MOVE REQU-ADINLOMR-NXT       TO W-6006-ADINLOMR                      
002623     PERFORM IMS-GU-PLAA-PLAA11                                           
002624     IF SEGMENT-SAKNAS                                                    
002625         MOVE ERR-MISS-REGISTER   TO RESP-IDMSG-ERROR                     
002626         MOVE NEJ                 TO INDATA-SW                            
002627         MOVE MFS-ALFA-FAELT-FEL  TO RESP-ADINLOMR-NXT-INM-ATTR           
002628     ELSE                                                                 
002629         IF PLAA-6006-KDINLOMR = 'F' OR 'FBP'                             
002630           MOVE JA TO FP-SW                                               
002631         END-IF                                                           
002632     END-IF                                                               
002633                                                                          
002634     IF RAD-ADINLOMR              =  SPACE                                
002635         MOVE ERR-CORR-HILITE-FLDS TO RESP-IDMSG-ERROR                    
002636         MOVE NEJ                 TO INDATA-SW                            
002637         MOVE MFS-ALFA-FAELT-FEL  TO RESP-ADINLOMR-NXT-INM-ATTR           
002638     END-IF                                                               
002639                                                                          
002640     IF REQU-ADINLOMR             =  ALL '+'                              
002641         IF REQU-ADINLOMR-NXT     =  RAD-ADINLOMR                         
002642             MOVE ERR-UPD-NOT-ALLOWED TO RESP-IDMSG-ERROR                 
002643             MOVE NEJ                 TO INDATA-SW                        
002644             MOVE MFS-ALFA-FAELT-FEL TO RESP-ADINLOMR-NXT-INM-ATTR        
002645         END-IF                                                           
002646     ELSE                                                                 
002647         IF REQU-ADINLOMR-NXT     =  REQU-ADINLOMR                        
002648             MOVE ERR-UPD-NOT-ALLOWED TO RESP-IDMSG-ERROR                 
002649             MOVE NEJ                 TO INDATA-SW                        
002650             MOVE MFS-ALFA-FAELT-FEL TO RESP-ADINLOMR-NXT-INM-ATTR        
002651         END-IF                                                           
002652     END-IF                                                               
002653     .                                                                    
002654     EJECT                                                                
002655 GKC-KOLLA-VAGNAENDRING  SECTION.                                         
002656                                                                          
002657     IF REQU-KDCMDVAL             =  ALL '+' OR SPACE OR                  
002658                                     'AKU' OR 'A  ' OR 'FL ' OR           
002659                                     'QTY' OR 'LA '                       
002660         CONTINUE                                                         
002661     ELSE                                                                 
002662         MOVE ERR-CORR-HILITE-FLDS TO RESP-IDMSG-ERROR                    
002663         MOVE NEJ                 TO INDATA-SW                            
002664         MOVE MFS-ALFA-FAELT-FEL  TO RESP-KDCMDVAL-INM-ATTR               
002665                                     RESP-IDINLVGN-INM-ATTR               
002666     END-IF                                                               
002667                                                                          
002668     IF RAD-FLDIVKLI              =  JA                                   
002669         MOVE ERR-MISC-CASE       TO RESP-IDMSG-ERROR                     
002670         MOVE NEJ                 TO INDATA-SW                            
002671         MOVE MFS-ALFA-FAELT-FEL  TO RESP-IDINLVGN-INM-ATTR               
002672     END-IF                                                               
002673                                                                          
002674     IF REQU-IDINLVGN             NUMERIC AND                             
002675        REQU-IDINLVGN             > ZERO                                  
002676         MOVE REQU-IDINLVGN       TO W-D1FSEQ-IDINLVGN                    
002677                                     W-IDINLVGN                           
002678         PERFORM IMS-GU-W6INLA21-F                                        
002679         IF SEGMENT-FINNS                                                 
002680             MOVE JA               TO VAGN-SW                             
002681             MOVE RAD-ADINLOMR     TO W-ADINLOMR                          
002682             MOVE RAD-ADINLOMR-NXT TO W-ADINLOMR-NXT                      
002683             IF RAD-ADINLOMR NOT = SPACE                                  
002684               MOVE RAD-ADINLOMR TO  W-6006-ADINLOMR                      
002685               PERFORM IMS-GU-PLAA-PLAA11                                 
002686               IF PLAA-6006-KDINLOMR = 'F' OR 'FBP'                       
002687                 MOVE JA TO FP-SW                                         
002688               END-IF                                                     
002689             END-IF                                                       
002690             IF RAD-ADINLOMR-NXT NOT = SPACE                              
002691               MOVE RAD-ADINLOMR-NXT TO W-6006-ADINLOMR                   
002692               PERFORM IMS-GU-PLAA-PLAA11                                 
002693               IF PLAA-6006-KDINLOMR = 'F' OR 'FBP'                       
002694                 MOVE JA TO FP-SW                                         
002695               END-IF                                                     
002696             END-IF                                                       
002697         ELSE                                                             
002698             IF REQU-ADINLOMR      = ALL '+'                              
002699                 MOVE ERR-CORR-HILITE-FLDS TO RESP-IDMSG-ERROR            
002700                 MOVE NEJ          TO INDATA-SW                           
002701                 MOVE MFS-ALFA-FAELT-FEL                                  
002702                                   TO RESP-ADINLOMR-INM-ATTR              
002703             END-IF                                                       
002704         END-IF                                                           
002705     ELSE                                                                 
002706         MOVE ERR-HILIGHT-FLDS-WRONG TO RESP-IDMSG-ERROR                  
002707         MOVE NEJ                    TO INDATA-SW                         
002708         MOVE MFS-ALFA-FAELT-FEL     TO RESP-IDINLVGN-INM-ATTR            
002709     END-IF                                                               
002710     .                                                                    
002711     EJECT                                                                
002712 GL-KOLLA-RETUR                SECTION.                                   
002713                                                                          
002714     PERFORM S04-KOLLA-KDINLSTA                                           
002715     PERFORM S06-KOLLA-KVINLART                                           
002716     IF INDATA-OK                                                         
002717       PERFORM S22B-KOLLA-RET                                             
002718       IF INDATA-OK                                                       
002719         IF WS-KVINLART-TOT-KVARET = WS-KVINLART OR                       
002720            RAD-IDRADNR = +1                                              
002721           CONTINUE                                                       
002722         ELSE                                                             
002723           IF RAD-FLDIVKLI = NEJ                                          
002724             MOVE '6F'                 TO PRT-IDPRTLST(1:2)               
002725           ELSE                                                           
002726             MOVE '6E'                 TO PRT-IDPRTLST(1:2)               
002727           END-IF                                                         
002728           PERFORM S05-KOLLA-ADINLOMR-PRT                                 
002729         END-IF                                                           
002730       END-IF                                                             
002731     END-IF                                                               
002732     .                                                                    
002733     EJECT                                                                
002734 GM-KOLLA-ANTALSKONTROLL-PARTI SECTION.                                   
002735                                                                          
002736     PERFORM S04-KOLLA-KDINLSTA                                           
002737                                                                          
002738     IF REQU-KVINLART NOT = ALL '+'                                       
002739       MOVE ERR-CORR-HILITE-FLDS TO RESP-IDMSG-ERROR                      
002740       MOVE NEJ               TO INDATA-SW                                
002741       MOVE MFS-NUM-FAELT-FEL TO RESP-KVINLART-INM-ATTR                   
002742     END-IF                                                               
002743     .                                                                    
002744     EJECT                                                                
002745 H-UPPDATERA   SECTION.                                                   
002746                                                                          
002747     MOVE +1                   TO 6191-IX                                 
002748     PERFORM HA-LAES-KOLLISEG                                             
002749                                                                          
002750     EVALUATE TRUE                                                        
002751       WHEN REQU-KDCMDVAL = 'AKU' OR 'A  ' OR 'QTY'                       
002752          PERFORM HB-UPD-ANTALSKONTROLL                                   
002753                                                                          
002754       WHEN REQU-KDCMDVAL = 'MOT' OR 'REC'                                
002755          PERFORM HC-UPD-ANTALSAVVIKELSE                                  
002756                                                                          
002757       WHEN REQU-KDCMDVAL = 'SEK'                                         
002758          PERFORM HD-UPD-SEKUNDAERUTPLOCK                                 
002759                                                                          
002760       WHEN REQU-KDCMDVAL = 'FL ' OR 'LA '                                
002761          PERFORM HE-UPD-FLAGGA                                           
002762                                                                          
002763       WHEN REQU-KDCMDVAL = 'ET ' OR 'ML '                                
002764          PERFORM HF-UPD-ETIKETT                                          
002765                                                                          
002766       WHEN REQU-KDCMDVAL = 'KVA' OR 'QD '                                
002767          PERFORM HG-UPD-KVALITETSAVVIKELSE                               
002768                                                                          
002769       WHEN REQU-KDCMDVAL = 'RET'                                         
002770          PERFORM HH-UPD-RETUR                                            
002771                                                                          
002772       WHEN REQU-KDCMDVAL = 'AKP' OR 'QB '                                
002773          PERFORM HJ-UPD-ANTALSKONTROLL-PARTI                             
002774                                                                          
002775       WHEN REQU-KDCMDVAL = 'KR ' OR 'IR '                                
002776          PERFORM S09A-STARTA-6202                                        
002777                                                                          
002778       WHEN REQU-KDCMDVAL = SPACE OR ALL '+'                              
002779         PERFORM S20-UPD-EV-OEVRIGA-INM-FAELT                             
002780         PERFORM IMS-REPL-W6INLA21-B                                      
002781         PERFORM S21-UPD-EV-KDKLIPRIO                                     
002782                                                                          
002783     END-EVALUATE                                                         
002784                                                                          
002785     IF 6191-IX                > 1                                        
002786         PERFORM HI-STARTA-6191                                           
002787     END-IF                                                               
002788                                                                          
002789     IF WS-FLADM-SKAPAD = JA                                              
002790       MOVE INF-ADM-REP            TO RESP-IDMSG-INFO                     
002791     ELSE                                                                 
002792       IF REQU-KDCMDVAL     = 'FL ' OR 'ET ' OR 'LA ' OR 'ML '            
002793          MOVE INF-PRINTING-REQUEST TO RESP-IDMSG-INFO                    
002794       END-IF                                                             
002795     END-IF                                                               
002796     PERFORM MFS-FORM-ATTR                                                
002797     PERFORM MFS-RENSA-FAELT-IN                                           
002798     .                                                                    
002799     EJECT                                                                
002800 HA-LAES-KOLLISEG          SECTION.                                       
002801                                                                          
002802     EVALUATE TRUE                                                        
002803                                                                          
002804       WHEN IDLEVNWKOLLI-NYCKEL                                           
002805         MOVE W-IDLEVNR-KOLLI  TO W-IDLEVNRK                              
002806                                  W-IDLEVNR                               
002807         MOVE W-IDOKOLLI       TO W-IDOKOLLIK                             
002808                                  W-IDOKOLLINR                            
002809         PERFORM IMS-GU-W6INLA11-C                                        
002810         PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT OR                  
002811                       ART-IDLOPNRM = W-NUM-IDLOPNRM                      
002812             PERFORM IMS-GN-W6INLA11-C                                    
002813         END-PERFORM                                                      
002814         MOVE ART-IDLOPNRM       TO W-IDLOPNR                             
002815         MOVE ART-BEFT           TO W-BEFT                                
002816         MOVE ART-IDARTNR        TO WS-IDARTNR                            
002817         IF ART-KVAVIS-PRIO > ZERO                                        
002818           MOVE JA TO PRIOGODS-SW                                         
002819         END-IF                                                           
002820         PERFORM IMS-GHU-W6INLA21-B                                       
002821                                                                          
002822       WHEN IDLOPNRM-NYCKEL                                               
002823         MOVE WS-IDLOPNRM-NUM  TO W-IDLOPNR                               
002824         MOVE 1                TO W-IDRADNR                               
002825         PERFORM IMS-GU-W6INLA11-B                                        
002826         MOVE ART-BEFT           TO W-BEFT                                
002827         MOVE ART-IDARTNR        TO WS-IDARTNR                            
002828         IF ART-KVAVIS-PRIO > ZERO                                        
002829           MOVE JA TO PRIOGODS-SW                                         
002830         END-IF                                                           
002831         PERFORM IMS-GHNP-W6INLA21-B                                      
002832                                                                          
002833     END-EVALUATE                                                         
002834     MOVE RAD-W6D121           TO SPAR-RAD-W6D121                         
002835     MOVE LOW-VALUE            TO DLI-IO-AREA-INL21-KVARET                
002836     .                                                                    
002837     EJECT                                                                
002838 HB-UPD-ANTALSKONTROLL    SECTION.                                        
002839                                                                          
002840     MOVE JA                   TO RAD-FLKVAANT                            
002841                                                                          
002842     PERFORM S20-UPD-EV-OEVRIGA-INM-FAELT                                 
002843     PERFORM IMS-REPL-W6INLA21-B                                          
002844                                                                          
002845     PERFORM S09-ANTALSKONTROLL                                           
002846     IF W-KVINLART             >  ART-KVAVIS                              
002847         MOVE ERR-SURPLUS-DLVRY  TO RESP-IDMSG-ERROR                      
002848     END-IF                                                               
002849                                                                          
002850     PERFORM S21-UPD-EV-KDKLIPRIO                                         
002851     .                                                                    
002852     EJECT                                                                
002853 HC-UPD-ANTALSAVVIKELSE SECTION.                                          
002854                                                                          
002855     MOVE RAD-IDRADNR          TO W-SPAR-IDRADNR                          
002856                                  W-6193-IDRADNR                          
002857     MOVE REQU-KVINLART        TO W-KVINLART-UPD                          
002858                                                                          
002859     IF W-KVINLART-UPD         = ZERO AND                                 
002860        RAD-IDRADNR            = 1                                        
002861         PERFORM IMS-DLET-W6INLA21-B                                      
002862         PERFORM S17-SKAPA-6191-BORT-RAD                                  
002863     ELSE                                                                 
002864         PERFORM HCA-UPD-AKTUELLT-KOLLISEG                                
002865         PERFORM S07-SKAPA-6191                                           
002866     END-IF                                                               
002867                                                                          
002868     IF (REQU-KVINLART         >  ZERO) OR                                
002869        (REQU-KVINLART         =  ZERO AND RAD-IDRADNR = 1)               
002870         PERFORM HCB-SKAPA-NY-AVV-RAD                                     
002871         PERFORM S11-SKAPA-6191-MID-NY-RAD                                
002872     END-IF                                                               
002873                                                                          
002874     PERFORM S09-ANTALSKONTROLL                                           
002875                                                                          
002876     IF SPAR-RAD-FLPRIO        = JA                                       
002877         PERFORM S16-CALL-W611PMRK                                        
002878     END-IF                                                               
002879                                                                          
002880     IF W-KVINLART-UPD         = ZERO                                     
002881         PERFORM S14-STARTA-6193                                          
002882     END-IF                                                               
002883     .                                                                    
002884     EJECT                                                                
002885 HCA-UPD-AKTUELLT-KOLLISEG SECTION.                                       
002886                                                                          
002887     MOVE JA                   TO RAD-FLKVAANT                            
002888     IF RAD-KDINLSTA           =  'SAK'                                   
002889         MOVE SPACE            TO RAD-KDINLSTA                            
002890     END-IF                                                               
002891                                                                          
002892     IF REQU-KVINLART          =  ZERO                                    
002893         IF ART-KDKVAANT       >  ZERO                                    
002894             MOVE 'ANT'        TO RAD-KDINLSTA                            
002895         ELSE                                                             
002896             MOVE 'AVV'        TO RAD-KDINLSTA                            
002897         END-IF                                                           
002898         PERFORM S08-REDIG-IDANSTNR                                       
002899         MOVE WS-IDANSTNR      TO RAD-IDANSTNR                            
002900         MOVE SPACE            TO RAD-ADINLOMR                            
002901                                  RAD-ADINLOMR-NXT                        
002902         MOVE ZERO             TO RAD-IDILIRAD                            
002903                                  RAD-IDILIST                             
002904                                  RAD-IDINLVGN                            
002905         MOVE 39               TO RAD-KDINLPRIO                           
002906         ACCEPT RAD-TIUPPDAT FROM DATE                                    
002907         MOVE NEJ              TO RAD-FLKVAANT                            
002908                                  RAD-FLINLFB                             
002909                                  RAD-FLINLFP                             
002910                                  RAD-FLDIVKLI                            
002911                                  RAD-FLSATS                              
002912                                  RAD-FLPRIO                              
002913     ELSE                                                                 
002914         MOVE REQU-KVINLART        TO RAD-KVINLART                        
002915         IF RAD-IDRADNR NOT = +1                                          
002916           IF RAD-FLDIVKLI = NEJ                                          
002917             PERFORM S15-STARTA-W60194                                    
002918           ELSE                                                           
002919             PERFORM S13-STARTA-6195                                      
002920           END-IF                                                         
002921         END-IF                                                           
002922     END-IF                                                               
002923     PERFORM IMS-REPL-W6INLA21-B                                          
002924     .                                                                    
002925     EJECT                                                                
002926 HCB-SKAPA-NY-AVV-RAD SECTION.                                            
002927                                                                          
002928** OBS INGA LÄSNINGAR MOT INLA2 PCB'ET MELLAN -LAST OCH ISRT              
002929** DET SKULLE FÖRSTÖRA POSITIONERINGEN                                    
002930                                                                          
002931     PERFORM S08-REDIG-IDANSTNR                                           
002932     PERFORM IMS-GNP-W6INLA21-B-LAST                                      
002933     IF SEGMENT-FINNS                                                     
002934         COMPUTE SPAR-RAD-IDRADNR = RAD-IDRADNR + 1                       
002935     ELSE                                                                 
002936         COMPUTE SPAR-RAD-IDRADNR = SPAR-RAD-IDRADNR + 1                  
002937     END-IF                                                               
002938     MOVE SPAR-RAD-IDRADNR TO W-6193-IDRADNR                              
002939     MOVE SPAR-RAD-W6D121        TO RAD-W6D121                            
002940     MOVE SPACE                  TO RAD-ADINLOMR                          
002941                                    RAD-ADINLOMR-NXT                      
002942     MOVE WS-IDANSTNR            TO RAD-IDANSTNR                          
002943     MOVE ZERO                   TO RAD-IDILIRAD                          
002944                                    RAD-IDILIST                           
002945                                    RAD-IDINLVGN                          
002946     MOVE 39                     TO RAD-KDINLPRIO                         
002947     IF ART-KDKVAANT             >  ZERO                                  
002948         MOVE 'ANT'              TO RAD-KDINLSTA                          
002949     ELSE                                                                 
002950         MOVE 'AVV'              TO RAD-KDINLSTA                          
002951     END-IF                                                               
002952     COMPUTE RAD-KVINLART        =  SPAR-RAD-KVINLART -                   
002953                                    W-KVINLART-UPD                        
002954     ACCEPT RAD-TIUPPDAT FROM DATE                                        
002955     MOVE NEJ                    TO RAD-FLKVAANT                          
002956                                    RAD-FLINLFB                           
002957                                    RAD-FLINLFP                           
002958                                    RAD-FLDIVKLI                          
002959                                    RAD-FLSATS                            
002960                                    RAD-FLPRIO                            
002961                                    RAD-FLSVSLS                           
002962                                                                          
002963     PERFORM IMS-ISRT-W6INLA21-B                                          
002964     .                                                                    
002965     EJECT                                                                
002966 HD-UPD-SEKUNDAERUTPLOCK SECTION.                                         
002967                                                                          
002968     MOVE RAD-IDRADNR          TO W-SPAR-IDRADNR                          
002969     PERFORM HDA-UPD-AKTUELLT-KOLLISEG                                    
002970     PERFORM S07-SKAPA-6191                                               
002971                                                                          
002972     PERFORM HDB-SKAPA-NY-SEK-RAD                                         
002973     PERFORM S11-SKAPA-6191-MID-NY-RAD                                    
002974                                                                          
002975     PERFORM IMS-GHU-W6INLA11-B                                           
002976     COMPUTE ART-KVKVASEK-VER  =                                          
002977             ART-KVKVASEK-VER  + W-KVINLART-UPD                           
002978     PERFORM IMS-REPL-W6INLA11-B                                          
002979                                                                          
002980     IF SPAR-RAD-FLPRIO        = JA                                       
002981         PERFORM S16-CALL-W611PMRK                                        
002982     END-IF                                                               
002983                                                                          
002984     IF W-KVINLART-UPD         < SPAR-RAD-KVINLART                        
002985         MOVE '6E'            TO PRT-IDPRTLST (1:2)                       
002986         PERFORM S13-STARTA-6195                                          
002987     END-IF                                                               
002988     .                                                                    
002989     EJECT                                                                
002990 HDA-UPD-AKTUELLT-KOLLISEG SECTION.                                       
002991                                                                          
002992     MOVE REQU-KVINLART        TO W-KVINLART-UPD                          
002993     IF RAD-KDINLSTA           =  'SAK'                                   
002994         MOVE SPACE            TO RAD-KDINLSTA                            
002995     END-IF                                                               
002996                                                                          
002997     COMPUTE ART-KVKVASEK-VER  = ART-KVKVASEK-VER +                       
002998                                 W-KVINLART-UPD                           
002999     COMPUTE RAD-KVINLART      =  RAD-KVINLART - W-KVINLART-UPD           
003000     IF RAD-KVINLART = +0                                                 
003001         PERFORM IMS-DLET-W6INLA21-B                                      
003002     ELSE                                                                 
003003         IF RAD-FLDIVKLI = NEJ AND RAD-IDRADNR > +1                       
003004           PERFORM S12-TA-EV-UT-NYTT-KOLLIID                              
003005           MOVE '6F'            TO PRT-IDPRTLST (1:2)                     
003006           PERFORM S15-STARTA-W60194                                      
003007         ELSE                                                             
003008           IF RAD-IDRADNR > +1                                            
003009             MOVE '6E'            TO PRT-IDPRTLST (1:2)                   
003010             PERFORM S13-STARTA-6195                                      
003011           END-IF                                                         
003012         END-IF                                                           
003013         PERFORM IMS-REPL-W6INLA21-B                                      
003014     END-IF                                                               
003015     .                                                                    
003016     EJECT                                                                
003017 HDB-SKAPA-NY-SEK-RAD SECTION.                                            
003018                                                                          
003019** OBS INGA LÄSNINGAR MOT INLA2 PCB'ET MELLAN -LAST OCH ISRT              
003020** DET SKULLE FÖRSTÖRA POSITIONERINGEN                                    
003021                                                                          
003022     PERFORM IMS-GNP-W6INLA21-B-LAST                                      
003023     IF SEGMENT-FINNS                                                     
003024         COMPUTE SPAR-RAD-IDRADNR = RAD-IDRADNR + 1                       
003025     ELSE                                                                 
003026         COMPUTE SPAR-RAD-IDRADNR = SPAR-RAD-IDRADNR + 1                  
003027     END-IF                                                               
003028     MOVE SPAR-RAD-W6D121        TO RAD-W6D121                            
003029     MOVE 'SEK'                  TO RAD-ADINLOMR                          
003030     MOVE SPACE                  TO RAD-ADINLOMR-NXT                      
003031                                    RAD-IDLEVNR-KOLLI                     
003032     MOVE ZERO                   TO RAD-IDANSTNR                          
003033                                    RAD-IDILIRAD                          
003034                                    RAD-IDILIST                           
003035                                    RAD-IDINLVGN                          
003036                                    RAD-IDOKOLLI                          
003037     MOVE 39                     TO RAD-KDINLPRIO                         
003038     MOVE W-KVINLART-UPD         TO RAD-KVINLART                          
003039     MOVE ZERO                   TO RAD-TIUPPDAT                          
003040     MOVE NEJ                    TO RAD-FLKVAANT                          
003041                                    RAD-FLDIVKLI                          
003042                                    RAD-FLSATS                            
003043                                    RAD-FLSVSLS                           
003044     MOVE JA                     TO RAD-FLINLFB                           
003045                                                                          
003046     PERFORM IMS-ISRT-W6INLA21-B                                          
003047     .                                                                    
003048     EJECT                                                                
003049 HE-UPD-FLAGGA  SECTION.                                                  
003050                                                                          
003051     MOVE NEJ                  TO NYTT-KOLLIID-SW                         
003052     PERFORM S12-TA-EV-UT-NYTT-KOLLIID                                    
003053     IF REQU-FLSATS       NOT = ALL '+' OR                                
003054        REQU-ADINLOMR     NOT = ALL '+' OR                                
003055        REQU-ADINLOMR-NXT NOT = ALL '+' OR                                
003056        REQU-IDINLVGN     NOT = ALL '+' OR                                
003057        NYTT-KOLLIID                                                      
003058         PERFORM S20-UPD-EV-OEVRIGA-INM-FAELT                             
003059     END-IF                                                               
003060     PERFORM S21-UPD-EV-KDKLIPRIO                                         
003061     PERFORM IMS-REPL-W6INLA21-B                                          
003062     PERFORM S15-STARTA-W60194                                            
003063     .                                                                    
003064     EJECT                                                                
003065 HF-UPD-ETIKETT SECTION.                                                  
003066                                                                          
003067     IF RAD-IDRADNR            = 1                                        
003068         PERFORM IMS-DLET-W6INLA21-B                                      
003069         PERFORM S17-SKAPA-6191-BORT-RAD                                  
003070         PERFORM IMS-GNP-W6INLA21-B-LAST                                  
003071         IF SEGMENT-FINNS                                                 
003072             COMPUTE SPAR-RAD-IDRADNR = RAD-IDRADNR + 1                   
003073         ELSE                                                             
003074             COMPUTE SPAR-RAD-IDRADNR = SPAR-RAD-IDRADNR + 1              
003075         END-IF                                                           
003076         MOVE SPAR-RAD-W6D121         TO RAD-W6D121                       
003077         PERFORM IMS-ISRT-W6INLA21-B                                      
003078         PERFORM S11-SKAPA-6191-MID-NY-RAD                                
003079     ELSE                                                                 
003080         IF RAD-FLDIVKLI = NEJ                                            
003081           MOVE ZERO                    TO SPAR-RAD-IDOKOLLI              
003082                                           SPAR-RAD-IDINLVGN              
003083           MOVE SPACE                   TO SPAR-RAD-IDLEVNR-KOLLI         
003084           MOVE SPAR-RAD-W6D121         TO RAD-W6D121                     
003085           PERFORM IMS-REPL-W6INLA21-B                                    
003086         END-IF                                                           
003087     END-IF                                                               
003088     PERFORM S13-STARTA-6195                                              
003089     .                                                                    
003090     EJECT                                                                
003091 HG-UPD-KVALITETSAVVIKELSE SECTION.                                       
003092                                                                          
003093     MOVE RAD-IDRADNR          TO W-SPAR-IDRADNR                          
003094                                  W-6193-IDRADNR                          
003095                                                                          
003096     IF (RAD-IDRADNR      = 1 AND KVARET-SAKNAS AND                       
003097         W-KVINLART-UPD   = RAD-KVINLART) OR                              
003098          (RAD-IDRADNR    =  1 AND KVARET-FINNS AND                       
003099           W-KVINLART-UPD = WS-KVINLART-TOT-KVARET)                       
003100         PERFORM IMS-DLET-W6INLA21-B                                      
003101         PERFORM S17-SKAPA-6191-BORT-RAD                                  
003102         IF KVARET-FINNS                                                  
003103           PERFORM IMS-GU-W6INLA11-B                                      
003104           PERFORM UNTIL KVARET-RAD-KDINLSTA = 'KVA'                      
003105             PERFORM IMS-GHNP-W6INLA21-KVARET2                            
003106           END-PERFORM                                                    
003107           MOVE KVARET-RAD-W6D121 TO SPAR-RAD-W6D121                      
003108           MOVE W-KVINLART-UPD TO KVARET-RAD-KVINLART                     
003109           PERFORM IMS-REPL-W6INLA21-KVARET                               
003110           MOVE KVARET-RAD-W6D121 TO RAD-W6D121                           
003111           MOVE RAD-IDRADNR       TO W-6193-IDRADNR                       
003112           PERFORM S07-SKAPA-6191                                         
003113         END-IF                                                           
003114     ELSE                                                                 
003115         PERFORM HGA-UPD-AKTUELLT-KOLLISEG                                
003116     END-IF                                                               
003117                                                                          
003118     IF KVARET-FINNS                                                      
003119       CONTINUE                                                           
003120     ELSE                                                                 
003121       IF (W-KVINLART-UPD NOT = SPAR-RAD-KVINLART) OR                     
003122          (W-KVINLART-UPD     =  RAD-KVINLART AND RAD-IDRADNR = 1)        
003123           PERFORM HGB-SKAPA-NY-KVA-RAD                                   
003124           PERFORM S11-SKAPA-6191-MID-NY-RAD                              
003125       END-IF                                                             
003126     END-IF                                                               
003127                                                                          
003128     IF SPAR-RAD-FLPRIO        = JA                                       
003129         PERFORM S16-CALL-W611PMRK                                        
003130     END-IF                                                               
003131                                                                          
003132     IF W-KVINLART-UPD         = SPAR-RAD-KVINLART                        
003133         PERFORM S14-STARTA-6193                                          
003134     END-IF                                                               
003135     .                                                                    
003136     EJECT                                                                
003137 HGA-UPD-AKTUELLT-KOLLISEG SECTION.                                       
003138                                                                          
003139     IF RAD-KDINLSTA           =  'SAK'                                   
003140         MOVE SPACE            TO RAD-KDINLSTA                            
003141     END-IF                                                               
003142                                                                          
003143     IF KVARET-FINNS                                                      
003144       PERFORM HGAA-UPD-KOLLISEG-MED-KVA                                  
003145     ELSE                                                                 
003146       PERFORM HGAB-UPD-KOLLISEG-UTAN-KVA                                 
003147       PERFORM IMS-REPL-W6INLA21-B                                        
003148       PERFORM S07-SKAPA-6191                                             
003149     END-IF                                                               
003150     .                                                                    
003151     EJECT                                                                
003152 HGAA-UPD-KOLLISEG-MED-KVA SECTION.                                       
003153                                                                          
003154     IF W-KVINLART-UPD         = WS-KVINLART-TOT-KVARET                   
003155***ALLT BLIR KVA                                                          
003156       PERFORM IMS-DLET-W6INLA21-B                                        
003157       PERFORM S17-SKAPA-6191-BORT-RAD                                    
003158       PERFORM IMS-GU-W6INLA11-B                                          
003159       PERFORM UNTIL KVARET-RAD-KDINLSTA = 'KVA'                          
003160         IF IDLEVNWKOLLI-NYCKEL                                           
003161           PERFORM IMS-GHNP-W6INLA21-KVARET1                              
003162         ELSE                                                             
003163           PERFORM IMS-GHNP-W6INLA21-KVARET2                              
003164         END-IF                                                           
003165       END-PERFORM                                                        
003166         MOVE KVARET-RAD-W6D121 TO SPAR-RAD-W6D121                        
003167         MOVE W-KVINLART-UPD TO KVARET-RAD-KVINLART                       
003168         PERFORM IMS-REPL-W6INLA21-KVARET                                 
003169         MOVE KVARET-RAD-W6D121 TO RAD-W6D121                             
003170         PERFORM S07-SKAPA-6191                                           
003171     ELSE                                                                 
003172       COMPUTE WS-KVINLART-TOT-KVARET =                                   
003173               WS-KVINLART-KVARET - W-KVINLART-UPD                        
003174       ADD WS-KVINLART-TOT-KVARET TO RAD-KVINLART                         
003175       PERFORM IMS-REPL-W6INLA21-B                                        
003176       PERFORM S07-SKAPA-6191                                             
003177       PERFORM IMS-GU-W6INLA11-B                                          
003178       PERFORM UNTIL KVARET-RAD-KDINLSTA = 'KVA'                          
003179         IF IDLEVNWKOLLI-NYCKEL                                           
003180           PERFORM IMS-GHNP-W6INLA21-KVARET1                              
003181         ELSE                                                             
003182           PERFORM IMS-GHNP-W6INLA21-KVARET2                              
003183         END-IF                                                           
003184       END-PERFORM                                                        
003185       IF W-KVINLART-UPD = ZERO                                           
003186         PERFORM IMS-DLET-W6INLA21-KVARET                                 
003187         MOVE KVARET-RAD-W6D121 TO RAD-W6D121                             
003188         PERFORM S17-SKAPA-6191-BORT-RAD                                  
003189       ELSE                                                               
003190         MOVE KVARET-RAD-W6D121 TO SPAR-RAD-W6D121                        
003191         MOVE W-KVINLART-UPD TO KVARET-RAD-KVINLART                       
003192         PERFORM IMS-REPL-W6INLA21-KVARET                                 
003193         MOVE KVARET-RAD-W6D121 TO RAD-W6D121                             
003194         PERFORM S07-SKAPA-6191                                           
003195       END-IF                                                             
003196       PERFORM HA-LAES-KOLLISEG                                           
003197       IF RAD-IDRADNR NOT = +1                                            
003198         IF RAD-FLDIVKLI = NEJ                                            
003199           PERFORM S15-STARTA-W60194                                      
003200         ELSE                                                             
003201           PERFORM S13-STARTA-6195                                        
003202         END-IF                                                           
003203       END-IF                                                             
003204     END-IF                                                               
003205     .                                                                    
003206     EJECT                                                                
003207 HGAB-UPD-KOLLISEG-UTAN-KVA SECTION.                                      
003208                                                                          
003209     IF W-KVINLART-UPD         =  RAD-KVINLART                            
003210        MOVE 'KVA'            TO RAD-KDINLSTA                             
003211        PERFORM S08-REDIG-IDANSTNR                                        
003212        MOVE WS-IDANSTNR      TO RAD-IDANSTNR                             
003213        MOVE ZERO             TO RAD-IDILIRAD                             
003214                                 RAD-IDILIST                              
003215                                 RAD-IDINLVGN                             
003216        MOVE 39               TO RAD-KDINLPRIO                            
003217        ACCEPT RAD-TIUPPDAT FROM DATE                                     
003218        MOVE JA               TO RAD-FLKVAANT                             
003219        MOVE NEJ              TO RAD-FLINLFB                              
003220                                 RAD-FLINLFP                              
003221                                 RAD-FLDIVKLI                             
003222                                 RAD-FLSATS                               
003223                                 RAD-FLPRIO                               
003224     ELSE                                                                 
003225        COMPUTE RAD-KVINLART = RAD-KVINLART - W-KVINLART-UPD              
003226        IF RAD-IDRADNR NOT = +1                                           
003227          IF RAD-FLDIVKLI = NEJ                                           
003228            PERFORM S15-STARTA-W60194                                     
003229          ELSE                                                            
003230            PERFORM S13-STARTA-6195                                       
003231          END-IF                                                          
003232        END-IF                                                            
003233     END-IF                                                               
003234     .                                                                    
003235     EJECT                                                                
003236 HGB-SKAPA-NY-KVA-RAD SECTION.                                            
003237                                                                          
003238** OBS INGA LÄSNINGAR MOT INLA2 PCB'ET MELLAN -LAST OCH ISRT              
003239** DET SKULLE FÖRSTÖRA POSITIONERINGEN                                    
003240                                                                          
003241     PERFORM S08-REDIG-IDANSTNR                                           
003242     PERFORM IMS-GNP-W6INLA21-B-LAST                                      
003243     IF SEGMENT-FINNS                                                     
003244         COMPUTE SPAR-RAD-IDRADNR = RAD-IDRADNR + 1                       
003245     ELSE                                                                 
003246         COMPUTE SPAR-RAD-IDRADNR = SPAR-RAD-IDRADNR + 1                  
003247     END-IF                                                               
003248     MOVE SPAR-RAD-IDRADNR       TO W-6193-IDRADNR                        
003249     MOVE SPAR-RAD-W6D121        TO RAD-W6D121                            
003250     MOVE WS-IDANSTNR            TO RAD-IDANSTNR                          
003251     MOVE ZERO                   TO RAD-IDILIRAD                          
003252                                    RAD-IDILIST                           
003253                                    RAD-IDINLVGN                          
003254     MOVE 39                     TO RAD-KDINLPRIO                         
003255     MOVE 'KVA'                  TO RAD-KDINLSTA                          
003256     MOVE W-KVINLART-UPD         TO RAD-KVINLART                          
003257     ACCEPT RAD-TIUPPDAT FROM DATE                                        
003258     MOVE JA                     TO RAD-FLKVAANT                          
003259     MOVE NEJ                    TO RAD-FLINLFB                           
003260                                    RAD-FLINLFP                           
003261                                    RAD-FLDIVKLI                          
003262                                    RAD-FLSATS                            
003263                                    RAD-FLPRIO                            
003264                                    RAD-FLSVSLS                           
003265                                                                          
003266     PERFORM IMS-ISRT-W6INLA21-B                                          
003267     .                                                                    
003268     EJECT                                                                
003269 HH-UPD-RETUR SECTION.                                                    
003270                                                                          
003271     MOVE RAD-IDRADNR          TO W-SPAR-IDRADNR                          
003272                                  W-6193-IDRADNR                          
003273                                                                          
003274     IF (RAD-IDRADNR      = 1 AND KVARET-SAKNAS AND                       
003275         W-KVINLART-UPD   = RAD-KVINLART) OR                              
003276          (RAD-IDRADNR    =  1 AND KVARET-FINNS AND                       
003277           W-KVINLART-UPD = WS-KVINLART-TOT-KVARET)                       
003278         PERFORM IMS-DLET-W6INLA21-B                                      
003279         PERFORM S17-SKAPA-6191-BORT-RAD                                  
003280         IF KVARET-FINNS                                                  
003281           PERFORM IMS-GU-W6INLA11-B                                      
003282           PERFORM UNTIL KVARET-RAD-KDINLSTA = 'RET'                      
003283             PERFORM IMS-GHNP-W6INLA21-KVARET2                            
003284           END-PERFORM                                                    
003285           MOVE KVARET-RAD-W6D121 TO SPAR-RAD-W6D121                      
003286           MOVE W-KVINLART-UPD TO KVARET-RAD-KVINLART                     
003287           PERFORM IMS-REPL-W6INLA21-KVARET                               
003288           MOVE KVARET-RAD-W6D121 TO RAD-W6D121                           
003289           MOVE RAD-IDRADNR TO W-6193-IDRADNR                             
003290           PERFORM S07-SKAPA-6191                                         
003291         END-IF                                                           
003292     ELSE                                                                 
003293         PERFORM HHA-UPD-AKTUELLT-KOLLISEG                                
003294     END-IF                                                               
003295                                                                          
003296     IF KVARET-FINNS                                                      
003297       CONTINUE                                                           
003298     ELSE                                                                 
003299       IF (W-KVINLART-UPD NOT = SPAR-RAD-KVINLART) OR                     
003300          (W-KVINLART-UPD     =  RAD-KVINLART AND RAD-IDRADNR = 1)        
003301           PERFORM HHB-SKAPA-NY-RET-RAD                                   
003302           PERFORM S11-SKAPA-6191-MID-NY-RAD                              
003303       END-IF                                                             
003304     END-IF                                                               
003305                                                                          
003306     IF SPAR-RAD-FLPRIO        = JA                                       
003307         PERFORM S16-CALL-W611PMRK                                        
003308     END-IF                                                               
003309                                                                          
003310     IF W-KVINLART-UPD         = SPAR-RAD-KVINLART                        
003311         PERFORM S14-STARTA-6193                                          
003312     END-IF                                                               
003313     .                                                                    
003314     EJECT                                                                
003315 HHA-UPD-AKTUELLT-KOLLISEG SECTION.                                       
003316                                                                          
003317     IF RAD-KDINLSTA           =  'SAK'                                   
003318         MOVE SPACE            TO RAD-KDINLSTA                            
003319     END-IF                                                               
003320                                                                          
003321     IF KVARET-FINNS                                                      
003322       PERFORM HHAA-UPD-KOLLISEG-MED-RET                                  
003323     ELSE                                                                 
003324       PERFORM HHAB-UPD-KOLLISEG-UTAN-RET                                 
003325       PERFORM IMS-REPL-W6INLA21-B                                        
003326       PERFORM S07-SKAPA-6191                                             
003327     END-IF                                                               
003328     .                                                                    
003329     EJECT                                                                
003330 HHAA-UPD-KOLLISEG-MED-RET SECTION.                                       
003331                                                                          
003332     IF W-KVINLART-UPD         = WS-KVINLART-TOT-KVARET                   
003333***ALLT GÅR I RETUR                                                       
003334       PERFORM IMS-DLET-W6INLA21-B                                        
003335       PERFORM S17-SKAPA-6191-BORT-RAD                                    
003336       PERFORM IMS-GU-W6INLA11-B                                          
003337       PERFORM UNTIL KVARET-RAD-KDINLSTA = 'RET'                          
003338         IF IDLEVNWKOLLI-NYCKEL                                           
003339           PERFORM IMS-GHNP-W6INLA21-KVARET1                              
003340         ELSE                                                             
003341           PERFORM IMS-GHNP-W6INLA21-KVARET2                              
003342         END-IF                                                           
003343       END-PERFORM                                                        
003344       MOVE KVARET-RAD-W6D121 TO SPAR-RAD-W6D121                          
003345       MOVE W-KVINLART-UPD TO KVARET-RAD-KVINLART                         
003346       PERFORM IMS-REPL-W6INLA21-KVARET                                   
003347       MOVE KVARET-RAD-W6D121 TO RAD-W6D121                               
003348       PERFORM S07-SKAPA-6191                                             
003349     ELSE                                                                 
003350       COMPUTE WS-KVINLART-TOT-KVARET =                                   
003351               WS-KVINLART-KVARET - W-KVINLART-UPD                        
003352       ADD WS-KVINLART-TOT-KVARET TO RAD-KVINLART                         
003353       PERFORM IMS-REPL-W6INLA21-B                                        
003354       PERFORM S07-SKAPA-6191                                             
003355       PERFORM IMS-GU-W6INLA11-B                                          
003356       PERFORM UNTIL KVARET-RAD-KDINLSTA = 'RET'                          
003357         IF IDLEVNWKOLLI-NYCKEL                                           
003358           PERFORM IMS-GHNP-W6INLA21-KVARET1                              
003359         ELSE                                                             
003360           PERFORM IMS-GHNP-W6INLA21-KVARET2                              
003361         END-IF                                                           
003362       END-PERFORM                                                        
003363       IF W-KVINLART-UPD = ZERO                                           
003364         PERFORM IMS-DLET-W6INLA21-KVARET                                 
003365         MOVE KVARET-RAD-W6D121 TO RAD-W6D121                             
003366         PERFORM S17-SKAPA-6191-BORT-RAD                                  
003367       ELSE                                                               
003368         MOVE KVARET-RAD-W6D121 TO SPAR-RAD-W6D121                        
003369         MOVE W-KVINLART-UPD TO KVARET-RAD-KVINLART                       
003370         PERFORM IMS-REPL-W6INLA21-KVARET                                 
003371         MOVE KVARET-RAD-W6D121 TO RAD-W6D121                             
003372         PERFORM S07-SKAPA-6191                                           
003373       END-IF                                                             
003374       PERFORM HA-LAES-KOLLISEG                                           
003375       IF RAD-IDRADNR NOT = +1                                            
003376         IF RAD-FLDIVKLI = NEJ                                            
003377           PERFORM S15-STARTA-W60194                                      
003378         ELSE                                                             
003379           PERFORM S13-STARTA-6195                                        
003380         END-IF                                                           
003381       END-IF                                                             
003382     END-IF                                                               
003383     .                                                                    
003384     EJECT                                                                
003385 HHAB-UPD-KOLLISEG-UTAN-RET SECTION.                                      
003386                                                                          
003387       IF W-KVINLART-UPD         =  RAD-KVINLART                          
003388           MOVE 'RET'            TO RAD-KDINLSTA                          
003389           PERFORM S08-REDIG-IDANSTNR                                     
003390           MOVE WS-IDANSTNR      TO RAD-IDANSTNR                          
003391           MOVE SPACE            TO RAD-ADINLOMR                          
003392                                    RAD-ADINLOMR-NXT                      
003393           MOVE ZERO             TO RAD-IDILIRAD                          
003394                                    RAD-IDILIST                           
003395                                    RAD-IDINLVGN                          
003396           MOVE 39               TO RAD-KDINLPRIO                         
003397           ACCEPT RAD-TIUPPDAT FROM DATE                                  
003398           MOVE JA               TO RAD-FLKVAANT                          
003399           MOVE NEJ              TO RAD-FLINLFB                           
003400                                    RAD-FLINLFP                           
003401                                    RAD-FLDIVKLI                          
003402                                    RAD-FLSATS                            
003403                                    RAD-FLPRIO                            
003404       ELSE                                                               
003405           COMPUTE RAD-KVINLART = RAD-KVINLART - W-KVINLART-UPD           
003406           IF RAD-IDRADNR NOT = +1                                        
003407             IF RAD-FLDIVKLI = NEJ                                        
003408               PERFORM S15-STARTA-W60194                                  
003409             ELSE                                                         
003410               PERFORM S13-STARTA-6195                                    
003411             END-IF                                                       
003412           END-IF                                                         
003413       END-IF                                                             
003414     .                                                                    
003415     EJECT                                                                
003416 HHB-SKAPA-NY-RET-RAD SECTION.                                            
003417                                                                          
003418** OBS INGA LÄSNINGAR MOT INLA2 PCB'ET MELLAN -LAST OCH ISRT              
003419** DET SKULLE FÖRSTÖRA POSITIONERINGEN                                    
003420                                                                          
003421     PERFORM S08-REDIG-IDANSTNR                                           
003422     PERFORM IMS-GNP-W6INLA21-B-LAST                                      
003423     IF SEGMENT-FINNS                                                     
003424         COMPUTE SPAR-RAD-IDRADNR = RAD-IDRADNR + 1                       
003425     ELSE                                                                 
003426         COMPUTE SPAR-RAD-IDRADNR = SPAR-RAD-IDRADNR + 1                  
003427     END-IF                                                               
003428     MOVE SPAR-RAD-IDRADNR       TO W-6193-IDRADNR                        
003429     MOVE SPAR-RAD-W6D121        TO RAD-W6D121                            
003430     MOVE SPACE                  TO RAD-ADINLOMR                          
003431                                    RAD-ADINLOMR-NXT                      
003432     MOVE WS-IDANSTNR            TO RAD-IDANSTNR                          
003433     MOVE ZERO                   TO RAD-IDILIRAD                          
003434                                    RAD-IDILIST                           
003435                                    RAD-IDINLVGN                          
003436     MOVE 39                     TO RAD-KDINLPRIO                         
003437     MOVE 'RET'                  TO RAD-KDINLSTA                          
003438     MOVE W-KVINLART-UPD         TO RAD-KVINLART                          
003439     ACCEPT RAD-TIUPPDAT FROM DATE                                        
003440     MOVE JA                     TO RAD-FLKVAANT                          
003441     MOVE NEJ                    TO RAD-FLINLFB                           
003442                                    RAD-FLINLFP                           
003443                                    RAD-FLDIVKLI                          
003444                                    RAD-FLSATS                            
003445                                    RAD-FLPRIO                            
003446                                    RAD-FLSVSLS                           
003447                                                                          
003448     PERFORM IMS-ISRT-W6INLA21-B                                          
003449     .                                                                    
003450     EJECT                                                                
003451 HI-STARTA-6191    SECTION.                                               
003452                                                                          
003453     COMPUTE MOD6191-MID-KVPOST = 6191-IX - 1                             
003454     COMPUTE P-TO-P-MSG-KVLL    =  LNG-P-TO-P-PREFIX +                    
003455                                  17 + (MOD6191-MID-KVPOST * 64)          
003456     MOVE 'W6T191X '           TO P-TO-P-MSG-KDTRANS                      
003457     MOVE '6133'               TO P-TO-P-MSG-IDTRANS                      
003458     IF REQU-IDSPRAK = 'SV'                                               
003459        MOVE '1'               TO P-TO-P-MSG-KDMFSFOR                     
003460     ELSE                                                                 
003461        MOVE '2'               TO P-TO-P-MSG-KDMFSFOR                     
003462     END-IF                                                               
003463                                                                          
003464     MOVE MOD6191-MID-W6I19101 TO P-TO-P-MSG-INDATA                       
003465                                                                          
003466     PERFORM IMS-ISRT-6191-MSG                                            
003467     .                                                                    
003468     EJECT                                                                
003469 HJ-UPD-ANTALSKONTROLL-PARTI SECTION.                                     
003470                                                                          
003471     PERFORM IMS-GHNP-W6INLA21-B-FIRST                                    
003472     PERFORM UNTIL SEGMENT-SAKNAS                                         
003473       IF RAD-KDINLSTA = '   ' OR 'FPK' OR 'SAK'                          
003474         IF RAD-FLKVAANT = NEJ                                            
003475           MOVE JA                   TO RAD-FLKVAANT                      
003476           IF RAD-KDINLSTA = 'SAK'                                        
003477             MOVE '   '              TO RAD-KDINLSTA                      
003478           END-IF                                                         
003479           PERFORM IMS-REPL-W6INLA21-B                                    
003480         END-IF                                                           
003481       END-IF                                                             
003482       PERFORM IMS-GHNP-W6INLA21-B-OKVAL                                  
003483     END-PERFORM                                                          
003484                                                                          
003485     PERFORM S09-ANTALSKONTROLL                                           
003486                                                                          
003487     IF W-KVINLART             >  ART-KVAVIS                              
003488         MOVE ERR-SURPLUS-DLVRY TO RESP-IDMSG-ERROR                       
003489     END-IF                                                               
003490     .                                                                    
003491     EJECT                                                                
003492 S01-BERAKNA SECTION.                                                     
003493                                                                          
003494     IF RAD-IDOKOLLI           > ZERO AND                                 
003495        (RAD-KDINLSTA          = SPACE OR 'FPK' OR 'SAK')                 
003496        ADD +1 TO WW-KVKOLLI-TOT                                          
003497     END-IF                                                               
003498                                                                          
003499     IF RAD-KDINLSTA = 'ANT' OR 'AVV' OR 'KVA'                            
003500                        ADD RAD-KVINLART TO WW-KVAVIS-KVAR                
003501     END-IF                                                               
003502                                                                          
003503     IF RAD-KDINLSTA = SPACE OR 'FPK' OR 'INL'                            
003504       IF RAD-FLPRIO             = JA                                     
003505          ADD RAD-KVINLART       TO WW-KVAVIS-PRIO-KVAR                   
003506       END-IF                                                             
003507                                                                          
003508       IF RAD-FLSATS             = JA                                     
003509          ADD RAD-KVINLART       TO WW-KVAVIS-KIT-KVAR                    
003510       END-IF                                                             
003511     END-IF                                                               
003512     .                                                                    
003513     EJECT                                                                
003514                                                                          
003515 S02-FYLL-TAB SECTION.                                                    
003516                                                                          
003517     SET TAB-IX TO 1                                                      
003518     PERFORM UNTIL (RAD-ADINLOMR = TAB-ADINLOMR(TAB-IX) AND               
003519                    RAD-ADINLOMR-NXT =                                    
003520                    TAB-ADINLOMR-NXT(TAB-IX) AND                          
003521                    RAD-KDINLSTA     =  TAB-KDINLSTA(TAB-IX) AND          
003522                    RAD-IDINLVGN     =  TAB-IDINLVGN(TAB-IX) AND          
003523                    TAB-IX   >  1)                OR                      
003524                    TAB-IX   >  FYLLD-IX                                  
003525          SET TAB-IX UP BY 1                                              
003526     END-PERFORM                                                          
003527                                                                          
003528     IF TAB-IX                 >  MAX-TAB-IX                              
003529         MOVE 'RAD TABELL FULL  '                                         
003530                               TO FELTEXT                                 
003531         CALL ABEND USING RKOD-ABEND                                      
003532     END-IF                                                               
003533                                                                          
003534     IF TAB-IX                 >  FYLLD-IX                                
003535         PERFORM S02A-TA-FRAM-KDINLOMR                                    
003536         ADD +1                TO FYLLD-IX                                
003537         MOVE RAD-KVINLART     TO TAB-KVINLART(TAB-IX)                    
003538         IF RAD-IDOKOLLI       >  ZERO                                    
003539             MOVE +1           TO TAB-KVKOLLI(TAB-IX)                     
003540         ELSE                                                             
003541             MOVE ZERO         TO TAB-KVKOLLI(TAB-IX)                     
003542         END-IF                                                           
003543         MOVE RAD-ADINLOMR     TO TAB-ADINLOMR(TAB-IX)                    
003544         MOVE RAD-KDINLSTA     TO TAB-KDINLSTA(TAB-IX)                    
003545         MOVE RAD-IDINLVGN     TO TAB-IDINLVGN(TAB-IX)                    
003546         MOVE RAD-ADINLOMR-NXT TO TAB-ADINLOMR-NXT(TAB-IX)                
003547         MOVE RAD-FLSATS       TO TAB-FLSATS(TAB-IX)                      
003548         MOVE RAD-FLKVAANT     TO TAB-FLKVAANT(TAB-IX)                    
003549         IF RAD-FLSATS         =  JA                                      
003550           IF WS-ENGLISH-TEXT                                             
003551             MOVE YES          TO TAB-FLSATS  (TAB-IX)                    
003552           ELSE                                                           
003553             MOVE JA           TO TAB-FLSATS  (TAB-IX)                    
003554           END-IF                                                         
003555         END-IF                                                           
003556         IF RAD-FLKVAANT       =  JA                                      
003557           IF WS-ENGLISH-TEXT                                             
003558             MOVE YES          TO TAB-FLKVAANT(TAB-IX)                    
003559           ELSE                                                           
003560             MOVE JA           TO TAB-FLKVAANT(TAB-IX)                    
003561           END-IF                                                         
003562         END-IF                                                           
003563         IF RAD-FLPRIO         =  JA                                      
003564             MOVE 'P'          TO TAB-KDKLIPRI(TAB-IX)                    
003565         ELSE                                                             
003566             MOVE 'N'          TO TAB-KDKLIPRI(TAB-IX)                    
003567         END-IF                                                           
003568     ELSE                                                                 
003569         IF RAD-IDOKOLLI       >  ZERO                                    
003570             ADD +1            TO TAB-KVKOLLI(TAB-IX)                     
003571         END-IF                                                           
003572         ADD RAD-KVINLART      TO TAB-KVINLART(TAB-IX)                    
003573     END-IF                                                               
003574     .                                                                    
003575     EJECT                                                                
003576 S02A-TA-FRAM-KDINLOMR   SECTION.                                         
003577                                                                          
003578     MOVE SPACE                TO TAB-PLAC-KDINLOMR(TAB-IX)               
003579                                  TAB-ADR-KDINLOMR(TAB-IX)                
003580     IF RAD-ADINLOMR                 = SPACE                              
003581         CONTINUE                                                         
003582     ELSE                                                                 
003583         MOVE  RAD-ADINLOMR          TO W-6006-ADINLOMR                   
003584         PERFORM IMS-GU-PLAA-PLAA11                                       
003585         IF SEGMENT-FINNS                                                 
003586             MOVE PLAA-6006-KDINLOMR TO TAB-PLAC-KDINLOMR(TAB-IX)         
003587         END-IF                                                           
003588     END-IF                                                               
003589                                                                          
003590     IF RAD-ADINLOMR-NXT             = SPACE                              
003591         CONTINUE                                                         
003592     ELSE                                                                 
003593         MOVE  RAD-ADINLOMR-NXT      TO W-6006-ADINLOMR                   
003594         PERFORM IMS-GU-PLAA-PLAA11                                       
003595         IF SEGMENT-FINNS                                                 
003596             MOVE PLAA-6006-KDINLOMR TO TAB-ADR-KDINLOMR(TAB-IX)          
003597         END-IF                                                           
003598     END-IF                                                               
003599     .                                                                    
003600     EJECT                                                                
003601 S03-SUDDA-UT-TAB-RAD SECTION.                                            
003602                                                                          
003603* SUDDA UT DEN RAD I TABELLEN SOM MAN PRECIS FLYTTAT TILL                 
003604* MODEN, FÖR ATT DEN EJ SKALL KOMMA UT FLERA GÅNGER                       
003605                                                                          
003606     MOVE ZERO                 TO TAB-KVINLART(TAB-IX)                    
003607                                  TAB-KVKOLLI (TAB-IX)                    
003608                                  TAB-IDINLVGN(TAB-IX)                    
003609     MOVE ALL '+'              TO TAB-ADINLOMR(TAB-IX)                    
003610                                  TAB-KDINLSTA(TAB-IX)                    
003611                                  TAB-ADINLOMR-NXT(TAB-IX)                
003612     MOVE SPACE                TO TAB-KDKLIPRI(TAB-IX)                    
003613                                  TAB-FLSATS(TAB-IX)                      
003614                                  TAB-FLKVAANT(TAB-IX)                    
003615                                  TAB-PLAC-KDINLOMR(TAB-IX)               
003616                                  TAB-ADR-KDINLOMR(TAB-IX)                
003617     .                                                                    
003618     EJECT                                                                
003619 S04-KOLLA-KDINLSTA   SECTION.                                            
003620                                                                          
003621     IF RAD-KDINLSTA           = SPACE OR 'FPK' OR 'SAK'                  
003622         CONTINUE                                                         
003623     ELSE                                                                 
003624         MOVE NEJ                 TO INDATA-SW                            
003625         MOVE ERR-UPD-NOT-ALLOWED TO RESP-IDMSG-ERROR                     
003626     END-IF                                                               
003627     .                                                                    
003628     EJECT                                                                
003629 S05-KOLLA-ADINLOMR-PRT     SECTION.                                      
003630                                                                          
003631     IF REQU-ADINLOMR-PRT         =  ALL '+'                              
003632         MOVE ERR-WRONG-PRINTER   TO RESP-IDMSG-ERROR                     
003633         MOVE NEJ                 TO INDATA-SW                            
003634         MOVE MFS-ALFA-FAELT-FEL  TO RESP-ADINLOMR-PRT-ATTR               
003635     ELSE                                                                 
003636         MOVE REQU-ADINLOMR-PRT TO PRT-IDPRTLST(3:6)                      
003637         MOVE 1                TO PRT-KDCALL                              
003638         CALL W006PRT  USING PRT-W006PRT                                  
003639                                                                          
003640         IF PRT-KDSVAR                 = 'F'                              
003641             MOVE ERR-WRONG-PRINTER    TO RESP-IDMSG-ERROR                
003642             MOVE NEJ                  TO INDATA-SW                       
003643             MOVE MFS-ALFA-FAELT-FEL   TO RESP-ADINLOMR-PRT-ATTR          
003644         ELSE                                                             
003645             MOVE PRT-BEPRTLST         TO RESP-BEPRTLST                   
003646             MOVE REQU-ADINLOMR-PRT    TO W-6006-ADINLOMR                 
003647             PERFORM IMS-GU-PLAA-PLAA11                                   
003648             IF SEGMENT-SAKNAS                                            
003649                 MOVE ERR-WRONG-PRINTER  TO RESP-IDMSG-ERROR              
003650                 MOVE NEJ                TO INDATA-SW                     
003651                 MOVE MFS-ALFA-FAELT-FEL TO RESP-ADINLOMR-PRT-ATTR        
003652             ELSE                                                         
003653                 MOVE PLAA-6006-IDLEVNR  TO W-SPAR-IDLEVNR                
003654             END-IF                                                       
003655         END-IF                                                           
003656     END-IF                                                               
003657     .                                                                    
003658     EJECT                                                                
003659 S06-KOLLA-KVINLART   SECTION.                                            
003660                                                                          
003661     IF REQU-KVINLART            = ALL '+' OR                             
003662        REQU-KVINLART            NOT NUMERIC                              
003663         MOVE NEJ                TO INDATA-SW                             
003664         MOVE MFS-NUM-FAELT-FEL  TO RESP-KVINLART-INM-ATTR                
003665     ELSE                                                                 
003666         MOVE REQU-KVINLART         TO W-KVINLART-UPD                     
003667         IF ((REQU-KDCMDVAL          = 'SEK')                             
003668             AND W-KVINLART-UPD         >  RAD-KVINLART) OR               
003669            (W-KVINLART-UPD         =  RAD-KVINLART AND                   
003670            (REQU-KDCMDVAL          =  'MOT' OR 'REC'))                   
003671             MOVE ERR-WRONG-QTY     TO RESP-IDMSG-ERROR                   
003672             MOVE NEJ               TO INDATA-SW                          
003673             MOVE MFS-NUM-FAELT-FEL TO RESP-KVINLART-INM-ATTR             
003674         END-IF                                                           
003675     END-IF                                                               
003676     .                                                                    
003677     EJECT                                                                
003678 S07-SKAPA-6191      SECTION.                                             
003679                                                                          
003680     MOVE 'W6013300'           TO MOD6191-MID-IDPGM                       
003681     MOVE DCS-IDDC             TO MOD6191-MID-IDDC                        
003682     MOVE ART-IDLOPNRM         TO MOD6191-MID-IDLOPNRM (6191-IX)          
003683     MOVE RAD-IDRADNR          TO MOD6191-MID-IDRADNR  (6191-IX)          
003684     MOVE ART-PRARTSTD         TO MOD6191-MID-PRARTSTD (6191-IX)          
003685     MOVE ART-KDINLPRIO        TO MOD6191-MID-KDINLPRIO(6191-IX)          
003686     MOVE +0                   TO MOD6191-MID-KVKOLLI  (6191-IX)          
003687     MOVE 'N'                  TO MOD6191-MID-FLINLI   (6191-IX)          
003688                                                                          
003689     MOVE SPAR-RAD-ADINLOMR         TO MOD6191-MID-ADINLOMR-OLD           
003690                                                       (6191-IX)          
003691     MOVE SPAR-RAD-ADINLOMR-NXT                                           
003692                                 TO MOD6191-MID-ADINLOMR-NXT-OLD          
003693                                                   (6191-IX)              
003694     MOVE SPAR-RAD-KDINLSTA      TO MOD6191-MID-KDINLSTA-OLD              
003695                                                   (6191-IX)              
003696     MOVE SPAR-RAD-KVINLART      TO MOD6191-MID-KVINLART-OLD              
003697                                                   (6191-IX)              
003698                                                                          
003699     MOVE RAD-ADINLOMR         TO MOD6191-MID-ADINLOMR-NEW                
003700                                                   (6191-IX)              
003701     MOVE RAD-ADINLOMR-NXT     TO MOD6191-MID-ADINLOMR-NXT-NEW            
003702                                                   (6191-IX)              
003703     MOVE RAD-KDINLSTA         TO MOD6191-MID-KDINLSTA-NEW                
003704                                                   (6191-IX)              
003705     MOVE RAD-KVINLART         TO MOD6191-MID-KVINLART-NEW                
003706                                                   (6191-IX)              
003707     ADD +1                    TO 6191-IX                                 
003708     .                                                                    
003709     EJECT                                                                
003710 S08-REDIG-IDANSTNR SECTION.                                              
003711                                                                          
003712**   IF QTY IS UPDATED FROM WEB, IT IS MANDATORY TO HAVE USERID           
003713**   UPDATED AND THIS NEED TO BE UPDATED FOR IDANSTNR                     
003714     IF REQU-IDMSGVER = 1 AND                                             
003715        REQU-KDCMDVAL = 'REC'                                             
003716       MOVE REQU-IDUSER-IN       TO WS-USERID                             
003717     ELSE                                                                 
003718       MOVE MSG-SIGNON-USERID      TO WS-USERID                           
003719     END-IF                                                               
003720                                                                          
003721     MOVE ZERO                   TO WS-IDANSTNR                           
003722     MOVE K-USERID-LNG           TO IX-USERID                             
003723     MOVE K-IDANSTNR-LNG         TO IX-IDANSTNR                           
003724                                                                          
003725     PERFORM UNTIL (IX-USERID    = ZERO                                   
003726                OR  IX-IDANSTNR  = ZERO)                                  
003727                                                                          
003728       IF  WS-USERID-TKN (IX-USERID) NUMERIC                              
003729         MOVE WS-USERID-TKN (IX-USERID)                                   
003730                                 TO WS-IDANSTNR-TKN (IX-IDANSTNR)         
003731         SUBTRACT 1              FROM IX-IDANSTNR                         
003732       END-IF                                                             
003733       SUBTRACT 1                FROM IX-USERID                           
003734     END-PERFORM                                                          
003735     .                                                                    
003736     EJECT                                                                
003737 S09-ANTALSKONTROLL SECTION.                                              
003738                                                                          
003739     MOVE JA                   TO ANTALSKONTR-SW                          
003740     MOVE ZERO                 TO W-KVINLART                              
003741                                  W-KVAANT                                
003742                                  W-KVAVIS-VERKL                          
003743     PERFORM IMS-GNP-W6INLA21-B-FIRST                                     
003744     PERFORM UNTIL SEGMENT-SAKNAS                                         
003745         IF RAD-FLKVAANT         = JA                                     
003746             COMPUTE W-KVINLART = W-KVINLART + RAD-KVINLART               
003747         END-IF                                                           
003748         IF ((RAD-KDINLSTA     = SPACE OR 'FPK') AND                      
003749              RAD-FLKVAANT     NOT = JA)              OR                  
003750              RAD-KDINLSTA     = 'SAK'                                    
003751               MOVE NEJ        TO ANTALSKONTR-SW                          
003752         END-IF                                                           
003753         IF RAD-KDINLSTA       =  'ANT' OR 'AVV'                          
003754             COMPUTE W-KVAANT  =  W-KVAANT + RAD-KVINLART                 
003755             MOVE RAD-IDANSTNR TO WS-IDANSTNR                             
003756         END-IF                                                           
003757         IF RAD-KDINLSTA       = 'FPK' OR 'SAK' OR 'INL' OR 'VOR'         
003758                                  OR SPACE OR 'KVA' OR 'RET' OR           
003759                                 'FRD'                                    
003760             COMPUTE W-KVAVIS-VERKL =                                     
003761                     W-KVAVIS-VERKL + RAD-KVINLART                        
003762         END-IF                                                           
003763         PERFORM IMS-GNP-W6INLA21-OKVAL-B                                 
003764     END-PERFORM                                                          
003765                                                                          
003766     IF W-KVAANT         NOT ZERO                                         
003767       PERFORM S09C-KOLLA-OM-TRANS-6202                                   
003768       IF 6202-TRANS                                                      
003769         PERFORM S09A-STARTA-6202                                         
003770       END-IF                                                             
003771     ELSE                                                                 
003772       IF ANTALSKONTR-KLAR                                                
003773         IF ART-KDKVAANT > +0                                             
003774           PERFORM S09B-UPD-KVAE                                          
003775         END-IF                                                           
003776       END-IF                                                             
003777     END-IF                                                               
003778     .                                                                    
003779     EJECT                                                                
003780 S09A-STARTA-6202      SECTION.                                           
003781                                                                          
003782     IF ART-KDRT     = 3 OR 6 OR 7 OR 8 OR 77                             
003783       MOVE NEJ                  TO FL-KR                                 
003784     ELSE                                                                 
003785       MOVE ALL '+'              TO 6202-AREA                             
003786       MOVE ZERO                 TO 6202-REQU-IDKR-KEY                    
003787       MOVE REQU-IDDC-KEY        TO 6202-REQU-IDDC-KEY                    
003788       MOVE WS-IDLOPNRM-NUM      TO 6202-REQU-IDLOPNRM-UPD                
003789       MOVE SEQB-TIAVIDAT        TO 6202-REQU-TIAVSDAT-UPD                
003790       IF WS-IDANSTNR = ZERO OR WS-IDANSTNR = ALL '+'                     
003791         MOVE MSG-SIGNON-USERID  TO 6202-REQU-BEKRBEH-UPD                 
003792       ELSE                                                               
003793         MOVE WS-IDANSTNR        TO 6202-REQU-BEKRBEH-UPD                 
003794       END-IF                                                             
003795       MOVE W-KVKRBEH            TO 6202-REQU-KVKRBEH-UPD                 
003796                                                                          
003797       IF REQU-KDCMDVAL = 'KR ' OR 'IR '                                  
003798         MOVE ART-KVAVIS         TO 6202-REQU-KVANTMOT-UPD                
003799                                    6202-REQU-KVART-RET-UPD               
003800         MOVE 1                  TO 6202-REQU-KDDISP-UPD                  
003801         MOVE '4'                TO 6202-REQU-KDKRUTF-UPD                 
003802       ELSE                                                               
003803         MOVE W-KVAVIS-VERKL     TO 6202-REQU-KVANTMOT-UPD                
003804       END-IF                                                             
003805                                                                          
003806       COMPUTE 6202-REQU-KVLL    = LENGTH OF 6202-AREA                    
003807       MOVE LOW-VALUE            TO 6202-REQU-KDZ1 6202-REQU-KDZ2         
003808       SET 6202-REQU-UPD-X       TO TRUE                                  
003809       MOVE 'W6W202T '           TO 6202-REQU-KDTRANS                     
003810       MOVE 101                  TO 6202-REQU-IDMSGVER                    
003811       MOVE REQU-IDUSER          TO 6202-REQU-IDUSER                      
003812                                                                          
003813       PERFORM IMS-ISRT-6202-MSG                                          
003814     END-IF                                                               
003815     .                                                                    
003816     EJECT                                                                
003817 S09B-UPD-KVAE         SECTION.                                           
003818                                                                          
003819     IF ART-KDKVAANT = 1                                                  
003820         MOVE NEJ          TO TRAFF-SW                                    
003821         MOVE LOW-VALUE    TO W-W6H7BSEQ-MIN-X                            
003822         MOVE HIGH-VALUE   TO W-W6H7BSEQ-MAX-X                            
003823         MOVE ART-IDARTNR  TO W-IDARTNR-H7-MIN                            
003824                              W-IDARTNR-H7-MAX                            
003825         MOVE ART-IDLOPNRM TO W-IDLOPNRM-BSEQ                             
003826                                                                          
003827         PERFORM IMS-GU-INLC-W6D1B                                        
003828         MOVE SEQB-IDLEVNR TO W-IDLEVNR-H7-MIN                            
003829                              W-IDLEVNR-H7-MAX                            
003830         MOVE +1           TO W-KVKRKNTR-H7-MIN                           
003831         MOVE +9           TO W-KVKRKNTR-H7-MAX                           
003832         PERFORM IMS-GHU-KVABSEQ-W6H701                                   
003833         PERFORM UNTIL TRAFF OR SEGMENT-SAKNAS                            
003834           IF SEGMENT-FINNS                                               
003835               IF KVAE-KR-IDKRFEL   = 'PA' OR 'PB' OR 'K'                 
003836                   IF KVAE-KR-KVKRKNTR > ZERO                             
003837                       SUBTRACT 1      FROM KVAE-KR-KVKRKNTR              
003838                       PERFORM IMS-REPL-KVABSEQ-W6H7                      
003839                       MOVE JA TO TRAFF-SW                                
003840                   END-IF                                                 
003841               END-IF                                                     
003842           END-IF                                                         
003843           PERFORM IMS-GHN-KVABSEQ-W6H701                                 
003844         END-PERFORM                                                      
003845     END-IF                                                               
003846     .                                                                    
003847     EJECT                                                                
003848 S09C-KOLLA-OM-TRANS-6202   SECTION.                                      
003849                                                                          
003850     MOVE JA TO 6202-SW                                                   
003851                                                                          
003852     MOVE ART-IDLOPNRM TO W-IDLOPNRM-BSEQ                                 
003853     PERFORM IMS-GU-INLC-W6D1B                                            
003854                                                                          
003855     IF ART-KDRT     = 3 OR 6 OR 7 OR 8 OR 77                             
003856       MOVE NEJ TO 6202-SW                                                
003857       MOVE NEJ TO FL-KR                                                  
003858     ELSE                                                                 
003859       MOVE SEQB-IDLOPNRM        TO W-IDLOPNRM-H7                         
003860       MOVE SEQB-TIAVIDAT        TO W-DAAVSDAT-H7                         
003861       IF SEQB-TIAVIDAT NOT = ZERO                                        
003862         IF SEQB-TIAVIDAT < 500000                                        
003863           MOVE 20               TO W-DAAVSDAT-H7 (1:2)                   
003864         ELSE                                                             
003865           IF SEQB-TIAVIDAT < 999999                                      
003866             MOVE 19             TO W-DAAVSDAT-H7 (1:2)                   
003867           ELSE                                                           
003868             MOVE 99999999       TO W-DAAVSDAT-H7                         
003869           END-IF                                                         
003870         END-IF                                                           
003871       END-IF                                                             
003872       PERFORM IMS-GHU-KVAE-W6H701                                        
003873       IF SEGMENT-FINNS                                                   
003874           IF KVAE-KR-IDKRFEL          = 'PA' OR 'PB' OR 'K '             
003875               IF (KVAE-KR-KDKRSTA = '0' OR '1' AND                       
003876                  KVAE-KR-FLANNULL = NEJ) OR                              
003877                  (KVAE-KR-KDKRSTA > '1'        AND                       
003878                   KVAE-KR-FLKRGODK = NEJ )                               
003879                 CONTINUE                                                 
003880               ELSE                                                       
003881                 MOVE NEJ                TO 6202-SW                       
003882               END-IF                                                     
003883           ELSE                                                           
003884               PERFORM IMS-GHN-KVAE-W6H701                                
003885               IF SEGMENT-FINNS                                           
003886                 IF KVAE-KR-IDKRFEL = 'PA' OR 'PB' OR 'K '                
003887                       IF (KVAE-KR-KDKRSTA = '0' OR '1' AND               
003888                        KVAE-KR-FLANNULL = NEJ) OR                        
003889                        (KVAE-KR-KDKRSTA > '1'        AND                 
003890                         KVAE-KR-FLKRGODK = NEJ )                         
003891                       CONTINUE                                           
003892                     ELSE                                                 
003893                       MOVE NEJ                TO 6202-SW                 
003894                     END-IF                                               
003895                 END-IF                                                   
003896             END-IF                                                       
003897           END-IF                                                         
003898       END-IF                                                             
003899     END-IF                                                               
003900     .                                                                    
003901     EJECT                                                                
003902 S10-KOLLA-KDPRTVAL SECTION.                                              
003903                                                                          
003904     IF (REQU-KDPRTVAL = YES                                              
003905     OR  REQU-KDPRTVAL = JA)                                              
003906       MOVE MSGI-IDARTNR     TO W-IDARTNR                                 
003907       PERFORM IMS-GU-WDK611                                              
003908       IF  SEGMENT-FINNS                                                  
003909       AND CLAG-ADLAGOMR-SVS > ZERO                                       
003910         CONTINUE                                                         
003911       ELSE                                                               
003912         MOVE ERR-CORR-HILITE-FLDS TO RESP-IDMSG-ERROR                    
003913         MOVE NEJ            TO INDATA-SW                                 
003914         MOVE MFS-ALFA-FAELT-FEL                                          
003915                             TO RESP-KDPRTVAL-ATTR                        
003916       END-IF                                                             
003917     END-IF                                                               
003918     .                                                                    
003919     EJECT                                                                
003920 S11-SKAPA-6191-MID-NY-RAD    SECTION.                                    
003921                                                                          
003922     MOVE 'W6013300'           TO MOD6191-MID-IDPGM                       
003923     MOVE DCS-IDDC             TO MOD6191-MID-IDDC                        
003924     MOVE ART-IDLOPNRM         TO MOD6191-MID-IDLOPNRM (6191-IX)          
003925     MOVE RAD-IDRADNR          TO MOD6191-MID-IDRADNR  (6191-IX)          
003926     MOVE ART-PRARTSTD         TO MOD6191-MID-PRARTSTD (6191-IX)          
003927     MOVE ART-KDINLPRIO        TO MOD6191-MID-KDINLPRIO(6191-IX)          
003928     MOVE +0                   TO MOD6191-MID-KVKOLLI  (6191-IX)          
003929     MOVE 'N'                  TO MOD6191-MID-FLINLI   (6191-IX)          
003930                                                                          
003931     MOVE SPACE                TO MOD6191-MID-ADINLOMR-OLD                
003932                                                   (6191-IX)              
003933                                  MOD6191-MID-ADINLOMR-NXT-OLD            
003934                                                   (6191-IX)              
003935                                  MOD6191-MID-KDINLSTA-OLD                
003936                                                   (6191-IX)              
003937     MOVE ZERO                 TO MOD6191-MID-KVINLART-OLD                
003938                                                   (6191-IX)              
003939                                                                          
003940     MOVE RAD-ADINLOMR         TO MOD6191-MID-ADINLOMR-NEW                
003941                                                   (6191-IX)              
003942     MOVE RAD-ADINLOMR-NXT     TO MOD6191-MID-ADINLOMR-NXT-NEW            
003943                                                   (6191-IX)              
003944     MOVE RAD-KDINLSTA         TO MOD6191-MID-KDINLSTA-NEW                
003945                                                   (6191-IX)              
003946     MOVE RAD-KVINLART         TO MOD6191-MID-KVINLART-NEW                
003947                                                   (6191-IX)              
003948     ADD +1                    TO 6191-IX                                 
003949     .                                                                    
003950     EJECT                                                                
003951 S12-TA-EV-UT-NYTT-KOLLIID    SECTION.                                    
003952                                                                          
003953     IF (RAD-IDLEVNR-KOLLI            >= '99400' AND                      
003954         RAD-IDLEVNR-KOLLI            <= '99599')                         
003955         CONTINUE                                                         
003956     ELSE                                                                 
003957       IF RAD-FLDIVKLI = JA                                               
003958         PERFORM IMS-GHU-LOPA-LOPA11                                      
003959         COMPUTE LOPA-6018-IDOKOLLI   = LOPA-6018-IDOKOLLI + 1            
003960         PERFORM IMS-REPL-LOPA-LOPA11                                     
003961         MOVE W-SPAR-IDLEVNR          TO RAD-IDLEVNR-KOLLI                
003962                                         RESP-IDLEVNR-KOLLI               
003963                                         W-IDLEVNR-KOLLI                  
003964         MOVE LOPA-6018-IDOKOLLI      TO RAD-IDOKOLLI                     
003965                                         W-IDOKOLLI                       
003966                                         RESP-IDOKOLLI                    
003967         INSPECT RESP-IDOKOLLI    REPLACING LEADING ZERO BY SPACE         
003968         MOVE JA                      TO NYTT-KOLLIID-SW                  
003969       END-IF                                                             
003970     END-IF                                                               
003971     .                                                                    
003972     EJECT                                                                
003973 S13-STARTA-6195             SECTION.                                     
003974                                                                          
003975     MOVE 'W6013300'           TO MOD6195-MID-IDPGM                       
003976     MOVE PRT-IDPRTLST         TO MOD6195-MID-IDPRTLST                    
003977     MOVE +1                   TO MOD6195-MID-KVPOST                      
003978                                  6195-IX                                 
003979     MOVE ART-IDLOPNRM         TO MOD6195-MID-IDLOPNRM  (6195-IX)         
003980     MOVE RAD-IDRADNR          TO MOD6195-MID-IDRADNR   (6195-IX)         
003981     MOVE ART-IDARTNR          TO MOD6195-MID-IDARTNR   (6195-IX)         
003982     MOVE RAD-KVINLART         TO MOD6195-MID-KVINLART  (6195-IX)         
003983     MOVE ART-BEART            TO MOD6195-MID-BEART     (6195-IX)         
003984                                                                          
003985     IF (REQU-KDPRTVAL = YES                                              
003986     OR  REQU-KDPRTVAL = JA)                                              
003987       MOVE CLAG-ADLAGOMR-SVS  TO MOD6195-MID-ADLAGOMR  (6195-IX)         
003988       MOVE CLAG-ADGANG-SVS    TO MOD6195-MID-ADGANG    (6195-IX)         
003989       MOVE CLAG-ADPLATS-SVS   TO MOD6195-MID-ADPLATS   (6195-IX)         
003990     ELSE                                                                 
003991       MOVE ART-ADLAGOMR       TO MOD6195-MID-ADLAGOMR  (6195-IX)         
003992       MOVE ART-ADGANG         TO MOD6195-MID-ADGANG    (6195-IX)         
003993       MOVE ART-ADPLATS        TO MOD6195-MID-ADPLATS   (6195-IX)         
003994     END-IF                                                               
003995                                                                          
003996     COMPUTE P-TO-P-MSG-KVLL   =  LNG-P-TO-P-PREFIX +                     
003997                                   23 + (MOD6195-MID-KVPOST * 61)         
003998     MOVE 'W6T195X '           TO P-TO-P-MSG-KDTRANS                      
003999     MOVE '6133'               TO P-TO-P-MSG-IDTRANS                      
004000     IF REQU-IDSPRAK = 'SV'                                               
004001        MOVE '1'               TO P-TO-P-MSG-KDMFSFOR                     
004002     ELSE                                                                 
004003        MOVE '2'               TO P-TO-P-MSG-KDMFSFOR                     
004004     END-IF                                                               
004005     MOVE MOD6195-MID-W6I19501 TO P-TO-P-MSG-INDATA                       
004006     PERFORM IMS-ISRT-6195-MSG                                            
004007     .                                                                    
004008     EJECT                                                                
004009 S14-STARTA-6193    SECTION.                                              
004010                                                                          
004011     ACCEPT DAGENS-DATUM       FROM DATE                                  
004012     ACCEPT DAGENS-TID         FROM TIME                                  
004013                                                                          
004014     MOVE SPACE                TO MSG-KOM-WMSGKOM                         
004015     COMPUTE MSG-KOM-KVLL = LENGTH OF MSG-KOM-WMSGKOM                     
004016     MOVE LOW-VALUE            TO MSG-KOM-KDZ1                            
004017     MOVE LOW-VALUE            TO MSG-KOM-KDZ2                            
004018     MOVE SPACE                TO MSG-KOM-KDTRANS                         
004019     MOVE 'W6I19301'           TO MSG-KOM-IDCPYTXT                        
004020     MOVE 'INLEV   '           TO MSG-KOM-IDSNDNOD                        
004021     MOVE 'W6013300'           TO MSG-KOM-IDSNDJOB                        
004022     MOVE DAGENS-DATUM         TO MSG-KOM-TIREGDAT                        
004023     MOVE DAGENS-TID           TO MSG-KOM-TIKLOCK                         
004024     MOVE SPACE                TO MSG-KOM-IDMFSMED                        
004025                                                                          
004026     MOVE ART-IDLOPNRM         TO MOD6193-MID-IDLOPNRM                    
004027     MOVE W-6193-IDRADNR       TO MOD6193-MID-IDRADNR                     
004028                                                                          
004029     COMPUTE P-TO-P-MSG-KVLL   =  LNG-P-TO-P-PREFIX + 12                  
004030     MOVE 'W6T193X '           TO P-TO-P-MSG-KDTRANS                      
004031     MOVE '6133'               TO P-TO-P-MSG-IDTRANS                      
004032     IF REQU-IDSPRAK = 'SV'                                               
004033        MOVE '1'               TO P-TO-P-MSG-KDMFSFOR                     
004034     ELSE                                                                 
004035        MOVE '2'               TO P-TO-P-MSG-KDMFSFOR                     
004036     END-IF                                                               
004037                                                                          
004038     MOVE MOD6193-MID-W6I19301 TO P-TO-P-MSG-INDATA                       
004039                                                                          
004040     CALL W006KOM USING MSG-PCB                                           
004041                        DISP-PCB                                          
004042                        KOM-KOMA-PCB                                      
004043                        MSG-KOM-WMSGKOM                                   
004044                        P-TO-P-MSG-IO-AREA-SNUF                           
004045     .                                                                    
004046     EJECT                                                                
004047 S15-STARTA-W60194         SECTION.                                       
004048                                                                          
004049     MOVE ART-IDLOPNRM    TO W-IDLOPNRM-BSEQ                              
004050                                                                          
004051     PERFORM IMS-GU-INLC-W6D1B                                            
004052     MOVE SEQB-IDDC        TO W-D101KY-IDDC                               
004053     MOVE SEQB-IDLEVNR     TO W-D101KY-IDLEVNR                            
004054     MOVE SEQB-IDFS        TO W-D101KY-IDFS                               
004055     MOVE SEQB-TIAVIDAT    TO W-D101KY-TIAVIDAT                           
004056                                                                          
004057     PERFORM IMS-GU-INLA1-INLA01                                          
004058                                                                          
004059     IF REQU-IDMSGVER = 1 AND PRT-IDPRTLST = '6F      '                   
004060       CONTINUE                                                           
004061     ELSE                                                                 
004062       MOVE PRT-IDPRTLST         TO MOD6194-MID-IDPRTLST                  
004063       MOVE 'W6013300'           TO MOD6194-MID-IDPGM                     
004064       MOVE +1                   TO MOD6194-MID-KVPOST                    
004065       MOVE RAD-IDLEVNR-KOLLI    TO MOD6194-MID-IDLEVNR-KOLLI(1)          
004066       MOVE RAD-IDOKOLLI         TO MOD6194-MID-IDOKOLLI(1)               
004067                                                                          
004068       IF (REQU-KDPRTVAL = YES                                            
004069       OR  REQU-KDPRTVAL = JA)                                            
004070         MOVE CLAG-ADLAGOMR-SVS  TO MOD6194-MID-ADLAGOMR(1)               
004071         MOVE CLAG-ADGANG-SVS    TO MOD6194-MID-IDARTNR (1)               
004072         MOVE CLAG-ADPLATS-SVS   TO MOD6194-MID-IDLOPNRM(1)               
004073       ELSE                                                               
004074         MOVE ART-ADLAGOMR       TO MOD6194-MID-ADLAGOMR(1)               
004075         MOVE ART-IDARTNR        TO MOD6194-MID-IDARTNR (1)               
004076         MOVE ART-IDLOPNRM       TO MOD6194-MID-IDLOPNRM(1)               
004077       END-IF                                                             
004078       MOVE RAD-KVINLART         TO MOD6194-MID-KVINLART(1)               
004079       MOVE INL-TIINLMOT         TO MOD6194-MID-TIINLMOT(1)               
004080                                                                          
004081       COMPUTE W-VKKOLLIN = RAD-KVINLART * ART-VKART / 1000               
004082                                                                          
004083       MOVE W-VKKOLLIN           TO MOD6194-MID-VKKOLLIN(1)               
004084       MOVE ZERO                 TO MOD6194-MID-VKKOLLIB(1)               
004085       MOVE ART-ADGANG           TO MOD6194-MID-ADGANG  (1)               
004086       MOVE ART-ADPLATS          TO MOD6194-MID-ADPLATS (1)               
004087       MOVE ART-KDSORT           TO MOD6194-MID-KDSORT  (1)               
004088       MOVE ART-BEFT             TO MOD6194-MID-BEFT    (1)               
004089                                                                          
004090       COMPUTE P-TO-P-MSG-KVLL   =  LNG-P-TO-P-PREFIX +                   
004091                                    23 + (MOD6194-MID-KVPOST * 67)        
004092       MOVE 'W6T194X '           TO P-TO-P-MSG-KDTRANS                    
004093       MOVE '6133'               TO P-TO-P-MSG-IDTRANS                    
004094       IF REQU-IDSPRAK = 'SV'                                             
004095          MOVE '1'               TO P-TO-P-MSG-KDMFSFOR                   
004096       ELSE                                                               
004097          MOVE '2'               TO P-TO-P-MSG-KDMFSFOR                   
004098       END-IF                                                             
004099                                                                          
004100       MOVE MOD6194-MID-W6I19401 TO P-TO-P-MSG-INDATA                     
004101                                                                          
004102       PERFORM IMS-ISRT-6194-MSG                                          
004103     END-IF                                                               
004104     .                                                                    
004105     EJECT                                                                
004106 S16-CALL-W611PMRK      SECTION.                                          
004107                                                                          
004108     IF RAD-KDINLSTA = '   ' OR 'FPK'                                     
004109       IF RAD-IDLEVNR-KOLLI    NOT = SPACE AND                            
004110          RAD-IDOKOLLI             >  ZERO                                
004111           MOVE RAD-IDLEVNR-KOLLI  TO PMRK-IDLEVNR                        
004112           MOVE RAD-IDOKOLLI       TO PMRK-IDOKOLLI                       
004113           MOVE ZERO               TO PMRK-IDLOPNRM                       
004114                                      PMRK-IDRADNR                        
004115       ELSE                                                               
004116           MOVE SPACE              TO PMRK-IDLEVNR                        
004117           MOVE ZERO               TO PMRK-IDOKOLLI                       
004118           MOVE ART-IDLOPNRM       TO PMRK-IDLOPNRM                       
004119           MOVE RAD-IDRADNR        TO PMRK-IDRADNR                        
004120       END-IF                                                             
004121     ELSE                                                                 
004122         MOVE SPACE              TO PMRK-IDLEVNR                          
004123         MOVE ZERO               TO PMRK-IDOKOLLI                         
004124         MOVE ART-IDLOPNRM       TO PMRK-IDLOPNRM                         
004125         MOVE RAD-IDRADNR        TO PMRK-IDRADNR                          
004126     END-IF                                                               
004127     CALL W611PMRK USING PMRK-W611PMRK PMRK-INLB-PCB                      
004128                         PMRK-INLC-PCB PMRK-PLAA-PCB                      
004129     .                                                                    
004130     EJECT                                                                
004131 S17-SKAPA-6191-BORT-RAD    SECTION.                                      
004132                                                                          
004133     MOVE 'W6013300'           TO MOD6191-MID-IDPGM                       
004134     MOVE DCS-IDDC             TO MOD6191-MID-IDDC                        
004135     MOVE ART-IDLOPNRM         TO MOD6191-MID-IDLOPNRM (6191-IX)          
004136     MOVE RAD-IDRADNR          TO MOD6191-MID-IDRADNR  (6191-IX)          
004137     MOVE ART-PRARTSTD         TO MOD6191-MID-PRARTSTD (6191-IX)          
004138     MOVE ART-KDINLPRIO        TO MOD6191-MID-KDINLPRIO(6191-IX)          
004139     MOVE +0                   TO MOD6191-MID-KVKOLLI  (6191-IX)          
004140     MOVE 'N'                  TO MOD6191-MID-FLINLI   (6191-IX)          
004141                                                                          
004142     MOVE RAD-ADINLOMR         TO MOD6191-MID-ADINLOMR-OLD                
004143                                                   (6191-IX)              
004144     MOVE RAD-ADINLOMR-NXT     TO MOD6191-MID-ADINLOMR-NXT-OLD            
004145                                                   (6191-IX)              
004146     MOVE RAD-KDINLSTA         TO MOD6191-MID-KDINLSTA-OLD                
004147                                                   (6191-IX)              
004148     MOVE RAD-KVINLART         TO MOD6191-MID-KVINLART-OLD                
004149                                                   (6191-IX)              
004150                                                                          
004151     MOVE RAD-ADINLOMR         TO MOD6191-MID-ADINLOMR-NEW                
004152                                                   (6191-IX)              
004153     MOVE RAD-ADINLOMR-NXT     TO MOD6191-MID-ADINLOMR-NXT-NEW            
004154                                                   (6191-IX)              
004155     MOVE RAD-KDINLSTA         TO MOD6191-MID-KDINLSTA-NEW                
004156                                                   (6191-IX)              
004157     MOVE ZERO                 TO MOD6191-MID-KVINLART-NEW                
004158                                                   (6191-IX)              
004159     ADD +1                    TO 6191-IX                                 
004160     .                                                                    
004161     EJECT                                                                
004162 S20-UPD-EV-OEVRIGA-INM-FAELT SECTION.                                    
004163                                                                          
004164     MOVE NEJ                  TO 6191-SW                                 
004165     IF RAD-KDINLSTA           =  'SAK'                                   
004166         MOVE SPACE            TO RAD-KDINLSTA                            
004167         MOVE JA               TO 6191-SW                                 
004168     END-IF                                                               
004169                                                                          
004170     IF REQU-FLSATS            = SPACE OR ALL '+'                         
004171         CONTINUE                                                         
004172     ELSE                                                                 
004173         MOVE REQU-FLSATS      TO RAD-FLSATS                              
004174     END-IF                                                               
004175                                                                          
004176     IF REQU-ADINLOMR          = ALL '+' AND                              
004177        REQU-ADINLOMR-NXT      = ALL '+' AND                              
004178        REQU-IDINLVGN          = ALL '+'                                  
004179         CONTINUE                                                         
004180     ELSE                                                                 
004181         PERFORM S20A-KOLLIFLYTT                                          
004182     END-IF                                                               
004183                                                                          
004184     IF 6191-TRANS                                                        
004185         PERFORM S07-SKAPA-6191                                           
004186     END-IF                                                               
004187                                                                          
004188     IF 6197-IX > ZERO                                                    
004189*-------SKICKA TRANS TILL MPP MED ART-ANT-LEV-KOLLI                       
004190        PERFORM S31-STARTA-6197-TRANS                                     
004191     END-IF                                                               
004192     .                                                                    
004193     EJECT                                                                
004194 S20A-KOLLIFLYTT           SECTION.                                       
004195                                                                          
004196     MOVE JA                   TO 6191-SW                                 
004197     EVALUATE TRUE                                                        
004198       WHEN REQU-ADINLOMR      NOT = ALL '+' AND                          
004199            REQU-ADINLOMR-NXT  = ALL '+'     AND                          
004200            REQU-IDINLVGN      = ALL '+'                                  
004201             MOVE REQU-ADINLOMR TO RAD-ADINLOMR                           
004202             MOVE NEJ          TO RAD-FLINLFB                             
004203                                  RAD-FLINLFP                             
004204             MOVE SPACE        TO RAD-ADINLOMR-NXT                        
004205             MOVE ZERO         TO RAD-IDINLVGN                            
004206                                                                          
004207       WHEN REQU-ADINLOMR-NXT      NOT = ALL '+' AND                      
004208            REQU-ADINLOMR          = ALL '+'     AND                      
004209            REQU-IDINLVGN          = ALL '+'                              
004210             MOVE REQU-ADINLOMR-NXT TO RAD-ADINLOMR-NXT                   
004211             MOVE NEJ              TO RAD-FLINLFB                         
004212                                      RAD-FLINLFP                         
004213             MOVE ZERO             TO RAD-IDINLVGN                        
004214                                                                          
004215       WHEN REQU-ADINLOMR          NOT = ALL '+' AND                      
004216            REQU-ADINLOMR-NXT      NOT = ALL '+'     AND                  
004217            REQU-IDINLVGN          = ALL '+'                              
004218             MOVE REQU-ADINLOMR    TO RAD-ADINLOMR                        
004219             MOVE NEJ              TO RAD-FLINLFB                         
004220                                      RAD-FLINLFP                         
004221             MOVE REQU-ADINLOMR-NXT TO RAD-ADINLOMR-NXT                   
004222             MOVE ZERO             TO RAD-IDINLVGN                        
004223                                                                          
004224       WHEN REQU-IDINLVGN          NOT = ALL '+'                          
004225             PERFORM S20A-SAETT-KOLLI-PAA-VAGN                            
004226                                                                          
004227     END-EVALUATE                                                         
004228                                                                          
004229     IF FP AND NOT PRIOGODS                                               
004230       PERFORM S30-SKRIV-EV-FBRAPPORT                                     
004231     END-IF                                                               
004232                                                                          
004233     .                                                                    
004234     EJECT                                                                
004235 S20A-SAETT-KOLLI-PAA-VAGN   SECTION.                                     
004236                                                                          
004237     MOVE REQU-IDINLVGN        TO RAD-IDINLVGN                            
004238     IF VAGN-FINNS                                                        
004239         MOVE W-ADINLOMR       TO RAD-ADINLOMR                            
004240         MOVE NEJ              TO RAD-FLINLFB                             
004241                                  RAD-FLINLFP                             
004242         MOVE W-ADINLOMR-NXT   TO RAD-ADINLOMR-NXT                        
004243         IF REQU-ADINLOMR      = ALL '+'                                  
004244             CONTINUE                                                     
004245         ELSE                                                             
004246             IF REQU-ADINLOMR    NOT = W-ADINLOMR                         
004247                 MOVE ERR-TROLLEY-HAS-LOC-ADD TO RESP-IDMSG-INFO          
004248             END-IF                                                       
004249         END-IF                                                           
004250                                                                          
004251         IF REQU-ADINLOMR-NXT  = ALL '+'                                  
004252             CONTINUE                                                     
004253         ELSE                                                             
004254             IF REQU-ADINLOMR-NXT NOT = W-ADINLOMR-NXT                    
004255                 MOVE ERR-TROLLEY-HAS-LOC-ADD TO RESP-IDMSG-INFO          
004256             END-IF                                                       
004257         END-IF                                                           
004258     ELSE                                                                 
004259         IF REQU-ADINLOMR      = ALL '+' OR SPACE                         
004260             MOVE SPACE        TO RAD-ADINLOMR                            
004261         ELSE                                                             
004262             MOVE REQU-ADINLOMR TO RAD-ADINLOMR                           
004263         END-IF                                                           
004264                                                                          
004265         IF REQU-ADINLOMR-NXT      = ALL '+' OR SPACE                     
004266             MOVE SPACE            TO RAD-ADINLOMR-NXT                    
004267         ELSE                                                             
004268             MOVE REQU-ADINLOMR-NXT TO RAD-ADINLOMR-NXT                   
004269         END-IF                                                           
004270         MOVE NEJ          TO RAD-FLINLFB                                 
004271                              RAD-FLINLFP                                 
004272     END-IF                                                               
004273     .                                                                    
004274     EJECT                                                                
004275 S21-UPD-EV-KDKLIPRIO  SECTION.                                           
004276                                                                          
004277     IF REQU-KDKLIPRI          = 'J' OR 'P' OR 'Y'                        
004278         IF RAD-FLPRIO         NOT = JA                                   
004279             PERFORM S16-CALL-W611PMRK                                    
004280         END-IF                                                           
004281     ELSE                                                                 
004282         IF REQU-KDKLIPRI      = NEJ                                      
004283             IF RAD-FLPRIO     = JA                                       
004284                 PERFORM S16-CALL-W611PMRK                                
004285             END-IF                                                       
004286         END-IF                                                           
004287     END-IF                                                               
004288     .                                                                    
004289     EJECT                                                                
004290 S22A-KOLLA-KVA  SECTION.                                                 
004291                                                                          
004292     MOVE REQU-KVINLART      TO WS-KVINLART                               
004293     IF IDLEVNWKOLLI-NYCKEL                                               
004294       MOVE W-IDLEVNR-KOLLI  TO W-IDLEVNRK                                
004295                                W-IDLEVNR                                 
004296       MOVE W-IDOKOLLI       TO W-IDOKOLLIK                               
004297                                W-IDOKOLLINR                              
004298       PERFORM IMS-GU-W6INLA11-C                                          
004299       PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT OR                    
004300                     ART-IDLOPNRM = W-NUM-IDLOPNRM                        
004301           PERFORM IMS-GN-W6INLA11-C                                      
004302       END-PERFORM                                                        
004303***MAX ETT KVA-SEGMENT FINNS PER KOLLINR                                  
004304       PERFORM UNTIL SEGMENT-SAKNAS OR KVARET-RAD-KDINLSTA = 'KVA'        
004305         PERFORM IMS-GNP-W6INLA21-KVARET-C                                
004306       END-PERFORM                                                        
004307       IF KVARET-RAD-KDINLSTA = 'KVA'                                     
004308         COMPUTE WS-KVINLART-TOT-KVARET =                                 
004309                 KVARET-RAD-KVINLART + RAD-KVINLART                       
004310         IF WS-KVINLART-TOT-KVARET < WS-KVINLART                          
004311           MOVE ERR-WRONG-QTY TO RESP-IDMSG-ERROR                         
004312           MOVE NEJ TO INDATA-SW                                          
004313         ELSE                                                             
004314           MOVE KVARET-RAD-KVINLART TO WS-KVINLART-KVARET                 
004315           MOVE JA TO KVARET-SW                                           
004316         END-IF                                                           
004317       ELSE                                                               
004318         IF  WS-KVINLART   >  RAD-KVINLART                                
004319           MOVE ERR-WRONG-QTY TO RESP-IDMSG-ERROR                         
004320           MOVE NEJ               TO INDATA-SW                            
004321           MOVE MFS-NUM-FAELT-FEL TO RESP-KVINLART-INM-ATTR               
004322         ELSE                                                             
004323           MOVE RAD-KVINLART TO  WS-KVINLART-TOT-KVARET                   
004324         END-IF                                                           
004325       END-IF                                                             
004326     ELSE                                                                 
004327       MOVE WS-IDLOPNRM-NUM  TO W-IDLOPNR                                 
004328       MOVE SPACE            TO W-IDLEVNR                                 
004329       MOVE ZERO             TO W-IDOKOLLINR                              
004330       PERFORM IMS-GU-W6INLA11-B                                          
004331***MAX ETT KVA-SEGMENT FINNS PER RAD=1, DVS UTAN KOLLINR                  
004332       PERFORM UNTIL SEGMENT-SAKNAS OR KVARET-RAD-KDINLSTA = 'KVA'        
004333         PERFORM IMS-GNP-W6INLA21-KVARET-B                                
004334       END-PERFORM                                                        
004335       IF KVARET-RAD-KDINLSTA = 'KVA'                                     
004336         COMPUTE WS-KVINLART-TOT-KVARET =                                 
004337                 KVARET-RAD-KVINLART + RAD-KVINLART                       
004338         IF WS-KVINLART-TOT-KVARET < WS-KVINLART                          
004339           MOVE ERR-WRONG-QTY TO RESP-IDMSG-ERROR                         
004340           MOVE NEJ TO INDATA-SW                                          
004341         ELSE                                                             
004342           MOVE KVARET-RAD-KVINLART TO WS-KVINLART-KVARET                 
004343           MOVE JA TO KVARET-SW                                           
004344         END-IF                                                           
004345       ELSE                                                               
004346         IF  WS-KVINLART   >  RAD-KVINLART                                
004347           MOVE ERR-WRONG-QTY TO RESP-IDMSG-ERROR                         
004348           MOVE NEJ               TO INDATA-SW                            
004349           MOVE MFS-NUM-FAELT-FEL TO RESP-KVINLART-INM-ATTR               
004350         END-IF                                                           
004351       END-IF                                                             
004352     END-IF                                                               
004353     .                                                                    
004354     EJECT                                                                
004355 S22B-KOLLA-RET  SECTION.                                                 
004356                                                                          
004357     MOVE REQU-KVINLART      TO WS-KVINLART                               
004358     IF IDLEVNWKOLLI-NYCKEL                                               
004359       MOVE W-IDLEVNR-KOLLI  TO W-IDLEVNRK                                
004360                                W-IDLEVNR                                 
004361       MOVE W-IDOKOLLI       TO W-IDOKOLLIK                               
004362                                W-IDOKOLLINR                              
004363       PERFORM IMS-GU-W6INLA11-C                                          
004364       PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT OR                    
004365                     ART-IDLOPNRM = W-NUM-IDLOPNRM                        
004366           PERFORM IMS-GN-W6INLA11-C                                      
004367       END-PERFORM                                                        
004368***MAX ETT RET-SEGMENT FINNS PER KOLLINR                                  
004369       PERFORM UNTIL SEGMENT-SAKNAS OR KVARET-RAD-KDINLSTA = 'RET'        
004370         PERFORM IMS-GNP-W6INLA21-KVARET-C                                
004371       END-PERFORM                                                        
004372       IF KVARET-RAD-KDINLSTA = 'RET'                                     
004373         COMPUTE WS-KVINLART-TOT-KVARET =                                 
004374                 KVARET-RAD-KVINLART + RAD-KVINLART                       
004375         IF WS-KVINLART-TOT-KVARET < WS-KVINLART                          
004376           MOVE ERR-WRONG-QTY     TO RESP-IDMSG-ERROR                     
004377           MOVE NEJ TO INDATA-SW                                          
004378         ELSE                                                             
004379           MOVE KVARET-RAD-KVINLART TO WS-KVINLART-KVARET                 
004380           MOVE JA TO KVARET-SW                                           
004381         END-IF                                                           
004382       ELSE                                                               
004383         IF  WS-KVINLART   >  RAD-KVINLART                                
004384           MOVE ERR-WRONG-QTY     TO RESP-IDMSG-ERROR                     
004385           MOVE NEJ               TO INDATA-SW                            
004386           MOVE MFS-NUM-FAELT-FEL TO RESP-KVINLART-INM-ATTR               
004387         ELSE                                                             
004388           MOVE RAD-KVINLART TO  WS-KVINLART-TOT-KVARET                   
004389         END-IF                                                           
004390       END-IF                                                             
004391     ELSE                                                                 
004392       MOVE WS-IDLOPNRM-NUM  TO W-IDLOPNR                                 
004393       MOVE SPACE            TO W-IDLEVNR                                 
004394       MOVE ZERO             TO W-IDOKOLLINR                              
004395       PERFORM IMS-GU-W6INLA11-B                                          
004396***MAX ETT RET-SEGMENT FINNS PER RAD=1, DVS UTAN KOLLINR                  
004397       PERFORM UNTIL SEGMENT-SAKNAS OR KVARET-RAD-KDINLSTA = 'RET'        
004398         PERFORM IMS-GNP-W6INLA21-KVARET-B                                
004399       END-PERFORM                                                        
004400       IF KVARET-RAD-KDINLSTA = 'RET'                                     
004401         COMPUTE WS-KVINLART-TOT-KVARET =                                 
004402                 KVARET-RAD-KVINLART + RAD-KVINLART                       
004403         IF WS-KVINLART-TOT-KVARET < WS-KVINLART                          
004404           MOVE ERR-WRONG-QTY     TO RESP-IDMSG-ERROR                     
004405           MOVE NEJ TO INDATA-SW                                          
004406         ELSE                                                             
004407           MOVE KVARET-RAD-KVINLART TO WS-KVINLART-KVARET                 
004408           MOVE JA TO KVARET-SW                                           
004409         END-IF                                                           
004410       ELSE                                                               
004411         IF  WS-KVINLART   >  RAD-KVINLART                                
004412           MOVE ERR-WRONG-QTY     TO RESP-IDMSG-ERROR                     
004413           MOVE NEJ               TO INDATA-SW                            
004414           MOVE MFS-NUM-FAELT-FEL TO RESP-KVINLART-INM-ATTR               
004415         END-IF                                                           
004416       END-IF                                                             
004417     END-IF                                                               
004418     .                                                                    
004419     EJECT                                                                
004420 S30-SKRIV-EV-FBRAPPORT SECTION.                                          
004421                                                                          
004422     MOVE SPACE   TO STYR-IDLEVNR                                         
004423     MOVE ZERO    TO STYR-IDARTNR                                         
004424                     STYR-IDFKNGRP                                        
004425     MOVE W-BEFT  TO STYR-BEFT                                            
004426     MOVE DCS-IDDC   TO STYR-BEFT                                         
004427     CALL W611STYR USING STYR-W611STYR STYR-HANA-PCB                      
004428                                       STYR-PLAA-PCB                      
004429     IF STYR-ADINLOMR-FP NOT = SPACE                                      
004430       PERFORM IMS-GU-INLA-W6D1D111                                       
004431       PERFORM IMS-GNP-INLA-W6D121                                        
004432       MOVE JA TO FBRAPP-SW                                               
004433       PERFORM UNTIL SEGMENT-SAKNAS OR FBRAPP-FINNS                       
004434         IF FB-RAD-KDINLSTA = 'FPK'                                       
004435           MOVE NEJ TO FBRAPP-SW                                          
004436         ELSE                                                             
004437           IF FB-RAD-KDINLSTA = SPACE                                     
004438             IF FB-RAD-ADINLOMR NOT = SPACE                               
004439               MOVE FB-RAD-ADINLOMR TO W-6006-ADINLOMR                    
004440               PERFORM IMS-GU-PLAA-W6G130                                 
004441               IF PLAA-6006-KDINLOMR = 'F' OR 'FBP'                       
004442                 MOVE NEJ TO FBRAPP-SW                                    
004443               END-IF                                                     
004444             END-IF                                                       
004445             IF FB-RAD-ADINLOMR-NXT NOT = SPACE                           
004446               MOVE FB-RAD-ADINLOMR-NXT TO W-6006-ADINLOMR                
004447               PERFORM IMS-GU-PLAA-W6G130                                 
004448               IF PLAA-6006-KDINLOMR = 'F' OR 'FBP'                       
004449                 MOVE NEJ TO FBRAPP-SW                                    
004450               END-IF                                                     
004451             END-IF                                                       
004452           END-IF                                                         
004453         END-IF                                                           
004454         PERFORM IMS-GNP-INLA-W6D121                                      
004455       END-PERFORM                                                        
004456       IF FBRAPP                                                          
004457         PERFORM S30A-SKAPA-6197-TRANS                                    
004458       END-IF                                                             
004459     END-IF                                                               
004460     .                                                                    
004461     EJECT                                                                
004462 S30A-SKAPA-6197-TRANS SECTION.                                           
004463                                                                          
004464     MOVE ART-IDLOPNRM    TO W-IDLOPNRM-BSEQ                              
004465                                                                          
004466     PERFORM IMS-GU-INLC-W6D1B                                            
004467     IF SEGMENT-FINNS                                                     
004468       ADD +1             TO 6197-IX                                      
004469       MOVE SEQB-IDLEVNR  TO 6197-MID-IDLEVNR(6197-IX)                    
004470       MOVE SEQB-IDFS     TO 6197-MID-IDFS(6197-IX)                       
004471       MOVE SEQB-TIAVIDAT TO 6197-MID-TIAVIDAT(6197-IX)                   
004472       MOVE SEQB-IDRADNR-INL TO 6197-MID-IDRADNR-INL(6197-IX)             
004473     END-IF                                                               
004474     IF 6197-IX = MAX-6197-IX                                             
004475       PERFORM S31-STARTA-6197-TRANS                                      
004476     END-IF                                                               
004477     .                                                                    
004478     EJECT                                                                
004479 S31-STARTA-6197-TRANS  SECTION.                                          
004480                                                                          
004481     MOVE SPACE                 TO 6197-MID-IDPRTLST                      
004482     MOVE '6L'                  TO 6197-MID-IDPRTLST(1:2)                 
004483     MOVE 'TR  '                TO 6197-MID-IDPRTLST(3:4)                 
004484     MOVE IDPGM                 TO 6197-MID-IDPGM                         
004485     MOVE 6197-IX               TO 6197-MID-KVPOST                        
004486     IF REQU-KDPRTVAL = JA OR YES                                         
004487       MOVE JA                  TO 6197-MID-FLSVS                         
004488     ELSE                                                                 
004489       MOVE NEJ                 TO 6197-MID-FLSVS                         
004490     END-IF                                                               
004491     COMPUTE P-TO-P-MSG-KVLL        = P-TO-P-PREFIX-LNG +                 
004492                                  26 + (6197-MID-KVPOST * 24)             
004493     MOVE 'W6T197X '           TO P-TO-P-MSG-KDTRANS                      
004494     MOVE '6133'               TO P-TO-P-MSG-IDTRANS                      
004495     IF REQU-IDSPRAK = 'SV'                                               
004496        MOVE '1'               TO P-TO-P-MSG-KDMFSFOR                     
004497     ELSE                                                                 
004498        MOVE '2'               TO P-TO-P-MSG-KDMFSFOR                     
004499     END-IF                                                               
004500     MOVE 6197-MID-W6I19701 TO P-TO-P-MSG-INDATA                          
004501     IF FOERSTA-6197                                                      
004502         PERFORM IMS-ISRT-ALT-MSG-6197                                    
004503         MOVE NEJ              TO FOERSTA-6197-SW                         
004504      ELSE                                                                
004505         PERFORM IMS-PURG-ALT-MSG-6197                                    
004506     END-IF                                                               
004507     MOVE ZERO                 TO 6197-IX                                 
004508     .                                                                    
004509     EJECT                                                                
004510 S40-BYT-ARTIKEL-UPPGIFTER SECTION.                                       
004511                                                                          
004512     MOVE ART-IDARTNR TO RESP-IDARTNR                                     
004513     MOVE ART-KVAVIS  TO RESP-KVAVIS                                      
004514     MOVE ART-BEART   TO RESP-BEART                                       
004515     IF REQU-IDMSGVER = 1                                                 
004516       PERFORM FAC-GET-BEART                                              
004517     END-IF                                                               
004518     MOVE ART-KDSORT  TO RESP-KDSORT                                      
004519     MOVE ART-BEFT    TO RESP-BEFT                                        
004520                                                                          
004521     IF ART-FLKVAFEL = JA OR ART-FLKVAKAR = JA                            
004522         MOVE ERR-QUALITY-ERROR TO RESP-IDMSG-ERROR                       
004523     ELSE                                                                 
004524         MOVE SPACE             TO RESP-IDMSG-ERROR                       
004525     END-IF                                                               
004526                                                                          
004527     EVALUATE TRUE                                                        
004528       WHEN ART-KDFARLIG       = 4                                        
004529         IF WS-ENGLISH-TEXT                                               
004530            MOVE YES           TO RESP-BEFARLIG                           
004531         ELSE                                                             
004532            MOVE JA            TO RESP-BEFARLIG                           
004533         END-IF                                                           
004534       WHEN ART-KDFARLIG       = 5                                        
004535         MOVE 'ASBEST'         TO RESP-BEFARLIG                           
004536       WHEN ART-KDFARLIG       = 6                                        
004537         IF WS-ENGLISH-TEXT                                               
004538           MOVE 'CHEMICALS '   TO RESP-BEFARLIG                           
004539         ELSE                                                             
004540           MOVE 'KEMIKALIER'   TO RESP-BEFARLIG                           
004541         END-IF                                                           
004542       WHEN ART-KDFARLIG       = 7                                        
004543         IF WS-ENGLISH-TEXT                                               
004544            MOVE YES           TO RESP-BEFARLIG                           
004545         ELSE                                                             
004546            MOVE JA            TO RESP-BEFARLIG                           
004547         END-IF                                                           
004548       WHEN OTHER                                                         
004549         IF WS-ENGLISH-TEXT                                               
004550           MOVE 'NO '            TO RESP-BEFARLIG                         
004551         ELSE                                                             
004552           MOVE 'NEJ'            TO RESP-BEFARLIG                         
004553         END-IF                                                           
004554     END-EVALUATE                                                         
004555                                                                          
004556     MOVE ART-KDLAGEMB         TO RESP-KDLAGEMB                           
004557                                                                          
004558     MOVE ART-ADLAGOMR         TO RESP-ADLAGOMR                           
004559     MOVE ART-ADGANG           TO RESP-ADGANG                             
004560     MOVE ART-ADPLATS          TO RESP-ADPLATS                            
004561     MOVE ART-ADTRDEST-KIT     TO RESP-ADTRDEST-KIT                       
004562                                                                          
004563     IF ART-KDKVAANT > ZERO                                               
004564       IF WS-ENGLISH-TEXT                                                 
004565          MOVE YES TO RESP-FLKVAANT-TOT                                   
004566       ELSE                                                               
004567          MOVE JA TO RESP-FLKVAANT-TOT                                    
004568       END-IF                                                             
004569     ELSE                                                                 
004570       MOVE NEJ TO RESP-FLKVAANT-TOT                                      
004571     END-IF                                                               
004572                                                                          
004573     MOVE ZERO                     TO WW-KVKVAPRIM-KVAR                   
004574     COMPUTE WW-KVKVAPRIM-KVAR =                                          
004575                       ART-KVKVAPRIM-BER - ART-KVKVAPRIM-VER              
004576                                                                          
004577     IF WW-KVKVAPRIM-KVAR < ZERO                                          
004578        MOVE ZERO TO RESP-KVKVAPRIM-KVAR                                  
004579     ELSE                                                                 
004580        MOVE WW-KVKVAPRIM-KVAR TO RESP-KVKVAPRIM-KVAR                     
004581     END-IF                                                               
004582                                                                          
004583     MOVE ART-IDARTNR TO W-IDARTNR                                        
004584     MOVE ZERO        TO WS-KVROS                                         
004585     IF DCS-CDC                                                           
004586       PERFORM IMS-GU-WDK611                                              
004587       IF SEGMENT-FINNS                                                   
004588         MOVE CLAG-KVROS TO WS-KVROS                                      
004589       END-IF                                                             
004590     END-IF                                                               
004591                                                                          
004592     MOVE WS-KVROS TO RESP-KVROS                                          
004593                                                                          
004594     MOVE ART-IDLOPNRM         TO ADR-IDLOPNRM                            
004595                                                                          
004596     CALL W611ADR USING ADR-W611ADR ADR-INLA-PCB ADR-INLC-PCB             
004597                        ADR-PLAA-PCB ADR-WDK6-PCB                         
004598                                     ADR-STYR-HANA-PCB                    
004599                                     ADR-STYR-PLAA-PCB                    
004600                                                                          
004601     MOVE ADR-ADINLOMR-NXT1    TO RESP-ADINLOMR-NXT1                      
004602     MOVE ADR-ADINLOMR-NXT2    TO RESP-ADINLOMR-NXT2                      
004603     MOVE ADR-ADINLOMR-NXT3    TO RESP-ADINLOMR-NXT3                      
004604     MOVE ADR-ADINLOMR-NXT4    TO RESP-ADINLOMR-NXT4                      
004605     MOVE ADR-ADINLOMR-NXT5    TO RESP-ADINLOMR-NXT5                      
004606     .                                                                    
004607     EJECT                                                                
004608                                                                          
004609 S50-PRIM-CONTROL SECTION.                                                
004610* THIS IS A CONTROL TO CHECK THE OLD PLACE IF FLAG FLKNTRGK = YES         
004611* IF THE FLAG IS YES WE HAVE TO DO A CHECK ON THE OLD PLACE BEFORE        
004612* WE CHECK THE NEW PLACE.                                                 
004613     MOVE RAD-ADINLOMR         TO W-6006-ADINLOMR                         
004614     PERFORM IMS-GU-PLAA-PLAA11                                           
004615     IF SEGMENT-FINNS                                                     
004616       IF PLAA-6006-FLKNTRGK = JA OR YES                                  
004617         MOVE REQU-ADINLOMR TO W-6006-ADINLOMR                            
004618         PERFORM IMS-GU-PLAA-PLAA11                                       
004619         IF (PLAA-6006-FLKNTRGK = JA OR YES)                              
004620         OR PLAA-6006-KDINLOMR = 'LPL'                                    
004621           MOVE 'N' TO PRIM-CONTROL-SW                                    
004622         ELSE                                                             
004623           MOVE 'J' TO PRIM-CONTROL-SW                                    
004624         END-IF                                                           
004625       ELSE                                                               
004626         MOVE 'N' TO PRIM-CONTROL-SW                                      
004627       END-IF                                                             
004628     ELSE                                                                 
004629       MOVE 'N' TO PRIM-CONTROL-SW                                        
004630     END-IF                                                               
004631     IF PRIM-CONTROL-YES                                                  
004632       PERFORM S51-CHECK-OLD-PLACE                                        
004633     END-IF                                                               
004634     .                                                                    
004635     EJECT                                                                
004636                                                                          
004637 S51-CHECK-OLD-PLACE     SECTION.                                         
004638     MOVE WS-IDLOPNRM-NUM  TO W-IDLOPNRM-BSEQ                             
004639     PERFORM IMS-GU-UPFA01                                                
004640     IF SEGMENT-FINNS                                                     
004641       IF UPPF-KVKVAPRIM > ZERO                                           
004642         IF UPPF-KDKVASTA-PRI = '2' OR '3'                                
004643           CONTINUE                                                       
004644         ELSE                                                             
004645           MOVE ERR-PRI-SPL-CTL-NOT-DONE TO RESP-IDMSG-ERROR              
004646           MOVE NEJ TO INDATA-SW                                          
004647         END-IF                                                           
004648       END-IF                                                             
004649       IF UPPF-KVKVASEK > ZERO                                            
004650         IF UPPF-KDKVASTA-PRI = '2' OR '3'                                
004651           CONTINUE                                                       
004652         ELSE                                                             
004653           MOVE ERR-PRI-SPL-CTL-NOT-DONE TO RESP-IDMSG-ERROR              
004654           MOVE NEJ TO INDATA-SW                                          
004655         END-IF                                                           
004656       END-IF                                                             
004657     END-IF                                                               
004658                                                                          
004659     IF INDATA-OK                                                         
004660       PERFORM IMS-GU-UPFA01                                              
004661       IF SEGMENT-FINNS                                                   
004662         PERFORM IMS-GNP-UPFA11                                           
004663         PERFORM UNTIL SEGMENT-SAKNAS                                     
004664           IF RAPP-KDKVASTA-PRI = '2' OR '3'                              
004665             CONTINUE                                                     
004666           ELSE                                                           
004667             MOVE ERR-PRI-SPL-CTL-NOT-DONE TO RESP-IDMSG-ERROR            
004668             MOVE NEJ TO INDATA-SW                                        
004669           END-IF                                                         
004670           PERFORM IMS-GNP-UPFA11                                         
004671         END-PERFORM                                                      
004672       END-IF                                                             
004673     END-IF                                                               
004674                                                                          
004675     IF INDATA-OK                                                         
004676       PERFORM IMS-GU-UPFA01                                              
004677       IF SEGMENT-FINNS                                                   
004678         PERFORM IMS-GNP-UPFA12                                           
004679         PERFORM UNTIL SEGMENT-SAKNAS                                     
004680           IF SPEC-KDKVASTA-PRI = '2' OR '3'                              
004681             CONTINUE                                                     
004682           ELSE                                                           
004683             MOVE ERR-PRI-SPL-CTL-NOT-DONE TO RESP-IDMSG-ERROR            
004684             MOVE NEJ TO INDATA-SW                                        
004685           END-IF                                                         
004686           PERFORM IMS-GNP-UPFA12                                         
004687         END-PERFORM                                                      
004688       END-IF                                                             
004689     END-IF                                                               
004690     .                                                                    
004691     EJECT                                                                
004692                                                                          
004693 MFS-RENSA-FAELT-UT SECTION.                                              
004694                                                                          
004695     MOVE ALL-UTF8-SPACE       TO RESP-BEART                              
004696     MOVE ALL-SPACE            TO RESP-IDARTNR                            
004697                                  RESP-KVAVIS                             
004698                                  RESP-KDSORT                             
004699                                  RESP-BEFT                               
004700                                  RESP-KVKOLLI-TOT                        
004701                                  RESP-BEFARLIG                           
004702                                  RESP-KVAVIS-KVAR                        
004703                                  RESP-KVAVIS-PRIO-KVAR                   
004704                                  RESP-KDLAGEMB                           
004705                                  RESP-ADLAGOMR                           
004706                                  RESP-ADGANG                             
004707                                  RESP-ADPLATS                            
004708                                  RESP-FLKVAANT-TOT                       
004709                                  RESP-KVKVAPRIM-KVAR                     
004710                                  RESP-KVROS                              
004711                                  RESP-KVAVIS-KIT-KVAR                    
004712                                  RESP-ADTRDEST-KIT                       
004713                                  RESP-ADINLOMR-NXT1                      
004714                                  RESP-ADINLOMR-NXT2                      
004715                                  RESP-ADINLOMR-NXT3                      
004716                                  RESP-ADINLOMR-NXT4                      
004717                                  RESP-ADINLOMR-NXT5                      
004718                                  RESP-IDLOPNRM-START                     
004719                                  RESP-IDLOPNRM-NEXT                      
004720                                                                          
004721     PERFORM MFS-RENSA-RAD-FAELT                                          
004722     .                                                                    
004723     EJECT                                                                
004724 MFS-RENSA-RAD-FAELT SECTION.                                             
004725                                                                          
004726     MOVE ALL-SPACE         TO RESP-KDKLIPRI                              
004727                               RESP-FLSATS                                
004728                               RESP-FLKVAANT                              
004729     MOVE +1 TO INDX                                                      
004730     PERFORM UNTIL INDX     > REQU-KVRADER                                
004731       MOVE ALL-SPACE       TO RESP-KVINLART-LINE(INDX)                   
004732                               RESP-KVKOLLI-LINE(INDX)                    
004733                               RESP-ADINLOMR-LINE(INDX)                   
004734                               RESP-KDINLSTA-LINE(INDX)                   
004735                               RESP-IDINLVGN-LINE(INDX)                   
004736                               RESP-ADINLOMR-NXT-LINE(INDX)               
004737       ADD +1 TO INDX                                                     
004738     END-PERFORM                                                          
004739     .                                                                    
004740     EJECT                                                                
004741 MFS-RENSA-FAELT-IN SECTION.                                              
004742                                                                          
004743*    --- ALLA INDATA-FÄLT                                                 
004744     MOVE ALL-SPACE            TO RESP-KDCMDVAL-INM                       
004745                                  RESP-KVINLART-INM                       
004746                                  RESP-ADINLOMR-INM                       
004747                                  RESP-IDINLVGN-INM                       
004748                                  RESP-ADINLOMR-NXT-INM                   
004749                                  RESP-FLSATS-INM                         
004750     .                                                                    
004751     EJECT                                                                
004752 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
004753                                                                          
004754     MOVE ALL-UTF8-PLUS        TO RESP-IDARTNR                            
004755     MOVE ALL-PLUS             TO RESP-IDARTNR                            
004756                                  RESP-KVAVIS                             
004757                                  RESP-KDSORT                             
004758                                  RESP-BEFT                               
004759                                  RESP-KVKOLLI-TOT                        
004760                                  RESP-BEFARLIG                           
004761                                  RESP-KVAVIS-KVAR                        
004762                                  RESP-KVAVIS-PRIO-KVAR                   
004763                                  RESP-KDLAGEMB                           
004764                                  RESP-ADLAGOMR                           
004765                                  RESP-ADGANG                             
004766                                  RESP-ADPLATS                            
004767                                  RESP-FLKVAANT-TOT                       
004768                                  RESP-KVKVAPRIM-KVAR                     
004769                                  RESP-KVROS                              
004770                                  RESP-KVAVIS-KIT-KVAR                    
004771                                  RESP-ADTRDEST-KIT                       
004772                                  RESP-ADINLOMR-NXT1                      
004773                                  RESP-ADINLOMR-NXT2                      
004774                                  RESP-ADINLOMR-NXT3                      
004775                                  RESP-ADINLOMR-NXT4                      
004776                                  RESP-ADINLOMR-NXT5                      
004777                                  RESP-IDLOPNRM-START                     
004778                                  RESP-IDLOPNRM-NEXT                      
004779                                                                          
004780     PERFORM MFS-ROER-EJ-RAD-FAELT                                        
004781     .                                                                    
004782     EJECT                                                                
004783 MFS-ROER-EJ-RAD-FAELT SECTION.                                           
004784                                                                          
004785     MOVE ALL-PLUS            TO RESP-KDKLIPRI                            
004786                                 RESP-FLSATS                              
004787                                 RESP-FLKVAANT                            
004788     MOVE +1 TO INDX                                                      
004789     PERFORM UNTIL INDX      > REQU-KVRADER                               
004790       MOVE ALL-PLUS          TO RESP-KVINLART-LINE(INDX)                 
004791                                 RESP-KVKOLLI-LINE(INDX)                  
004792                                 RESP-ADINLOMR-LINE(INDX)                 
004793                                 RESP-KDINLSTA-LINE(INDX)                 
004794                                 RESP-IDINLVGN-LINE(INDX)                 
004795                                 RESP-ADINLOMR-NXT-LINE(INDX)             
004796       ADD +1 TO INDX                                                     
004797     END-PERFORM                                                          
004798     .                                                                    
004799     EJECT                                                                
004800 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
004801                                                                          
004802     MOVE ALL-PLUS             TO RESP-KDCMDVAL-INM                       
004803                                  RESP-KVINLART-INM                       
004804                                  RESP-ADINLOMR-INM                       
004805                                  RESP-IDINLVGN-INM                       
004806                                  RESP-ADINLOMR-NXT-INM                   
004807                                  RESP-FLSATS-INM                         
004808                                  RESP-KDKLIPRI-INM                       
004809                                  RESP-ADINLOMR-PRT                       
004810     .                                                                    
004811     SKIP2                                                                
004812 MFS-FORM-ATTR SECTION.                                                   
004813                                                                          
004814     MOVE MFS-FORMATETS-ATTR   TO RESP-KDCMDVAL-INM-ATTR                  
004815                                  RESP-KVINLART-INM-ATTR                  
004816                                  RESP-ADINLOMR-INM-ATTR                  
004817                                  RESP-IDINLVGN-INM-ATTR                  
004818                                  RESP-ADINLOMR-NXT-INM-ATTR              
004819                                  RESP-FLSATS-INM-ATTR                    
004820                                  RESP-KDKLIPRI-INM-ATTR                  
004821                                  RESP-ADINLOMR-PRT-ATTR                  
004822     .                                                                    
004823     EJECT                                                                
004824                                                                          
004825* --- IMS SEKTIONER ---                                                   
004826                                                                          
004827 IMS-ISRT-6191-MSG SECTION.                                               
004828     MOVE SPACE TO GODK-STATUSKODER                                       
004829     CALL CBLTDLI USING ISRT ALT1-PCB P-TO-P-MSG-IO-AREA-SNUF             
004830     MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
004831     PERFORM IMS-STATUSKONTROLL                                           
004832     .                                                                    
004833     SKIP3                                                                
004834 IMS-ISRT-6194-MSG SECTION.                                               
004835     MOVE SPACE TO GODK-STATUSKODER                                       
004836     CALL CBLTDLI USING ISRT ALT2-PCB P-TO-P-MSG-IO-AREA-SNUF             
004837     MOVE ALT2-STATUS-CODE TO STATUS-WS                                   
004838     PERFORM IMS-STATUSKONTROLL                                           
004839     .                                                                    
004840     SKIP3                                                                
004841 IMS-ISRT-6195-MSG SECTION.                                               
004842     MOVE SPACE TO GODK-STATUSKODER                                       
004843     CALL CBLTDLI USING ISRT ALT3-PCB P-TO-P-MSG-IO-AREA-SNUF             
004844     MOVE ALT3-STATUS-CODE TO STATUS-WS                                   
004845     PERFORM IMS-STATUSKONTROLL                                           
004846     .                                                                    
004847     SKIP3                                                                
004848 IMS-ISRT-ALT-MSG-6197  SECTION.                                          
004849     MOVE SPACE TO GODK-STATUSKODER                                       
004850     CALL  CBLTDLI  USING ISRT 6197-PCB P-TO-P-MSG-IO-AREA-SNUF           
004851     MOVE 6197-STATUS-CODE TO STATUS-WS                                   
004852     PERFORM IMS-STATUSKONTROLL                                           
004853     .                                                                    
004854     SKIP3                                                                
004855 IMS-PURG-ALT-MSG-6197  SECTION.                                          
004856     MOVE SPACE TO GODK-STATUSKODER                                       
004857     CALL  CBLTDLI  USING PURG 6197-PCB P-TO-P-MSG-IO-AREA-SNUF           
004858     MOVE 6197-STATUS-CODE TO STATUS-WS                                   
004859     PERFORM IMS-STATUSKONTROLL                                           
004860     .                                                                    
004861     EJECT                                                                
004862 IMS-ISRT-6202-MSG SECTION.                                               
004863     MOVE SPACE TO GODK-STATUSKODER                                       
004864     CALL CBLTDLI USING ISRT ALT4-PCB 6202-AREA                           
004865     MOVE ALT4-STATUS-CODE TO STATUS-WS                                   
004866     PERFORM IMS-STATUSKONTROLL                                           
004867     .                                                                    
004868     EJECT                                                                
004869 IMS-GU-INLA1-INLA01 SECTION.                                             
004870     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
004871          DELIMITED BY SIZE INTO SSA1                                     
004872     MOVE '  ' TO GODK-STATUSKODER                                        
004873     CALL CBLTDLI USING GU INLA-PCB DLI-IO-AREA-INL01 SSA1                
004874     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
004875     PERFORM IMS-STATUSKONTROLL                                           
004876     .                                                                    
004877     EJECT                                                                
004878 IMS-GU-W6INLA11-C SECTION.                                               
004879     STRING 'W6INLA11(W6D1CSEQ =' W-W6D1CSEQ-X                            
004880                    '&IDDC     =' W-IDDC-X ')'                            
004881          DELIMITED BY SIZE INTO SSA1                                     
004882     MOVE '  GEGB' TO GODK-STATUSKODER                                    
004883     CALL CBLTDLI USING GU INLD-PCB DLI-IO-AREA-INL11 SSA1                
004884     MOVE INLD-STATUS-CODE TO STATUS-WS                                   
004885     PERFORM IMS-STATUSKONTROLL                                           
004886     .                                                                    
004887     SKIP3                                                                
004888 IMS-GN-W6INLA11-C SECTION.                                               
004889     STRING 'W6INLA11(W6D1CSEQ =' W-W6D1CSEQ-X                            
004890                    '&IDDC     =' W-IDDC-X ')'                            
004891          DELIMITED BY SIZE INTO SSA1                                     
004892     MOVE '  GEGB' TO GODK-STATUSKODER                                    
004893     CALL CBLTDLI USING GN INLD-PCB DLI-IO-AREA-INL11 SSA1                
004894     MOVE INLD-STATUS-CODE TO STATUS-WS                                   
004895     PERFORM IMS-STATUSKONTROLL                                           
004896     .                                                                    
004897     SKIP3                                                                
004898 IMS-GU-W6INLA11-B SECTION.                                               
004899     STRING 'W6INLA11(W6D1BSEQ =' W-W6D1BSEQ-X                            
004900                    '&IDDC     =' W-IDDC-X ')'                            
004901          DELIMITED BY SIZE INTO SSA1                                     
004902     MOVE '  GEGB' TO GODK-STATUSKODER                                    
004903     CALL CBLTDLI USING GU INLC-PCB DLI-IO-AREA-INL11 SSA1                
004904     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
004905     PERFORM IMS-STATUSKONTROLL                                           
004906     .                                                                    
004907     SKIP3                                                                
004908 IMS-GHU-W6INLA11-B SECTION.                                              
004909     STRING 'W6INLA11(W6D1BSEQ =' W-W6D1BSEQ-X                            
004910                    '&IDDC     =' W-IDDC-X ')'                            
004911          DELIMITED BY SIZE INTO SSA1                                     
004912     MOVE '    ' TO GODK-STATUSKODER                                      
004913     CALL CBLTDLI USING GHU INLC-PCB DLI-IO-AREA-INL11 SSA1               
004914     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
004915     PERFORM IMS-STATUSKONTROLL                                           
004916     .                                                                    
004917     SKIP3                                                                
004918 IMS-REPL-W6INLA11-B SECTION.                                             
004919     MOVE '    ' TO GODK-STATUSKODER                                      
004920     CALL CBLTDLI USING REPL INLC-PCB DLI-IO-AREA-INL11                   
004921     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
004922     PERFORM IMS-STATUSKONTROLL                                           
004923     .                                                                    
004924     SKIP3                                                                
004925 IMS-GU-W6INLA21-B SECTION.                                               
004926     STRING 'W6INLA11(W6D1BSEQ =' W-W6D1BSEQ-X                            
004927                    '&IDDC     =' W-IDDC-X ')'                            
004928          DELIMITED BY SIZE INTO SSA1                                     
004929     STRING 'W6INLA21(IDRADNR  =' W-IDRADNR-X ')'                         
004930          DELIMITED BY SIZE INTO SSA2                                     
004931     MOVE '  GE' TO GODK-STATUSKODER                                      
004932     CALL CBLTDLI USING GU INLC-PCB DLI-IO-AREA-INL21 SSA1 SSA2           
004933     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
004934     PERFORM IMS-STATUSKONTROLL                                           
004935     .                                                                    
004936     SKIP3                                                                
004937 IMS-GHU-W6INLA21-B SECTION.                                              
004938     STRING 'W6INLA11*P(W6D1BSEQ =' W-W6D1BSEQ-X                          
004939                    '&IDDC     =' W-IDDC-X ')'                            
004940          DELIMITED BY SIZE INTO SSA1                                     
004941     STRING 'W6INLA21(IDLEVNRK =' W-IDLEVNRK-X                            
004942                    '&IDOKOLLI =' W-IDOKOLLI-X ')'                        
004943          DELIMITED BY SIZE INTO SSA2                                     
004944     MOVE '    ' TO GODK-STATUSKODER                                      
004945     CALL CBLTDLI USING GHU INLC-PCB DLI-IO-AREA-INL21 SSA1 SSA2          
004946     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
004947     PERFORM IMS-STATUSKONTROLL                                           
004948     .                                                                    
004949     SKIP3                                                                
004950 IMS-GHNP-W6INLA21-B SECTION.                                             
004951     STRING 'W6INLA21(IDRADNR  =' W-IDRADNR-X ')'                         
004952          DELIMITED BY SIZE INTO SSA1                                     
004953     MOVE '    ' TO GODK-STATUSKODER                                      
004954     CALL CBLTDLI USING GHNP INLC-PCB DLI-IO-AREA-INL21 SSA1              
004955     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
004956     PERFORM IMS-STATUSKONTROLL                                           
004957     .                                                                    
004958     SKIP3                                                                
004959 IMS-GHNP-W6INLA21-B-OKVAL SECTION.                                       
004960     MOVE  'W6INLA21'         TO SSA1                                     
004961     MOVE '  GE' TO GODK-STATUSKODER                                      
004962     CALL CBLTDLI USING GHNP INLC-PCB DLI-IO-AREA-INL21 SSA1              
004963     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
004964     PERFORM IMS-STATUSKONTROLL                                           
004965     .                                                                    
004966     SKIP3                                                                
004967 IMS-GHNP-W6INLA21-B-FIRST SECTION.                                       
004968     MOVE  'W6INLA21*F'       TO SSA1                                     
004969     MOVE '    ' TO GODK-STATUSKODER                                      
004970     CALL CBLTDLI USING GHNP INLC-PCB DLI-IO-AREA-INL21 SSA1              
004971     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
004972     PERFORM IMS-STATUSKONTROLL                                           
004973     .                                                                    
004974     SKIP3                                                                
004975 IMS-DLET-W6INLA21-B SECTION.                                             
004976     MOVE '    ' TO GODK-STATUSKODER                                      
004977     CALL CBLTDLI USING DLET INLC-PCB DLI-IO-AREA-INL21                   
004978     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
004979     PERFORM IMS-STATUSKONTROLL                                           
004980     .                                                                    
004981     SKIP3                                                                
004982 IMS-REPL-W6INLA21-B SECTION.                                             
004983     MOVE '    ' TO GODK-STATUSKODER                                      
004984     CALL CBLTDLI USING REPL INLC-PCB DLI-IO-AREA-INL21                   
004985     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
004986     PERFORM IMS-STATUSKONTROLL                                           
004987     .                                                                    
004988     SKIP3                                                                
004989 IMS-ISRT-W6INLA21-B SECTION.                                             
004990     MOVE 'W6INLA21'      TO SSA1                                         
004991     MOVE '    ' TO GODK-STATUSKODER                                      
004992     CALL CBLTDLI USING ISRT INLC-PCB DLI-IO-AREA-INL21 SSA1              
004993     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
004994     PERFORM IMS-STATUSKONTROLL                                           
004995     .                                                                    
004996     SKIP3                                                                
004997 IMS-GNP-W6INLA21 SECTION.                                                
004998     STRING 'W6INLA21(IDLEVNRK =' W-IDLEVNRK-X                            
004999                    '&IDOKOLLI =' W-IDOKOLLI-X ')'                        
005000          DELIMITED BY SIZE INTO SSA1                                     
005001     MOVE '  GEGB' TO GODK-STATUSKODER                                    
005002     CALL CBLTDLI USING GNP INLD-PCB DLI-IO-AREA-INL21 SSA1               
005003     MOVE INLD-STATUS-CODE TO STATUS-WS                                   
005004     PERFORM IMS-STATUSKONTROLL                                           
005005     .                                                                    
005006     SKIP3                                                                
005007 IMS-GNP-W6INLA21-C-FIRST SECTION.                                        
005008     MOVE 'W6INLA21*F' TO SSA1                                            
005009     MOVE '  GEGB' TO GODK-STATUSKODER                                    
005010     CALL CBLTDLI USING GNP INLD-PCB DLI-IO-AREA-INL21 SSA1               
005011     MOVE INLD-STATUS-CODE TO STATUS-WS                                   
005012     PERFORM IMS-STATUSKONTROLL                                           
005013     .                                                                    
005014     SKIP3                                                                
005015 IMS-GNP-W6INLA21-B-FIRST SECTION.                                        
005016     MOVE 'W6INLA21*F' TO SSA1                                            
005017     MOVE '  GE' TO GODK-STATUSKODER                                      
005018     CALL CBLTDLI USING GNP INLC-PCB DLI-IO-AREA-INL21 SSA1               
005019     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
005020     PERFORM IMS-STATUSKONTROLL                                           
005021     .                                                                    
005022     SKIP3                                                                
005023 IMS-GNP-W6INLA21-B-LAST SECTION.                                         
005024     MOVE 'W6INLA21*L' TO SSA1                                            
005025     MOVE '  GE' TO GODK-STATUSKODER                                      
005026     CALL CBLTDLI USING GNP INLC-PCB DLI-IO-AREA-INL21 SSA1               
005027     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
005028     PERFORM IMS-STATUSKONTROLL                                           
005029     .                                                                    
005030     SKIP3                                                                
005031 IMS-GNP-W6INLA21-OKVAL-B SECTION.                                        
005032     MOVE 'W6INLA21' TO SSA1                                              
005033     MOVE '  GEGB' TO GODK-STATUSKODER                                    
005034     CALL CBLTDLI USING GNP INLC-PCB DLI-IO-AREA-INL21 SSA1               
005035     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
005036     PERFORM IMS-STATUSKONTROLL                                           
005037     .                                                                    
005038     SKIP3                                                                
005039 IMS-GU-W6INLA21-F SECTION.                                               
005040     STRING 'W6INLA11(W6D1FSEQ =' W-W6D1FSEQ-X                            
005041                    '&IDDC     =' W-IDDC-X ')'                            
005042          DELIMITED BY SIZE INTO SSA1                                     
005043     STRING 'W6INLA21(IDINLVGN =' W-IDINLVGN-X ')'                        
005044          DELIMITED BY SIZE INTO SSA2                                     
005045     MOVE '  GE' TO GODK-STATUSKODER                                      
005046     CALL CBLTDLI USING GU INLG-PCB DLI-IO-AREA-INL21 SSA1 SSA2           
005047     MOVE INLG-STATUS-CODE TO STATUS-WS                                   
005048     PERFORM IMS-STATUSKONTROLL                                           
005049     .                                                                    
005050     SKIP3                                                                
005051 IMS-GNP-W6INLA21-KVARET-B SECTION.                                       
005052     STRING 'W6INLA21(IDOKOLLI =' W-IDOKOLLI-X ')'                        
005053          DELIMITED BY SIZE INTO SSA1                                     
005054     MOVE '  GEGB' TO GODK-STATUSKODER                                    
005055     CALL CBLTDLI USING GNP INLC-PCB DLI-IO-AREA-INL21-KVARET SSA1        
005056     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
005057     PERFORM IMS-STATUSKONTROLL                                           
005058     .                                                                    
005059     SKIP3                                                                
005060 IMS-GNP-W6INLA21-KVARET-C SECTION.                                       
005061     STRING 'W6INLA21(IDLEVNRK =' W-IDLEVNRK-X                            
005062                    '&IDOKOLLI =' W-IDOKOLLI-X ')'                        
005063          DELIMITED BY SIZE INTO SSA1                                     
005064     MOVE '  GEGB' TO GODK-STATUSKODER                                    
005065     CALL CBLTDLI USING GNP INLD-PCB DLI-IO-AREA-INL21-KVARET SSA1        
005066     MOVE INLD-STATUS-CODE TO STATUS-WS                                   
005067     PERFORM IMS-STATUSKONTROLL                                           
005068     .                                                                    
005069     SKIP3                                                                
005070 IMS-GHNP-W6INLA21-KVARET1 SECTION.                                       
005071     STRING 'W6INLA21(IDLEVNRK =' W-IDLEVNRK-X                            
005072                    '&IDOKOLLI =' W-IDOKOLLI-X ')'                        
005073          DELIMITED BY SIZE INTO SSA1                                     
005074     MOVE '  ' TO GODK-STATUSKODER                                        
005075     CALL CBLTDLI USING GHNP INLC-PCB DLI-IO-AREA-INL21-KVARET            
005076                             SSA1                                         
005077     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
005078     PERFORM IMS-STATUSKONTROLL                                           
005079     .                                                                    
005080     SKIP3                                                                
005081 IMS-GHNP-W6INLA21-KVARET2 SECTION.                                       
005082     STRING 'W6INLA21(IDOKOLLI =' W-IDOKOLLI-X ')'                        
005083          DELIMITED BY SIZE INTO SSA1                                     
005084     MOVE '  ' TO GODK-STATUSKODER                                        
005085     CALL CBLTDLI USING GHNP INLC-PCB DLI-IO-AREA-INL21-KVARET            
005086                             SSA1                                         
005087     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
005088     PERFORM IMS-STATUSKONTROLL                                           
005089     .                                                                    
005090     SKIP3                                                                
005091 IMS-DLET-W6INLA21-KVARET SECTION.                                        
005092     MOVE '    ' TO GODK-STATUSKODER                                      
005093     CALL CBLTDLI USING DLET INLC-PCB DLI-IO-AREA-INL21-KVARET            
005094     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
005095     PERFORM IMS-STATUSKONTROLL                                           
005096     .                                                                    
005097     SKIP3                                                                
005098 IMS-REPL-W6INLA21-KVARET SECTION.                                        
005099     MOVE '    ' TO GODK-STATUSKODER                                      
005100     CALL CBLTDLI USING REPL INLC-PCB DLI-IO-AREA-INL21-KVARET            
005101     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
005102     PERFORM IMS-STATUSKONTROLL                                           
005103     .                                                                    
005104     SKIP3                                                                
005105 IMS-GU-INLC-W6D1B   SECTION.                                             
005106     STRING 'W6INLC01(W6D1B1KY =' W-IDLOPNRM-X ')'                        
005107             DELIMITED BY SIZE INTO SSA1                                  
005108     MOVE '  '                 TO GODK-STATUSKODER                        
005109     CALL CBLTDLI USING GU       INLB1-PCB                                
005110                                 DLI-IO-AREA-INLC01                       
005111                                 SSA1                                     
005112     MOVE INLB1-STATUS-CODE      TO STATUS-WS                             
005113     PERFORM IMS-STATUSKONTROLL                                           
005114     .                                                                    
005115     EJECT                                                                
005116 IMS-GU-INLA-W6D1D111 SECTION.                                            
005117     STRING 'W6INLA11(W6D1BSEQ =' W-W6D1BSEQ-X                            
005118                    '&IDDC     =' W-IDDC-X ')'                            
005119             DELIMITED BY SIZE INTO SSA1                                  
005120     MOVE '  '                 TO GODK-STATUSKODER                        
005121     CALL CBLTDLI USING GU       ALT-INLC-PCB                             
005122                                 DLI-IO-AREA-INL21-FB                     
005123                                 SSA1                                     
005124     MOVE ALT-INLC-STATUS-CODE       TO STATUS-WS                         
005125     PERFORM IMS-STATUSKONTROLL                                           
005126     .                                                                    
005127     SKIP2                                                                
005128 IMS-GNP-INLA-W6D121 SECTION.                                             
005129     MOVE 'W6INLA21 '  TO SSA1                                            
005130     MOVE '  GE'            TO GODK-STATUSKODER                           
005131     CALL CBLTDLI USING GNP ALT-INLC-PCB                                  
005132                            DLI-IO-AREA-INL21-FB                          
005133                            SSA1                                          
005134     MOVE ALT-INLC-STATUS-CODE  TO STATUS-WS                              
005135     PERFORM IMS-STATUSKONTROLL                                           
005136     .                                                                    
005137     EJECT                                                                
005138 IMS-GU-PLAA-W6G130 SECTION.                                              
005139     STRING 'W6PLAA01(W6GXKEY  =' W-W6GXKEY-6005-X ')'                    
005140          DELIMITED BY SIZE INTO SSA1                                     
005141     STRING 'W6PLAA11(W6GXKEY  =' W-W6GXKEY-6006-X ')'                    
005142          DELIMITED BY SIZE INTO SSA2                                     
005143     MOVE '  ' TO GODK-STATUSKODER                                        
005144     CALL CBLTDLI USING GU PLAA-PCB DLI-IO-AREA-PLAA01 SSA1 SSA2          
005145     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
005146     PERFORM IMS-STATUSKONTROLL                                           
005147     .                                                                    
005148     SKIP2                                                                
005149*----------------------------------------------------------------*        
005150 IMS-GU-PLAA-PLAA11 SECTION.                                              
005151     STRING 'W6PLAA01(W6GXKEY  =' W-W6GXKEY-6005-X ')'                    
005152          DELIMITED BY SIZE INTO SSA1                                     
005153     STRING 'W6PLAA11(W6GXKEY  =' W-W6GXKEY-6006-X ')'                    
005154          DELIMITED BY SIZE INTO SSA2                                     
005155     MOVE '  GE' TO GODK-STATUSKODER                                      
005156     CALL CBLTDLI USING GU PLAA-PCB DLI-IO-AREA-PLAA01 SSA1 SSA2          
005157     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
005158     PERFORM IMS-STATUSKONTROLL                                           
005159     .                                                                    
005160     SKIP3                                                                
005161 IMS-GHU-KVAE-W6H701 SECTION.                                             
005162     STRING 'W6KVAE01(W6H7CSEQ =' W-W6H7CSEQ-X ')'                        
005163          DELIMITED BY SIZE INTO SSA1                                     
005164     MOVE '  GE' TO GODK-STATUSKODER                                      
005165     CALL CBLTDLI USING GHU KVAE-PCB DLI-IO-AREA-KVAE01 SSA1              
005166     MOVE KVAE-STATUS-CODE TO STATUS-WS                                   
005167     PERFORM IMS-STATUSKONTROLL                                           
005168     .                                                                    
005169     SKIP3                                                                
005170 IMS-GHN-KVAE-W6H701 SECTION.                                             
005171     STRING 'W6KVAE01(W6H7CSEQ =' W-W6H7CSEQ-X ')'                        
005172          DELIMITED BY SIZE INTO SSA1                                     
005173     MOVE '  GEGB' TO GODK-STATUSKODER                                    
005174     CALL CBLTDLI USING GHN KVAE-PCB DLI-IO-AREA-KVAE01 SSA1              
005175     MOVE KVAE-STATUS-CODE TO STATUS-WS                                   
005176     PERFORM IMS-STATUSKONTROLL                                           
005177     .                                                                    
005178     SKIP3                                                                
005179 IMS-REPL-KVAE-W6H7 SECTION.                                              
005180     MOVE '  ' TO GODK-STATUSKODER                                        
005181     CALL CBLTDLI USING REPL KVAE-PCB DLI-IO-AREA-KVAE01                  
005182     MOVE KVAE-STATUS-CODE TO STATUS-WS                                   
005183     PERFORM IMS-STATUSKONTROLL                                           
005184     .                                                                    
005185     EJECT                                                                
005186 IMS-GHU-KVABSEQ-W6H701 SECTION.                                          
005187     STRING 'W6KVAE01(W6H7BSEQ>=' W-W6H7BSEQ-MIN-X                        
005188                    '&W6H7BSEQ<=' W-W6H7BSEQ-MAX-X                        
005189                    '&IDLEVNR  =' W-IDLEVNR-H7-MIN-X                      
005190                    '&KVKRKNTR=>' W-KVKRKNTR-H7-MIN-X                     
005191                    '&KVKRKNTR=<' W-KVKRKNTR-H7-MAX-X ')'                 
005192          DELIMITED BY SIZE INTO SSA1                                     
005193     MOVE '  GE' TO GODK-STATUSKODER                                      
005194     CALL CBLTDLI USING GHU KVABSEQ-PCB DLI-IO-AREA-KVAE01 SSA1           
005195     MOVE KVABSEQ-STATUS-CODE TO STATUS-WS                                
005196     PERFORM IMS-STATUSKONTROLL                                           
005197     .                                                                    
005198     SKIP3                                                                
005199 IMS-GHN-KVABSEQ-W6H701 SECTION.                                          
005200     STRING 'W6KVAE01(W6H7BSEQ>=' W-W6H7BSEQ-MIN-X                        
005201                    '&W6H7BSEQ<=' W-W6H7BSEQ-MAX-X                        
005202                    '&IDLEVNR  =' W-IDLEVNR-H7-MIN-X                      
005203                    '&KVKRKNTR=>' W-KVKRKNTR-H7-MIN-X                     
005204                    '&KVKRKNTR=<' W-KVKRKNTR-H7-MAX-X ')'                 
005205          DELIMITED BY SIZE INTO SSA1                                     
005206     MOVE '  GEGB' TO GODK-STATUSKODER                                    
005207     CALL CBLTDLI USING GHN KVABSEQ-PCB DLI-IO-AREA-KVAE01 SSA1           
005208     MOVE KVABSEQ-STATUS-CODE TO STATUS-WS                                
005209     PERFORM IMS-STATUSKONTROLL                                           
005210     .                                                                    
005211     SKIP3                                                                
005212 IMS-REPL-KVABSEQ-W6H7 SECTION.                                           
005213     MOVE '  ' TO GODK-STATUSKODER                                        
005214     CALL CBLTDLI USING REPL KVABSEQ-PCB DLI-IO-AREA-KVAE01               
005215     MOVE KVABSEQ-STATUS-CODE TO STATUS-WS                                
005216     PERFORM IMS-STATUSKONTROLL                                           
005217     .                                                                    
005218     EJECT                                                                
005219 IMS-GHU-LOPA-LOPA11 SECTION.                                             
005220     STRING 'W6LOPA01(W6GXKEY  =' W-W6GXKEY-6017-X ')'                    
005221          DELIMITED BY SIZE INTO SSA1                                     
005222     STRING 'W6LOPA11(KDSEGKEY =' W-W6GXKEY-6018-X ')'                    
005223          DELIMITED BY SIZE INTO SSA2                                     
005224     MOVE '  GE' TO GODK-STATUSKODER                                      
005225     CALL CBLTDLI USING GHU LOPA-PCB DLI-IO-AREA SSA1 SSA2                
005226     MOVE LOPA-STATUS-CODE TO STATUS-WS                                   
005227     PERFORM IMS-STATUSKONTROLL                                           
005228     .                                                                    
005229     SKIP3                                                                
005230 IMS-REPL-LOPA-LOPA11 SECTION.                                            
005231     MOVE '  ' TO GODK-STATUSKODER                                        
005232     CALL CBLTDLI USING REPL LOPA-PCB DLI-IO-AREA                         
005233     MOVE LOPA-STATUS-CODE TO STATUS-WS                                   
005234     PERFORM IMS-STATUSKONTROLL                                           
005235     .                                                                    
005236     EJECT                                                                
005237 IMS-GU-UPFA01 SECTION.                                                   
005238     STRING 'W6UPFA01(IDLOPNRM =' W-IDLOPNRM-X ')'                        
005239          DELIMITED BY SIZE INTO SSA1                                     
005240     MOVE '  GE' TO GODK-STATUSKODER                                      
005241     CALL CBLTDLI USING GU UPFA-PCB DLI-IO-AREA-UPFA01 SSA1               
005242     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
005243     PERFORM IMS-STATUSKONTROLL                                           
005244     .                                                                    
005245     SKIP3                                                                
005246 IMS-GU-WDK611 SECTION.                                                   
005247     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
005248          DELIMITED BY SIZE INTO SSA1                                     
005249     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY ')'                          
005250          DELIMITED BY SIZE INTO SSA2                                     
005251     MOVE '  GE' TO GODK-STATUSKODER                                      
005252     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-WDK6 SSA1 SSA2            
005253     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
005254     PERFORM IMS-STATUSKONTROLL                                           
005255     .                                                                    
005256     SKIP2                                                                
005257 IMS-GNP-UPFA11 SECTION.                                                  
005258     STRING 'W6UPFA01(IDLOPNRM =' W-IDLOPNRM-X ')'                        
005259          DELIMITED BY SIZE INTO SSA1                                     
005260     MOVE 'W6UPFA11 ' TO SSA2                                             
005261     MOVE '  GE' TO GODK-STATUSKODER                                      
005262     CALL CBLTDLI USING GNP UPFA-PCB DLI-IO-AREA-UPFA11 SSA1 SSA2         
005263     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
005264     PERFORM IMS-STATUSKONTROLL                                           
005265     .                                                                    
005266     SKIP3                                                                
005267 IMS-GNP-UPFA12 SECTION.                                                  
005268     STRING 'W6UPFA01(IDLOPNRM =' W-IDLOPNRM-X ')'                        
005269          DELIMITED BY SIZE INTO SSA1                                     
005270     MOVE 'W6UPFA12 ' TO SSA2                                             
005271     MOVE '  GE' TO GODK-STATUSKODER                                      
005272     CALL CBLTDLI USING GNP UPFA-PCB DLI-IO-AREA-UPFA12 SSA1 SSA2         
005273     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
005274     PERFORM IMS-STATUSKONTROLL                                           
005275     .                                                                    
005276     SKIP3                                                                
005277 IMS-GU-WDB601    SECTION.                                                
005278     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
005279          DELIMITED BY SIZE INTO SSA1                                     
005280     MOVE '  GE' TO GODK-STATUSKODER                                      
005281     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
005282     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
005283     PERFORM IMS-STATUSKONTROLL                                           
005284     IF SEGMENT-SAKNAS                                                    
005285         MOVE SPACE TO DCS-KDDC                                           
005286     END-IF                                                               
005287     .                                                                    
005288 IMS-GU-WDD311 SECTION.                                                   
005289     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
005290             DELIMITED BY SIZE INTO SSA1                                  
005291     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
005292              DELIMITED BY SIZE INTO SSA2                                 
005293     MOVE '  GE' TO GODK-STATUSKODER                                      
005294     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
005295     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
005296     PERFORM IMS-STATUSKONTROLL                                           
005297     .                                                                    
005298     EJECT                                                                
005299 IMS-STATUSKONTROLL SECTION.                                              
005300     SET STATUS-IX TO 1                                                   
005301     SEARCH GODK-STATUS                                                   
005302       AT END                                                             
005303         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
005304         DELIMITED BY SIZE INTO FELTEXT                                   
005305         CALL FELLOG                                                      
005306       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
005307         CONTINUE                                                         
005308     END-SEARCH                                                           
005309     .                                                                    
