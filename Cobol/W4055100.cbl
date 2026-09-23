000001 ID DIVISION.                                                             
000002     SKIP2                                                                
000003 PROGRAM-ID.     W4055100.                                                
000004 AUTHOR.         GERRY CARMICHAEL.                                        
000005 DATE-WRITTEN.   90/10/01.                                                
000006                                                                          
000007     REMARKS.                                                             
000008*                                                                         
000009*    FUNKTION.                                                            
000010*        PROGRAMMET VISAR TRANSPORTAVGÅNGSTID,DESTINATION,                
000011*        TRANSPORTÖR OCH TOTAL VÄRDE,-VIKT OCH -VOLYM PER                 
000012*        AVGÅNGSTID.BLÄDDRING FÖREKOMMER(ENTER OCH PF8).                  
000013*        ORDRAR MED TRANSPORTAVGÅNGSTIDER SOM LIGGER BAKÅT                
000014*        I TIDEN VISAS UPPLYST TILLSAMMANS MED DE FÖRSTA                  
000015*        ORDRAR SOM HAR EN TRANSPORTAVGÅNGSTID FRAMÅT I                   
000016*        TIDEN.DÄREFTER VISAS FRAMTIDA TRANSPORTAVGÅNGSTIDER              
000017*        VAR FÖR SIG TILLS DET INTE FINNS FLER OCH SIST VISAS             
000018*        ORDRAR SOM INTE HAR NÅGON TRANSPORTAVGÅNGSTID.                   
000019*        VIKT,VOLYM OCH VÄRDE TAS FRÅN RDQ3 DÅ STATUS ÄR                  
000020*        'R' ELLER 'U' OCH FRÅN RDE6 DÅ STATUS ÄR 'P'.                    
000021*                                                                         
000022*                                                                         
000023*        PROGRAMMET ÄR EN FRÅGE-MPP                                       
000024*        PROGRAMMET LÄSER      WLORQA (WDQ3)                              
000025*        PROGRAMMET LÄSER      WLORQI (WDQ2)                              
000026*        PROGRAMMET LÄSER      WDE6                                       
000027*        PROGRAMMET LÄSER      WLXXKA (WDR1)                              
000028*        PROGRAMMET LÄSER      WLXXKB (WDR1)                              
000029*                                                                         
000030*                                                                         
000031*    INDATA.                                                              
000032*        TRANSAKTION: W4T551                                              
000033*        MID:         W4I55101                                            
000034*                                                                         
000035*    UTDATA.                                                              
000036*        MOD:         W4O55101                                            
000037                                                                          
000038     SKIP3                                                                
000039 ENVIRONMENT DIVISION.                                                    
000040     EJECT                                                                
000041 DATA DIVISION.                                                           
000042 WORKING-STORAGE SECTION.                                                 
000043*    -COPY WY2000W9                                                       
000044     SKIP3                                                                
000045*    -COPY WY2000W1                                                       
000046     SKIP3                                                                
000047 77  IDPGM                       PIC X(08)   VALUE 'W4055100'.            
000048 77    PGM-POS                   PIC X(24).                               
000049                                                                          
000050 77  JA                          PIC X       VALUE 'J'.                   
000051 77  NEJ                         PIC X       VALUE 'N'.                   
000052 77  FELTEXT                     PIC X(64)  VALUE SPACE.                  
000053 77  RKOD-ABEND                  PIC S9(4)  VALUE +33   COMP SYNC.        
000054                                                                          
000055*    --- INDEX FÖR BLÄDDRINGSRADER                                        
000056 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
000057 77  IX                          PIC S9(4)  VALUE +0    COMP SYNC.        
000058 77  IX-MAX                      PIC S9(4)  VALUE +7    COMP SYNC.        
000059 77  MAX-INDX                    PIC S9(4)  VALUE +13   COMP SYNC.        
000060 77  MAX-INDX-PLUS1              PIC S9(4)  VALUE +14   COMP SYNC.        
000061                                                                          
000062 77  DAG-INDX                    PIC S9(4)  VALUE +0    COMP SYNC.        
000063 77  MAX-DAG-INDX                PIC S9(4)  VALUE +5    COMP SYNC.        
000064 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
000065 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +1135 COMP SYNC.        
000066                                                                          
000067*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
000068 01  WS-IDTRP.                                                            
000069   03  WS-IDTRPLOS               PIC X(3)    VALUE SPACE.                 
000070   03  WS-IDTRPVAR               PIC X(2)    VALUE SPACE.                 
000071 01  WS-SPARSTATUS.                                                       
000072   03  WS-R                      PIC X(1)    VALUE SPACE.                 
000073   03  WS-U                      PIC X(1)    VALUE SPACE.                 
000074   03  WS-P                      PIC X(1)    VALUE SPACE.                 
000075 01  WS-HELPSTATUS               PIC X(3)    VALUE SPACE.                 
000076 01  WS-STATUS-MOD               PIC X(2)    VALUE SPACE.                 
000077 01 WS-VVDTTMM.                                                           
000078   03  WS-VECKA                  PIC 9(2)    VALUE ZERO.                  
000079   03  WS-DAG                    PIC 9       VALUE ZERO.                  
000080   03  WS-TTMM                   PIC 9(4)    VALUE ZERO.                  
000081 77  HELP-TITRPAVG               PIC 9(7)    VALUE ZERO.                  
000082 77  FULL-TITRPAVG               PIC 9(7)    VALUE ZERO.                  
000083 77  HELP-NUTID                  PIC 9(7)    VALUE ZERO.                  
000084 77  WS-VECKODAG                 PIC 9(1)    VALUE ZERO.                  
000085 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
000086 77  DAGENS-DATUM-Y2K            PIC 9(8)    VALUE ZERO.                  
000087 77  WS-Q3-DATUM                 PIC 9(6)    VALUE ZERO.                  
000088 77  DAGENS-TID                  PIC 9(8)    VALUE ZERO.                  
000089 77  DAGENS-HOUR                 PIC 9(4)    VALUE ZERO.                  
000090 01 WS-NUTID.                                                             
000091   03  WS-WEEK                   PIC 9(2)    VALUE ZERO.                  
000092   03  WS-DAY                    PIC 9       VALUE ZERO.                  
000093   03  WS-HOUR                   PIC 9(4)    VALUE ZERO.                  
000094 77  WS-KDODELSTA                PIC X(1)    VALUE SPACE.                 
000095 77  WS-KDORDSTA                 PIC X(2)    VALUE SPACE.                 
000096 77  WS-IDORDER                  PIC S9(7)      VALUE +0   COMP-3.        
000097 01  WS-DATRPAVT.                                                         
000098   03  WS-DATRPAVD               PIC  9(8)      VALUE ZERO.               
000099   03  WS-TIHHMM                 PIC S9(5)      VALUE +0   COMP-3.        
000100 01  SPAR-DATRPAVT.                                                       
000101   03  SPAR-DATRPAVD             PIC  9(8)      VALUE ZERO.               
000102   03  SPAR-TIHHMM               PIC S9(5)      VALUE +0   COMP-3.        
000103 01  SPAR-ODEL-DATRPAVT.                                                  
000104   03  SPAR-ODEL-DATRPAVD        PIC  9(8)      VALUE ZERO.               
000105   03  FILLER REDEFINES SPAR-ODEL-DATRPAVD.                               
000106       05  FILLER                PIC  9(2).                               
000107       05  SPAR-ODEL-YYMMDD      PIC  9(6).                               
000108   03  SPAR-ODEL-TIHHMM          PIC S9(5)      VALUE +0   COMP-3.        
000109 01  W-SAVE-DATRPAVT.                                                     
000110   03  W-SAVE-DATRPAVD           PIC  9(8)      VALUE ZERO.               
000111   03  W-SAVE-TIHHMM             PIC S9(5)      VALUE +0   COMP-3.        
000112                                                                          
000113 77  WS-VLORDNTO-SUM             PIC S9(4)V9(3) VALUE +0   COMP-3.        
000114 77  WS-VLORDNTO-TOT             PIC S9(4)V9(3) VALUE +0   COMP-3.        
000115 77  WS-VKORDNTO-SUM             PIC S9(6)V9(1) VALUE +0   COMP-3.        
000116 77  WS-VKORDNTO-TOT             PIC S9(6)V9(1) VALUE +0   COMP-3.        
000117 77  WS-SUORDV-SUM               PIC S9(9)V9(2) VALUE +0   COMP-3.        
000118 77  WS-SUORDV-LOC-SUM           PIC S9(9)V9(2) VALUE +0   COMP-3.        
000119 77  WS-SUORDV-LOCPREL-SUM       PIC S9(9)V9(2) VALUE +0   COMP-3.        
000120 77  WS-SUORDV-TOT               PIC S9(9)V9(2) VALUE +0   COMP-3.        
000121 77  WS-SUORDV-LOC-TOT           PIC S9(9)V9(2) VALUE +0   COMP-3.        
000122 77  WS-SUORDV-LOCPREL-TOT       PIC S9(9)V9(2) VALUE +0   COMP-3.        
000123                                                                          
000124 77  SPAR-ODEL-IDTRP             PIC X(5)    VALUE SPACE.                 
000125 77  SPAR-ODEL-IDDISTR           PIC S9(5)   VALUE +0   COMP-3.           
000126 77  SPAR-ODEL-IDKUNDNR          PIC S9(7)   VALUE +0   COMP-3.           
000127 77  SPAR-ODEL-IDPRODNR          PIC S9(7)   VALUE +0   COMP-3.           
000128 77  SPAR-ODEL-IDKUNDRF          PIC 9(7)    VALUE ZERO.                  
000129                                                                          
000130 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
000131     88  NYCKLAR-OK                          VALUE 'J'.                   
000132     88  NYCKLAR-FEL                         VALUE 'N'.                   
000133                                                                          
000134 77  ALLT-SW                     PIC X       VALUE 'J'.                   
000135     88  ALLT-OK                             VALUE 'J'.                   
000136                                                                          
000137 77  IDTRP-SW                    PIC X       VALUE 'J'.                   
000138     88  IDTRP-OK                            VALUE 'J'.                   
000139                                                                          
000140 77  SCROLL-SW                   PIC X       VALUE 'J'.                   
000141     88  SCROLL-OK                           VALUE 'J'.                   
000142                                                                          
000143 77  TOM-BILD-SW                 PIC X       VALUE 'J'.                   
000144     88  TOM-BILD                            VALUE 'J'.                   
000145                                                                          
000146 77  FRAMTIDA-AVGANG-SW          PIC X       VALUE 'J'.                   
000147     88  FRAMTIDA-AVGANGAR                   VALUE 'J'.                   
000148                                                                          
000149 77  ANNAN-AVGANG-SW             PIC X       VALUE 'J'.                   
000150     88  ANNAN-AVGANG                        VALUE 'J'.                   
000151                                                                          
000152 77  KDODELSTA-SW                PIC X       VALUE 'J'.                   
000153     88  KDODELSTA-IFYLLT                    VALUE 'J'.                   
000154                                                                          
000155 77  STATUS-SW                   PIC X       VALUE 'J'.                   
000156     88  STATUS-OK                           VALUE 'J'.                   
000157                                                                          
000158 77  FIRST-TIME-SW               PIC X       VALUE 'J'.                   
000159     88  FIRST-TIME                          VALUE 'J'.                   
000160                                                                          
000161 77  Q3-SW                       PIC X       VALUE 'J'.                   
000162     88  Q3-FOUND                            VALUE 'J'.                   
000163                                                                          
000164 77  KDODELSTA-KONTROLL          PIC X       VALUE SPACE.                 
000165     88  GODK-KDODELSTA                      VALUE 'P' 'R' 'U'            
000166                                                   '*'.                   
000167                                                                          
000168 77  KDORDSTA-KONTROLL            PIC X(2)   VALUE SPACE.                 
000169     88  GODK-KDORDSTA                       VALUE 'R ' 'R*' 'U '         
000170                                                   'U*' 'P ' 'P*'.        
000171                                                                          
000172 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
000173     88  EGEN-MID                            VALUE '4551'.                
000174     88  GODK-MID                            VALUE '4551' '4552'          
000175                                                   '4553' '4554'          
000176                                                   '4555' '4556'          
000177                                                   '4557' '4558'          
000178                                                   '4559'.                
000179     EJECT                                                                
000180*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
000181 01  GENERELLA-SUBPROGRAM.                                                
000182     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
000183     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
000184     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000185     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000186     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
000187     EJECT                                                                
000188*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
000189*   -COPY WMEDAREA                                                        
000190     SKIP3                                                                
000191*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
000192*   -COPY WDATAREA                                                        
000193*                                                                         
000194*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
000195 01  FILLER                     PIC X(16)   VALUE 'WMSGINIT '.            
000196*01 -COPY WMSGINIT                                                        
000197*                                                                         
000198     SKIP3                                                                
000199 01  MESSAGE-CODES.                                                       
000200     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
000201     03  INF-NEW-DEPARTURE       PIC X(3)    VALUE '043'.                 
000202     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
000203     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
000204     EJECT                                                                
000205*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
000206*                                                                         
000207 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
000208     SKIP3                                                                
000209*01  MID -COPY W4I55101                                                   
000210     EJECT                                                                
000211 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
000212     SKIP3                                                                
000213*01  -COPY WMSGAREA                                                       
000214     EJECT                                                                
000215     03  MOD REDEFINES MSG-AREA.                                          
000216*      05  -COPY W4O55101                                                 
000217     EJECT                                                                
000218 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
000219     SKIP3                                                                
000220*01  -COPY WMFSAREA                                                       
000221     EJECT                                                                
000222*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
000223*                                                                         
000224 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000225     SKIP3                                                                
000226 01  NYCKLAR-TILL-DLI.                                                    
000227     03  W-WDQ301KY-MIN-X.                                                
000228         05  W-ODEL-IDORDER-MIN    PIC S9(7)  VALUE +0   COMP-3.          
000229         05  W-ODEL-IDDC-MIN       PIC X(2)   VALUE SPACE.                
000230         05  W-ODEL-IDPRODNR-MIN   PIC S9(7)  VALUE +0   COMP-3.          
000231         05  W-ODEL-IDPLKLST-MIN   PIC S9(3)  VALUE +0   COMP-3.          
000232     03  W-WDQ301KY-MAX-X.                                                
000233         05  W-ODEL-IDORDER-MAX    PIC S9(7)  VALUE +0   COMP-3.          
000234         05  W-ODEL-IDDC-MAX       PIC X(2)   VALUE SPACE.                
000235         05  W-ODEL-IDPRODNR-MAX   PIC S9(7)  VALUE +0   COMP-3.          
000236         05  W-ODEL-IDPLKLST-MAX   PIC S9(3)  VALUE +0   COMP-3.          
000237     03  W-KDODELST-X.                                                    
000238         05  W-KDODELST            PIC X(1)   VALUE SPACE.                
000239     03  W-IDPRODNR-X.                                                    
000240         05  W-VORD-IDPRODNR       PIC S9(7)  VALUE +0   COMP-3.          
000241     03  W-WDQ2BSEQ-MIN-X.                                                
000242         05  W-SEQB-IDDC-MIN       PIC X(2)   VALUE SPACE.                
000243         05  W-SEQB-IDTRP-MIN.                                            
000244           07  W-SEQB-IDTRPLOS-MIN PIC X(3)   VALUE SPACE.                
000245           07  W-SEQB-IDTRPVAR-MIN PIC X(2)   VALUE SPACE.                
000246         05  W-SEQB-DATRPAVT-MIN.                                         
000247           07  W-SEQB-DATRPAVD-MIN PIC  9(8)  VALUE ZERO.                 
000248           07  W-SEQB-TIHHMM-MIN   PIC S9(5)  VALUE +0   COMP-3.          
000249     03  W-WDQ2BSEQ-MAX-X.                                                
000250         05  W-SEQB-IDDC-MAX       PIC X(2)   VALUE SPACE.                
000251         05  W-SEQB-IDTRP-MAX.                                            
000252           07  W-SEQB-IDTRPLOS-MAX PIC X(3)   VALUE SPACE.                
000253           07  W-SEQB-IDTRPVAR-MAX PIC X(2)   VALUE SPACE.                
000254         05  W-SEQB-DATRPAVT-MAX.                                         
000255          07  W-SEQB-DATRPAVD-MAX PIC  9(8) VALUE ZERO.                   
000256          07  W-SEQB-TIHHMM-MAX   PIC S9(5) VALUE +0     COMP-3.          
000257     03  W-4431-IDHTYP-X.                                                 
000258         05  W-4431-IDHTYP       PIC X(4)     VALUE '4431'.               
000259         05  W-4431-IDDC         PIC X(2)     VALUE SPACE.                
000260         05  W-4431-LOW-VALUE    PIC X(24)    VALUE LOW-VALUE.            
000261     03  W-4432-IDTRP-X.                                                  
000262         05  W-4432-IDTRP.                                                
000263           07  W-4432-IDTRPLOS   PIC X(3)     VALUE SPACE.                
000264           07  W-4432-IDTRPVAR   PIC X(2)     VALUE SPACE.                
000265     03  W-4433-IDHTYP-X.                                                 
000266         05  W-4433-IDHTYP       PIC X(4)     VALUE '4433'.               
000267         05  W-4433-IDDC         PIC X(2)     VALUE SPACE.                
000268         05  W-4433-LOW-VALUE    PIC X(24)    VALUE LOW-VALUE.            
000269     03  W-4434-IDTRP-X.                                                  
000270         05  W-4434-IDTRP.                                                
000271           07  W-4434-IDTRPLOS   PIC X(3)     VALUE SPACE.                
000272           07  W-4434-IDTRPVAR   PIC X(2)     VALUE SPACE.                
000273         05  W-4434-TITRPAVG     PIC S9(7)    VALUE +0   COMP-3.          
000274         05  W-4434-LOW-VALUE    PIC X(1)     VALUE LOW-VALUE.            
000275                                                                          
000276     03  W-IDDC-B6-X.                                                     
000277         05 W-IDDC-B6                  PIC X(2).                          
000278                                                                          
000279     03  W-IDDC-X.                                                        
000280         05  W-IDDC                    PIC X(2).                          
000281*                                                                         
000282*    --- STATUS-KOD FRÅN IMS                                              
000283 01  STATUS-WS                   PIC XX.                                  
000284     88  SEGMENT-FINNS                       VALUE '  '.                  
000285     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
000286     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
000287     88  BASEN-SLUT                          VALUE 'GB'.                  
000288     SKIP2                                                                
000289 01  GODK-STATUSKODER.                                                    
000290     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000291     SKIP3                                                                
000292 01  SSA1                        PIC X(128).                              
000293 01  SSA2                        PIC X(64).                               
000294     EJECT                                                                
000295*    --- IMS FUNKTIONSKODER                                               
000296*01  -COPY W0003                                                          
000297     EJECT                                                                
000298*    ---  DLI INPUT-OUTPUT AREA                                           
000299     SKIP3                                                                
000300 01  FILLER                  PIC X(16) VALUE 'DLI-IO-AREA-VORD'.          
000301 01  DLI-IO-AREA-VORD.                                                    
000302*        05  -COPY WDE601                                                 
000303     SKIP3                                                                
000304 01  FILLER                  PIC X(16) VALUE 'DLI-IO-AREA-KOLI'.          
000305 01  DLI-IO-AREA-KOLLI.                                                   
000306*        05  -COPY WDE611                                                 
000307     SKIP3                                                                
000308 01  FILLER                  PIC X(16) VALUE 'DLI-IO-AREA-ODEL'.          
000309 01  DLI-IO-AREA-ODEL.                                                    
000310     03  WLORQA01.                                                        
000311*        05  -COPY WDQ301                                                 
000312     SKIP3                                                                
000313 01  FILLER                  PIC X(16) VALUE 'DLI-IO-AREA-ORQI'.          
000314 01  DLI-IO-AREA-ORQI.                                                    
000315     03  WLORQI01.                                                        
000316*        05  -COPY WDQ201                                                 
000317     SKIP3                                                                
000318 01  FILLER                  PIC X(16) VALUE 'DLI-IO-AREA-ARB'.           
000319 01  DLI-IO-AREA-ARB.                                                     
000320     03  WLORQI12.                                                        
000321*        05  -COPY WDQ212                                                 
000322     SKIP3                                                                
000323 01  FILLER                  PIC X(16) VALUE 'DLI-IO-AREA-4431'.          
000324 01  DLI-IO-AREA-4431.                                                    
000325     03  WLXXKA01.                                                        
000326*        05  -COPY WDGX4431                                               
000327     EJECT                                                                
000328 01  FILLER                  PIC X(16) VALUE 'DLI-IO-AREA-4432'.          
000329 01  DLI-IO-AREA-4432.                                                    
000330     03  WLXXKA11.                                                        
000331*        05  -COPY WDGX4432                                               
000332     SKIP3                                                                
000333 01  FILLER                  PIC X(16) VALUE 'DLI-IO-AREA-4433'.          
000334 01  DLI-IO-AREA-4433.                                                    
000335     03  WLXXKB01.                                                        
000336*        05  -COPY WDGX4433                                               
000337     EJECT                                                                
000338 01  FILLER                  PIC X(16) VALUE 'DLI-IO-AREA-4434'.          
000339 01  DLI-IO-AREA-4434.                                                    
000340     03  WLXXKB11.                                                        
000341*        05  -COPY WDGX4434                                               
000342                                                                          
000343 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
000344 01   DLI-IO-AREA-B601.                                                   
000345*     03  -COPY WDB601                                                    
000346     EJECT                                                                
000347*    ----DISTR-DEALER-PRICE-----                                          
000348*01  -COPY WWDIST79                                                       
000349     EJECT                                                                
000350 LINKAGE SECTION.                                                         
000351                                                                          
000352*01  -COPY W0009      -PRE MSG-                                           
000353     EJECT                                                                
000354*01  -COPY W0008      -PRE USEA-                                          
000355     05  FILLER                  PIC X.                                   
000356     EJECT                                                                
000357*01  -COPY W0008      -PRE WDE6-                                          
000358     05  FILLER                  PIC X.                                   
000359     EJECT                                                                
000360*01  -COPY W0008      -PRE ORQA-                                          
000361     05  FILLER                  PIC X.                                   
000362     EJECT                                                                
000363*01  -COPY W0008      -PRE ORQI-                                          
000364     05  FILLER                  PIC X(7).                                
000365     05  KEYFB-DATRPAVT.                                                  
000366       07  KEYFB-DATRPAVD          PIC  9(8).                             
000367       07  KEYFB-TIHHMM            PIC S9(5) COMP-3.                      
000368     EJECT                                                                
000369*01  -COPY W0008      -PRE XXKA-                                          
000370     05  FILLER                  PIC X.                                   
000371     EJECT                                                                
000372*01  -COPY W0008      -PRE XXKB-                                          
000373     05  FILLER                  PIC X.                                   
000374     EJECT                                                                
000375*01  -COPY W0008      -PRE WDB6-                                          
000376     05  FILLER                  PIC X.                                   
000377     EJECT                                                                
000378 PROCEDURE DIVISION  USING MSG-PCB  USEA-PCB WDE6-PCB ORQA-PCB            
000379                           ORQI-PCB XXKA-PCB XXKB-PCB WDB6-PCB.           
000380     ENTRY 'DLITCBL' USING MSG-PCB  USEA-PCB WDE6-PCB ORQA-PCB            
000381                           ORQI-PCB XXKA-PCB XXKB-PCB WDB6-PCB.           
000382                                                                          
000383     PERFORM IMS-GET-MSG                                                  
000384     IF SEGMENT-FINNS                                                     
000385       PERFORM A-INIT                                                     
000386       PERFORM B-KOLLA-NYCKLAR                                            
000387       IF NYCKLAR-OK                                                      
000388           IF MFS-FIRST                                                   
000389             PERFORM C-FOERSTA-SIDA                                       
000390           ELSE                                                           
000391             IF MID-FLAGGA-SCROLL = JA                                    
000392               MOVE '8'        TO MFS-IDPFK                               
000393             ELSE                                                         
000394               MOVE SPACE      TO MFS-IDPFK                               
000395             END-IF                                                       
000396             IF MFS-NEXT                                                  
000397               PERFORM D-NAESTA-SIDA                                      
000398             ELSE                                                         
000399               PERFORM E-SAMMA-SIDA                                       
000400             END-IF                                                       
000401           END-IF                                                         
000402           IF ALLT-OK                                                     
000403             PERFORM F-KOLLA-OM-INFO-FINNS                                
000404             IF SEGMENT-FINNS                                             
000405               PERFORM G-LAES-VISA-INFO                                   
000406             ELSE                                                         
000407               MOVE '413' TO MED-IDMFSINF                                 
000408               CALL WMEDKONV USING MED-WMEDAREA                           
000409               MOVE MED-MFSINF TO MOD-TEMFSINF                            
000410             END-IF                                                       
000411           END-IF                                                         
000412       END-IF                                                             
000413       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
000414       PERFORM IMS-INSERT-MSG                                             
000415     END-IF                                                               
000416                                                                          
000417     MOVE ZERO TO RETURN-CODE                                             
000418     GOBACK                                                               
000419     .                                                                    
000420     EJECT                                                                
000421 A-INIT SECTION.                                                          
000422                                                                          
000423     IF MSG-DUBBLA-TRANSKODER                                             
000424       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I55101                 
000425       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
000426       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
000427     ELSE                                                                 
000428       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I55101                  
000429       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
000430       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
000431     END-IF                                                               
000432                                                                          
000433     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
000434     MOVE MSG-IDPFK TO MFS-IDPFK                                          
000435     MOVE MFS-IDTRANS TO W-IDTRANS                                        
000436                                                                          
000437     MOVE LOW-VALUE TO MSG-AREA                                           
000438     MOVE 'W4O551N1' TO MFS-IDMOD                                         
000439     MOVE '4551' TO MOD-IDTRANS                                           
000440     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
000441                                                                          
000442     IF NOT EGEN-MID                                                      
000443       MOVE SPACE TO MFS-KDTRTYP                                          
000444       MOVE '7' TO MFS-IDPFK                                              
000445     END-IF                                                               
000446                                                                          
000447     IF ENGLISH-TEXT                                                      
000448       MOVE +2                 TO SPRAK-IX                                
000449       MOVE 'GB '              TO MED-IDSKYLT                             
000450     ELSE                                                                 
000451       MOVE +1                 TO SPRAK-IX                                
000452       MOVE '   '              TO MED-IDSKYLT                             
000453     END-IF                                                               
000454                                                                          
000455     PERFORM AA-HAEMTA-TIDEN                                              
000456     .                                                                    
000457     EJECT                                                                
000458 AA-HAEMTA-TIDEN SECTION.                                                 
000459                                                                          
000460     ACCEPT DAGENS-TID FROM TIME                                          
000461     ACCEPT DAGENS-DATUM FROM DATE                                        
000462     MOVE FUNCTION CURRENT-DATE (1:8) TO DAGENS-DATUM-Y2K                 
000463                                                                          
000464     MOVE ALL '+'              TO MSGI-WMSGINIT                           
000465     MOVE '011'                TO MSGI-KDCALL                             
000466     MOVE MSG-SIGNON-USERID    TO MSGI-IDUSER                             
000467                                  MSGI-IDLTERM-USER                       
000468     MOVE DAGENS-DATUM         TO MSGI-TILOKDAT                           
000469     MOVE DAGENS-TID           TO MSGI-TILOKTID                           
000470     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
000471     MOVE MSGI-TILOKDAT        TO DAGENS-DATUM                            
000472     MOVE MSGI-TILOKTID        TO DAGENS-TID                              
000473                                                                          
000474     MOVE DAGENS-TID (1:4)     TO WS-HOUR                                 
000475                                  DAGENS-HOUR                             
000476     MOVE DAGENS-HOUR          TO SPAR-TIHHMM                             
000477     MOVE DAGENS-DATUM-Y2K     TO SPAR-DATRPAVD                           
000478     MOVE 'AAMMDD'             TO DAT-KDDATFORM                           
000479     MOVE DAGENS-DATUM         TO DAT-I-TIDATUM                           
000480     CALL WDATKONV USING DAT-KDDATFORM, DAT-I-TIDATUM,                    
000481                         DAT-O-TIDATUM, DAT-KDSVAR                        
000482     IF DAT-KDSVAR-OK                                                     
000483       MOVE DAT-TID            TO WS-DAY                                  
000484       MOVE DAT-TIVV           TO WS-WEEK                                 
000485     END-IF                                                               
000486     MOVE WS-NUTID TO HELP-NUTID                                          
000487     .                                                                    
000488     EJECT                                                                
000489 B-KOLLA-NYCKLAR SECTION.                                                 
000490                                                                          
000491     MOVE JA                   TO NYCKLAR-SW                              
000492                                  IDTRP-SW                                
000493     MOVE NEJ                  TO KDODELSTA-SW                            
000494                                  ANNAN-AVGANG-SW                         
000495                                                                          
000496     MOVE LOW-VALUE            TO W-4432-IDTRP-X                          
000497                                  W-4434-IDTRP-X                          
000498                                  W-WDQ2BSEQ-MIN-X                        
000499                                  W-WDQ301KY-MIN-X                        
000500                                  W-IDPRODNR-X                            
000501                                                                          
000502     MOVE HIGH-VALUE           TO W-WDQ301KY-MAX-X                        
000503                                  W-WDQ2BSEQ-MAX-X                        
000504                                                                          
000505     MOVE MFS-RENSA-FAELT      TO MOD-IDDC-IN                             
000506                                  MOD-IDTRP-IN                            
000507                                  MOD-KDODELSTA-IN                        
000508                                                                          
000509     PERFORM BA-KOLLA-IDDC                                                
000510     PERFORM BB-KOLLA-IDTRP                                               
000511     PERFORM BC-KOLLA-KDODELSTA                                           
000512                                                                          
000513     IF GODK-MID OR NYCKLAR-OK                                            
000514       MOVE DCS-IDDC           TO MOD-IDDC-UT                             
000515       MOVE WS-IDTRP           TO MOD-IDTRP-UT                            
000516       MOVE WS-KDODELSTA       TO MOD-KDODELSTA-UT                        
000517     ELSE                                                                 
000518       MOVE MFS-RENSA-FAELT    TO MOD-IDDC-UT                             
000519                                  MOD-IDTRP-UT                            
000520                                  MOD-KDODELSTA-UT                        
000521     END-IF                                                               
000522                                                                          
000523     IF NYCKLAR-FEL                                                       
000524       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
000525       CALL WMEDKONV USING MED-WMEDAREA                                   
000526       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
000527       PERFORM MFS-RENSA-FAELT-IN                                         
000528       PERFORM MFS-RENSA-FAELT-UT                                         
000529     END-IF                                                               
000530     .                                                                    
000531     EJECT                                                                
000532 BA-KOLLA-IDDC SECTION.                                                   
000533                                                                          
000534     IF MID-IDDC-IN = ALL '+'                                             
000535       IF MID-IDDC-UT = SPACE                                             
000536         MOVE NEJ              TO NYCKLAR-SW                              
000537       ELSE                                                               
000538         MOVE MID-IDDC-UT      TO W-IDDC-B6                               
000539       END-IF                                                             
000540     ELSE                                                                 
000541       MOVE NEJ TO MID-FLAGGA-EOF                                         
000542       MOVE MID-IDDC-IN        TO W-IDDC-B6                               
000543       MOVE '7'                TO MFS-IDPFK                               
000544       MOVE SPACE              TO MFS-KDTRTYP                             
000545     END-IF                                                               
000546                                                                          
000547     IF NYCKLAR-OK                                                        
000548        PERFORM IMS-GU-WDB601                                             
000549        IF DCS-KDDC NOT = SPACE                                           
000550           MOVE DCS-IDDC       TO W-SEQB-IDDC-MIN                         
000551                                  W-SEQB-IDDC-MAX                         
000552                                  W-ODEL-IDDC-MIN                         
000553                                  W-ODEL-IDDC-MAX                         
000554                                  W-4431-IDDC                             
000555                                  W-4433-IDDC                             
000556        ELSE                                                              
000557          MOVE NEJ             TO NYCKLAR-SW                              
000558        END-IF                                                            
000559     END-IF                                                               
000560     .                                                                    
000561     EJECT                                                                
000562                                                                          
000563 BB-KOLLA-IDTRP SECTION.                                                  
000564                                                                          
000565     IF MID-IDTRP-IN NOT = ALL '+'                                        
000566       MOVE +1                 TO IX                                      
000567       PERFORM 5 TIMES                                                    
000568         IF MID-IDTRP-IN (IX:1) = SPACE                                   
000569           MOVE NEJ            TO IDTRP-SW                                
000570                                  NYCKLAR-SW                              
000571         END-IF                                                           
000572         ADD +1                TO IX                                      
000573       END-PERFORM                                                        
000574     END-IF                                                               
000575                                                                          
000576     IF IDTRP-OK                                                          
000577       IF MID-IDTRP-IN (1:3) = ALL '+'                                    
000578         MOVE MID-IDTRP-UT (1:3) TO WS-IDTRPLOS                           
000579         INSPECT WS-IDTRPLOS REPLACING LEADING SPACE BY ZERO              
000580       ELSE                                                               
000581         MOVE NEJ TO MID-FLAGGA-EOF                                       
000582         MOVE MID-IDTRP-IN (1:3) TO WS-IDTRPLOS                           
000583         INSPECT WS-IDTRPLOS REPLACING LEADING SPACE BY ZERO              
000584         MOVE '7'              TO MFS-IDPFK                               
000585         MOVE SPACE            TO MFS-KDTRTYP                             
000586       END-IF                                                             
000587       IF WS-IDTRPLOS NUMERIC                                             
000588         MOVE WS-IDTRPLOS      TO W-SEQB-IDTRPLOS-MIN                     
000589         MOVE WS-IDTRPLOS      TO W-SEQB-IDTRPLOS-MAX                     
000590       ELSE                                                               
000591         MOVE NEJ TO NYCKLAR-SW                                           
000592       END-IF                                                             
000593       IF MID-IDTRP-IN (4:2) = ALL '+'                                    
000594         MOVE MID-IDTRP-UT (4:2) TO WS-IDTRPVAR                           
000595         INSPECT WS-IDTRPVAR REPLACING LEADING SPACE BY ZERO              
000596       ELSE                                                               
000597         MOVE MID-IDTRP-IN (4:2) TO WS-IDTRPVAR                           
000598         INSPECT WS-IDTRPVAR REPLACING LEADING SPACE BY ZERO              
000599         MOVE '7'       TO MFS-IDPFK                                      
000600         MOVE SPACE     TO MFS-KDTRTYP                                    
000601       END-IF                                                             
000602       MOVE WS-IDTRPVAR        TO W-SEQB-IDTRPVAR-MIN                     
000603       MOVE WS-IDTRPVAR        TO W-SEQB-IDTRPVAR-MAX                     
000604     ELSE                                                                 
000605       MOVE MID-IDTRP-IN (1:3) TO WS-IDTRPLOS                             
000606       MOVE MID-IDTRP-IN (4:2) TO WS-IDTRPVAR                             
000607     END-IF                                                               
000608     .                                                                    
000609     EJECT                                                                
000610 BC-KOLLA-KDODELSTA SECTION.                                              
000611                                                                          
000612     IF MID-KDODELSTA-IN = ALL '+'                                        
000613       MOVE MID-KDODELSTA-UT   TO WS-KDODELSTA                            
000614     ELSE                                                                 
000615       MOVE NEJ TO MID-FLAGGA-EOF                                         
000616       MOVE MID-KDODELSTA-IN   TO WS-KDODELSTA                            
000617       MOVE '7'                TO MFS-IDPFK                               
000618       MOVE SPACE              TO MFS-KDTRTYP                             
000619     END-IF                                                               
000620     IF WS-KDODELSTA NOT = SPACE                                          
000621       MOVE WS-KDODELSTA       TO KDODELSTA-KONTROLL                      
000622       IF GODK-KDODELSTA                                                  
000623         IF WS-KDODELSTA NOT = '*'                                        
000624           MOVE JA             TO KDODELSTA-SW                            
000625           MOVE WS-KDODELSTA   TO W-KDODELST                              
000626         END-IF                                                           
000627         IF WS-KDODELSTA = '*'                                            
000628           MOVE SPACE          TO WS-KDODELSTA                            
000629         END-IF                                                           
000630       ELSE                                                               
000631         MOVE NEJ              TO NYCKLAR-SW                              
000632       END-IF                                                             
000633     END-IF                                                               
000634     .                                                                    
000635     EJECT                                                                
000636 C-FOERSTA-SIDA SECTION.                                                  
000637                                                                          
000638*    MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
000639*    CALL WMEDKONV USING MED-WMEDAREA                                     
000640*    MOVE MED-MFSINF TO MOD-TEMFSINF                                      
000641                                                                          
000642*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
000643     MOVE ZERO                 TO MOD-TIAAMMDD-ENTER                      
000644                                  MOD-TIHHMM-ENTER                        
000645                                  MOD-TIAAMMDD-NEXT                       
000646                                  MOD-TIHHMM-NEXT                         
000647                                  MOD-IDORDER-ENTER                       
000648                                  MOD-IDORDER-NEXT                        
000649     MOVE JA                   TO ALLT-SW                                 
000650     .                                                                    
000651     EJECT                                                                
000652 D-NAESTA-SIDA SECTION.                                                   
000653     MOVE 'STA D-NAESTA-SIDA'  TO PGM-POS                                 
000654                                                                          
000655     IF MID-FLAGGA-SCROLL = JA                                            
000656       MOVE MID-TIAAMMDD-NEXT  TO W-SEQB-DATRPAVD-MIN                     
000657       IF MID-TIAAMMDD-NEXT NOT = ZERO                                    
000658         IF MID-TIAAMMDD-NEXT < 500000                                    
000659           MOVE 20             TO W-SEQB-DATRPAVD-MIN (1:2)               
000660         ELSE                                                             
000661           IF MID-TIAAMMDD-NEXT < 999999                                  
000662             MOVE 19           TO W-SEQB-DATRPAVD-MIN (1:2)               
000663           ELSE                                                           
000664             MOVE 99999999     TO W-SEQB-DATRPAVD-MIN                     
000665           END-IF                                                         
000666         END-IF                                                           
000667       END-IF                                                             
000668       MOVE MID-TIHHMM-NEXT    TO W-SEQB-TIHHMM-MIN                       
000669       MOVE MID-IDORDER-NEXT   TO WS-IDORDER                              
000670       MOVE MID-VLORDNTO-SPAR  TO MOD-VLORDNTO-TOTAL                      
000671                                  MOD-VLORDNTO-SPAR                       
000672                                  WS-VLORDNTO-TOT                         
000673       MOVE MID-VKORDNTO-SPAR  TO MOD-VKORDNTO-TOTAL                      
000674                                  MOD-VKORDNTO-SPAR                       
000675                                  WS-VKORDNTO-TOT                         
000676       MOVE MID-SUORDV-SPAR    TO MOD-SUORDV-TOTAL                        
000677                                  MOD-SUORDV-SPAR                         
000678       MOVE JA                 TO ALLT-SW                                 
000679     END-IF                                                               
000680     MOVE 'END D-NAESTA-SIDA'  TO PGM-POS                                 
000681     .                                                                    
000682     EJECT                                                                
000683 E-SAMMA-SIDA SECTION.                                                    
000684     MOVE 'STA E-SAMMA-SIDA '  TO PGM-POS                                 
000685                                                                          
000686     MOVE JA                   TO ALLT-SW                                 
000687     IF MID-FLAGGA-EOF NOT = JA                                           
000688       MOVE MID-TIAAMMDD-NEXT  TO TMP1-YYMMDD                             
000689       MOVE DAGENS-DATUM       TO TMP2-YYMMDD                             
000690       PERFORM WY2000P1                                                   
000691       IF ((MID-TIAAMMDD-ENTER = MID-TIAAMMDD-NEXT AND                    
000692          MID-TIHHMM-ENTER = MID-TIHHMM-NEXT) OR                          
000693        ((TMP1-YYMMDD      < TMP2-YYMMDD) OR                              
000694         (TMP1-YYMMDD      = TMP2-YYMMDD  AND                             
000695         MID-TIHHMM-NEXT < DAGENS-HOUR)))                                 
000696         MOVE MID-TIAAMMDD-ENTER TO    W-SEQB-DATRPAVD-MIN                
000697         IF MID-TIAAMMDD-ENTER NOT = ZERO                                 
000698           IF MID-TIAAMMDD-ENTER < 500000                                 
000699             MOVE 20            TO     W-SEQB-DATRPAVD-MIN (1:2)          
000700           ELSE                                                           
000701             IF MID-TIAAMMDD-ENTER < 999999                               
000702               MOVE 19          TO     W-SEQB-DATRPAVD-MIN (1:2)          
000703             ELSE                                                         
000704               MOVE 99999999    TO     W-SEQB-DATRPAVD-MIN                
000705             END-IF                                                       
000706           END-IF                                                         
000707         END-IF                                                           
000708                                                                          
000709         MOVE MID-TIHHMM-ENTER  TO       W-SEQB-TIHHMM-MIN                
000710         MOVE MID-IDORDER-ENTER TO       WS-IDORDER                       
000711       ELSE                                                               
000712         MOVE MID-TIAAMMDD-NEXT TO       W-SEQB-DATRPAVD-MIN              
000713         IF MID-TIAAMMDD-NEXT NOT = ZERO                                  
000714           IF MID-TIAAMMDD-NEXT < 500000                                  
000715             MOVE 20            TO     W-SEQB-DATRPAVD-MIN (1:2)          
000716           ELSE                                                           
000717             IF MID-TIAAMMDD-NEXT < 999999                                
000718               MOVE 19          TO     W-SEQB-DATRPAVD-MIN (1:2)          
000719             ELSE                                                         
000720               MOVE 99999999    TO     W-SEQB-DATRPAVD-MIN                
000721             END-IF                                                       
000722           END-IF                                                         
000723         END-IF                                                           
000724         MOVE MID-TIHHMM-NEXT  TO W-SEQB-TIHHMM-MIN                       
000725         MOVE MID-IDORDER-NEXT TO WS-IDORDER                              
000726         MOVE JA               TO ANNAN-AVGANG-SW                         
000727       END-IF                                                             
000728     END-IF                                                               
000729     MOVE 'END E-SAMMA-SIDA '  TO PGM-POS                                 
000730     .                                                                    
000731     EJECT                                                                
000732 F-KOLLA-OM-INFO-FINNS SECTION.                                           
000733     MOVE 'STA F-KOLLA-OM-INFO-FI'  TO PGM-POS                            
000734                                                                          
000735********** KEYFB SPARAS FÖR ATT TRPAVGTID KAN ÄNDRAS PÅ ********          
000736********** ARBETSTABELLEN MEN EJ PÅ ORDERDELSREGISTRET  ********          
000737                                                                          
000738     MOVE JA                   TO FIRST-TIME-SW                           
000739                                  TOM-BILD-SW                             
000740                                  FRAMTIDA-AVGANG-SW                      
000741                                  STATUS-SW                               
000742                                                                          
000743     MOVE NEJ                  TO SCROLL-SW                               
000744                                  MOD-FLAGGA-SCROLL                       
000745                                  Q3-SW                                   
000746     MOVE +1                   TO INDX                                    
000747                                                                          
000748     IF MFS-NEXT AND MID-FLAGGA-SCROLL = JA                               
000749                                                                          
000750       PERFORM FA-BLAEDDRA-VIDARE                                         
000751                                                                          
000752     ELSE                                                                 
000753                                                                          
000754       PERFORM FB-VAELJA-LAESNING                                         
000755                                                                          
000756     END-IF                                                               
000757     MOVE 'END F-KOLLA-OM-INFO-FI'  TO PGM-POS                            
000758     .                                                                    
000759     EJECT                                                                
000760 FA-BLAEDDRA-VIDARE SECTION.                                              
000761     MOVE 'STA FA-BLAEDDRA-VIDARE' TO PGM-POS                             
000762                                                                          
000763     IF MID-FLAGGA-EOF NOT = JA                                           
000764                                                                          
000765       MOVE MID-TIAAMMDD-NEXT   TO TMP1-YYMMDD                            
000766       MOVE DAGENS-DATUM        TO TMP2-YYMMDD                            
000767       PERFORM WY2000P1                                                   
000768       IF ((TMP1-YYMMDD < TMP2-YYMMDD ) OR                                
000769          (MID-TIAAMMDD-NEXT = DAGENS-DATUM AND                           
000770          MID-TIHHMM-NEXT < DAGENS-HOUR))                                 
000771                                                                          
000772         MOVE DAGENS-DATUM-Y2K  TO W-SEQB-DATRPAVD-MIN                    
000773         MOVE DAGENS-TID (1:4)  TO W-SEQB-TIHHMM-MIN                      
000774       END-IF                                                             
000775                                                                          
000776       PERFORM S01-LAES-TRP-FOM-NU                                        
000777                                                                          
000778       IF NOT Q3-FOUND                                                    
000779         MOVE NEJ TO FRAMTIDA-AVGANG-SW                                   
000780       END-IF                                                             
000781                                                                          
000782       IF FRAMTIDA-AVGANGAR                                               
000783         MOVE ODEL-DATRPAVD   TO WS-DATRPAVD                              
000784         MOVE ODEL-TIHHMM     TO WS-TIHHMM                                
000785       ELSE                                                               
000786         MOVE DAGENS-DATUM-Y2K TO WS-DATRPAVD                             
000787         MOVE DAGENS-HOUR      TO WS-TIHHMM                               
000788       END-IF                                                             
000789                                                                          
000790       MOVE MID-TIAAMMDD-NEXT TO W-SEQB-DATRPAVD-MIN                      
000791                                 W-SEQB-DATRPAVD-MAX                      
000792       IF MID-TIAAMMDD-NEXT NOT = ZERO                                    
000793         IF MID-TIAAMMDD-NEXT < 500000                                    
000794           MOVE 20            TO W-SEQB-DATRPAVD-MIN (1:2)                
000795                                 W-SEQB-DATRPAVD-MAX (1:2)                
000796         ELSE                                                             
000797           IF MID-TIAAMMDD-NEXT < 999999                                  
000798             MOVE 19          TO W-SEQB-DATRPAVD-MIN (1:2)                
000799                                 W-SEQB-DATRPAVD-MAX (1:2)                
000800           ELSE                                                           
000801             MOVE 99999999    TO W-SEQB-DATRPAVD-MIN                      
000802                                 W-SEQB-DATRPAVD-MAX                      
000803           END-IF                                                         
000804         END-IF                                                           
000805       END-IF                                                             
000806       MOVE MID-TIHHMM-NEXT   TO W-SEQB-TIHHMM-MIN                        
000807                                 W-SEQB-TIHHMM-MAX                        
000808       PERFORM IMS-01-GU-ORQI01                                           
000809                                                                          
000810       PERFORM UNTIL OHUV-IDORDER = WS-IDORDER                            
000811                  OR SEGMENT-SAKNAS                                       
000812         PERFORM IMS-02-GN-ORQI01                                         
000813       END-PERFORM                                                        
000814                                                                          
000815       MOVE NEJ                TO Q3-SW                                   
000816       PERFORM UNTIL Q3-FOUND OR SEGMENT-SAKNAS OR BASEN-SLUT             
000817         MOVE OHUV-IDORDER     TO W-ODEL-IDORDER-MIN                      
000818                                  W-ODEL-IDORDER-MAX                      
000819         MOVE KEYFB-DATRPAVD   TO W-SAVE-DATRPAVD                         
000820         MOVE KEYFB-TIHHMM     TO W-SAVE-TIHHMM                           
000821         IF KDODELSTA-IFYLLT                                              
000822           PERFORM IMS-GU-ORQA01-STATUS                                   
000823         ELSE                                                             
000824           PERFORM IMS-GU-ORQA01                                          
000825         END-IF                                                           
000826         IF SEGMENT-FINNS                                                 
000827           MOVE JA             TO Q3-SW                                   
000828           PERFORM S09-SPAR-ENTER                                         
000829           PERFORM S11-SPAR-NEXT                                          
000830         ELSE                                                             
000831           PERFORM IMS-02-GN-ORQI01                                       
000832         END-IF                                                           
000833       END-PERFORM                                                        
000834                                                                          
000835       MOVE 99999999           TO W-SEQB-DATRPAVD-MAX                     
000836       MOVE +99999             TO W-SEQB-TIHHMM-MAX                       
000837                                                                          
000838     ELSE                                                                 
000839       MOVE JA TO MOD-FLAGGA-EOF                                          
000840       MOVE DAGENS-DATUM-Y2K   TO WS-DATRPAVD                             
000841       MOVE DAGENS-HOUR        TO WS-TIHHMM                               
000842       PERFORM IMS-01-GU-ORQI01                                           
000843     END-IF                                                               
000844     MOVE 'END FA-BLAEDDRA-VIDARE' TO PGM-POS                             
000845     .                                                                    
000846     EJECT                                                                
000847 FB-VAELJA-LAESNING SECTION.                                              
000848     MOVE 'STA FB-VAELJA-LAESNING' TO PGM-POS                             
000849                                                                          
000850     IF MID-FLAGGA-EOF = JA                                               
000851                                                                          
000852       PERFORM S05-LAES-TRP-UTAN-TID                                      
000853                                                                          
000854       IF NOT Q3-FOUND                                                    
000855         MOVE DAGENS-DATUM-Y2K TO W-SEQB-DATRPAVD-MIN                     
000856         MOVE DAGENS-TID (1:4) TO W-SEQB-TIHHMM-MIN                       
000857         MOVE 99999999         TO W-SEQB-DATRPAVD-MAX                     
000858         MOVE +99999           TO W-SEQB-TIHHMM-MAX                       
000859         PERFORM S01-LAES-TRP-FOM-NU                                      
000860       END-IF                                                             
000861                                                                          
000862       IF NOT Q3-FOUND                                                    
000863         PERFORM S03-LAES-FRAM-TILL-NU                                    
000864       END-IF                                                             
000865                                                                          
000866       IF FRAMTIDA-AVGANGAR AND Q3-FOUND                                  
000867         MOVE ODEL-DATRPAVD    TO WS-DATRPAVD                             
000868         MOVE ODEL-TIHHMM      TO WS-TIHHMM                               
000869         MOVE 0                TO W-SEQB-DATRPAVD-MIN                     
000870         MOVE +0               TO W-SEQB-TIHHMM-MIN                       
000871         MOVE NEJ              TO Q3-SW                                   
000872         PERFORM IMS-03-GU-ORQI01                                         
000873         IF SEGMENT-FINNS                                                 
000874                                                                          
000875           PERFORM UNTIL Q3-FOUND OR SEGMENT-SAKNAS OR BASEN-SLUT         
000876             IF KEYFB-DATRPAVD > 0 OR                                     
000877                KEYFB-TIHHMM   > +0                                       
000878               MOVE OHUV-IDORDER TO W-ODEL-IDORDER-MIN                    
000879                                    W-ODEL-IDORDER-MAX                    
000880               MOVE KEYFB-DATRPAVD TO W-SAVE-DATRPAVD                     
000881               MOVE KEYFB-TIHHMM TO W-SAVE-TIHHMM                         
000882               IF KDODELSTA-IFYLLT                                        
000883                 PERFORM IMS-GU-ORQA01-STATUS                             
000884               ELSE                                                       
000885                 PERFORM IMS-GU-ORQA01                                    
000886               END-IF                                                     
000887               IF SEGMENT-FINNS                                           
000888                 MOVE JA       TO Q3-SW                                   
000889                 PERFORM S09-SPAR-ENTER                                   
000890                 PERFORM S11-SPAR-NEXT                                    
000891               ELSE                                                       
000892                 PERFORM IMS-04-GN-ORQI01                                 
000893               END-IF                                                     
000894             ELSE                                                         
000895               PERFORM IMS-04-GN-ORQI01                                   
000896             END-IF                                                       
000897           END-PERFORM                                                    
000898                                                                          
000899         END-IF                                                           
000900       END-IF                                                             
000901     ELSE                                                                 
000902                                                                          
000903       IF NOT ANNAN-AVGANG                                                
000904         MOVE DAGENS-DATUM-Y2K TO W-SEQB-DATRPAVD-MIN                     
000905         MOVE DAGENS-TID (1:4) TO W-SEQB-TIHHMM-MIN                       
000906                                                                          
000907         PERFORM S01-LAES-TRP-FOM-NU                                      
000908                                                                          
000909         IF NOT Q3-FOUND                                                  
000910           PERFORM S03-LAES-FRAM-TILL-NU                                  
000911         END-IF                                                           
000912                                                                          
000913         IF NOT Q3-FOUND                                                  
000914           PERFORM S05-LAES-TRP-UTAN-TID                                  
000915         END-IF                                                           
000916                                                                          
000917         IF FRAMTIDA-AVGANGAR AND Q3-FOUND                                
000918           MOVE ODEL-DATRPAVD  TO WS-DATRPAVD                             
000919           MOVE ODEL-TIHHMM    TO WS-TIHHMM                               
000920           MOVE 0              TO W-SEQB-DATRPAVD-MIN                     
000921           MOVE +0             TO W-SEQB-TIHHMM-MIN                       
000922           MOVE NEJ            TO Q3-SW                                   
000923           PERFORM IMS-03-GU-ORQI01                                       
000924           IF SEGMENT-FINNS                                               
000925                                                                          
000926             PERFORM UNTIL Q3-FOUND OR SEGMENT-SAKNAS                     
000927                                    OR BASEN-SLUT                         
000928               IF KEYFB-DATRPAVD > 0 OR                                   
000929                  KEYFB-TIHHMM > +0                                       
000930                 MOVE OHUV-IDORDER TO W-ODEL-IDORDER-MIN                  
000931                                      W-ODEL-IDORDER-MAX                  
000932                 MOVE KEYFB-DATRPAVD TO W-SAVE-DATRPAVD                   
000933                 MOVE KEYFB-TIHHMM TO W-SAVE-TIHHMM                       
000934                 IF KDODELSTA-IFYLLT                                      
000935                   PERFORM IMS-GU-ORQA01-STATUS                           
000936                 ELSE                                                     
000937                   PERFORM IMS-GU-ORQA01                                  
000938                 END-IF                                                   
000939                 IF SEGMENT-FINNS                                         
000940                   MOVE JA TO Q3-SW                                       
000941                   PERFORM S09-SPAR-ENTER                                 
000942                   PERFORM S11-SPAR-NEXT                                  
000943                 ELSE                                                     
000944                   PERFORM IMS-04-GN-ORQI01                               
000945                 END-IF                                                   
000946               ELSE                                                       
000947                 PERFORM IMS-04-GN-ORQI01                                 
000948               END-IF                                                     
000949             END-PERFORM                                                  
000950                                                                          
000951           END-IF                                                         
000952         END-IF                                                           
000953       ELSE                                                               
000954         MOVE NEJ TO FRAMTIDA-AVGANG-SW                                   
000955         MOVE MID-TIAAMMDD-NEXT TO W-SEQB-DATRPAVD-MIN                    
000956                                   W-SEQB-DATRPAVD-MAX                    
000957         IF MID-TIAAMMDD-NEXT NOT = ZERO                                  
000958           IF MID-TIAAMMDD-NEXT < 500000                                  
000959             MOVE 20          TO W-SEQB-DATRPAVD-MIN (1:2)                
000960                                 W-SEQB-DATRPAVD-MAX (1:2)                
000961           ELSE                                                           
000962             IF MID-TIAAMMDD-NEXT < 999999                                
000963               MOVE 19        TO W-SEQB-DATRPAVD-MIN (1:2)                
000964                                 W-SEQB-DATRPAVD-MAX (1:2)                
000965             ELSE                                                         
000966               MOVE 99999999  TO W-SEQB-DATRPAVD-MIN                      
000967                                 W-SEQB-DATRPAVD-MAX                      
000968             END-IF                                                       
000969           END-IF                                                         
000970         END-IF                                                           
000971         MOVE MID-TIHHMM-NEXT TO W-SEQB-TIHHMM-MIN                        
000972                                 W-SEQB-TIHHMM-MAX                        
000973         PERFORM IMS-01-GU-ORQI01                                         
000974                                                                          
000975         PERFORM UNTIL OHUV-IDORDER = WS-IDORDER                          
000976                  OR SEGMENT-SAKNAS                                       
000977           PERFORM IMS-02-GN-ORQI01                                       
000978         END-PERFORM                                                      
000979                                                                          
000980         MOVE NEJ              TO Q3-SW                                   
000981         PERFORM UNTIL Q3-FOUND OR SEGMENT-SAKNAS OR BASEN-SLUT           
000982           MOVE OHUV-IDORDER   TO W-ODEL-IDORDER-MIN                      
000983                                  W-ODEL-IDORDER-MAX                      
000984           MOVE KEYFB-DATRPAVD TO W-SAVE-DATRPAVD                         
000985           MOVE KEYFB-TIHHMM   TO W-SAVE-TIHHMM                           
000986           IF KDODELSTA-IFYLLT                                            
000987             PERFORM IMS-GU-ORQA01-STATUS                                 
000988           ELSE                                                           
000989             PERFORM IMS-GU-ORQA01                                        
000990           END-IF                                                         
000991           IF SEGMENT-FINNS                                               
000992             MOVE JA           TO Q3-SW                                   
000993             PERFORM S09-SPAR-ENTER                                       
000994             PERFORM S11-SPAR-NEXT                                        
000995           ELSE                                                           
000996             PERFORM IMS-02-GN-ORQI01                                     
000997           END-IF                                                         
000998         END-PERFORM                                                      
000999                                                                          
001000         MOVE 99999999         TO W-SEQB-DATRPAVD-MAX                     
001001         MOVE +99999           TO W-SEQB-TIHHMM-MAX                       
001002         IF Q3-FOUND                                                      
001003           MOVE ODEL-DATRPAVD  TO WS-DATRPAVD                             
001004           MOVE ODEL-TIHHMM    TO WS-TIHHMM                               
001005         END-IF                                                           
001006       END-IF                                                             
001007                                                                          
001008     END-IF                                                               
001009     MOVE 'END FB-VAELJA-LAESNING' TO PGM-POS                             
001010     .                                                                    
001011     EJECT                                                                
001012                                                                          
001013 G-LAES-VISA-INFO SECTION.                                                
001014     MOVE 'STA G-LAES-VISA-INFO  ' TO PGM-POS                             
001015                                                                          
001016     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT OR                        
001017                   INDX > MAX-INDX                                        
001018         OR                                                               
001019                                                                          
001020         ((KEYFB-DATRPAVT NOT = SPAR-ODEL-DATRPAVT) AND                   
001021           ((KEYFB-DATRPAVT > WS-DATRPAVT AND NOT TOM-BILD) AND           
001022            (KEYFB-DATRPAVT > SPAR-DATRPAVT)))                            
001023                                                                          
001024         OR                                                               
001025                                                                          
001026          (WS-DATRPAVD = +0 AND WS-TIHHMM = +0 AND NOT TOM-BILD           
001027           AND (KEYFB-DATRPAVD > 0 OR KEYFB-TIHHMM > +0))                 
001028                                                                          
001029       PERFORM GA-REDIGERA-BILD                                           
001030                                                                          
001031     END-PERFORM                                                          
001032                                                                          
001033     IF SEGMENT-FINNS                                                     
001034                                                                          
001035       PERFORM GB-KOLLA-BLAEDDRINSLAEGGE                                  
001036                                                                          
001037     END-IF                                                               
001038                                                                          
001039     PERFORM GC-REDIGERA-TOTAL                                            
001040                                                                          
001041     PERFORM GD-LAEGG-UT-INFOTEXT                                         
001042     MOVE 'END G-LAES-VISA-INFO  ' TO PGM-POS                             
001043     .                                                                    
001044     EJECT                                                                
001045                                                                          
001046 GA-REDIGERA-BILD SECTION.                                                
001047     MOVE 'STA GA-REDIGERA-BILD  ' TO PGM-POS                             
001048                                                                          
001049     PERFORM S16-GET-KDORDSTA                                             
001050                                                                          
001073     IF GODK-KDORDSTA                                                     
001074       IF KDODELSTA-IFYLLT                                                
001075         PERFORM S07-EVALUERA-KDODELSTA                                   
001076       END-IF                                                             
001077       IF STATUS-OK                                                       
001078         MOVE OHUV-IDORDER     TO W-ODEL-IDORDER-MIN                      
001079                                  W-ODEL-IDORDER-MAX                      
001080         IF KDODELSTA-IFYLLT                                              
001081           PERFORM IMS-GU-ORQA01-STATUS                                   
001082         ELSE                                                             
001083           PERFORM IMS-GU-ORQA01                                          
001084         END-IF                                                           
001085         IF SEGMENT-FINNS                                                 
001086           PERFORM GAA-LAES-ORDERDELAR                                    
001087         ELSE                                                             
001088           PERFORM IMS-02-GN-ORQI01                                       
001089         END-IF                                                           
001090       ELSE                                                               
001091         PERFORM IMS-02-GN-ORQI01                                         
001092       END-IF                                                             
001093     ELSE                                                                 
001094       PERFORM IMS-02-GN-ORQI01                                           
001095     END-IF                                                               
001096     MOVE 'END GA-REDIGERA-BILD  ' TO PGM-POS                             
001097     .                                                                    
001098     EJECT                                                                
001099 GAA-LAES-ORDERDELAR SECTION.                                             
001100     MOVE 'STA GAA-LAES-ORDERDEL ' TO PGM-POS                             
001101                                                                          
001102     MOVE +0                   TO WS-VLORDNTO-SUM                         
001103                                  WS-VKORDNTO-SUM                         
001104                                  WS-SUORDV-SUM                           
001105                                  WS-SUORDV-LOC-SUM                       
001106                                  WS-SUORDV-LOCPREL-SUM                   
001107                                                                          
001108     MOVE  ODEL-IDTRP          TO SPAR-ODEL-IDTRP                         
001109     MOVE  ODEL-IDDISTR        TO SPAR-ODEL-IDDISTR                       
001110     MOVE  ODEL-IDKUNDNR       TO SPAR-ODEL-IDKUNDNR                      
001111     MOVE  ODEL-IDORDNR7       TO SPAR-ODEL-IDKUNDRF                      
001112     MOVE  ODEL-DATRPAVD       TO SPAR-ODEL-DATRPAVD                      
001113     MOVE  ODEL-TIHHMM         TO SPAR-ODEL-TIHHMM                        
001114     MOVE  +0                  TO SPAR-ODEL-IDPRODNR                      
001115                                                                          
001116     PERFORM S13-HAEMTA-VVDTTMM                                           
001117     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
001118       PERFORM GAAA-READ-WDQ3                                             
001119     END-PERFORM                                                          
001120                                                                          
001121                                                                          
001122     COMPUTE WS-VLORDNTO-TOT = WS-VLORDNTO-TOT +                          
001123                               WS-VLORDNTO-SUM                            
001124                                                                          
001125     COMPUTE WS-VKORDNTO-TOT = WS-VKORDNTO-TOT +                          
001126                               WS-VKORDNTO-SUM                            
001127                                                                          
001128     IF DIST79-DEALER-PRICE                                               
001129       COMPUTE WS-SUORDV-LOC-TOT = WS-SUORDV-LOC-TOT +                    
001130                                   WS-SUORDV-LOC-SUM                      
001131                                                                          
001132       COMPUTE WS-SUORDV-LOCPREL-TOT = WS-SUORDV-LOCPREL-TOT +            
001133                                       WS-SUORDV-LOCPREL-SUM              
001134     ELSE                                                                 
001135       COMPUTE WS-SUORDV-TOT = WS-SUORDV-TOT +                            
001136                               WS-SUORDV-SUM                              
001137     END-IF                                                               
001138                                                                          
001139     PERFORM IMS-02-GN-ORQI01                                             
001140     MOVE 'END GAA-LAES-ORDERDEL ' TO PGM-POS                             
001141     .                                                                    
001142     EJECT                                                                
001143 GAAA-READ-WDQ3 SECTION.                                                  
001144     MOVE 'STA GAAA-READ-WDQ3    ' TO PGM-POS                             
001145                                                                          
001146     IF ODEL-KDODELSTA = 'P'                                              
001147       IF ODEL-KVRADER > +0                                               
001148         PERFORM GAAAA-LAES-VIDARE                                        
001149       ELSE                                                               
001150         IF KDODELSTA-IFYLLT                                              
001151           PERFORM IMS-GN-ORQA01-STATUS                                   
001152         ELSE                                                             
001153           PERFORM IMS-GN-ORQA01                                          
001154         END-IF                                                           
001155       END-IF                                                             
001156     ELSE                                                                 
001157       PERFORM GAAAA-LAES-VIDARE                                          
001158     END-IF                                                               
001159     MOVE 'END GAAA-READ-WDQ3    ' TO PGM-POS                             
001160     .                                                                    
001161     EJECT                                                                
001162 GAAAA-LAES-VIDARE SECTION.                                               
001163     MOVE 'STA  GAAAA-LAES-VIDARE' TO PGM-POS                             
001164                                                                          
001165     MOVE SPACE                TO STATUS-WS                               
001166     IF ODEL-KDODELSTA = 'P'                                              
001167       IF ODEL-IDPRODNR NOT = SPAR-ODEL-IDPRODNR                          
001168         MOVE ODEL-IDPRODNR    TO W-VORD-IDPRODNR                         
001169                                  SPAR-ODEL-IDPRODNR                      
001170         PERFORM IMS-GU-WDE601                                            
001171         IF SEGMENT-FINNS                                                 
001172           COMPUTE WS-VLORDNTO-SUM = WS-VLORDNTO-SUM +                    
001173                                     VORD-VLORDNTO                        
001174                                                                          
001175           COMPUTE WS-VKORDNTO-SUM = WS-VKORDNTO-SUM +                    
001176                                     VORD-VKORDNTO                        
001177           IF DIST79-DEALER-PRICE                                         
001178             COMPUTE WS-SUORDV-LOC-SUM = WS-SUORDV-LOC-SUM +              
001179                                         VORD-SUORDV-LOC                  
001180                                                                          
001181             COMPUTE WS-SUORDV-LOCPREL-SUM = WS-SUORDV-LOCPREL-SUM        
001182                                           + VORD-SUORDV-LOCPREL          
001183           ELSE                                                           
001184*NEW CODE FOR VORD-SUORDV-EXP                                             
001185*            IF VORD-IDDC = DCS-IDDC AND                                  
001186             IF VORD-SUORDV-EXP > 0                                       
001187*               *SUORDV-EXP BASED ON PRAVCOST.                            
001188               COMPUTE WS-SUORDV-SUM = WS-SUORDV-SUM +                    
001189                                       VORD-SUORDV-EXP                    
001190             ELSE                                                         
001191               COMPUTE WS-SUORDV-SUM = WS-SUORDV-SUM +                    
001192                                       VORD-SUORDV                        
001193             END-IF                                                       
001194           END-IF                                                         
001195                                                                          
001196           IF WS-KDORDSTA NOT = 'P '                                      
001197             PERFORM IMS-GNP-WDE611                                       
001198             IF SEGMENT-FINNS                                             
001199               PERFORM UNTIL SEGMENT-SAKNAS                               
001200                 IF KOLLI-KDKOLSTA > +1                                   
001201                   COMPUTE WS-VLORDNTO-SUM = WS-VLORDNTO-SUM -            
001202                                             VORD-VLORDNTO                
001203                                                                          
001204                   COMPUTE WS-VKORDNTO-SUM = WS-VKORDNTO-SUM -            
001205                                             VORD-VKORDNTO                
001206                   IF DIST79-DEALER-PRICE                                 
001207                     COMPUTE WS-SUORDV-LOC-SUM =                          
001208                     WS-SUORDV-LOC-SUM - VORD-SUORDV-LOC                  
001209                                                                          
001210                     COMPUTE WS-SUORDV-LOCPREL-SUM =                      
001211                     WS-SUORDV-LOCPREL-SUM - VORD-SUORDV-LOCPREL          
001212                   ELSE                                                   
001213                                                                          
001214*NEW         CODE FOR VORD-SUORDV-EXP                                     
001215*                    IF VORD-IDDC = DCS-IDDC AND                          
001216                     IF VORD-SUORDV-EXP > 0                               
001217*                     *SUORDV-EXP BASED ON PRAVCOST.                      
001218                       COMPUTE WS-SUORDV-SUM =                            
001219                       WS-SUORDV-SUM - VORD-SUORDV-EXP                    
001220                     ELSE                                                 
001221                       COMPUTE WS-SUORDV-SUM =                            
001222                       WS-SUORDV-SUM - VORD-SUORDV                        
001223                     END-IF                                               
001224                   END-IF                                                 
001225                                                                          
001226                 END-IF                                                   
001227                 PERFORM IMS-GNP-WDE611                                   
001228               END-PERFORM                                                
001229               PERFORM GAAAAA-FOREBEREDD-BILD-RAD                         
001230             ELSE                                                         
001231               PERFORM GAAAAA-FOREBEREDD-BILD-RAD                         
001232             END-IF                                                       
001233           ELSE                                                           
001234             PERFORM GAAAAA-FOREBEREDD-BILD-RAD                           
001235           END-IF                                                         
001236         ELSE                                                             
001237           PERFORM GAAAAA-FOREBEREDD-BILD-RAD                             
001238         END-IF                                                           
001239       ELSE                                                               
001240         COMPUTE WS-VLORDNTO-SUM = WS-VLORDNTO-SUM +                      
001241                                   ODEL-VLORDNTO                          
001242                                                                          
001243         COMPUTE WS-VKORDNTO-SUM = WS-VKORDNTO-SUM +                      
001244                                   ODEL-VKORDNTO                          
001245                                                                          
001246         IF DIST79-DEALER-PRICE                                           
001247           COMPUTE WS-SUORDV-SUM = WS-SUORDV-LOC-SUM +                    
001248                                 ODEL-SUORDV-LOC                          
001249                                                                          
001250           COMPUTE WS-SUORDV-SUM = WS-SUORDV-LOCPREL-SUM +                
001251                                 ODEL-SUORDV-LOCPREL                      
001252         ELSE                                                             
001253                                                                          
001254           COMPUTE WS-SUORDV-SUM = WS-SUORDV-SUM +                        
001255                                 ODEL-SUORDV                              
001256         END-IF                                                           
001257                                                                          
001258         PERFORM GAAAAA-FOREBEREDD-BILD-RAD                               
001259       END-IF                                                             
001260     ELSE                                                                 
001261       COMPUTE WS-VLORDNTO-SUM = WS-VLORDNTO-SUM +                        
001262                                 ODEL-VLORDNTO                            
001263                                                                          
001264       COMPUTE WS-VKORDNTO-SUM = WS-VKORDNTO-SUM +                        
001265                                 ODEL-VKORDNTO                            
001266                                                                          
001267       IF DIST79-DEALER-PRICE                                             
001268         COMPUTE WS-SUORDV-SUM = WS-SUORDV-LOC-SUM +                      
001269                               ODEL-SUORDV-LOC                            
001270                                                                          
001271         COMPUTE WS-SUORDV-SUM = WS-SUORDV-LOCPREL-SUM +                  
001272                               ODEL-SUORDV-LOCPREL                        
001273       ELSE                                                               
001274                                                                          
001275         COMPUTE WS-SUORDV-SUM = WS-SUORDV-SUM +                          
001276                               ODEL-SUORDV                                
001277       END-IF                                                             
001278                                                                          
001279       PERFORM GAAAAA-FOREBEREDD-BILD-RAD                                 
001280     END-IF                                                               
001281     MOVE 'END  GAAAA-LAES-VIDARE' TO PGM-POS                             
001282     .                                                                    
001283     EJECT                                                                
001284 GAAAAA-FOREBEREDD-BILD-RAD  SECTION.                                     
001285     MOVE 'STA GAAAAA-FOREBEREDD-' TO PGM-POS                             
001286                                                                          
001287     IF ODEL-KDODELSTA = 'R'                                              
001288       MOVE 'R'                TO WS-R                                    
001289     END-IF                                                               
001290     IF ODEL-KDODELSTA = 'U'                                              
001291       MOVE 'U'                TO WS-U                                    
001292     END-IF                                                               
001293     IF ODEL-KDODELSTA = 'P'                                              
001294       MOVE 'P'                TO WS-P                                    
001295     END-IF                                                               
001296                                                                          
001297     IF KDODELSTA-IFYLLT                                                  
001298       PERFORM IMS-GN-ORQA01-STATUS                                       
001299     ELSE                                                                 
001300       PERFORM IMS-GN-ORQA01                                              
001301     END-IF                                                               
001302     IF SEGMENT-SAKNAS OR BASEN-SLUT                                      
001303       IF WS-VLORDNTO-SUM > +0        OR                                  
001304          WS-VKORDNTO-SUM > +0        OR                                  
001305          WS-SUORDV-LOC-SUM > +0      OR                                  
001306          WS-SUORDV-LOCPREL-SUM > +0  OR                                  
001307          WS-SUORDV-SUM > +0                                              
001308           IF INDX NOT > MAX-INDX                                         
001309             MOVE WS-SPARSTATUS TO WS-HELPSTATUS                          
001310             PERFORM GAAAAAA-EVALUATE-STATUS                              
001311             PERFORM GAAAAAB-LAEGG-UT-BILD                                
001312             MOVE SPACE TO WS-SPARSTATUS                                  
001313           END-IF                                                         
001314           PERFORM GAAAAAC-KOLLA-PLACERING                                
001315           ADD +1 TO INDX                                                 
001316       ELSE                                                               
001317         MOVE SPACE TO WS-SPARSTATUS                                      
001318       END-IF                                                             
001319     END-IF                                                               
001320     MOVE 'END GAAAAA-FOREBEREDD-' TO PGM-POS                             
001321     .                                                                    
001322     EJECT                                                                
001323 GAAAAAA-EVALUATE-STATUS SECTION.                                         
001324     MOVE 'STA GAAAAAA-EVALUATE-ST'  TO PGM-POS                           
001325                                                                          
001326     EVALUATE WS-HELPSTATUS                                               
001327       WHEN 'R  ' MOVE 'R '    TO WS-STATUS-MOD                           
001328                                                                          
001329       WHEN ' U ' MOVE 'U '    TO WS-STATUS-MOD                           
001330                                                                          
001331       WHEN '  P' MOVE 'P '    TO WS-STATUS-MOD                           
001332                                                                          
001333       WHEN 'RU ' MOVE 'R*'    TO WS-STATUS-MOD                           
001334                                                                          
001335       WHEN 'R P' MOVE 'R*'    TO WS-STATUS-MOD                           
001336                                                                          
001337       WHEN 'RUP' MOVE 'R*'    TO WS-STATUS-MOD                           
001338                                                                          
001339       WHEN ' UP' MOVE 'U*'    TO WS-STATUS-MOD                           
001340                                                                          
001341     END-EVALUATE                                                         
001342     MOVE 'END GAAAAAA-EVALUATE-ST'  TO PGM-POS                           
001343     .                                                                    
001344     EJECT                                                                
001345 GAAAAAB-LAEGG-UT-BILD SECTION.                                           
001346     MOVE 'STA GAAAAAB-LAEGG-UT-BILD'   TO PGM-POS                        
001347                                                                          
001348     MOVE NEJ TO TOM-BILD-SW                                              
001349     MOVE WS-Q3-DATUM  (1:2)  TO TMP1-YY                                  
001350     MOVE DAGENS-DATUM (1:2)  TO TMP2-YY                                  
001351     PERFORM WY2000P9                                                     
001352     IF FULL-TITRPAVG  > ZERO   AND                                       
001353       ((FULL-TITRPAVG < HELP-NUTID AND                                   
001354          TMP1-YY      <= TMP2-YY)                                        
001355        OR                                                                
001356       (TMP1-YY        < TMP2-YY))                                        
001357       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDTRP-ATTR(INDX)                 
001358                                     MOD-IDDISTR-ATTR(INDX)               
001359                                     MOD-IDKUNDNR-ATTR(INDX)              
001360                                     MOD-IDKUNDRF-ATTR(INDX)              
001361                                     MOD-STATUS-ATTR(INDX)                
001362                                     MOD-VLORDNTO-ATTR(INDX)              
001363                                     MOD-VKORDNTO-ATTR(INDX)              
001364                                     MOD-SUORDV-ATTR(INDX)                
001365     ELSE                                                                 
001366       MOVE MFS-FORMATETS-ATTR TO MOD-IDTRP-ATTR(INDX)                    
001367                                  MOD-IDDISTR-ATTR(INDX)                  
001368                                  MOD-IDKUNDNR-ATTR(INDX)                 
001369                                  MOD-IDKUNDRF-ATTR(INDX)                 
001370                                  MOD-STATUS-ATTR(INDX)                   
001371                                  MOD-VLORDNTO-ATTR(INDX)                 
001372                                  MOD-VKORDNTO-ATTR(INDX)                 
001373                                  MOD-SUORDV-ATTR(INDX)                   
001374     END-IF                                                               
001375                                                                          
001376     MOVE  SPAR-ODEL-IDTRP     TO MOD-IDTRP(INDX)                         
001377     MOVE  SPAR-ODEL-IDDISTR   TO MOD-IDDISTR(INDX)                       
001378     MOVE  SPAR-ODEL-IDKUNDNR  TO MOD-IDKUNDNR(INDX)                      
001379     MOVE  SPAR-ODEL-IDKUNDRF  TO MOD-IDKUNDRF(INDX)                      
001380     MOVE  WS-STATUS-MOD       TO MOD-STATUS(INDX)                        
001381     MOVE  WS-VLORDNTO-SUM     TO MOD-VLORDNTO(INDX)                      
001382     MOVE  WS-VKORDNTO-SUM     TO MOD-VKORDNTO(INDX)                      
001383     IF DIST79-DEALER-PRICE                                               
001384        IF WS-SUORDV-LOC-SUM > +0                                         
001385           MOVE WS-SUORDV-LOC-SUM     TO MOD-SUORDV(INDX)                 
001386           MOVE SPACE                 TO MOD-TEASTRIX-RAD(INDX)           
001387        ELSE                                                              
001388           MOVE WS-SUORDV-LOCPREL-SUM TO MOD-SUORDV(INDX)                 
001389           MOVE '*'                   TO MOD-TEASTRIX-RAD(INDX)           
001390        END-IF                                                            
001391     ELSE                                                                 
001392        MOVE  WS-SUORDV-SUM       TO MOD-SUORDV(INDX)                     
001393        MOVE SPACE                TO MOD-TEASTRIX-RAD(INDX)               
001394     END-IF                                                               
001395     MOVE 'END GAAAAAB-LAEGG-UT-BILD'   TO PGM-POS                        
001396     .                                                                    
001397     EJECT                                                                
001398 GAAAAAC-KOLLA-PLACERING SECTION.                                         
001399     MOVE 'STA GAAAAAC-KOLLA-PLACERIN'  TO PGM-POS                        
001400                                                                          
001401     MOVE WS-Q3-DATUM (1:2)    TO TMP1-YY                                 
001402     MOVE DAGENS-DATUM(1:2)    TO TMP2-YY                                 
001403     PERFORM WY2000P9                                                     
001404     IF INDX = MAX-INDX-PLUS1 AND                                         
001405       ((ODEL-DATRPAVD = WS-DATRPAVD AND                                  
001406       ODEL-TIHHMM = WS-TIHHMM) OR                                        
001407       (FULL-TITRPAVG < HELP-NUTID) OR                                    
001408       (TMP1-YY       < TMP2-YY))                                         
001409       PERFORM S11-SPAR-NEXT                                              
001410       MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                          
001411       CALL WMEDKONV USING MED-WMEDAREA                                   
001412       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
001413       MOVE JA                 TO SCROLL-SW                               
001414                                  MOD-FLAGGA-SCROLL                       
001415     END-IF                                                               
001416     MOVE 'END GAAAAAC-KOLLA-PLACERIN'  TO PGM-POS                        
001417     .                                                                    
001418     EJECT                                                                
001419 GB-KOLLA-BLAEDDRINSLAEGGE SECTION.                                       
001420     MOVE 'STA GB-KOLLA-BLAEDDRINSLAE'  TO PGM-POS                        
001421                                                                          
001422     MOVE NEJ                  TO Q3-SW                                   
001423     PERFORM UNTIL Q3-FOUND OR SEGMENT-SAKNAS OR BASEN-SLUT               
001424       MOVE OHUV-IDORDER       TO W-ODEL-IDORDER-MIN                      
001425                                  W-ODEL-IDORDER-MAX                      
001426       MOVE KEYFB-DATRPAVD     TO W-SAVE-DATRPAVD                         
001427       MOVE KEYFB-TIHHMM       TO W-SAVE-TIHHMM                           
001428       IF KDODELSTA-IFYLLT                                                
001429         PERFORM IMS-GU-ORQA01-STATUS                                     
001430       ELSE                                                               
001431         PERFORM IMS-GU-ORQA01                                            
001432       END-IF                                                             
001433       IF SEGMENT-FINNS                                                   
001434         IF KDODELSTA-IFYLLT AND WS-KDODELSTA = 'P'                       
001435           PERFORM UNTIL SEGMENT-SAKNAS OR Q3-FOUND                       
001436             IF ODEL-KVRADER > +0                                         
001437               MOVE JA         TO Q3-SW                                   
001438             ELSE                                                         
001439               PERFORM IMS-GN-ORQA01-STATUS                               
001440             END-IF                                                       
001441           END-PERFORM                                                    
001442         ELSE                                                             
001443           MOVE JA             TO Q3-SW                                   
001444         END-IF                                                           
001445       ELSE                                                               
001446         PERFORM IMS-02-GN-ORQI01                                         
001447       END-IF                                                             
001448     END-PERFORM                                                          
001449                                                                          
001450     IF Q3-FOUND                                                          
001451       MOVE WS-Q3-DATUM (1:2)  TO TMP1-YY                                 
001452       MOVE DAGENS-DATUM(1:2)  TO TMP2-YY                                 
001453       PERFORM WY2000P9                                                   
001454       IF INDX = MAX-INDX-PLUS1 AND                                       
001455         ((ODEL-DATRPAVD = WS-DATRPAVD AND                                
001456         ODEL-TIHHMM = WS-TIHHMM) OR                                      
001457         (FULL-TITRPAVG < HELP-NUTID) OR                                  
001458         (TMP1-YY       < TMP2-YY))                                       
001459         PERFORM S11-SPAR-NEXT                                            
001460         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
001461         CALL WMEDKONV USING MED-WMEDAREA                                 
001462         MOVE MED-MFSINF       TO MOD-TEMFSINF                            
001463         MOVE JA               TO SCROLL-SW                               
001464                                  MOD-FLAGGA-SCROLL                       
001465       END-IF                                                             
001466     END-IF                                                               
001467     MOVE 'END GB-KOLLA-BLAEDDRINSLAE'  TO PGM-POS                        
001468     .                                                                    
001469     EJECT                                                                
001470 GC-REDIGERA-TOTAL SECTION.                                               
001471     MOVE 'STA GC-REDIGERA-TOTAL     '  TO PGM-POS                        
001472                                                                          
001473     IF WS-VLORDNTO-TOT > +0 OR                                           
001474        WS-VKORDNTO-TOT > +0 OR                                           
001475        WS-SUORDV-LOC-TOT > +0 OR                                         
001476        WS-SUORDV-LOCPREL-TOT > +0 OR                                     
001477        WS-SUORDV-TOT > +0                                                
001478       MOVE WS-VLORDNTO-TOT    TO MOD-VLORDNTO-TOTAL                      
001479                                  MOD-VLORDNTO-SPAR                       
001480                                                                          
001481       MOVE WS-VKORDNTO-TOT    TO MOD-VKORDNTO-TOTAL                      
001482                                  MOD-VKORDNTO-SPAR                       
001483                                                                          
001484       IF DIST79-DEALER-PRICE                                             
001485         IF WS-SUORDV-LOC-TOT > +0                                        
001486           MOVE WS-SUORDV-LOC-TOT   TO MOD-SUORDV-TOTAL                   
001487                                       MOD-SUORDV-SPAR                    
001488           MOVE ' '                 TO MOD-TEASTRIX-ORD                   
001489         ELSE                                                             
001490           IF WS-SUORDV-LOCPREL-TOT > +0                                  
001491             MOVE WS-SUORDV-LOCPREL-TOT TO MOD-SUORDV-TOTAL               
001492                                           MOD-SUORDV-SPAR                
001493             MOVE '*'                   TO MOD-TEASTRIX-ORD               
001494           END-IF                                                         
001495         END-IF                                                           
001496       ELSE                                                               
001497         MOVE WS-SUORDV-TOT          TO MOD-SUORDV-TOTAL                  
001498                                        MOD-SUORDV-SPAR                   
001499         MOVE ' '                    TO MOD-TEASTRIX-ORD                  
001500       END-IF                                                             
001501     END-IF                                                               
001502     MOVE 'END GC-REDIGERA-TOTAL     '  TO PGM-POS                        
001503     .                                                                    
001504     EJECT                                                                
001505 GD-LAEGG-UT-INFOTEXT SECTION.                                            
001506     MOVE 'STA GD-LAEGG-UT-INFOTEXT  '  TO PGM-POS                        
001507                                                                          
001508     IF NOT SCROLL-OK                                                     
001509       IF SEGMENT-FINNS                                                   
001510         MOVE NEJ TO Q3-SW                                                
001511                                                                          
001512         PERFORM UNTIL Q3-FOUND OR SEGMENT-SAKNAS OR                      
001513           BASEN-SLUT                                                     
001514                                                                          
001515           PERFORM S16-GET-KDORDSTA                                       
001524                                                                          
001540           IF GODK-KDORDSTA                                               
001541             IF KDODELSTA-IFYLLT                                          
001542               PERFORM S07-EVALUERA-KDODELSTA                             
001543             END-IF                                                       
001544             IF STATUS-OK                                                 
001545               MOVE OHUV-IDORDER TO W-ODEL-IDORDER-MIN                    
001546                                    W-ODEL-IDORDER-MAX                    
001547               IF KDODELSTA-IFYLLT                                        
001548                 PERFORM IMS-GU-ORQA01-STATUS                             
001549               ELSE                                                       
001550                 PERFORM IMS-GU-ORQA01                                    
001551               END-IF                                                     
001552               IF SEGMENT-FINNS                                           
001553                  MOVE JA      TO Q3-SW                                   
001554               ELSE                                                       
001555                 PERFORM IMS-02-GN-ORQI01                                 
001556               END-IF                                                     
001557             ELSE                                                         
001558               PERFORM IMS-02-GN-ORQI01                                   
001559             END-IF                                                       
001560           ELSE                                                           
001561             PERFORM IMS-02-GN-ORQI01                                     
001562           END-IF                                                         
001563         END-PERFORM                                                      
001564         IF NOT Q3-FOUND                                                  
001565           MOVE JA             TO MOD-FLAGGA-EOF                          
001566           IF SPRAK-IX = +1                                               
001567              MOVE 'TRANSPORT UTAN BOKAD AVGÅNGSTID KAN FINNAS' TO        
001568              MOD-TEMFSINF                                                
001569           ELSE                                                           
001570              MOVE 'POSSIBILITY OF TRANSPORT WITHOUT DEP.TIME' TO         
001571              MOD-TEMFSINF                                                
001572           END-IF                                                         
001573         ELSE                                                             
001574           PERFORM S11-SPAR-NEXT                                          
001575           MOVE INF-NEW-DEPARTURE TO MED-IDMFSINF                         
001576           CALL WMEDKONV USING MED-WMEDAREA                               
001577           MOVE MED-MFSINF TO MOD-TEMFSINF                                
001578         END-IF                                                           
001579       ELSE                                                               
001580         IF MOD-FLAGGA-SCROLL NOT = JA AND MID-FLAGGA-EOF NOT = JA        
001581           MOVE JA TO MOD-FLAGGA-EOF                                      
001582             IF SPRAK-IX = +1                                             
001583              MOVE 'TRANSPORT UTAN BOKAD AVGÅNGSTID KAN FINNAS' TO        
001584              MOD-TEMFSINF                                                
001585             ELSE                                                         
001586              MOVE 'POSSIBILITY OF TRANSPORT WITHOUT DEP.TIME' TO         
001587              MOD-TEMFSINF                                                
001588             END-IF                                                       
001589         END-IF                                                           
001590       END-IF                                                             
001591     END-IF                                                               
001592     MOVE 'END GD-LAEGG-UT-INFOTEXT  '  TO PGM-POS                        
001593     .                                                                    
001594     EJECT                                                                
001595 S01-LAES-TRP-FOM-NU SECTION.                                             
001596     MOVE 'STA S01-LAES-TRP-FOM-NU   '  TO PGM-POS                        
001597                                                                          
001598     PERFORM IMS-01-GU-ORQI01                                             
001599     IF SEGMENT-FINNS                                                     
001600       PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT OR Q3-FOUND             
001601         PERFORM S01A-LAES-TILL-AKT-TRP                                   
001602       END-PERFORM                                                        
001603     END-IF                                                               
001604     MOVE 'END S01-LAES-TRP-FOM-NU   '  TO PGM-POS                        
001605     .                                                                    
001606     EJECT                                                                
001607 S01A-LAES-TILL-AKT-TRP SECTION.                                          
001608     MOVE 'STA S01A-LAES-TILL-AKT-   '  TO PGM-POS                        
001609                                                                          
001619                                                                          
001620     PERFORM S16-GET-KDORDSTA                                             
001630                                                                          
001635     MOVE KEYFB-DATRPAVD       TO W-SAVE-DATRPAVD                         
001636     MOVE KEYFB-TIHHMM         TO W-SAVE-TIHHMM                           
001637     IF GODK-KDORDSTA                                                     
001638       IF KDODELSTA-IFYLLT                                                
001639         PERFORM S07-EVALUERA-KDODELSTA                                   
001640       END-IF                                                             
001641       IF STATUS-OK                                                       
001642         PERFORM S02-LAES-ORDERDELAR                                      
001643         IF Q3-FOUND                                                      
001644           PERFORM S09-SPAR-ENTER                                         
001645           PERFORM S11-SPAR-NEXT                                          
001646           PERFORM S13-HAEMTA-VVDTTMM                                     
001647         ELSE                                                             
001648           PERFORM IMS-04-GN-ORQI01                                       
001649         END-IF                                                           
001650       ELSE                                                               
001651         PERFORM IMS-04-GN-ORQI01                                         
001652       END-IF                                                             
001653     ELSE                                                                 
001654       PERFORM IMS-04-GN-ORQI01                                           
001655     END-IF                                                               
001656     MOVE 'END S01A-LAES-TILL-AKT-   '  TO PGM-POS                        
001657     .                                                                    
001658     EJECT                                                                
001659 S02-LAES-ORDERDELAR SECTION.                                             
001660     MOVE 'STA S02-LAES-ORDERDELAR   '  TO PGM-POS                        
001661                                                                          
001662     MOVE OHUV-IDORDER         TO W-ODEL-IDORDER-MIN                      
001663                                  W-ODEL-IDORDER-MAX                      
001664     IF KDODELSTA-IFYLLT                                                  
001665       PERFORM IMS-GU-ORQA01-STATUS                                       
001666     ELSE                                                                 
001667       PERFORM IMS-GU-ORQA01                                              
001668     END-IF                                                               
001669     IF SEGMENT-FINNS                                                     
001670       MOVE JA                 TO Q3-SW                                   
001671     END-IF                                                               
001672     MOVE 'END S02-LAES-ORDERDELAR   '  TO PGM-POS                        
001673     .                                                                    
001674     EJECT                                                                
001675 S03-LAES-FRAM-TILL-NU SECTION.                                           
001676     MOVE 'STA S03-LAES-FRAM-TILL-NU '  TO PGM-POS                        
001677                                                                          
001678     MOVE NEJ                  TO MOD-FLAGGA-EOF                          
001679     MOVE 0                    TO W-SEQB-DATRPAVD-MIN                     
001680     MOVE +0                  TO  W-SEQB-TIHHMM-MIN                       
001681     PERFORM IMS-03-GU-ORQI01                                             
001682     IF SEGMENT-FINNS                                                     
001683       PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT OR Q3-FOUND             
001684         PERFORM S03A-LAES-TILL-AKT-TRP                                   
001685       END-PERFORM                                                        
001686     END-IF                                                               
001687     MOVE 'END S03-LAES-FRAM-TILL-NU '  TO PGM-POS                        
001688     .                                                                    
001689     EJECT                                                                
001690 S03A-LAES-TILL-AKT-TRP SECTION.                                          
001691     MOVE 'STA S03A-LAES-TILL-AKT-TRP'  TO PGM-POS                        
001692                                                                          
001693     IF KEYFB-DATRPAVD > 0 OR                                             
001694        KEYFB-TIHHMM   > +0                                               
001695                                                                          
001696       PERFORM S16-GET-KDORDSTA                                           
001706                                                                          
001720       MOVE KEYFB-DATRPAVD     TO W-SAVE-DATRPAVD                         
001721       MOVE KEYFB-TIHHMM       TO W-SAVE-TIHHMM                           
001722       IF GODK-KDORDSTA                                                   
001723         IF KDODELSTA-IFYLLT                                              
001724           PERFORM S07-EVALUERA-KDODELSTA                                 
001725         END-IF                                                           
001726         IF STATUS-OK                                                     
001727           PERFORM S02-LAES-ORDERDELAR                                    
001728           IF Q3-FOUND                                                    
001729             PERFORM S09-SPAR-ENTER                                       
001730             PERFORM S11-SPAR-NEXT                                        
001731             PERFORM S13-HAEMTA-VVDTTMM                                   
001732             MOVE 99999999     TO WS-DATRPAVD                             
001733             MOVE NEJ          TO FRAMTIDA-AVGANG-SW                      
001734                                    FIRST-TIME-SW                         
001735           ELSE                                                           
001736             PERFORM IMS-04-GN-ORQI01                                     
001737           END-IF                                                         
001738         ELSE                                                             
001739           PERFORM IMS-04-GN-ORQI01                                       
001740         END-IF                                                           
001741       ELSE                                                               
001742         PERFORM IMS-04-GN-ORQI01                                         
001743       END-IF                                                             
001744     ELSE                                                                 
001745       PERFORM IMS-04-GN-ORQI01                                           
001746     END-IF                                                               
001747     MOVE 'END S03A-LAES-TILL-AKT-TRP'  TO PGM-POS                        
001748     .                                                                    
001749     EJECT                                                                
001750 S05-LAES-TRP-UTAN-TID SECTION.                                           
001751     MOVE 'STA S05-LAES-TRP-UTAN-TIDP'  TO PGM-POS                        
001752                                                                          
001753     MOVE 0                    TO W-SEQB-DATRPAVD-MIN                     
001754                                  W-SEQB-DATRPAVD-MAX                     
001755     MOVE +0                  TO  W-SEQB-TIHHMM-MIN                       
001756                                  W-SEQB-TIHHMM-MAX                       
001757     PERFORM IMS-01-GU-ORQI01                                             
001758     IF SEGMENT-FINNS                                                     
001759       PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT OR Q3-FOUND             
001760         PERFORM S05A-LAES-TILL-AKT-TRP                                   
001761       END-PERFORM                                                        
001762     END-IF                                                               
001763     MOVE 'END S05-LAES-TRP-UTAN-TIDP'  TO PGM-POS                        
001764     .                                                                    
001765     EJECT                                                                
001766 S05A-LAES-TILL-AKT-TRP SECTION.                                          
001767     MOVE 'STA S05A-LAES-TILL-AKT-TID'  TO PGM-POS                        
001768                                                                          
001769     PERFORM S16-GET-KDORDSTA                                             
001787                                                                          
001788     MOVE KEYFB-DATRPAVD       TO W-SAVE-DATRPAVD                         
001789     MOVE KEYFB-TIHHMM         TO W-SAVE-TIHHMM                           
001790     IF GODK-KDORDSTA                                                     
001791       IF KDODELSTA-IFYLLT                                                
001792         PERFORM S07-EVALUERA-KDODELSTA                                   
001793       END-IF                                                             
001794       IF STATUS-OK                                                       
001795         PERFORM S02-LAES-ORDERDELAR                                      
001796         IF Q3-FOUND                                                      
001797           PERFORM S09-SPAR-ENTER                                         
001798           PERFORM S11-SPAR-NEXT                                          
001799           PERFORM S13-HAEMTA-VVDTTMM                                     
001800           MOVE NEJ            TO FRAMTIDA-AVGANG-SW                      
001801                                  FIRST-TIME-SW                           
001802         ELSE                                                             
001803           PERFORM IMS-02-GN-ORQI01                                       
001804         END-IF                                                           
001805       ELSE                                                               
001806         PERFORM IMS-02-GN-ORQI01                                         
001807       END-IF                                                             
001808     ELSE                                                                 
001809       PERFORM IMS-02-GN-ORQI01                                           
001810     END-IF                                                               
001811     MOVE 'END S05A-LAES-TILL-AKT-TID'  TO PGM-POS                        
001812     .                                                                    
001813     EJECT                                                                
001814 S07-EVALUERA-KDODELSTA SECTION.                                          
001815     MOVE 'STA S07-EVALUERA-KDODELSTA'  TO PGM-POS                        
001816                                                                          
001817     MOVE NEJ                  TO STATUS-SW                               
001818                                                                          
001819     EVALUATE WS-KDODELSTA                                                
001820                                                                          
001821       WHEN 'R'                                                           
001822                                                                          
001823         IF WS-KDORDSTA = 'R ' OR 'R*'                                    
001824           MOVE JA             TO STATUS-SW                               
001825         END-IF                                                           
001826                                                                          
001827       WHEN 'U'                                                           
001828                                                                          
001829         IF WS-KDORDSTA = 'R*' OR 'U ' OR 'U*'                            
001830           MOVE JA             TO STATUS-SW                               
001831         END-IF                                                           
001832                                                                          
001833       WHEN 'P'                                                           
001834                                                                          
001835         IF WS-KDORDSTA = 'R*' OR 'U*' OR 'P ' OR 'P*'                    
001836           MOVE JA             TO STATUS-SW                               
001837         END-IF                                                           
001838                                                                          
001839     END-EVALUATE                                                         
001840     MOVE 'END S07-EVALUERA-KDODELSTA'  TO PGM-POS                        
001841     .                                                                    
001842     EJECT                                                                
001843 S09-SPAR-ENTER SECTION.                                                  
001844     MOVE 'STA S09-SPAR-ENTER        '  TO PGM-POS                        
001845                                                                          
001846     MOVE W-SAVE-DATRPAVD (3:6) TO MOD-TIAAMMDD-ENTER                     
001847     MOVE W-SAVE-TIHHMM        TO MOD-TIHHMM-ENTER                        
001848     MOVE ODEL-IDORDER         TO MOD-IDORDER-ENTER                       
001849     MOVE 'END S09-SPAR-ENTER        '  TO PGM-POS                        
001850     .                                                                    
001851     EJECT                                                                
001852 S11-SPAR-NEXT SECTION.                                                   
001853     MOVE 'STA S11-SPAR-NEXT         '  TO PGM-POS                        
001854                                                                          
001855     MOVE W-SAVE-DATRPAVD (3:6)  TO MOD-TIAAMMDD-NEXT                     
001856     MOVE W-SAVE-TIHHMM        TO MOD-TIHHMM-NEXT                         
001857     MOVE ODEL-IDORDER         TO MOD-IDORDER-NEXT                        
001858     MOVE 'END S11-SPAR-NEXT         '  TO PGM-POS                        
001859     .                                                                    
001860     EJECT                                                                
001861 S13-HAEMTA-VVDTTMM SECTION.                                              
001862     MOVE 'STA S13-HAEMTA-VVDTTMM    '  TO PGM-POS                        
001863                                                                          
001864     MOVE ODEL-TIHHMM          TO WS-TTMM                                 
001865     MOVE 'AAMMDD'             TO DAT-KDDATFORM                           
001866     MOVE ODEL-DATRPAVD (3:6)  TO DAT-I-TIDATUM                           
001867                                  WS-Q3-DATUM                             
001868                                                                          
001869     CALL WDATKONV USING DAT-KDDATFORM, DAT-I-TIDATUM,                    
001870                         DAT-O-TIDATUM, DAT-KDSVAR                        
001871     IF DAT-KDSVAR-OK                                                     
001872       MOVE DAT-TID            TO WS-DAG                                  
001873       MOVE DAT-TIVV           TO WS-VECKA                                
001874     END-IF                                                               
001875     MOVE WS-VVDTTMM           TO HELP-TITRPAVG                           
001876                                  FULL-TITRPAVG                           
001877     IF FIRST-TIME                                                        
001878       PERFORM S15-REDIGERA-RUBRIKEN                                      
001879       MOVE NEJ                TO FIRST-TIME-SW                           
001880     END-IF                                                               
001881     MOVE 'END S13-HAEMTA-VVDTTMM    '  TO PGM-POS                        
001882     .                                                                    
001883     EJECT                                                                
001884 S15-REDIGERA-RUBRIKEN SECTION.                                           
001885     MOVE 'STA S15-REDIGERA-RUBRIKEN '  TO PGM-POS                        
001886                                                                          
001887     MOVE ODEL-IDDC            TO W-4431-IDDC                             
001888                                  W-4433-IDDC                             
001889     MOVE ODEL-IDTRP           TO W-4432-IDTRP                            
001890                                  W-4434-IDTRP                            
001891     MOVE HELP-TITRPAVG        TO W-4434-TITRPAVG                         
001892     MOVE WS-Q3-DATUM (1:2)    TO TMP1-YY                                 
001893     MOVE DAGENS-DATUM(1:2)    TO TMP2-YY                                 
001894     PERFORM WY2000P9                                                     
001895     IF WS-VVDTTMM > WS-NUTID OR                                          
001896       TMP1-YY     > TMP2-YY                                              
001897       MOVE WS-VVDTTMM         TO MOD-TITRPAVT                            
001898     END-IF                                                               
001899     MOVE HELP-TITRPAVG        TO W-4434-TITRPAVG                         
001900     PERFORM S15A-LAES-TRANSPORT-TABELL                                   
001901     IF FULL-TITRPAVG > ZERO                                              
001902       PERFORM S15B-LAES-AVGANGS-TABELL                                   
001903     END-IF                                                               
001904     MOVE 'END S15-REDIGERA-RUBRIKEN '  TO PGM-POS                        
001905     .                                                                    
001906     EJECT                                                                
001907 S15A-LAES-TRANSPORT-TABELL SECTION.                                      
001908     MOVE 'STA S15A-LAES-TRANSPORT-TA'  TO PGM-POS                        
001909                                                                          
001910     PERFORM IMS-GU-XXKA11                                                
001911     IF SEGMENT-FINNS                                                     
001912       MOVE 4432-BETRPDST      TO MOD-BETRPDST                            
001913     END-IF                                                               
001914     MOVE 'END S15A-LAES-TRANSPORT-TA'  TO PGM-POS                        
001915     .                                                                    
001916     EJECT                                                                
001917 S15B-LAES-AVGANGS-TABELL SECTION.                                        
001918     MOVE 'STA S15B-LAES-AVGANGS-TAB '  TO PGM-POS                        
001919                                                                          
001920     PERFORM IMS-GU-XXKB11                                                
001921     IF SEGMENT-FINNS                                                     
001922       MOVE 4434-BETRPFIR      TO MOD-BETRPFIR                            
001923     ELSE                                                                 
001924       MOVE ZERO TO HELP-TITRPAVG (1:3)                                   
001925       MOVE HELP-TITRPAVG       TO W-4434-TITRPAVG                        
001926       PERFORM IMS-GU-XXKB11                                              
001927       IF SEGMENT-FINNS                                                   
001928         MOVE 4434-BETRPFIR     TO MOD-BETRPFIR                           
001929       ELSE                                                               
001930         MOVE +1               TO DAG-INDX                                
001931         MOVE 1                TO WS-VECKODAG                             
001932         PERFORM UNTIL DAG-INDX > MAX-DAG-INDX                            
001933           MOVE WS-VECKODAG TO HELP-TITRPAVG (3:1)                        
001934           MOVE HELP-TITRPAVG  TO W-4434-TITRPAVG                         
001935           PERFORM IMS-GU-XXKB11                                          
001936           IF SEGMENT-FINNS                                               
001937             MOVE 4434-BETRPFIR TO       MOD-BETRPFIR                     
001938             ADD +5            TO DAG-INDX                                
001939           ELSE                                                           
001940             ADD +1            TO DAG-INDX                                
001941             ADD 1             TO WS-VECKODAG                             
001942           END-IF                                                         
001943         END-PERFORM                                                      
001944       END-IF                                                             
001945     END-IF                                                               
001946     MOVE 'END S15B-LAES-AVGANGS-TAB '  TO PGM-POS                        
001947     .                                                                    
001948     EJECT                                                                
001949 S16-GET-KDORDSTA SECTION.                                                
001950                                                                          
001958     MOVE DCS-IDDC TO W-IDDC                                              
001959     PERFORM IMS-GNP-ORQI12                                               
001961     MOVE ARB-KDORDSTA      TO KDORDSTA-KONTROLL                          
001962                               WS-KDORDSTA                                
001967                                                                          
001968     MOVE 'END S16-GET-KDORDSTA '       TO PGM-POS                        
001969     .                                                                    
001970 MFS-RENSA-FAELT-UT SECTION.                                              
001971     MOVE 'STA MFS-RENSA-FAELT-UT    '  TO PGM-POS                        
001972                                                                          
001973*    --- ALLA UTDATA-FÄLT                                                 
001974*    --- INKL. BLÄDDRINGSNYCKLAR                                          
001975                                                                          
001976     MOVE MFS-RENSA-FAELT      TO MOD-TIAAMMDD-ENTER                      
001977                                  MOD-TIHHMM-ENTER                        
001978                                  MOD-TIAAMMDD-NEXT                       
001979                                  MOD-TIHHMM-NEXT                         
001980                                  MOD-IDORDER-ENTER                       
001981                                  MOD-IDORDER-NEXT                        
001982                                  MOD-FLAGGA-EOF                          
001983                                  MOD-FLAGGA-SCROLL                       
001984                                  MOD-VLORDNTO-SPAR                       
001985                                  MOD-VKORDNTO-SPAR                       
001986                                  MOD-SUORDV-SPAR                         
001987                                  MOD-BETRPDST                            
001988                                  MOD-TITRPAVT                            
001989                                  MOD-BETRPFIR                            
001990                                  MOD-VLORDNTO-TOTAL                      
001991                                  MOD-VKORDNTO-TOTAL                      
001992                                  MOD-SUORDV-TOTAL                        
001993                                                                          
001994     MOVE +1                   TO INDX                                    
001995     PERFORM UNTIL INDX > MAX-INDX                                        
001996       MOVE MFS-RENSA-FAELT    TO MOD-IDTRP (INDX)                        
001997                                  MOD-IDDISTR (INDX)                      
001998                                  MOD-IDKUNDNR (INDX)                     
001999                                  MOD-IDKUNDRF (INDX)                     
002000                                  MOD-STATUS (INDX)                       
002001                                  MOD-VLORDNTO (INDX)                     
002002                                  MOD-VKORDNTO (INDX)                     
002003                                  MOD-SUORDV (INDX)                       
002004       ADD +1                  TO INDX                                    
002005     END-PERFORM                                                          
002006     MOVE 'END MFS-RENSA-FAELT-UT    '  TO PGM-POS                        
002007     .                                                                    
002008     SKIP2                                                                
002009 MFS-RENSA-FAELT-IN  SECTION.                                             
002010     MOVE 'STA MFS-RENSA-FAELT-IN    '  TO PGM-POS                        
002011                                                                          
002012*    --- ALLA INDATA-FÄLT                                                 
002013                                                                          
002014     MOVE MFS-RENSA-FAELT      TO MOD-IDDC-IN                             
002015                                  MOD-IDTRP-IN                            
002016                                  MOD-KDODELSTA-IN                        
002017     .                                                                    
002018     EJECT                                                                
002019* --- IMS SEKTIONER ---                                                   
002020     SKIP3                                                                
002021 IMS-GET-MSG SECTION.                                                     
002022                                                                          
002023     MOVE '  QC' TO GODK-STATUSKODER                                      
002024     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
002025     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
002026     PERFORM IMS-STATUSKONTROLL                                           
002027     .                                                                    
002028     SKIP3                                                                
002029 IMS-INSERT-MSG SECTION.                                                  
002030                                                                          
002031     IF NOT ENGLISH-TEXT                                                  
002032       MOVE '0' TO MFS-KDHUVOMR                                           
002033     END-IF                                                               
002034     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
002035     MOVE SPACE TO GODK-STATUSKODER                                       
002036     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
002037     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
002038     PERFORM IMS-STATUSKONTROLL                                           
002039     .                                                                    
002040     EJECT                                                                
002041 IMS-GU-WDE601 SECTION.                                                   
002042     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
002043          DELIMITED BY SIZE INTO SSA1                                     
002044     MOVE '  GE' TO GODK-STATUSKODER                                      
002045     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-AREA-VORD SSA1                 
002046     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
002047     PERFORM IMS-STATUSKONTROLL                                           
002048     .                                                                    
002049     SKIP3                                                                
002050 IMS-GNP-WDE611 SECTION.                                                  
002051     MOVE 'WDE611  ' TO SSA1                                              
002052     MOVE '  GE' TO GODK-STATUSKODER                                      
002053     CALL CBLTDLI USING GNP WDE6-PCB DLI-IO-AREA-KOLLI SSA1               
002054     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
002055     PERFORM IMS-STATUSKONTROLL                                           
002056     .                                                                    
002057     SKIP3                                                                
002058 IMS-GU-ORQA01 SECTION.                                                   
002059                                                                          
002060     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN-X                        
002061                    '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                    
002062          DELIMITED BY SIZE INTO SSA1                                     
002063     MOVE '  GBGE' TO GODK-STATUSKODER                                    
002064     CALL CBLTDLI USING GU ORQA-PCB DLI-IO-AREA-ODEL SSA1                 
002065     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
002066     PERFORM IMS-STATUSKONTROLL                                           
002067     .                                                                    
002068     EJECT                                                                
002069 IMS-GN-ORQA01 SECTION.                                                   
002070                                                                          
002071     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN-X                        
002072                    '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                    
002073          DELIMITED BY SIZE INTO SSA1                                     
002074     MOVE '  GBGE' TO GODK-STATUSKODER                                    
002075     CALL CBLTDLI USING GN ORQA-PCB DLI-IO-AREA-ODEL SSA1                 
002076     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
002077     PERFORM IMS-STATUSKONTROLL                                           
002078     .                                                                    
002079     EJECT                                                                
002080 IMS-GU-ORQA01-STATUS SECTION.                                            
002081                                                                          
002082     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN-X                        
002083                    '&WDQ301KY<=' W-WDQ301KY-MAX-X                        
002084                    '&KDODELST =' W-KDODELST-X ')'                        
002085          DELIMITED BY SIZE INTO SSA1                                     
002086     MOVE '  GBGE' TO GODK-STATUSKODER                                    
002087     CALL CBLTDLI USING GU ORQA-PCB DLI-IO-AREA-ODEL SSA1                 
002088     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
002089     PERFORM IMS-STATUSKONTROLL                                           
002090     .                                                                    
002091     EJECT                                                                
002092 IMS-GN-ORQA01-STATUS SECTION.                                            
002093                                                                          
002094     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN-X                        
002095                    '&WDQ301KY<=' W-WDQ301KY-MAX-X                        
002096                    '&KDODELST =' W-KDODELST-X ')'                        
002097          DELIMITED BY SIZE INTO SSA1                                     
002098     MOVE '  GBGE' TO GODK-STATUSKODER                                    
002099     CALL CBLTDLI USING GN ORQA-PCB DLI-IO-AREA-ODEL SSA1                 
002100     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
002101     PERFORM IMS-STATUSKONTROLL                                           
002102     .                                                                    
002103     EJECT                                                                
002104 IMS-01-GU-ORQI01 SECTION.                                                
002105                                                                          
002106     STRING 'WLORQI01(WDQ2BSEQ>=' W-WDQ2BSEQ-MIN-X                        
002107                    '&WDQ2BSEQ<=' W-WDQ2BSEQ-MAX-X ')'                    
002108          DELIMITED BY SIZE INTO SSA1                                     
002109     MOVE '  GBGE' TO GODK-STATUSKODER                                    
002110     CALL CBLTDLI USING GU ORQI-PCB DLI-IO-AREA-ORQI SSA1                 
002111     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
002112     PERFORM IMS-STATUSKONTROLL                                           
002113     .                                                                    
002114     EJECT                                                                
002115 IMS-02-GN-ORQI01 SECTION.                                                
002116                                                                          
002117     STRING 'WLORQI01(WDQ2BSEQ>=' W-WDQ2BSEQ-MIN-X                        
002118                    '&WDQ2BSEQ<=' W-WDQ2BSEQ-MAX-X ')'                    
002119          DELIMITED BY SIZE INTO SSA1                                     
002120     MOVE '  GBGE' TO GODK-STATUSKODER                                    
002121     CALL CBLTDLI USING GN ORQI-PCB DLI-IO-AREA-ORQI SSA1                 
002122     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
002123     PERFORM IMS-STATUSKONTROLL                                           
002124     .                                                                    
002125     EJECT                                                                
002126 IMS-03-GU-ORQI01 SECTION.                                                
002127                                                                          
002128     STRING 'WLORQI01(WDQ2BSEQ >' W-WDQ2BSEQ-MIN-X                        
002129                    '&WDQ2BSEQ<=' W-WDQ2BSEQ-MAX-X ')'                    
002130          DELIMITED BY SIZE INTO SSA1                                     
002131     MOVE '  GBGE' TO GODK-STATUSKODER                                    
002132     CALL CBLTDLI USING GU ORQI-PCB DLI-IO-AREA-ORQI SSA1                 
002133     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
002134     PERFORM IMS-STATUSKONTROLL                                           
002135     .                                                                    
002136     EJECT                                                                
002137 IMS-04-GN-ORQI01 SECTION.                                                
002138                                                                          
002139     STRING 'WLORQI01(WDQ2BSEQ >' W-WDQ2BSEQ-MIN-X                        
002140                    '&WDQ2BSEQ<=' W-WDQ2BSEQ-MAX-X ')'                    
002141          DELIMITED BY SIZE INTO SSA1                                     
002142     MOVE '  GBGE' TO GODK-STATUSKODER                                    
002143     CALL CBLTDLI USING GN ORQI-PCB DLI-IO-AREA-ORQI SSA1                 
002144     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
002145     PERFORM IMS-STATUSKONTROLL                                           
002146     .                                                                    
002147     EJECT                                                                
002148 IMS-GNP-ORQI12   SECTION.                                                
002149                                                                          
002150     STRING 'WLORQI12*F(IDDC     =' W-IDDC-X ')'                          
002151          DELIMITED BY SIZE INTO SSA1                                     
002160     MOVE '    '               TO GODK-STATUSKODER                        
002200     CALL CBLTDLI USING GNP  ORQI-PCB DLI-IO-AREA-ARB SSA1                
002300     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
002400     PERFORM IMS-STATUSKONTROLL                                           
002500     .                                                                    
002501     SKIP3                                                                
002502 IMS-GU-XXKA11 SECTION.                                                   
002503                                                                          
002504     STRING 'WLXXKA01(WDGXKEY  =' W-4431-IDHTYP-X ')'                     
002505          DELIMITED BY SIZE INTO SSA1                                     
002506     STRING 'WLXXKA11(WDGXKEY  =' W-4432-IDTRP-X ')'                      
002507          DELIMITED BY SIZE INTO SSA2                                     
002508     MOVE '  GE' TO GODK-STATUSKODER                                      
002509     CALL CBLTDLI USING GU XXKA-PCB DLI-IO-AREA-4432 SSA1 SSA2            
002510     MOVE XXKA-STATUS-CODE TO STATUS-WS                                   
002511     PERFORM IMS-STATUSKONTROLL                                           
002512     .                                                                    
002513     EJECT                                                                
002514 IMS-GU-XXKB11 SECTION.                                                   
002515                                                                          
002516     STRING 'WLXXKB01(WDGXKEY  =' W-4433-IDHTYP-X ')'                     
002517          DELIMITED BY SIZE INTO SSA1                                     
002518     STRING 'WLXXKB11(WDGXKEY  =' W-4434-IDTRP-X ')'                      
002519          DELIMITED BY SIZE INTO SSA2                                     
002520     MOVE '  GE' TO GODK-STATUSKODER                                      
002521     CALL CBLTDLI USING GU XXKB-PCB DLI-IO-AREA-4434 SSA1 SSA2            
002522     MOVE XXKB-STATUS-CODE TO STATUS-WS                                   
002523     PERFORM IMS-STATUSKONTROLL                                           
002524     .                                                                    
002525     EJECT                                                                
002526 IMS-GU-WDB601    SECTION.                                                
002527     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
002528          DELIMITED BY SIZE INTO SSA1                                     
002529     MOVE '  GE' TO GODK-STATUSKODER                                      
002530     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
002531     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
002532     PERFORM IMS-STATUSKONTROLL                                           
002533     IF SEGMENT-SAKNAS                                                    
002534         MOVE SPACE TO DCS-KDDC                                           
002535     END-IF                                                               
002536     .                                                                    
002537 IMS-STATUSKONTROLL SECTION.                                              
002538                                                                          
002539     SET STATUS-IX TO 1                                                   
002540     SEARCH GODK-STATUS                                                   
002541       AT END CALL FELLOG                                                 
002542       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
002543     END-SEARCH                                                           
002544     .                                                                    
002545     EJECT                                                                
002546*    -COPY WY2000P1                                                       
002547     EJECT                                                                
002548*    -COPY WY2000P9                                                       
