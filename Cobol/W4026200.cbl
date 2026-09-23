000001 ID DIVISION.                                                             
000002     SKIP2                                                                
000003 PROGRAM-ID.     W4026200.                                                
000004 AUTHOR.         ANNELIE ENGLUND                                          
000005 DATE-WRITTEN.   APRIL -91.                                               
000006                                                                          
000007     REMARKS.                                                             
000008*                                                                         
000009*    FUNKTION.                                                            
000010*        PROGRAMMET HANTERAR UPPLÄGGNING AV PROFORMARADER.                
000011*        REGISTRERADE VÄRDEN KONTROLLERAS OCH ÖVRIGA                      
000012*        HÄMTAS FRÅN ARTIKELREGISTRET.                                    
000013*                                                                         
000014*        EFTER UPPLÄGGNING AV GODKÄNDA RADER SKER UTHOPP TILL             
000015*        SVARSBILD - 4263.                                                
000016*                                                                         
000017*                                                                         
000018*        PROGRAMMET ÄR ETT UPPDATERINGS-MPP                               
000019*        PROGRAMMET LÄSER      WLORQM (WDQ1)  ORDERBEKRÄFTELSER           
000020*        PROGRAMMET UPPDATERAR WLPROC (WDE8)  PROFORMA HUVUD              
000021*        PROGRAMMET UPPDATERAR WLPROD (WDE9)  PROFORMA RADER              
000022*        PROGRAMMET LÄSER      WLGMTA (WDB2)  KUNDREG                     
000023*        PROGRAMMET LÄSER              WDK6   ARTIKELREG                  
000024*        PROGRAMMET LÄSER      WLARTM (WDK9)  ARTIKELREG                  
000025*        PROGRAMMET LÄSER      WLLEVF (WDF2)  STYRREG DIRLEV              
000026*        PROGRAMMET LÄSER      WLLEVG (WDF2)  STYRREG DIRLEV ASEQ         
000027*        PROGRAMMET LÄSER      WLLEVA (WDF1)  LEVERANSREG                 
000028*        PROGRAMMET LÄSER      WLERSA (WDD7)  ERSÄTTNINGSREG              
000029*                                                                         
000030*    INDATA.                                                              
000031*        TRANSAKTION: W4T262                                              
000032*                     W4T262U                                             
000033*        MID:         W4I26201                                            
000034*    UTDATA.                                                              
000035*        MOD:         W4O26201                                            
000036* CHANGE LOG:                                                             
000037*                                                                         
000038*                                                                         
000039*    E'TRACKER: 5444132 DATED 2007-08-21                                  
000040*    E'TRACKER: 7450328 DATED 2008-HÖST  VOHF                             
000041*    E'TRACKER: 10254592      2015       DECOMISSION VOHF                 
000042*                                                                         
000043     EJECT                                                                
000044 ENVIRONMENT DIVISION.                                                    
000045                                                                          
000046 DATA DIVISION.                                                           
000047 WORKING-STORAGE SECTION.                                                 
000048                                                                          
000049     SKIP3                                                                
000050*    -COPY WY2000W1                                                       
000051     SKIP3                                                                
000052 77  IDPGM                       PIC X(08)   VALUE 'W4026200'.            
000053 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
000054 77  KDRC-DISPLAY                PIC Z(5)   VALUE ZERO.                   
000055 77  HOPP-TILL-4263              PIC X(1)   VALUE 'N'.                    
000056 77  YES                         PIC X(1)   VALUE 'Y'.                    
000057 77  JA                          PIC X(1)   VALUE 'J'.                    
000058 77  NEJ                         PIC X(1)   VALUE 'N'.                    
000059 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
000060 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +1137 COMP SYNC.        
000061 77  RKOD-ABEND                  PIC S9(4)   COMP SYNC VALUE +33.         
000062 77  RKOD-ABEND-MED-DUMP         PIC S9(4)  VALUE +1000 COMP SYNC.        
000063 77  IX-DCCLEAR-MAX              PIC S9(3)   COMP SYNC VALUE +99.         
000064 77  WS-INDEX                    PIC S9(9)   COMP SYNC VALUE ZERO.        
000066 77  WS-INDEX-MID                PIC S9(9)   COMP SYNC VALUE ZERO.        
000067 77  WS-INDEX-MID-MAX            PIC S9(9)   COMP SYNC VALUE +14.         
000068 77  WS-INDEX-TILLK              PIC S9(9)   COMP SYNC VALUE ZERO.        
000069 77  WS-INDEX-TILLK-MAX          PIC S9(9)   COMP SYNC VALUE +20.         
000070 77  WS-INDEX-CL                 PIC S9(9)   COMP SYNC VALUE ZERO.        
000071 77  WS-INDEX-CL-MAX             PIC S9(9)   COMP SYNC VALUE +2.          
000072 77  WS-IDDISTR                  PIC X(4)    VALUE SPACE.                 
000073 77  WS-IDKUNDNR                 PIC X(6)    VALUE SPACE.                 
000074 77  WS-IDORDNR                  PIC X(7)    VALUE SPACE.                 
000075 77  WS-IDPRQUES                 PIC S9(7)   VALUE +0.                    
000076 77  WS-KVPREAVB                 PIC S9(7)   VALUE +0  COMP-3.            
000077 77  WS-SUORDV                   PIC S9(9)V9(2) VALUE +0  COMP-3.         
000078 77  WS-SUORDV-LOC               PIC S9(9)V9(2) VALUE +0  COMP-3.         
000079 77  WS-SUORDV-LOCPREL           PIC S9(9)V9(2) VALUE +0  COMP-3.         
000080 77  WS-VLORDBTO                 PIC S9(4)V9(3) VALUE +0  COMP-3.         
000081 77  WS-VKORDNTO                 PIC S9(6)V9(1) VALUE +0  COMP-3.         
000082 77  WS-SUORDV-RAKN              PIC S9(9)V9(2) VALUE +0  COMP-3.         
000083 77  WS-SUORDV-RAKN-LOC          PIC S9(9)V9(2) VALUE +0  COMP-3.         
000084 77  WS-SUORDV-RAKN-LOCPREL      PIC S9(9)V9(2) VALUE +0  COMP-3.         
000085 77  WS-SA-VLARTNTO              PIC S9(8)V9(1) VALUE +0  COMP-3.         
000086 77  WS-SA-VLARTNTO-RAKN         PIC S9(9)V9(1) VALUE +0  COMP-3.         
000087 77  WS-SA-VKART                 PIC S9(9)      VALUE +0  COMP-3.         
000088 77  WS-SA-VKART-RAKN            PIC S9(9)      VALUE +0  COMP-3.         
000089 77  WS-KVMOTOR                  PIC S9(3)   VALUE +0.                    
000090 77  WS-KVKAROSS                 PIC S9(3)   VALUE +0.                    
000091 77  WS-TIFORDAT                 PIC 9(6)    VALUE ZERO.                  
000092 77  OK-BEHANDLAD                PIC X(3)    VALUE '101'.                 
000093 77  STARTAD-AV-DISPATCHEN-SW    PIC X       VALUE 'N'.                   
000094     88  STARTAD-AV-DISPATCHEN               VALUE 'J'.                   
000095                                                                          
000101 77  WX-KDORDBEK                 PIC S9(2)  VALUE +0.                     
000102     EJECT                                                                
000103                                                                          
000104 77  ALLT-SW                     PIC X       VALUE 'J'.                   
000105     88  ALLT-OK                             VALUE 'J'.                   
000106                                                                          
000107 77  NYCKEL-SW                   PIC X       VALUE 'J'.                   
000108     88  NYCKEL-OK                           VALUE 'J'.                   
000109                                                                          
000110 77  TILLK-SW                    PIC X       VALUE 'N'.                   
000111     88  TILLKOMMANDE-RAD                    VALUE 'J'.                   
000112                                                                          
000113 77  SVARSBILD-SW                PIC X       VALUE 'N'.                   
000114     88  SVARSBILD                           VALUE 'J'.                   
000115                                                                          
000116 77  OBKR-SW                     PIC X       VALUE 'N'.                   
000117     88  SKRIV-OBKR                          VALUE 'J'.                   
000118     88  OBKR-SKRIVEN                        VALUE 'S'.                   
000119                                                                          
000120 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
000121     88  EGEN-MID                            VALUE '4262'.                
000122     88  GODK-MID                            VALUE '4264'                 
000123                                                   '4265' '4266'          
000124                                                   '4267' '4268'.         
000125                                                                          
000126 01  DUMMY-PCB                   PIC X(4)   VALUE LOW-VALUE.              
000127*    --- VALID IDDC CODES                                                 
000128*                                                                         
000129*01  -COPY WWDCKONS                                                       
000130     EJECT                                                                
000131 01  TEST-IDDISTR                PIC  9(5)   COMP-3.                      
000132*01  FILLER  -COPY WWDIST18    -RED TEST-IDDISTR                          
000133*    ----DISTR-DEALER-PRICE----                                           
000134*01  FILLER  -COPY WWDIST79    -RED TEST-IDDISTR                          
000135     EJECT                                                                
000136 01  WS-ALFA-6.                                                           
000137     03  WS-NUM-6                PIC 9(6).                                
000138                                                                          
000139 01  DAGENS-DATUM                PIC 9(6)   VALUE ZERO.                   
000140 01  DAGENS-TID                  PIC 9(8)   VALUE ZERO.                   
000141 01  FILLER REDEFINES DAGENS-TID.                                         
000142     03 DAGENS-HHMMSS            PIC 9(6).                                
000143     03 FILLER                   PIC 9(2).                                
000144     EJECT                                                                
000145                                                                          
000146 01  WS-TITIREGD-9KOMPL          PIC 9(8).                                
000147 01  FILLER REDEFINES WS-TITIREGD-9KOMPL.                                 
000148     03  WS-SEKEL-9KOMPL         PIC 9(2).                                
000149     03  WS-AAMMDD-9KOMPL        PIC 9(6).                                
000150                                                                          
000151 01  WS-IDDC-AKT                 PIC X(2).                                
000152                                                                          
000153     EJECT                                                                
000154 01 FILLER                    PIC X(16) VALUE 'TILLKOMMANDE TAB'.         
000155                                                                          
000156*    -COPY W411TILK                                                       
000157     EJECT                                                                
000158*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
000159 01  GENERELLA-SUBPROGRAM.                                                
000160     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
000161     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000162     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000163     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
000164     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
000165*                                                                         
000166*                                                                         
000167*                                                                         
000168 01  GEMENSAMMA-SUBPROGRAM.                                               
000169     03  W335PRIS                PIC X(8)    VALUE 'W335PRIS'.            
000170*        PRISTILLÄMPNING                                                  
000171     03  W335PRNO                PIC X(8)    VALUE 'W335PRNO'.            
000172*        HÄMTA PRISFRÅGENR                                                
000173     03  W335PRQU                PIC X(8)    VALUE 'W335PRQU'.            
000174*        DEALER PRISFRÅGABEHANDLING                                       
000175     03  W411AREG                PIC X(8)    VALUE 'W411AREG'.            
000176*        LÄSNING ARTIKELREGISTER                                          
000177     03  W411ARTM                PIC X(8)    VALUE 'W411ARTM'.            
000178*        NYUPPLÄGG AV TOM ROT PÅ WDK9                                     
000179     03  W411DLEV                PIC X(8)    VALUE 'W411DLEV'.            
000180*        KONTROLL DIREKTLEVERANS                                          
000181     03  W411KERS                PIC X(8)    VALUE 'W411KERS'.            
000182*        KONTROLL ERSÄTTNINGAR                                            
000183     03  W411KVAN                PIC X(8)    VALUE 'W411KVAN'.            
000184*        KONTROLL KVANTANPASSNING                                         
000185     03  W411ORFK                PIC X(8)    VALUE 'W411ORFK'.            
000186*        FORMELLA KONTROLLER AV INDATA                                    
000187     03  W411CDCA                PIC X(8)    VALUE 'W411CDCA'.            
000188*        KONTROLL PRELIMINÄRAVBOKNING                                     
000189     03  W411SPAR                PIC X(8)    VALUE 'W411SPAR'.            
000190*        KONTROLL SPÄRRAR                                                 
000191     03  W411STOR                PIC X(8)    VALUE 'W411STOR'.            
000192*        KONTROLL STORA UTTAG                                             
000193     03  W411EXCH                PIC X(8)    VALUE 'W411EXCH'.            
000194*        RÄKNA OM VALUTA  DDI                                             
000195     EJECT                                                                
000196*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
000197*   -COPY WMEDAREA                                                        
000198     EJECT                                                                
000199 01  MESSAGE-CODES.                                                       
000200     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
000201     03  ERR-OBEHORIG            PIC X(3)    VALUE '405'.                 
000202     03  ERR-ORDER-SAKNAS        PIC X(3)    VALUE '701'.                 
000203     03  ERR-ORDER-AVSLUTAD      PIC X(3)    VALUE '057'.                 
000204     03  ERR-STORT-UTTAG         PIC X(3)    VALUE '725'.                 
000205     03  ERR-UPPLYSTA-FEL        PIC X(3)    VALUE '001'.                 
000206     03  ERR-IDARTNR-SAKNAS      PIC X(3)    VALUE '017'.                 
000207     03  ERR-ORDER-ANNULLERAD    PIC X(3)    VALUE '052'.                 
000208     EJECT                                                                
000209*    --- PARAMETRAR TILL GEMENSAMMA SUBPROGRAM                            
000210*                                                                         
000211 01 FILLER                       PIC X(8) VALUE 'W335PRIS'.               
000212*   -COPY W335PRIS                                                        
000213*                                                                         
000214     EJECT                                                                
000215 01 FILLER                       PIC X(8) VALUE 'W335PRNO'.               
000216*   -COPY W335PRNO                                                        
000217*                                                                         
000218     EJECT                                                                
000219 01 FILLER                       PIC X(8) VALUE 'W335PRQU'.               
000220*   -COPY W335PRQU                                                        
000221*                                                                         
000222     EJECT                                                                
000223 01 FILLER                       PIC X(8) VALUE 'W411AREG'.               
000224*   -COPY W411AREG                                                        
000225*                                                                         
000226     EJECT                                                                
000227*                                                                         
000228 01 FILLER                       PIC X(8) VALUE 'W411ARTM'.               
000229*   -COPY W411ARTM                                                        
000230*                                                                         
000231     EJECT                                                                
000232*                                                                         
000233*                                                                         
000234 01 FILLER                       PIC X(8) VALUE 'W411DLEV'.               
000235*   -COPY W411DLEV                                                        
000236*                                                                         
000237     EJECT                                                                
000238*                                                                         
000239 01 FILLER                       PIC X(8) VALUE 'W411KERS'.               
000240*   -COPY W411KERS                                                        
000241*                                                                         
000242     EJECT                                                                
000243*                                                                         
000244 01 FILLER                       PIC X(8) VALUE 'W411KVAN'.               
000245*   -COPY W411KVAN                                                        
000246*                                                                         
000247     EJECT                                                                
000248*                                                                         
000249 01 FILLER                       PIC X(8) VALUE 'W411ORFK'.               
000250*   -COPY W411ORFK                                                        
000251*                                                                         
000252     EJECT                                                                
000253*                                                                         
000254 01 FILLER                       PIC X(8) VALUE 'W411CDCA'.               
000255*   -COPY W411CDCA                                                        
000256*                                                                         
000257     EJECT                                                                
000258*                                                                         
000259 01 FILLER                       PIC X(8) VALUE 'W411SPAR'.               
000260*   -COPY W411SPAR                                                        
000261*                                                                         
000262     EJECT                                                                
000263*                                                                         
000264 01 FILLER                       PIC X(8) VALUE 'W411STOR'.               
000265*   -COPY W411STOR                                                        
000266*                                                                         
000267     EJECT                                                                
000268*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
000269*                                                                         
000270 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
000271     SKIP3                                                                
000272*01  MID -COPY W4I26201                                                   
000273     EJECT                                                                
000274 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
000275     SKIP3                                                                
000276*01  -COPY WMSGAREA                                                       
000277     EJECT                                                                
000278*    03  MOD -COPY W4O26201   -RED MSG-AREA.                              
000279     EJECT                                                                
000280 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
000281     SKIP3                                                                
000282 01  KOM-IO-AREA.                                                         
000283*03  -COPY WMSGKOM                                                        
000284     EJECT                                                                
000285 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
000286     SKIP3                                                                
000287*01  -COPY WMFSAREA                                                       
000288     EJECT                                                                
000289*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
000290*                                                                         
000291 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000292     SKIP3                                                                
000293 01  NYCKLAR-TILL-DLI.                                                    
000294                                                                          
000295     03  W-IDGMT-WDB2-X.                                                  
000296         05  W-IDDISTR-WDB2      PIC S9(5)   VALUE ZERO COMP-3.           
000297         05  W-IDKUNDNR-WDB2     PIC S9(7)   VALUE ZERO COMP-3.           
000298                                                                          
000299     03  W-IDGMT-MIN-X.                                                   
000300         05  W-IDDISTR-WDB2-MIN  PIC S9(5)   VALUE ZERO COMP-3.           
000301         05  W-IDKUNDNR-WDB2-MIN PIC S9(7)   VALUE ZERO COMP-3.           
000302                                                                          
000303     03  W-IDGMT-MAX-X.                                                   
000304         05  W-IDDISTR-WDB2-MAX  PIC S9(5)   VALUE ZERO COMP-3.           
000305         05  W-IDKUNDNR-WDB2-MAX PIC S9(7)   VALUE ZERO COMP-3.           
000306                                                                          
000307     03  W-WDE801KY-X.                                                    
000308         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
000309         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
000310         05  W-IDKUNDRF          PIC X(10)   VALUE SPACE.                 
000311                                                                          
000312     03  W-IDARTNR-X.                                                     
000313         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
000314                                                                          
000315     03  W-WDQ101KY-MIN-X.                                                
000316         05  W-IDORDER-Q1-MIN    PIC S9(7)   VALUE ZERO COMP-3.           
000317         05  W-IDARTNR-Q1-MIN    PIC S9(9)   VALUE ZERO COMP-3.           
000318         05  W-IDLOPNR-Q1-MIN    PIC S9(3)   VALUE ZERO COMP-3.           
000319         05  W-IDSEKVNR-Q1-MIN   PIC S9(3)   VALUE ZERO COMP-3.           
000320         05  FILLER              PIC X(04)   VALUE LOW-VALUE.             
000321                                                                          
000322     03  W-WDQ101KY-MAX-X.                                                
000323         05  W-IDORDER-Q1-MAX    PIC S9(7)   VALUE ZERO COMP-3.           
000324         05  W-IDARTNR-Q1-MAX    PIC S9(9)   VALUE ZERO COMP-3.           
000325         05  W-IDLOPNR-Q1-MAX    PIC S9(3)   VALUE ZERO COMP-3.           
000326         05  W-IDSEKVNR-Q1-MAX   PIC S9(3)   VALUE ZERO COMP-3.           
000327         05  FILLER              PIC X(04)   VALUE HIGH-VALUE.            
000328                                                                          
000329     03  W-WDB101KY-X.                                                    
000330         05  W-WDB1-IDPARTNR     PIC X(9)    VALUE SPACE.                 
000331         05  W-WDB1-IDFTG        PIC 9(2)    VALUE ZERO.                  
000332                                                                          
000333     EJECT                                                                
000334*    --- STATUS-KOD FRÅN IMS                                              
000335 01  STATUS-WS                   PIC XX.                                  
000336     88  SEGMENT-FINNS                       VALUE '  '.                  
000337     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
000338     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
000339     88  BASEN-SLUT                          VALUE 'GB'.                  
000340     SKIP2                                                                
000341 01  GODK-STATUSKODER.                                                    
000342     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000343                                                                          
000344 01  SSA1                        PIC X(96).                               
000345 01  SSA2                        PIC X(64).                               
000346     EJECT                                                                
000347*    --- IMS FUNKTIONSKODER                                               
000348*01  -COPY W0003                                                          
000349     EJECT                                                                
000350*    ---  DLI INPUT-OUTPUT AREA                                           
000351 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
000352     SKIP3                                                                
000353 01  FILLER                      PIC X(16)   VALUE 'WDB101-AREA'.         
000354 01  DLI-IO-AREA-WDB101.                                                  
000355     03  WLBETC01.                                                        
000356*        05  -COPY WDB101                                                 
000357 01  FILLER                      PIC X(16)   VALUE 'WDB201-AREA'.         
000358 01  DLI-IO-AREA-KUND.                                                    
000359     03  WLGMTA01.                                                        
000360*        05  -COPY WDB201                                                 
000361     EJECT                                                                
000362 01  FILLER                      PIC X(16)   VALUE 'WDQ101-AREA'.         
000363 01  DLI-IO-AREA-OBKR.                                                    
000364     03  WLORQM01.                                                        
000365*        05  -COPY WDQ101                                                 
000366     EJECT                                                                
000367 01  FILLER                      PIC X(16)   VALUE 'WDE801-AREA'.         
000368 01  DLI-IO-AREA-PHUV.                                                    
000369     03  WLPROC01.                                                        
000370*        05  -COPY WDE801                                                 
000371     EJECT                                                                
000372 01  FILLER                      PIC X(16)   VALUE 'WDE901-AREA'.         
000373 01  DLI-IO-AREA-PRAD.                                                    
000374     03  WLPROD01.                                                        
000375*        05  -COPY WDE901                                                 
000376     EJECT                                                                
000377 01  FILLER                      PIC X(16)   VALUE 'WDK901-AREA'.         
000378 01  DLI-IO-AREA-ART.                                                     
000379     03  WLARTM01.                                                        
000380*        05  -COPY WDK901                                                 
000381     EJECT                                                                
000382                                                                          
000383*---MSG-AREA FÖR HOPP TILL 4263-SVARSBILDEN                               
000384                                                                          
000385 01  FILLER                  PIC X(16)  VALUE '4263-MSG-IO-AREA'.         
000386 01  4263-MSG-IO-AREA.                                                    
000387     03  4263-LL               PIC S9(4)  VALUE +85  COMP SYNC.           
000388     03  4263-Z1               PIC X.                                     
000389     03  4263-Z2               PIC X.                                     
000390     03  4263-TRANSKOD         PIC X(8)   VALUE 'W4T263  '.               
000391     03  4263-IDTRANS          PIC X(4)   VALUE '4262'.                   
000392     03  4263-SPRAK            PIC X.                                     
000393     03  4263-IDDISTR-IN       PIC X(4).                                  
000394     03  4263-IDKUNDNR-IN      PIC X(6).                                  
000395     03  4263-IDORDNR-IN       PIC X(7).                                  
000396     03  4263-IDARTNR-IN       PIC X(9).                                  
000397     03  4263-IDDISTR-UT       PIC X(4).                                  
000398     03  4263-IDKUNDNR-UT      PIC X(6).                                  
000399     03  4263-IDORDNR-UT       PIC X(7).                                  
000400     03  4263-KDORDKL-UT       PIC X      VALUE SPACE.                    
000401     03  FILLER                PIC X(24)  VALUE ZERO.                     
000402     EJECT                                                                
000403 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
000404     SKIP3                                                                
000405 01  -COPY WZ01SEND                                                       
000406     EJECT                                                                
000407 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
000408     SKIP3                                                                
000409 01  SEND-AREA.                                                           
000410*    03  -COPY WZ01REQU  -PRE 3039-                                       
000411*    03  -COPY W30391I1  -PRE 3039-                                       
000412     EJECT                                                                
000413     EJECT                                                                
000414 LINKAGE SECTION.                                                         
000415                                                                          
000416*01  -COPY W0009      -PRE MSG-                                           
000417     EJECT                                                                
000418*01  -COPY W0009      -PRE DISP-                                          
000419     EJECT                                                                
000420*01  -COPY W0009      -PRE 4263-                                          
000421     EJECT                                                                
000422*01  -COPY W0009      -PRE 4263V-                                         
000423     EJECT                                                                
000424*01  -COPY W0009      -PRE PRQRY-                                         
000425     SKIP2                                                                
000426*01  -COPY W0008      -PRE ORQM-                                          
000427     05  FILLER                  PIC X.                                   
000428     EJECT                                                                
000429*01  -COPY W0008      -PRE PROC-                                          
000430     05  FILLER                  PIC X.                                   
000431     EJECT                                                                
000432*01  -COPY W0008      -PRE PROD-                                          
000433     05  FILLER                  PIC X.                                   
000434     EJECT                                                                
000435*01  -COPY W0008      -PRE ARTM-                                          
000436     05  FILLER                  PIC X.                                   
000437     EJECT                                                                
000438*01  -COPY W0008      -PRE WDB2-                                          
000439     05  FILLER                  PIC X.                                   
000440     EJECT                                                                
000441*01  -COPY W0008      -PRE WDB1-                                          
000442     05  FILLER                  PIC X.                                   
000443     EJECT                                                                
000444 01  PRIS-ARTC-PCB               PIC X.                                   
000445 01  PRIS-WDK7-PCB               PIC X.                                   
000446 01  PRIS-GMTA-PCB               PIC X.                                   
000447 01  PRIS-BETA-PCB               PIC X.                                   
000448 01  PRIS-GPRIA-PCB              PIC X.                                   
000449 01  PRIS-GPRIB-PCB              PIC X.                                   
000450 01  PRIS-COST-WDK6-PCB          PIC X.                                   
000451 01  PRIS-COST-WDK7-PCB          PIC X.                                   
000452 01  PRIS-COST-WDF1-PCB          PIC X.                                   
000453 01  PRIS-COST-9305-PCB          PIC X.                                   
000454 01  PRIS-COST-WDK72-PCB         PIC X.                                   
000455 01  PRIS-COST-WDB6-PCB          PIC X.                                   
000456 01  PRNO-3107-PCB               PIC X.                                   
000457 01  PRQU-WDG2-PCB               PIC X.                                   
000458 01  PRQU-WDC7-PCB               PIC X.                                   
000459 01  PRQU-SJKO-WDK6-PCB          PIC X.                                   
000460 01  AREG-WDK6-PCB               PIC X.                                   
000461 01  AREG-WDK7-PCB               PIC X.                                   
000462 01  ARTM-ARTM-PCB               PIC X.                                   
000463 01  DLEV-LEVF-PCB               PIC X.                                   
000464 01  DLEV-LEVG-PCB               PIC X.                                   
000465 01  DLEV-LEVA-PCB               PIC X.                                   
000466 01  DLEV-ARTS-PCB               PIC X.                                   
000467 01  DLEV-WDB6-PCB               PIC X.                                   
000468 01  SPAR-WDF8-PCB               PIC X.                                   
000469 01  SPAR-WDF8A-PCB              PIC X.                                   
000470 01  SPAR-WDK6-PCB               PIC X.                                   
000471 01  KERS-ARTC-PCB               PIC X.                                   
000472 01  KERS-ERSA-PCB               PIC X.                                   
000473 01  CDCA-ARTM-PCB               PIC X.                                   
000474 01  CDCA-INLB-PCB               PIC X.                                   
000475 01  CDCA-WDB2-PCB               PIC X.                                   
000476 01  CDCA-WDC1-PCB               PIC X.                                   
000477 01  KVAN-WDB2-PCB               PIC X.                                   
000478 01  KVAN-WDC1-PCB               PIC X.                                   
000479     EJECT                                                                
000480 PROCEDURE DIVISION  USING MSG-PCB DISP-PCB 4263-PCB 4263V-PCB            
000481        PRQRY-PCB                                                         
000482        ORQM-PCB PROC-PCB PROD-PCB ARTM-PCB WDB2-PCB WDB1-PCB             
000483        PRIS-ARTC-PCB PRIS-WDK7-PCB                                       
000484        PRIS-GMTA-PCB PRIS-BETA-PCB                                       
000485        PRIS-GPRIA-PCB PRIS-GPRIB-PCB                                     
000486        PRIS-COST-WDK6-PCB                                                
000487        PRIS-COST-WDK7-PCB                                                
000488        PRIS-COST-WDF1-PCB                                                
000489        PRIS-COST-9305-PCB                                                
000490        PRIS-COST-WDK72-PCB                                               
000491        PRIS-COST-WDB6-PCB                                                
000492        PRNO-3107-PCB                                                     
000493        PRQU-WDG2-PCB                                                     
000494        PRQU-WDC7-PCB                                                     
000495        PRQU-SJKO-WDK6-PCB                                                
000496        AREG-WDK6-PCB                                                     
000497        AREG-WDK7-PCB                                                     
000498        ARTM-ARTM-PCB                                                     
000499        DLEV-LEVF-PCB                                                     
000500        DLEV-LEVG-PCB                                                     
000501        DLEV-LEVA-PCB                                                     
000502        DLEV-ARTS-PCB                                                     
000503        DLEV-WDB6-PCB                                                     
000504        SPAR-WDF8-PCB                                                     
000505        SPAR-WDF8A-PCB                                                    
000506        SPAR-WDK6-PCB                                                     
000507        KERS-ARTC-PCB KERS-ERSA-PCB                                       
000508        CDCA-ARTM-PCB CDCA-INLB-PCB CDCA-WDB2-PCB CDCA-WDC1-PCB           
000509        KVAN-WDB2-PCB KVAN-WDC1-PCB.                                      
000510                                                                          
000511     ENTRY 'DLITCBL' USING MSG-PCB DISP-PCB 4263-PCB 4263V-PCB            
000512        ORQM-PCB PROC-PCB PROD-PCB ARTM-PCB WDB2-PCB WDB1-PCB             
000513        PRIS-ARTC-PCB PRIS-WDK7-PCB                                       
000514        PRIS-GMTA-PCB PRIS-BETA-PCB                                       
000515        PRIS-GPRIA-PCB PRIS-GPRIB-PCB                                     
000516        PRIS-COST-WDK6-PCB                                                
000517        PRIS-COST-WDK7-PCB                                                
000518        PRIS-COST-WDF1-PCB                                                
000519        PRIS-COST-9305-PCB                                                
000520        PRIS-COST-WDK72-PCB                                               
000521        PRIS-COST-WDB6-PCB                                                
000522        PRNO-3107-PCB                                                     
000523        PRQU-WDG2-PCB                                                     
000524        PRQU-WDC7-PCB                                                     
000525        PRQU-SJKO-WDK6-PCB                                                
000526        AREG-WDK6-PCB                                                     
000527        AREG-WDK7-PCB                                                     
000528        ARTM-ARTM-PCB                                                     
000529        DLEV-LEVF-PCB                                                     
000530        DLEV-LEVF-PCB                                                     
000531        DLEV-LEVG-PCB                                                     
000532        DLEV-LEVA-PCB                                                     
000533        DLEV-WDB6-PCB                                                     
000534        SPAR-WDF8-PCB                                                     
000535        SPAR-WDF8A-PCB                                                    
000536        SPAR-WDK6-PCB                                                     
000537        KERS-ARTC-PCB KERS-ERSA-PCB                                       
000538        CDCA-ARTM-PCB CDCA-INLB-PCB CDCA-WDB2-PCB CDCA-WDC1-PCB           
000539        KVAN-WDB2-PCB KVAN-WDC1-PCB.                                      
000540                                                                          
000541     EJECT                                                                
000542                                                                          
000543     PERFORM IMS-GET-MSG                                                  
000544     IF SEGMENT-FINNS                                                     
000545        PERFORM IMS-GN-MSG                                                
000546        IF SEGMENT-FINNS                                                  
000547           MOVE JA TO STARTAD-AV-DISPATCHEN-SW                            
000548        END-IF                                                            
000549        PERFORM A-INIT                                                    
000550        PERFORM B-KOLLA-NYCKLAR                                           
000551        IF ALLT-OK                                                        
000552           PERFORM C-KOLLA-ATT-ORDER-FINNS                                
000553           IF ALLT-OK                                                     
000554              PERFORM D-FORMELL-KONTROLL                                  
000555              IF ALLT-OK                                                  
000556                PERFORM E-BEHANDLA-RADER                                  
000557                PERFORM F-UPPDATERA-PHUV                                  
000558              END-IF                                                      
000559           END-IF                                                         
000560        END-IF                                                            
000561        IF ALLT-OK AND NOT STARTAD-AV-DISPATCHEN                          
000562          IF MID-IDARTNR-006(WS-INDEX-MID-MAX) = ALL '+'                  
000563          OR SVARSBILD                                                    
000564            IF DIST79-DEALER-PRICE AND WS-IDPRQUES NOT = +0               
000565               PERFORM I-SKICKA-PRISFRAGA                                 
000566            END-IF                                                        
000567            PERFORM G-HOPPA-TILL-SVARSBILD                                
000568          ELSE                                                            
000569            PERFORM H-VISA-TOM-SIDA                                       
000570          END-IF                                                          
000571        END-IF                                                            
000572                                                                          
000573        IF HOPP-TILL-4263 = NEJ                                           
000574          PERFORM Z-FINIT-INSERT-MSG                                      
000575        END-IF                                                            
000576     END-IF                                                               
000577     MOVE +0 TO RETURN-CODE                                               
000578     GOBACK                                                               
000579     .                                                                    
000580     EJECT                                                                
000581 A-INIT SECTION.                                                          
000582                                                                          
000583     ACCEPT DAGENS-DATUM       FROM DATE                                  
000584     ACCEPT DAGENS-TID         FROM TIME                                  
000585     MOVE SPACE                TO MED-IDMFSFEL                            
000586                                                                          
000587     IF MSG-DUBBLA-TRANSKODER                                             
000588       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I26201-CTX             
000589       MOVE MSG-IDTRANS-2      TO MFS-IDTRANS                             
000590       MOVE MSG-KDMFSFOR-2     TO MFS-KDMFSFOR                            
000591     ELSE                                                                 
000592       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I26201-CTX              
000593       MOVE MSG-IDTRANS-1      TO MFS-IDTRANS                             
000594       MOVE MSG-KDMFSFOR-1     TO MFS-KDMFSFOR                            
000595     END-IF                                                               
000596     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
000597     MOVE MSG-IDPFK            TO MFS-IDPFK                               
000598     MOVE MFS-IDTRANS          TO W-IDTRANS                               
000599     IF W-IDTRANS = '4263' AND MSG-KDTRANS-1 = 'W4T262U '                 
000600        MOVE JA TO SVARSBILD-SW                                           
000601     END-IF                                                               
000602     MOVE LOW-VALUE            TO MSG-AREA                                
000603     MOVE 'W4O26201'           TO MFS-IDMOD                               
000604     MOVE '4262'               TO MOD-IDTRANS                             
000605     MOVE MFS-RENSA-FAELT      TO MOD-TEMFSFEL                            
000606                                  MOD-TEMFSINF                            
000607     IF NOT EGEN-MID AND NOT SVARSBILD                                    
000608       MOVE SPACE              TO MFS-KDTRTYP                             
000609       MOVE '7'                TO MFS-IDPFK                               
000610     END-IF                                                               
000611     IF ENGLISH-TEXT                                                      
000612       MOVE +2                 TO SPRAK-IX                                
000613       MOVE 'GB '              TO MED-IDSKYLT                             
000614     ELSE                                                                 
000615       MOVE +1                 TO SPRAK-IX                                
000616       MOVE 'S  '              TO MED-IDSKYLT                             
000617     END-IF                                                               
000618                                                                          
000619     MOVE  LOW-VALUE         TO W-IDGMT-MIN-X                             
000620                                                                          
000621     MOVE HIGH-VALUE         TO W-IDGMT-MAX-X                             
000622                                                                          
000623     .                                                                    
000624     EJECT                                                                
000625 B-KOLLA-NYCKLAR SECTION.                                                 
000626                                                                          
000627     MOVE MFS-RENSA-FAELT      TO MOD-IDDISTR-IN                          
000628                                  MOD-IDKUNDNR-IN                         
000629                                  MOD-IDORDNR-IN                          
000630                                  MOD-IDARTNR-IN                          
000631                                                                          
000632     IF MID-IDDISTR-IN NOT = ALL '+'                                      
000633       MOVE MID-IDDISTR-IN    TO WS-IDDISTR                               
000634       MOVE SPACE             TO MFS-KDTRTYP                              
000635       MOVE '7'               TO MFS-IDPFK                                
000636     ELSE                                                                 
000637       MOVE MID-IDDISTR-UT     TO WS-IDDISTR                              
000638       INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO                 
000639     END-IF                                                               
000640                                                                          
000641     IF MID-IDKUNDNR-IN NOT = ALL '+'                                     
000642       MOVE MID-IDKUNDNR-IN   TO WS-IDKUNDNR                              
000643       MOVE SPACE             TO MFS-KDTRTYP                              
000644       MOVE '7'               TO MFS-IDPFK                                
000645     ELSE                                                                 
000646       MOVE MID-IDKUNDNR-UT   TO WS-IDKUNDNR                              
000647       INSPECT WS-IDKUNDNR REPLACING LEADING SPACE BY ZERO                
000648     END-IF                                                               
000649                                                                          
000650     IF MID-IDORDNR-IN NOT = ALL '+'                                      
000651       MOVE MID-IDORDNR-IN    TO WS-IDORDNR                               
000652       MOVE SPACE             TO MFS-KDTRTYP                              
000653       MOVE '7'               TO MFS-IDPFK                                
000654     ELSE                                                                 
000655       MOVE MID-IDORDNR-UT    TO WS-IDORDNR                               
000656       INSPECT WS-IDORDNR REPLACING LEADING SPACE BY ZERO                 
000657     END-IF                                                               
000658                                                                          
000659     IF WS-IDDISTR NUMERIC AND WS-IDDISTR > ZERO                          
000660       MOVE WS-IDDISTR           TO W-IDDISTR                             
000661                                    W-IDDISTR-WDB2                        
000662     ELSE                                                                 
000663       MOVE NEJ                  TO ALLT-SW                               
000664       MOVE ZERO                 TO WS-IDDISTR                            
000665     END-IF                                                               
000666                                                                          
000667     MOVE WS-IDDISTR                TO TEST-IDDISTR                       
000668     IF DIST79-DEALER-PRICE                                               
000669       IF ENGLISH-TEXT                                                    
000670         MOVE 'DEALERPRICE'         TO MOD-TEDDI                          
000671       ELSE                                                               
000672         MOVE '    ÅF PRIS'         TO MOD-TEDDI                          
000673       END-IF                                                             
000674     ELSE                                                                 
000675       MOVE SPACE                   TO MOD-TEDDI                          
000680     END-IF                                                               
000681                                                                          
000682     IF WS-IDKUNDNR NUMERIC                                               
000683       MOVE WS-IDKUNDNR          TO W-IDKUNDNR                            
000684                                    W-IDKUNDNR-WDB2                       
000685     ELSE                                                                 
000686       MOVE NEJ                  TO ALLT-SW                               
000687     END-IF                                                               
000688                                                                          
000689     IF WS-IDORDNR NUMERIC AND WS-IDORDNR > ZERO                          
000690       MOVE WS-IDORDNR           TO W-IDKUNDRF                            
000691     ELSE                                                                 
000692       MOVE NEJ                  TO ALLT-SW                               
000693     END-IF                                                               
000694                                                                          
000695     IF NOT ALLT-OK                                                       
000696       MOVE ERR-WRONG-KEY        TO MED-IDMFSFEL                          
000697       IF SVARSBILD                                                       
000698         MOVE 'FEL NYCKLAR FÅR EJ INTRÄFFA VID START FRÅN 4263'           
000699                                 TO FELTEXT                               
000700         CALL ABEND USING RKOD-ABEND                                      
000701       END-IF                                                             
000702     END-IF                                                               
000703                                                                          
000704     IF GODK-MID OR ALLT-OK                                               
000705       MOVE WS-IDDISTR              TO MOD-IDDISTR-UT                     
000706       INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE             
000707                                                                          
000708       IF WS-IDKUNDNR = ZERO                                              
000709         MOVE '     0'              TO MOD-IDKUNDNR-UT                    
000710       ELSE                                                               
000711         MOVE WS-IDKUNDNR           TO MOD-IDKUNDNR-UT                    
000712         INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE          
000713       END-IF                                                             
000714                                                                          
000715       MOVE WS-IDORDNR              TO MOD-IDORDNR-UT                     
000716       INSPECT MOD-IDORDNR-UT REPLACING LEADING ZERO BY SPACE             
000717                                                                          
000718       IF W-IDTRANS = '4261' AND NOT ALLT-OK                              
000719         MOVE MFS-RENSA-FAELT      TO MOD-IDDISTR-UT                      
000720                                      MOD-IDKUNDNR-UT                     
000721                                      MOD-IDORDNR-UT                      
000722       END-IF                                                             
000723                                                                          
000724       PERFORM BA-HAMTA-KUND                                              
000725     ELSE                                                                 
000726       MOVE MFS-RENSA-FAELT         TO MOD-IDDISTR-UT                     
000727                                       MOD-IDKUNDNR-UT                    
000728                                       MOD-IDORDNR-UT                     
000729     END-IF                                                               
000730                                                                          
000731     .                                                                    
000732     EJECT                                                                
000733 BA-HAMTA-KUND SECTION.                                                   
000734     MOVE WS-IDDISTR                TO W-IDDISTR-WDB2                     
000735                                       W-IDDISTR-WDB2-MIN                 
000736                                       W-IDDISTR-WDB2-MAX                 
000737     MOVE WS-IDKUNDNR               TO W-IDKUNDNR-WDB2                    
000738                                       W-IDKUNDNR-WDB2-MIN                
000739                                       W-IDKUNDNR-WDB2-MAX                
000740     PERFORM IMS-GU-WDB201                                                
000741     IF SEGMENT-FINNS                                                     
000742        CONTINUE                                                          
000743     ELSE                                                                 
000744        PERFORM IMS-GET-WDB201                                            
000745     END-IF                                                               
000746     IF DIST79-DEALER-PRICE                                               
000747       MOVE GMT-IDFTG              TO W-WDB1-IDFTG                        
000748       MOVE GMT-IDPARTNR           TO W-WDB1-IDPARTNR                     
000749       PERFORM IMS-GU-WDB101                                              
000750       MOVE BET-KDVALISO           TO MOD-KDVALISO                        
000751     ELSE                                                                 
000752       MOVE 'SEK'                  TO MOD-KDVALISO                        
000753     END-IF                                                               
000754                                                                          
000755     .                                                                    
000756     EJECT                                                                
000757 C-KOLLA-ATT-ORDER-FINNS SECTION.                                         
000758                                                                          
000759     PERFORM IMS-GHU-PROC-WDE801                                          
000760                                                                          
000770     IF SEGMENT-FINNS                                                     
000771       IF PHUV-FLBORT = JA                                                
000772         MOVE ERR-ORDER-ANNULLERAD TO MED-IDMFSFEL                        
000773         MOVE NEJ                  TO ALLT-SW                             
000774       ELSE                                                               
000775         PERFORM CB-KOLLA-DATUM                                           
000776         IF ALLT-OK                                                       
000777           IF PHUV-IDUSER NOT = MSG-SIGNON-USERID AND                     
000778             PHUV-KDPROTYP = 'L'                                          
000779             MOVE ERR-OBEHORIG      TO MED-IDMFSFEL                       
000780             MOVE NEJ               TO ALLT-SW                            
000781             MOVE NEJ               TO NYCKEL-SW                          
000782           ELSE                                                           
000783             MOVE PHUV-KDORDKL      TO MOD-KDORDKL-UT                     
000784             MOVE PHUV-KDFRAKT      TO MOD-KDFRAKT-UT                     
000785             MOVE PHUV-KDPROTYP     TO MOD-KDPROTYP-UT                    
000786           END-IF                                                         
000787         END-IF                                                           
000788       END-IF                                                             
000789     ELSE                                                                 
000790       MOVE ERR-ORDER-SAKNAS        TO MED-IDMFSFEL                       
000791       MOVE NEJ                     TO ALLT-SW                            
000792     END-IF                                                               
000793                                                                          
000794     IF NOT STARTAD-AV-DISPATCHEN                                         
000795        IF MFS-FIRST AND ALLT-OK                                          
000796          MOVE MFS-ADD-SAETT-CURSOR TO MOD-IDARTNR-ATTR(1)                
000797          PERFORM MFS-RENSA-MOD-RADER                                     
000798          MOVE NEJ                 TO ALLT-SW                             
000799        END-IF                                                            
000800     END-IF                                                               
000801                                                                          
000802     .                                                                    
000803     EJECT                                                                
000804 CB-KOLLA-DATUM SECTION.                                                  
000805                                                                          
000806     MOVE PHUV-TIFORDAT TO WS-TIFORDAT                                    
000807     MOVE WS-TIFORDAT    TO TMP1-YYMMDD                                   
000808     MOVE DAGENS-DATUM   TO TMP2-YYMMDD                                   
000809     PERFORM WY2000P1                                                     
000810     IF TMP1-YYMMDD <= TMP2-YYMMDD OR PHUV-TIORDDAT NOT = ZERO            
000811       MOVE ERR-ORDER-AVSLUTAD TO MED-IDMFSFEL                            
000812       MOVE NEJ TO ALLT-SW                                                
000813     END-IF                                                               
000814                                                                          
000815     .                                                                    
000816     EJECT                                                                
000817 D-FORMELL-KONTROLL SECTION.                                              
000818                                                                          
000819     MOVE 'PROF'                     TO ORFK-IDSYSTEM                     
000820     MOVE SPACE                      TO ORFK-IDDC                         
000821     MOVE PHUV-IDDISTR               TO ORFK-IDDISTR                      
000822     MOVE PHUV-IDKUNDNR              TO ORFK-IDKUNDNR                     
000823     MOVE PHUV-IDUSER                TO ORFK-IDUSER                       
000824     MOVE PHUV-KDFAKTYP              TO ORFK-KDFAKTYP                     
000825     MOVE PHUV-KDORDKL               TO ORFK-KDORDKL                      
000826     MOVE +0                         TO ORFK-KDTPOTYP                     
000827     MOVE NEJ                        TO ORFK-FLFORBI                      
000828     MOVE NEJ                        TO ORFK-FLEMBORD                     
000829     MOVE NEJ                        TO ORFK-FLORDSPE                     
000830     MOVE NEJ                        TO ORFK-FLOVRLEV                     
000831     MOVE PHUV-IDFTG                 TO ORFK-IDFTG                        
000832     MOVE +1                   TO WS-INDEX-MID                            
000833                                                                          
000834     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX                        
000835       IF MID-FLINVEST(WS-INDEX-MID) =  SPACE                             
000836          MOVE ALL '+'      TO MID-FLINVEST(WS-INDEX-MID)                 
000837       END-IF                                                             
000838       IF MID-FLINVEST(WS-INDEX-MID) = ALL '+'                            
000839         MOVE NEJ           TO ORFK-FLINVEST(WS-INDEX-MID)                
000840       ELSE                                                               
000841         IF MID-FLINVEST(WS-INDEX-MID) = 'Y'                              
000842           MOVE JA          TO MID-FLINVEST(WS-INDEX-MID)                 
000843         END-IF                                                           
000844         MOVE MID-FLINVEST(WS-INDEX-MID)                                  
000845                            TO ORFK-FLINVEST(WS-INDEX-MID)                
000846       END-IF                                                             
000847                                                                          
000848       MOVE PHUV-FLRESTN    TO ORFK-FLRESTN(WS-INDEX-MID)                 
000849                                                                          
000850       MOVE NEJ             TO ORFK-FLSLATT(WS-INDEX-MID)                 
000851                                                                          
000852       IF MID-IDARTNR-006(WS-INDEX-MID) = SPACE                           
000853          MOVE ALL '+'      TO ORFK-IDARTNR-IN(WS-INDEX-MID)              
000854                               MID-IDARTNR-006(WS-INDEX-MID)              
000855       ELSE                                                               
000856          MOVE MID-IDARTNR-006(WS-INDEX-MID)                              
000857                            TO ORFK-IDARTNR-IN(WS-INDEX-MID)              
000858       END-IF                                                             
000859                                                                          
000860       IF MID-KDKVBRYT(WS-INDEX-MID) = SPACE                              
000861          MOVE ALL '+'       TO ORFK-KDKVBRYT(WS-INDEX-MID)               
000862                                MID-KDKVBRYT(WS-INDEX-MID)                
000863       ELSE                                                               
000864          MOVE MID-KDKVBRYT(WS-INDEX-MID)                                 
000865                             TO ORFK-KDKVBRYT(WS-INDEX-MID)               
000866       END-IF                                                             
000867                                                                          
000868       MOVE +2               TO ORFK-KDVRINFO(WS-INDEX-MID)               
000869                                                                          
000870       IF MID-KVBEART(WS-INDEX-MID) = SPACE                               
000871          MOVE ALL '+'       TO ORFK-KVBEART(WS-INDEX-MID)                
000872                                MID-KVBEART(WS-INDEX-MID)                 
000873       ELSE                                                               
000874          MOVE MID-KVBEART(WS-INDEX-MID)                                  
000875                             TO ORFK-KVBEART(WS-INDEX-MID)                
000876       END-IF                                                             
000877                                                                          
000878       IF DIST79-DEALER-PRICE                                             
000879          IF MID-PRARTNTO(WS-INDEX-MID) = SPACE                           
000880             MOVE ALL '+'     TO MID-PRARTNTO(WS-INDEX-MID)               
000881          END-IF                                                          
000882          MOVE MID-PRARTNTO(WS-INDEX-MID)                                 
000883                              TO ORFK-PRARTNTO-LOC(WS-INDEX-MID)          
000884          MOVE ALL '+'        TO ORFK-PRARTNTO(WS-INDEX-MID)              
000885          MOVE ALL '+'        TO                                          
000886                         ORFK-PRARTNTO-LOCPREL(WS-INDEX-MID)              
000887       ELSE                                                               
000888          IF MID-PRARTNTO(WS-INDEX-MID) = SPACE                           
000889             MOVE ALL '+'     TO ORFK-PRARTNTO(WS-INDEX-MID)              
000890                                 MID-PRARTNTO(WS-INDEX-MID)               
000891          ELSE                                                            
000892             MOVE MID-PRARTNTO(WS-INDEX-MID)                              
000893                              TO ORFK-PRARTNTO(WS-INDEX-MID)              
000894          END-IF                                                          
000895          MOVE ALL '+'        TO ORFK-PRARTNTO-LOC(WS-INDEX-MID)          
000896          MOVE ALL '+'        TO                                          
000897                         ORFK-PRARTNTO-LOCPREL(WS-INDEX-MID)              
000898       END-IF                                                             
000899                                                                          
000900       MOVE '++++++'         TO ORFK-TITPO-RAD(WS-INDEX-MID)              
000901       MOVE ALL '+'           TO ORFK-PRARTBTO-LOC(WS-INDEX-MID)          
000902                                                                          
000903       ADD +1                TO WS-INDEX-MID                              
000904     END-PERFORM                                                          
000905                                                                          
000906     CALL W411ORFK USING ORFK-W411ORFK                                    
000907                         AREG-WDK6-PCB                                    
000908                         AREG-WDK7-PCB                                    
000909                                                                          
000910     MOVE +1                   TO WS-INDEX-MID                            
000911     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX                        
000912       PERFORM DA-KOLLA-FEL-FK                                            
000913       ADD +1                 TO WS-INDEX-MID                             
000914     END-PERFORM                                                          
000915     IF SVARSBILD AND NOT ALLT-OK                                         
000916       MOVE 'FORMELLA FEL FÅR EJ INTRÄFFA VID START FRÅN 4213'            
000917                               TO FELTEXT                                 
000918       CALL ABEND USING RKOD-ABEND                                        
000919     END-IF                                                               
000920     .                                                                    
000921     EJECT                                                                
000922 DA-KOLLA-FEL-FK SECTION.                                                 
000923                                                                          
000924     IF ORFK-FLINVEST-OK(WS-INDEX-MID) = NEJ                              
000925       MOVE ERR-UPPLYSTA-FEL   TO MED-IDMFSFEL                            
000926       MOVE MFS-ALFA-FAELT-FEL TO MOD-FLINVEST-ATTR(WS-INDEX-MID)         
000927       MOVE NEJ                 TO ALLT-SW                                
000928     END-IF                                                               
000929                                                                          
000930     IF ORFK-IDARTNR-OK(WS-INDEX-MID) = NEJ                               
000931       MOVE ERR-UPPLYSTA-FEL    TO MED-IDMFSFEL                           
000932       MOVE MFS-NUM-FAELT-FEL   TO MOD-IDARTNR-ATTR(WS-INDEX-MID)         
000933       MOVE NEJ                 TO ALLT-SW                                
000934     ELSE                                                                 
000935       IF (ORFK-KDORDBEK(WS-INDEX-MID) = 58 OR 59)                        
000936                 AND NOT MFS-UPDATE                                       
000937         MOVE ERR-IDARTNR-SAKNAS TO MED-IDMFSFEL                          
000938         MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-ATTR(WS-INDEX-MID)         
000939         MOVE NEJ               TO ALLT-SW                                
000940       END-IF                                                             
000941     END-IF                                                               
000942                                                                          
000943     IF ORFK-KDKVBRYT-OK(WS-INDEX-MID) = NEJ                              
000944       MOVE ERR-UPPLYSTA-FEL    TO MED-IDMFSFEL                           
000945       MOVE MFS-NUM-FAELT-FEL TO MOD-KDKVBRYT-ATTR(WS-INDEX-MID)          
000946       MOVE NEJ                 TO ALLT-SW                                
000947     END-IF                                                               
000948                                                                          
000949     IF ORFK-KVBEART-OK(WS-INDEX-MID) = NEJ                               
000950       IF MED-IDMFSFEL = SPACE                                            
000951         IF ORFK-KVBEART(WS-INDEX-MID) NUMERIC AND                        
000952                 ORFK-KVBEART(WS-INDEX-MID) > ZERO                        
000953           IF NOT MFS-UPDATE                                              
000954             MOVE ERR-STORT-UTTAG TO MED-IDMFSFEL                         
000955             MOVE MFS-NUM-FAELT-FEL   TO                                  
000956                             MOD-KVBEART-ATTR(WS-INDEX-MID)               
000957             MOVE NEJ             TO ALLT-SW                              
000958           END-IF                                                         
000959         ELSE                                                             
000960           MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                          
000961           MOVE MFS-NUM-FAELT-FEL   TO                                    
000962                                MOD-KVBEART-ATTR(WS-INDEX-MID)            
000963           MOVE NEJ                 TO ALLT-SW                            
000964         END-IF                                                           
000965       ELSE                                                               
000966         MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                            
000967         MOVE MFS-NUM-FAELT-FEL   TO                                      
000968                               MOD-KVBEART-ATTR(WS-INDEX-MID)             
000969         MOVE NEJ                 TO ALLT-SW                              
000970       END-IF                                                             
000971     END-IF                                                               
000972                                                                          
000973     IF ORFK-PRARTNTO-OK(WS-INDEX-MID) = NEJ                              
000974       MOVE ERR-UPPLYSTA-FEL    TO MED-IDMFSFEL                           
000975       MOVE MFS-NUM-FAELT-FEL TO MOD-PRARTNTO-ATTR(WS-INDEX-MID)          
000976       MOVE NEJ                 TO ALLT-SW                                
000977     END-IF                                                               
000978                                                                          
000979     IF ORFK-PRARTNTO-LOC-OK(WS-INDEX-MID) = NEJ                          
000980        MOVE ERR-UPPLYSTA-FEL    TO MED-IDMFSFEL                          
000981        MOVE MFS-NUM-FAELT-FEL TO MOD-PRARTNTO-ATTR(WS-INDEX-MID)         
000982        MOVE NEJ                 TO ALLT-SW                               
000983     END-IF                                                               
000984                                                                          
000985     IF ORFK-PRARTNTO-LOCPREL-OK(WS-INDEX-MID) = NEJ                      
000986        MOVE ERR-UPPLYSTA-FEL    TO MED-IDMFSFEL                          
000987        MOVE MFS-NUM-FAELT-FEL TO MOD-PRARTNTO-ATTR(WS-INDEX-MID)         
000988        MOVE NEJ                 TO ALLT-SW                               
000989     END-IF                                                               
000990     .                                                                    
000991     EJECT                                                                
000992 E-BEHANDLA-RADER SECTION.                                                
000993                                                                          
000994     PERFORM EA-LAES-KUNDREGISTER                                         
000995     MOVE +1 TO WS-INDEX-MID                                              
000996     MOVE NEJ                     TO TILLK-SW                             
000997                                     OBKR-SW                              
000998     MOVE +0                      TO WS-IDPRQUES                          
000999                                                                          
001000     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX                        
001001       IF MID-IDARTNR-006(WS-INDEX-MID) NOT = ALL '+'                     
001002         PERFORM S01-RENSA-TILLK-TAB                                      
001003         MOVE ORFK-W411AREG-001(WS-INDEX-MID)                             
001004                               TO AREG-W411AREG-001                       
001005         PERFORM EC-BEHANDLA-RAD                                          
001006         MOVE JA               TO TILLK-SW                                
001007         MOVE +1               TO WS-INDEX-TILLK                          
001008         PERFORM UNTIL WS-INDEX-TILLK > WS-INDEX-TILLK-MAX OR             
001009           TILK-IDARTNR-TILLK(WS-INDEX-TILLK) = ZERO                      
001010           IF TILK-FLTILLK-X(WS-INDEX-TILLK) = JA                         
001011             PERFORM ED-LAES-TILLK-DATA                                   
001012             PERFORM EC-BEHANDLA-RAD                                      
001013           END-IF                                                         
001014           ADD +1              TO WS-INDEX-TILLK                          
001015         END-PERFORM                                                      
001016       END-IF                                                             
001017       MOVE NEJ                TO TILLK-SW                                
001018                                  OBKR-SW                                 
001019       ADD +1 TO WS-INDEX-MID                                             
001020     END-PERFORM                                                          
001021                                                                          
001022     IF DIST79-DEALER-PRICE AND WS-IDPRQUES NOT = +0                      
001023       MOVE WS-IDPRQUES             TO PRNO-IDPRQUES-IN                   
001024       MOVE +3                      TO PRNO-KDCALL                        
001025                                                                          
001026       CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                    
001027     END-IF                                                               
001028                                                                          
001029     MOVE JA                      TO ALLT-SW                              
001030     .                                                                    
001031     EJECT                                                                
001032 EA-LAES-KUNDREGISTER SECTION.                                            
001033                                                                          
001034                                                                          
001035     PERFORM IMS-GU-WDB201                                                
001036*----FLPRERS  TILL W411KERS                                               
001037                                                                          
001038     .                                                                    
001039     EJECT                                                                
001040                                                                          
001041 EC-BEHANDLA-RAD SECTION.                                                 
001042                                                                          
001043     PERFORM ECA-NOLLSTALL-OBKR                                           
001044     PERFORM ECB-BYGG-UPP-ORDERRAD                                        
001045     PERFORM ECC-LAS-NYA-ARTIKELREG                                       
001046     PERFORM ECF-KOMPLETTERA-KVANT-BRYTES                                 
001047     PERFORM ECI-KOMPLETTERA-DIREKTLEVERANS                               
001048                                                                          
001049     IF NOT TILLKOMMANDE-RAD                                              
001050       PERFORM ECK-KOMPLETTERA-ERSATTNING                                 
001051     END-IF                                                               
001052                                                                          
001053     PERFORM ECL-KOMPLETTERA-SPARRAR                                      
001054     PERFORM ECJ-KOMPLETTERA-PRIS                                         
001055     PERFORM ECQ-KOMPLETTERA-STORA-UTTAG                                  
001056     PERFORM ECR-PREL-AVBOKNING                                           
001057                                                                          
001058     IF NOT TILLKOMMANDE-RAD                                              
001059       IF SKRIV-OBKR                                                      
001060         PERFORM ECS-SKRIV-OBKR                                           
001061       ELSE                                                               
001062         PERFORM ECV-SKRIV-PRAD                                           
001063       END-IF                                                             
001064     ELSE                                                                 
001065       IF SKRIV-OBKR                                                      
001066         PERFORM ECS-SKRIV-OBKR                                           
001067       END-IF                                                             
001068     END-IF                                                               
001069     .                                                                    
001070     EJECT                                                                
001071 ECA-NOLLSTALL-OBKR SECTION.                                              
001072                                                                          
001073     MOVE +0                   TO KVAN-KDORDBEK-UT                        
001074     MOVE +0                   TO DLEV-KDORDBEK-UT                        
001075     MOVE +0                   TO KERS-KDERS                              
001076     IF NOT TILLKOMMANDE-RAD                                              
001077       MOVE +0                 TO KERS-KDORDBEK                           
001078     ELSE                                                                 
001079       MOVE JA                 TO OBKR-SW                                 
001080     END-IF                                                               
001081     MOVE +0                   TO STOR-KDORDBEK                           
001082     MOVE +0                   TO CDCA-KDORDBEK-UT                        
001083     MOVE ZERO                 TO SPAR-KDORDBEK                           
001084     MOVE +0                   TO WS-KVPREAVB                             
001085                                                                          
001086     MOVE JA                   TO ALLT-SW                                 
001087                                                                          
001088     IF ORFK-KDORDBEK(WS-INDEX-MID) > 0                                   
001089       MOVE NEJ                TO ALLT-SW                                 
001090       MOVE JA                 TO OBKR-SW                                 
001091     END-IF                                                               
001092                                                                          
001093     .                                                                    
001094     EJECT                                                                
001095 ECB-BYGG-UPP-ORDERRAD SECTION.                                           
001096                                                                          
001097     MOVE PHUV-IDORDER         TO PRAD-IDORDER                            
001098     IF TILLKOMMANDE-RAD                                                  
001099       MOVE AREG-IDARTNR       TO PRAD-IDARTNR                            
001100     ELSE                                                                 
001101       MOVE ORFK-IDARTNR(WS-INDEX-MID)                                    
001102                               TO PRAD-IDARTNR                            
001103     END-IF                                                               
001104     MOVE +1                   TO PRAD-IDLOPNR                            
001105     MOVE SPACE                TO PRAD-BEART                              
001106                                                                          
001107     IF MID-BERADREF(WS-INDEX-MID) = ALL '+'                              
001108       MOVE SPACE              TO PRAD-BERADREF                           
001109     ELSE                                                                 
001110       MOVE MID-BERADREF(WS-INDEX-MID)                                    
001111                               TO PRAD-BERADREF                           
001112     END-IF                                                               
001113                                                                          
001114     IF MID-FLINVEST(WS-INDEX-MID) = ALL '+'                              
001115       MOVE NEJ                TO PRAD-FLINVEST                           
001116     ELSE                                                                 
001117       MOVE MID-FLINVEST(WS-INDEX-MID)                                    
001118                               TO PRAD-FLINVEST                           
001119     END-IF                                                               
001120     IF TILLKOMMANDE-RAD                                                  
001121      IF DIST79-DEALER-PRICE                                              
001122        MOVE NEJ               TO PRAD-FLPRTILL                           
001123      ELSE                                                                
001124       IF TILK-PRARTNTO(WS-INDEX-TILLK) > +0                              
001125         MOVE TILK-FLPRTILL(WS-INDEX-TILLK)                               
001126                               TO PRAD-FLPRTILL                           
001127       ELSE                                                               
001128         MOVE NEJ              TO PRAD-FLPRTILL                           
001129       END-IF                                                             
001130      END-IF                                                              
001131     ELSE                                                                 
001132       MOVE NEJ                TO PRAD-FLPRTILL                           
001133     END-IF                                                               
001134     MOVE NEJ                  TO PRAD-FLRESTN                            
001135     MOVE SPACE                TO PRAD-IDLEVNR                            
001136     MOVE ZERO                 TO PRAD-IDSPECEMB                          
001137     MOVE +0                   TO PRAD-KDARTURS                           
001138     MOVE PHUV-IDSYSTEM        TO PRAD-IDSYSTEM                           
001139     IF MID-KDKVBRYT(WS-INDEX-MID) = ALL '+'                              
001140       MOVE +1                 TO PRAD-KDKVBRYT                           
001141     ELSE                                                                 
001142       MOVE MID-KDKVBRYT(WS-INDEX-MID)                                    
001143                               TO PRAD-KDKVBRYT                           
001144     END-IF                                                               
001145     MOVE PHUV-KDORDING        TO PRAD-KDORDING                           
001146     MOVE AREG-KDPRODSL        TO PRAD-KDPRODSL                           
001147                                                                          
001148     IF TILLKOMMANDE-RAD                                                  
001149      IF DIST79-DEALER-PRICE                                              
001150        MOVE SPACE             TO PRAD-KDPRTYP                            
001151      ELSE                                                                
001152       IF TILK-PRARTNTO(WS-INDEX-TILLK) > +0                              
001153         MOVE TILK-KDPRTYP(WS-INDEX-TILLK)                                
001154                               TO PRAD-KDPRTYP                            
001155       ELSE                                                               
001156         MOVE SPACE            TO PRAD-KDPRTYP                            
001157       END-IF                                                             
001158      END-IF                                                              
001159     ELSE                                                                 
001160       MOVE SPACE              TO PRAD-KDPRTYP                            
001161     END-IF                                                               
001162     MOVE ZERO                 TO PRAD-KDSPEEMB                           
001163     MOVE +0                   TO PRAD-KDTPOTYP                           
001164                                  PRAD-KVVECKOR-TPO5                      
001165     IF TILLKOMMANDE-RAD                                                  
001166       MOVE TILK-KVBEART(WS-INDEX-TILLK)                                  
001167                               TO PRAD-KVBEART                            
001168     ELSE                                                                 
001169       MOVE ORFK-KVBEART(WS-INDEX-MID) TO WS-ALFA-6                       
001170       MOVE WS-NUM-6           TO PRAD-KVBEART                            
001171     END-IF                                                               
001172     MOVE +0                   TO PRAD-KVBEART-Q                          
001173     MOVE +0                   TO PRAD-IDPRQUES                           
001174     MOVE +0                   TO PRAD-PRARTBTO-LOC                       
001175     MOVE +0                   TO PRAD-RERAB                              
001176     MOVE SPACE                TO PRAD-KDVALISO                           
001177     MOVE SPACE                TO PRAD-KDVAT                              
001178     MOVE SPACE                TO PRAD-KDRAB                              
001179     MOVE SPACE                TO PRAD-BEART-VIPS                         
001180                                                                          
001181     IF TILLKOMMANDE-RAD                                                  
001182      IF DIST79-DEALER-PRICE                                              
001183         MOVE +0               TO PRAD-PRARTNTO                           
001184         MOVE +0               TO PRAD-PRARTNTO-LOC                       
001185         MOVE +0               TO PRAD-PRARTNTO-LOCPREL                   
001186      ELSE                                                                
001187       IF TILK-PRARTNTO(WS-INDEX-TILLK) > +0                              
001188           MOVE TILK-PRARTNTO(WS-INDEX-TILLK)                             
001189                               TO PRAD-PRARTNTO                           
001190           MOVE ZERO           TO PRAD-PRARTNTO-LOC                       
001191       ELSE                                                               
001192         MOVE +0               TO PRAD-PRARTNTO                           
001193         MOVE +0               TO PRAD-PRARTNTO-LOC                       
001194         MOVE +0               TO PRAD-PRARTNTO-LOCPREL                   
001195       END-IF                                                             
001196      END-IF                                                              
001197     ELSE                                                                 
001198       IF MID-PRARTNTO(WS-INDEX-MID) = ALL '+'                            
001199         MOVE +0               TO PRAD-PRARTNTO                           
001200                                  PRAD-PRARTNTO-LOC                       
001201                                  PRAD-PRARTNTO-LOCPREL                   
001202       ELSE                                                               
001203         IF DIST79-DEALER-PRICE                                           
001204           MOVE ORFK-PRARTNTO-LOC-UT(WS-INDEX-MID)                        
001205                               TO PRAD-PRARTNTO-LOC                       
001206           MOVE +0             TO PRAD-PRARTNTO-LOCPREL                   
001207           MOVE +0             TO PRAD-PRARTNTO                           
001208         ELSE                                                             
001209           MOVE ORFK-PRARTNTO-UT(WS-INDEX-MID)                            
001210                               TO PRAD-PRARTNTO                           
001211           MOVE +0             TO PRAD-PRARTNTO-LOC                       
001212           MOVE +0             TO PRAD-PRARTNTO-LOCPREL                   
001213         END-IF                                                           
001214       END-IF                                                             
001215       MOVE +0                 TO PRAD-PRBPRIS                            
001216     END-IF                                                               
001217     IF ORFK-KDORDBEK(WS-INDEX-MID) = 59                                  
001218       MOVE MID-IDARTNR-006(WS-INDEX-MID) (11:1)                          
001219                               TO PRAD-REKSIFFR                           
001220     ELSE                                                                 
001221       MOVE AREG-REKSIFFR      TO PRAD-REKSIFFR                           
001222     END-IF                                                               
001223     IF TILLKOMMANDE-RAD                                                  
001224      IF DIST79-DEALER-PRICE                                              
001225         MOVE +0               TO PRAD-TIPRIS                             
001226      ELSE                                                                
001227       IF TILK-PRARTNTO(WS-INDEX-TILLK) > +0                              
001228         MOVE TILK-TIPRIS(WS-INDEX-TILLK)                                 
001229                               TO PRAD-TIPRIS                             
001230       ELSE                                                               
001231         MOVE +0               TO PRAD-TIPRIS                             
001232       END-IF                                                             
001233      END-IF                                                              
001234     ELSE                                                                 
001235       MOVE +0                 TO PRAD-TIPRIS                             
001236     END-IF                                                               
001237                                                                          
001238     MOVE DAGENS-DATUM         TO PRAD-TIREGDAT                           
001239     MOVE DAGENS-HHMMSS        TO PRAD-TIREGTID                           
001240     MOVE AREG-VKART           TO PRAD-VKART                              
001241     MOVE AREG-VLARTNTO        TO PRAD-VLARTNTO                           
001242     MOVE AREG-KDSPEEMB        TO PRAD-KDSPEEMB                           
001243     .                                                                    
001244     EJECT                                                                
001245 ECC-LAS-NYA-ARTIKELREG SECTION.                                          
001246                                                                          
001247     IF ALLT-OK                                                           
001248       MOVE PRAD-IDARTNR        TO W-IDARTNR                              
001249       PERFORM IMS-GU-01-ARTM-WDK901                                      
001250       IF SEGMENT-SAKNAS                                                  
001251         MOVE W-IDARTNR         TO ARTM-IDARTNR-IN                        
001252         CALL W411ARTM USING ARTM-W411ARTM ARTM-ARTM-PCB                  
001253       END-IF                                                             
001254     END-IF                                                               
001255                                                                          
001256     .                                                                    
001257     EJECT                                                                
001258 ECF-KOMPLETTERA-KVANT-BRYTES SECTION.                                    
001259                                                                          
001260     IF ALLT-OK                                                           
001261                                                                          
001262       MOVE PRAD-KDKVBRYT        TO KVAN-KDKVBRYT-IN                      
001263       MOVE PRAD-IDSYSTEM        TO KVAN-IDSYSTEM-IN                      
001264       MOVE PRAD-KVBEART         TO KVAN-KVBEART-IN                       
001265       MOVE AREG-KVQPACK-0       TO KVAN-KVQPACK-0-IN                     
001266       MOVE AREG-KVQPACK-1       TO KVAN-KVQPACK-1-IN                     
001267       MOVE AREG-KDPRODSL        TO KVAN-KDPRODSL-IN                      
001268       MOVE AREG-KDSORT          TO KVAN-KDSORT-IN                        
001269       MOVE AREG-IDFKNGRP        TO KVAN-IDFKNGRP-IN                      
001270       MOVE PHUV-KDORDKL         TO KVAN-KDORDKL-IN                       
001271       MOVE NEJ                  TO KVAN-FLEMBORD-IN                      
001272       MOVE NEJ                  TO KVAN-FLFORBI-IN                       
001273       MOVE NEJ                  TO KVAN-FLORDSPE-IN                      
001274       MOVE NEJ                  TO KVAN-FLOVRLEV-IN                      
001275       MOVE +0                   TO KVAN-IDKAMPRF-IN                      
001276       MOVE SPACE                TO KVAN-IDDC-IN                          
001277       MOVE PHUV-IDDISTR         TO KVAN-IDDISTR-IN                       
001278       MOVE PHUV-IDKUNDNR        TO KVAN-IDKUNDNR-IN                      
001279       MOVE PRAD-BERADREF        TO KVAN-BERADREF-IN                      
001280       MOVE PRAD-IDARTNR         TO KVAN-IDARTNR-IN                       
001281                                                                          
001282       CALL W411KVAN USING KVAN-W411KVAN KVAN-WDB2-PCB                    
001283                                         KVAN-WDC1-PCB                    
001284                                                                          
001285       MOVE KVAN-KDKVBRYT-UT         TO PRAD-KDKVBRYT                     
001286       MOVE KVAN-KVBEART-Q-UT        TO PRAD-KVBEART-Q                    
001287                                                                          
001288       IF KVAN-KDORDBEK-UT > +0                                           
001289         MOVE JA                     TO OBKR-SW                           
001290       END-IF                                                             
001291                                                                          
001292     END-IF                                                               
001293     .                                                                    
001294     EJECT                                                                
001295 ECI-KOMPLETTERA-DIREKTLEVERANS SECTION.                                  
001296                                                                          
001297     IF ALLT-OK                                                           
001298                                                                          
001299       MOVE WC-CDC-SE            TO DLEV-IDDC-IN                          
001300       MOVE WC-CDC-SE            TO DLEV-IDDC-ORD-IN                      
001301       MOVE PHUV-IDDISTR         TO DLEV-IDDISTR-IN                       
001302       MOVE PHUV-IDKUNDNR        TO DLEV-IDKUNDNR-IN                      
001303       MOVE PHUV-KDORDKL         TO DLEV-KDORDKL-IN                       
001304       MOVE PRAD-IDARTNR         TO DLEV-IDARTNR-IN                       
001305       MOVE PRAD-IDLEVNR         TO DLEV-IDLEVNR-IN                       
001306       MOVE PRAD-KVBEART-Q       TO DLEV-KVBEART-Q-IN                     
001307       MOVE AREG-REDIRLEV        TO DLEV-REDIRLEV-IN                      
001308       MOVE +0                   TO DLEV-IDKAMPRF-IN                      
001309       MOVE PRAD-KDTPOTYP        TO DLEV-KDTPOTYP-IN                      
001310       MOVE AREG-KDUART          TO DLEV-KDUART-IN                        
001311       MOVE NEJ                  TO DLEV-FLFORBI-IN                       
001312       MOVE AREG-FLREFILL        TO DLEV-FLREFILL-IN                      
001313       MOVE PRAD-KDORDING        TO DLEV-KDORDING-IN                      
001314       MOVE SPACE                TO DLEV-CLEARGROUP                       
001315                                    DLEV-KDOI-UT                          
001316       MOVE +1 TO WS-INDEX                                                
001317       PERFORM UNTIL WS-INDEX  > IX-DCCLEAR-MAX                           
001318          MOVE SPACE             TO DLEV-IDDC-CLEAR-IN(WS-INDEX)          
001319          ADD +1 TO WS-INDEX                                              
001320       END-PERFORM                                                        
001321                                                                          
001328       MOVE ZERO                 TO DLEV-KDCALL                           
001329                                    DLEV-IDKUNDRF-IN                      
001330                                                                          
001331       CALL W411DLEV USING DLEV-W411DLEV DLEV-LEVF-PCB                    
001332                                         DLEV-LEVG-PCB                    
001333                                         DLEV-LEVA-PCB                    
001334                                         DLEV-ARTS-PCB                    
001335                                         DLEV-WDB6-PCB                    
001336                                         DUMMY-PCB                        
001337       IF DLEV-KDORDBEK-UT = 21 OR 53 OR 82                               
001338         MOVE JA                  TO OBKR-SW                              
001339         MOVE NEJ                 TO ALLT-SW                              
001340       ELSE                                                               
001341         IF DLEV-KDORDBEK-UT = 95                                         
001342           MOVE JA                TO OBKR-SW                              
001343         END-IF                                                           
001344         MOVE DLEV-IDLEVNR-UT     TO PRAD-IDLEVNR                         
001345       END-IF                                                             
001346       MOVE AREG-KDARTURS         TO PRAD-KDARTURS                        
001347                                                                          
001348       IF DLEV-KDORDBEK-UT = 0 AND AREG-ADLAGOMR = 79                     
001349          MOVE PHUV-IDDISTR       TO TEST-IDDISTR                         
001350          IF DIST18-SKROT                                                 
001351            CONTINUE                                                      
001352          ELSE                                                            
001353            MOVE 26               TO DLEV-KDORDBEK-UT                     
001354            MOVE JA               TO OBKR-SW                              
001355          END-IF                                                          
001356       END-IF                                                             
001357                                                                          
001358     END-IF                                                               
001359     .                                                                    
001360     EJECT                                                                
001361 ECK-KOMPLETTERA-ERSATTNING SECTION.                                      
001362                                                                          
001363     IF ALLT-OK                                                           
001364                                                                          
001365       MOVE PRAD-IDARTNR         TO KERS-IDARTNR                          
001366       MOVE WC-CDC-SE            TO KERS-IDDC                             
001367       MOVE GMT-FLPRERS          TO KERS-FLPRERS                          
001368       MOVE PRAD-FLPRTILL        TO KERS-FLPRTILL                         
001369       MOVE NEJ                  TO KERS-FLFORBI                          
001370       MOVE NEJ                  TO KERS-FLORDSPE                         
001371       MOVE NEJ                  TO KERS-FLOVRLEV                         
001372       MOVE +0                   TO KERS-IDKAMPRF                         
001373       MOVE AREG-KDERS           TO KERS-KDERS                            
001374       MOVE AREG-KDERS-UTG       TO KERS-KDERS-UTG                        
001375       MOVE PRAD-KDPRTYP         TO KERS-KDPRTYP                          
001376       MOVE PRAD-KDTPOTYP        TO KERS-KDTPOTYP                         
001377       MOVE AREG-KDUART          TO KERS-KDUART                           
001378       MOVE PRAD-KVBEART         TO KERS-KVBEART                          
001379       MOVE PRAD-PRARTNTO        TO KERS-PRARTNTO                         
001380       MOVE PRAD-DEAL-PR-LINE    TO KERS-DEAL-PR-LINE                     
001381       MOVE PRAD-TIPRIS          TO KERS-TIPRIS                           
001382       MOVE ZERO                 TO WX-KDORDBEK                           
001383                                                                          
001384       CALL W411KERS USING KERS-W411KERS TILK-W411TILK                    
001385                           KERS-ARTC-PCB KERS-ERSA-PCB                    
001386                           DLEV-ARTS-PCB CDCA-ARTM-PCB                    
001387                                                                          
001388       IF KERS-KDORDBEK > ZERO   AND                                      
001389          ((KERS-KDERS-UTG > +20 AND KERS-KDERS = +0)  OR                 
001390            KERS-KDERS > 10 )                                             
001391          MOVE JA                  TO OBKR-SW                             
001392          MOVE NEJ                 TO ALLT-SW                             
001393       END-IF                                                             
001394       IF KERS-KDORDBEK > +0 AND KVAN-KDORDBEK-UT > +0                    
001395         IF KERS-KDERS = 01 AND (AREG-KDSORT = 'L' OR 'M')                
001396***        CHECK FOR 'KERS-KDERS = 01' AND KDSROT IS 'L' OR 'M' SO        
001397***        THAT QUANTITY ADAPTION DO HAPPEN EVEN WHEN THE                 
001398***        SUPERSESSION CODE IS EQUAL TO 1 AND HENCE Q1 QUANTITY          
001399***        IS NOT BROKEN FURTHER. MIN QTY ORDERED WILL BECOME Q1          
001400           CONTINUE                                                       
001401         ELSE                                                             
001402           MOVE +0                 TO KVAN-KDORDBEK-UT                    
001403           MOVE PRAD-KVBEART       TO PRAD-KVBEART-Q                      
001404         END-IF                                                           
001405       END-IF                                                             
001406       IF KERS-KDORDBEK > +0 AND DLEV-KDORDBEK-UT > +0                    
001407         MOVE +0                  TO DLEV-KDORDBEK-UT                     
001408       END-IF                                                             
001409     ELSE                                                                 
001410       MOVE +0                  TO KERS-KDERS                             
001411     END-IF                                                               
001412     .                                                                    
001413     EJECT                                                                
001414 ECL-KOMPLETTERA-SPARRAR SECTION.                                         
001415                                                                          
001416     IF ALLT-OK                                                           
001417                                                                          
001418       MOVE PRAD-BERADREF        TO SPAR-BERADREF                         
001419       MOVE PHUV-BEKUNDRF        TO SPAR-BEKUNDRF                         
001420       MOVE AREG-FLAVRART        TO SPAR-FLAVRART                         
001421       MOVE NEJ                  TO SPAR-FLEMBORD                         
001422       MOVE NEJ                  TO SPAR-FLFORBI                          
001423       MOVE AREG-FLLSRDEL        TO SPAR-FLLSRDEL                         
001424       MOVE NEJ                  TO SPAR-FLORDSPE                         
001425       MOVE NEJ                  TO SPAR-FLOVRLEV                         
001426       MOVE AREG-FLRADREF        TO SPAR-FLRADREF                         
001427       MOVE NEJ                  TO SPAR-FLRESTN                          
001428       MOVE PRAD-IDARTNR         TO SPAR-IDARTNR                          
001429       MOVE AREG-FLIART          TO SPAR-FLIART                           
001430       MOVE AREG-FLMARKSP        TO SPAR-FLMARKSP                         
001431       MOVE PHUV-IDDISTR         TO SPAR-IDDISTR                          
001432       MOVE PHUV-IDKUNDNR        TO SPAR-IDKUNDNR                         
001433       MOVE WC-CDC-SE            TO SPAR-IDDC                             
001434       MOVE PRAD-IDSYSTEM        TO SPAR-IDSYSTEM                         
001435       MOVE AREG-KDERS-UTG       TO SPAR-KDERS-UTG                        
001436       MOVE AREG-KDERS           TO SPAR-KDERS                            
001437       MOVE PHUV-KDFAKTYP        TO SPAR-KDFAKTYP                         
001438       MOVE AREG-KDLEVSP         TO SPAR-KDLEVSP                          
001439       MOVE +1                   TO SPAR-KDORDBEH                         
001440       MOVE PHUV-KDORDKL         TO SPAR-KDORDKL                          
001441       MOVE AREG-KDPRODSL        TO SPAR-KDPRODSL                         
001442       MOVE PRAD-KDPRTYP         TO SPAR-KDPRTYP                          
001443       MOVE AREG-KDSORT          TO SPAR-KDSORT                           
001444       MOVE AREG-KDUART          TO SPAR-KDUART                           
001445       MOVE AREG-PRARTSTD        TO SPAR-PRARTSTD                         
001446       MOVE AREG-TIFINLV         TO SPAR-TIFINLV                          
001447       MOVE +0                   TO SPAR-TITPO                            
001448       MOVE '0000000   '         TO SPAR-IDKUNDRF-RO                      
001449       MOVE PRAD-KDTPOTYP        TO SPAR-KDTPOTYP                         
001450       MOVE +0                   TO SPAR-TIRODAT                          
001451       MOVE NEJ                  TO SPAR-FLSDCLEV                         
001452       MOVE ZERO                 TO SPAR-TIREPDAT                         
001453                                                                          
001454       CALL W411SPAR USING SPAR-W411SPAR SPAR-WDF8-PCB                    
001455                                         SPAR-WDF8A-PCB                   
001456                                         SPAR-WDK6-PCB                    
001457                                                                          
001458       IF SPAR-KDORDBEK      > +0                                         
001459         MOVE JA             TO OBKR-SW                                   
001460         MOVE NEJ            TO ALLT-SW                                   
001461         IF KVAN-KDORDBEK-UT > +0                                         
001462           MOVE +0          TO KVAN-KDORDBEK-UT                           
001463           MOVE PRAD-KVBEART TO PRAD-KVBEART-Q                            
001464         END-IF                                                           
001465         IF DLEV-KDORDBEK-UT > +0                                         
001466           MOVE +0          TO DLEV-KDORDBEK-UT                           
001467         END-IF                                                           
001468       END-IF                                                             
001469                                                                          
001470     END-IF                                                               
001471     .                                                                    
001472     EJECT                                                                
001473 ECJ-KOMPLETTERA-PRIS SECTION.                                            
001474                                                                          
001475     IF ALLT-OK                                                           
001476                                                                          
001477     IF DIST79-DEALER-PRICE                                               
001478       IF WS-IDPRQUES                = +0                                 
001479          MOVE WS-IDPRQUES           TO PRNO-IDPRQUES-IN                  
001480          MOVE +1                    TO PRNO-KDCALL                       
001481                                                                          
001482          CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                 
001483                                                                          
001484          MOVE PRNO-IDPRQUES-UT      TO PRQU-IDPRQUES                     
001485                                        WS-IDPRQUES                       
001486          MOVE +1                    TO PRQU-KDCALL                       
001487       ELSE                                                               
001488          MOVE WS-IDPRQUES           TO PRNO-IDPRQUES-IN                  
001489          MOVE +2                    TO PRNO-KDCALL                       
001490                                                                          
001491          CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                 
001492                                                                          
001493          MOVE PRNO-IDPRQUES-UT      TO PRQU-IDPRQUES                     
001494                                        WS-IDPRQUES                       
001495          MOVE +2                    TO PRQU-KDCALL                       
001496       END-IF                                                             
001497                                                                          
001498       MOVE PHUV-IDDISTR             TO PRQU-IDDISTR                      
001499       MOVE PHUV-IDKUNDNR            TO PRQU-IDKUNDNR                     
001500       MOVE PHUV-IDKUNDRF            TO PRQU-IDKUNDRF                     
001501       MOVE PHUV-IDORDER             TO PRQU-IDORDER                      
001502       MOVE PHUV-KDORDKL             TO PRQU-KDORDKL                      
001503       MOVE 'N'                      TO PRQU-KDPRSTA                      
001504       MOVE PRAD-IDARTNR             TO PRQU-IDARTNR                      
001505       MOVE PRAD-KVBEART-Q           TO PRQU-KVBEART-Q                    
001506                                                                          
001507       MOVE GMT-IDFTG                TO W-WDB1-IDFTG                      
001508       MOVE GMT-IDPARTNR             TO W-WDB1-IDPARTNR                   
001509       PERFORM IMS-GU-WDB101                                              
001510       MOVE BET-KDVALISO             TO PRAD-KDVALISO                     
001511                                        PRQU-KDVALISO                     
001512                                                                          
001513       MOVE PRAD-PRARTNTO-LOC        TO PRQU-PRARTNTO-LOC                 
001514       MOVE +0                       TO PRQU-PRARTNTO-LOCPREL             
001515       MOVE PRAD-IDSYSTEM            TO PRQU-IDSYSTEM                     
001516                                                                          
001517       CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                   
001518                                          PRQU-WDC7-PCB                   
001519                                          PRQU-SJKO-WDK6-PCB              
001520       MOVE PRQU-IDPRQUES            TO  PRAD-IDPRQUES                    
001521                                         WS-IDPRQUES                      
001522       MOVE PRQU-FLPRTILL            TO  PRAD-FLPRTILL                    
001523       IF PRAD-PRARTNTO-LOC = +0                                          
001524          MOVE PRQU-PRARTNTO-LOCPREL TO  PRAD-PRARTNTO-LOCPREL            
001525       END-IF                                                             
001526                                                                          
001527       IF PRAD-PRARTNTO-LOC NOT = +0                                      
001528         IF PRAD-KDPRTYP = SPACE                                          
001529           MOVE 'P'            TO PRAD-KDPRTYP                            
001530           MOVE PRAD-TIREGDAT  TO PRAD-TIPRIS                             
001531         END-IF                                                           
001532       END-IF                                                             
001533                                                                          
001534     ELSE                                                                 
001535*      NOT DIST79-DEALER-PRICE                                            
001536                                                                          
001537       IF PRAD-PRARTNTO = +0                                              
001538                                                                          
001539         MOVE '1'                  TO PRIS-KDCALL                         
001540         MOVE IDPGM                TO PRIS-IDPGM                          
001541         MOVE PRAD-IDARTNR         TO PRIS-IDARTNR                        
001542         MOVE PHUV-IDDISTR         TO PRIS-IDDISTR                        
001543         MOVE PHUV-IDKUNDNR        TO PRIS-IDKUNDNR                       
001544         MOVE WC-CDC-SE            TO PRIS-IDDC                           
001545         MOVE PHUV-KDORDKL         TO PRIS-KDORDKL                        
001546         MOVE PRAD-KVBEART-Q       TO PRIS-KVBEART                        
001547         MOVE PRAD-FLINVEST        TO PRIS-FLINVEST                       
001548                                                                          
001549         CALL W335PRIS USING PRIS-W335PRIS PRIS-ARTC-PCB                  
001550                             PRIS-WDK7-PCB                                
001551                             PRIS-GMTA-PCB PRIS-BETA-PCB                  
001552                             PRIS-GPRIA-PCB PRIS-GPRIB-PCB                
001553                             PRIS-COST-WDK6-PCB                           
001554                             PRIS-COST-WDK7-PCB                           
001555                             PRIS-COST-WDF1-PCB                           
001556                             PRIS-COST-9305-PCB                           
001557                             PRIS-COST-WDK72-PCB                          
001558                             PRIS-COST-WDB6-PCB                           
001559                                                                          
001560         IF PRIS-KDSVAR = '2'                                             
001561           MOVE 'DIST/KUND SAKNAS NÄR W335PRIS LÄSER KUNDREG'             
001562                                     TO FELTEXT                           
001563           CALL ABEND USING RKOD-ABEND                                    
001564         END-IF                                                           
001565                                                                          
001566         MOVE PRIS-PRARTNTO  TO PRAD-PRARTNTO                             
001567         MOVE PRIS-FLPRTILL  TO PRAD-FLPRTILL                             
001568         MOVE PRIS-KDPRTYP   TO PRAD-KDPRTYP                              
001569         MOVE PRIS-PRBPRIS   TO PRAD-PRBPRIS                              
001570         MOVE PRAD-TIREGDAT  TO PRAD-TIPRIS                               
001571       ELSE                                                               
001572         IF PRAD-KDPRTYP = SPACE                                          
001573           MOVE 'P'            TO PRAD-KDPRTYP                            
001574           MOVE PRAD-TIREGDAT  TO PRAD-TIPRIS                             
001575         END-IF                                                           
001576       END-IF                                                             
001577       MOVE 'SEK'            TO PRAD-KDVALISO                             
001578     END-IF                                                               
001579     END-IF                                                               
001580     .                                                                    
001581     EJECT                                                                
001582 ECQ-KOMPLETTERA-STORA-UTTAG SECTION.                                     
001583                                                                          
001584     IF ALLT-OK                                                           
001585                                                                          
001586       MOVE PRAD-IDSYSTEM        TO STOR-IDSYSTEM                         
001587       MOVE PRAD-IDLEVNR         TO STOR-IDLEVNR                          
001588       MOVE '0000000   '         TO STOR-IDKUNDRF-RO                      
001589       MOVE PHUV-KDPROTYP        TO STOR-KDPROTYP                         
001590       MOVE PRAD-BERADREF        TO STOR-BERADREF                         
001591       MOVE NEJ                  TO STOR-FLFORBI                          
001592       MOVE NEJ                  TO STOR-FLORDSPE                         
001593       MOVE NEJ                  TO STOR-FLOVRLEV                         
001594       MOVE PHUV-KDORDKL         TO STOR-KDORDKL                          
001595       MOVE AREG-KDVVKL          TO STOR-KDVVKL                           
001596       MOVE AREG-KDERS           TO STOR-KDERS                            
001597       MOVE PRAD-KVBEART-Q       TO STOR-KVBEART-Q                        
001598       MOVE AREG-KVPB-SEP        TO STOR-KVPB-SEP                         
001599       MOVE AREG-KVSLUTKP        TO STOR-KVSLUTKP                         
001600       MOVE +1                   TO STOR-RERF-ART                         
001601       MOVE +0                   TO STOR-IDKAMPRF                         
001602       MOVE PHUV-IDDISTR         TO STOR-IDDISTR                          
001603       MOVE AREG-KDPRODSL        TO STOR-KDPRODSL                         
001604                                                                          
001605       MOVE +0                   TO STOR-KDORDBEK                         
001606                                                                          
001607       CALL W411STOR USING STOR-W411STOR                                  
001608                                                                          
001609       IF STOR-KDORDBEK > +0                                              
001610          MOVE +5                   TO PRAD-KDTPOTYP                      
001611          MOVE JA                   TO OBKR-SW                            
001612       END-IF                                                             
001613       IF STOR-KDORDBEK = 0                                               
001614         IF PRAD-BERADREF = 'W480      ' AND AREG-KDUART = 'M'            
001615           CONTINUE                                                       
001616         ELSE                                                             
001617           IF AREG-KDUART = 'S' OR 'P' OR 'M' OR 'L'                      
001618             IF  PHUV-KDPROTYP NOT = 'F'                                  
001619               MOVE +5 TO PRAD-KDTPOTYP                                   
001620               MOVE 70 TO STOR-KDORDBEK                                   
001621               MOVE JA TO OBKR-SW                                         
001622             END-IF                                                       
001623           END-IF                                                         
001624         END-IF                                                           
001625       END-IF                                                             
001626                                                                          
001627     END-IF                                                               
001628     .                                                                    
001629     EJECT                                                                
001630 ECR-PREL-AVBOKNING SECTION.                                              
001631                                                                          
001632     IF ALLT-OK                                                           
001633                                                                          
001634       MOVE TILK-FLFINLV(WS-INDEX-TILLK)                                  
001635                                 TO CDCA-FLFINLV-IN                       
001636       MOVE NEJ                  TO CDCA-FLFORBI-IN                       
001637       MOVE NEJ                  TO CDCA-FLORDSPE-IN                      
001638       MOVE NEJ                  TO CDCA-FLOVRLEV-IN                      
001639       MOVE NEJ                  TO CDCA-FLPRELRO-IN                      
001640       MOVE NEJ                  TO CDCA-FLRESTN-IN                       
001641       MOVE NEJ                  TO CDCA-FLSLATT-IN                       
001642       MOVE PRAD-IDARTNR         TO CDCA-IDARTNR-IN                       
001643       MOVE PHUV-IDDISTR         TO CDCA-IDDISTR-IN                       
001644       MOVE WC-CDC-SE            TO CDCA-IDDC-IN                          
001645       MOVE +0                   TO CDCA-IDKAMPRF-IN                      
001646       MOVE '0000000   '         TO CDCA-IDKUNDRF-RO-IN                   
001647       MOVE PRAD-IDSYSTEM        TO CDCA-IDSYSTEM-IN                      
001648       MOVE PRAD-IDLEVNR         TO CDCA-IDLEVNR-IN                       
001649       MOVE AREG-KDERS           TO CDCA-KDERS-IN                         
001650       MOVE PRAD-KDKVBRYT        TO CDCA-KDKVBRYT-IN                      
001651       MOVE PHUV-KDPROTYP        TO CDCA-KDPROTYP-IN                      
001652       MOVE PHUV-KDORDKL         TO CDCA-KDORDKL-IN                       
001653       MOVE PRAD-KDPRODSL        TO CDCA-KDPRODSL-IN                      
001654       MOVE AREG-KDSORT          TO CDCA-KDSORT-IN                        
001655       MOVE PRAD-KDTPOTYP        TO CDCA-KDTPOTYP-IN                      
001656       MOVE AREG-KDUART          TO CDCA-KDUART-IN                        
001657       MOVE PRAD-KVBEART         TO CDCA-KVBEART-IN                       
001658       MOVE PRAD-KVBEART-Q       TO CDCA-KVBEART-Q-IN                     
001659       MOVE AREG-KVQPACK-0       TO CDCA-KVQPACK-0-IN                     
001660       MOVE AREG-KVQPACK-1       TO CDCA-KVQPACK-1-IN                     
001661       MOVE AREG-IDFKNGRP        TO CDCA-IDFKNGRP-IN                      
001662       MOVE +1                   TO CDCA-RERF-RAD-IN                      
001663       MOVE +1                   TO CDCA-RERF-ART-IN                      
001664       MOVE +0                   TO CDCA-RESLATT-IN                       
001665       MOVE AREG-KVAKS-CDC       TO CDCA-KVAKS-CDC-IN                     
001666       MOVE AREG-KVAKS-PAV       TO CDCA-KVAKS-PAV-IN                     
001667       MOVE AREG-KVLS            TO CDCA-KVLS-IN                          
001668       MOVE AREG-KVRESS          TO CDCA-KVRESS-IN                        
001669       MOVE AREG-KVSPANT         TO CDCA-KVSPANT-IN                       
001670       MOVE AREG-KVUTRS          TO CDCA-KVUTRS-IN                        
001671       MOVE AREG-KVSPARR-KVAL    TO CDCA-KVSPARR-KVAL-IN                  
001672       MOVE PHUV-IDKUNDNR        TO CDCA-IDKUNDNR-IN                      
001673       MOVE PRAD-BERADREF        TO CDCA-BERADREF-IN                      
001674       MOVE +1                   TO CDCA-KDCALL                           
001675                                                                          
001676       CALL W411CDCA USING CDCA-W411CDCA CDCA-ARTM-PCB                    
001677                                         CDCA-INLB-PCB                    
001678                                         CDCA-WDB2-PCB                    
001679                                         CDCA-WDC1-PCB                    
001680                                                                          
001681       IF KERS-KDERS = +1                                                 
001682          IF CDCA-KVPREAVB-UT = +0                                        
001683             MOVE JA       TO OBKR-SW                                     
001684          ELSE                                                            
001685             PERFORM S01-RENSA-TILLK-TAB                                  
001686             MOVE ZERO     TO KERS-KDORDBEK                               
001687             MOVE NEJ      TO TILLK-SW                                    
001688          END-IF                                                          
001689       END-IF                                                             
001690                                                                          
001691* TEST FÖR ATT SE ATT W411CDCA ANROPATS                                   
001692       MOVE PRAD-KVBEART-Q TO WS-KVPREAVB                                 
001693     END-IF                                                               
001694     .                                                                    
001695     EJECT                                                                
001696 ECS-SKRIV-OBKR SECTION.                                                  
001697                                                                          
001698*--- EFTERSOM KVPREAVB OCH KVPRERO ENDAST SKALL SKRIVAS PÅ                
001699*    DEN SISTA ORDERBEKRÄFTELSERADEN FÖR ETT CLAGER 'SLÄPAR'              
001700*    ISRT AV RADEN.                                                       
001701*    OBKR-SW SÄTTS = JA NÄR EN ORDERBEKRÄFTELSE RAD SKALL                 
001702*    SKRIVAS.  DENNA TESTAR VI PÅ SIST I SECTIONEN FÖR ATT                
001703*    FÅ MED DEN SISTA ORDERBEKRÄFTELSEN MED KVPREAVB OCH KVPRERO          
001704*---                                                                      
001705     PERFORM ECSA-REDIGERA-OBKR-RAD                                       
001706                                                                          
001707     IF TILLKOMMANDE-RAD                                                  
001708       IF KERS-KDORDBEK = 41                                              
001709         MOVE KERS-KDORDBEK     TO OBKR-KDORDBEK                          
001710         MOVE '4262KER1'        TO OBKR-IDPGM                             
001711         MOVE TILK-KVBEART(WS-INDEX-TILLK)                                
001712                                TO OBKR-KVBEART-TILLK                     
001713         COMPUTE OBKR-DIERS-KVOT =                                        
001714                              TILK-DIERS-TILLK(WS-INDEX-TILLK)            
001715                              / TILK-DIERS-ERS(WS-INDEX-TILLK)            
001716         MOVE 'S'               TO OBKR-SW                                
001717       END-IF                                                             
001718     END-IF                                                               
001719     EJECT                                                                
001720     IF ORFK-KDORDBEK(WS-INDEX-MID) > 0                                   
001721*----(KOD 58, 59, 98)                                                     
001722       IF OBKR-SKRIVEN                                                    
001723         PERFORM IMS-ISRT-ORQM-WDQ101                                     
001724         ADD +1              TO OBKR-IDSEKVNR                             
001725       END-IF                                                             
001726       MOVE ORFK-KDORDBEK(WS-INDEX-MID)                                   
001727                              TO OBKR-KDORDBEK                            
001728       MOVE '4262ORFK'          TO OBKR-IDPGM                             
001729       MOVE 'S'               TO OBKR-SW                                  
001730     END-IF                                                               
001731     EJECT                                                                
001732     IF KVAN-KDORDBEK-UT > +0                                             
001733*----(KOD 43, 44)                                                         
001734        IF OBKR-SKRIVEN                                                   
001735          PERFORM IMS-ISRT-ORQM-WDQ101                                    
001736          ADD +1              TO OBKR-IDSEKVNR                            
001737        END-IF                                                            
001738        IF TILLKOMMANDE-RAD                                               
001739          MOVE PRAD-KVBEART   TO OBKR-KVBEART-TILLK                       
001740          COMPUTE OBKR-DIERS-KVOT =                                       
001741                               TILK-DIERS-TILLK(WS-INDEX-TILLK)           
001742                               / TILK-DIERS-ERS(WS-INDEX-TILLK)           
001743        END-IF                                                            
001744        MOVE KVAN-KDORDBEK-UT  TO OBKR-KDORDBEK                           
001745        MOVE '4262KVAN'        TO OBKR-IDPGM                              
001746        MOVE KVAN-KVBEART-IN   TO OBKR-KVBEART                            
001747        MOVE KVAN-KVBEART-Q-UT TO OBKR-KVBEART-Q                          
001748        MOVE 'S'               TO OBKR-SW                                 
001749     END-IF                                                               
001750     EJECT                                                                
001751     IF DLEV-KDORDBEK-UT = 21 OR 53 OR 82 OR 95 OR 26                     
001752*----(KOD 21, 53, 82, 95) ,26                                             
001753       IF OBKR-SKRIVEN                                                    
001754         PERFORM IMS-ISRT-ORQM-WDQ101                                     
001755         ADD +1              TO OBKR-IDSEKVNR                             
001756       END-IF                                                             
001757       IF TILLKOMMANDE-RAD                                                
001758         MOVE PRAD-KVBEART   TO OBKR-KVBEART-TILLK                        
001759         COMPUTE OBKR-DIERS-KVOT =                                        
001760                              TILK-DIERS-TILLK(WS-INDEX-TILLK)            
001761                              / TILK-DIERS-ERS(WS-INDEX-TILLK)            
001762       END-IF                                                             
001763       MOVE DLEV-KDORDBEK-UT  TO OBKR-KDORDBEK                            
001764       MOVE '4262DLEV'        TO OBKR-IDPGM                               
001765       MOVE 'S'               TO OBKR-SW                                  
001766     END-IF                                                               
001767     EJECT                                                                
001768     IF KERS-KDORDBEK > +0                                                
001769*----(KOD 41, 61)                                                         
001770                                                                          
001771       IF KERS-KDORDBEK = 61                                              
001772*----FÖR KOD 41 OCH 42 SKER ORDERBEKRÄFTELSE ENDAST PÅ TILL-              
001773*----KOMMANDE ARTIKLAR EFTER DET ATT DESSA HAR GENOMGÅTT                  
001774*----RADBEHANDLINGEN                                                      
001775         IF OBKR-SKRIVEN                                                  
001776            PERFORM IMS-ISRT-ORQM-WDQ101                                  
001777            ADD +1           TO OBKR-IDSEKVNR                             
001778         END-IF                                                           
001779         MOVE KERS-KDORDBEK  TO OBKR-KDORDBEK                             
001780         MOVE '4262KER2'     TO OBKR-IDPGM                                
001781         MOVE 'S'            TO OBKR-SW                                   
001782         PERFORM ECSC-OBKR-FRAN-TILLK-TAB                                 
001783         PERFORM S01-RENSA-TILLK-TAB                                      
001784       ELSE                                                               
001785         IF NOT TILLKOMMANDE-RAD                                          
001786            IF OBKR-SKRIVEN                                               
001787               PERFORM IMS-ISRT-ORQM-WDQ101                               
001788               ADD +1        TO OBKR-IDSEKVNR                             
001789            END-IF                                                        
001790            MOVE 'S'            TO OBKR-SW                                
001791            MOVE KERS-KDORDBEK  TO OBKR-KDORDBEK                          
001792            MOVE '4262KER3'     TO OBKR-IDPGM                             
001793         END-IF                                                           
001794       END-IF                                                             
001795     END-IF                                                               
001796     EJECT                                                                
001797     IF SPAR-KDORDBEK        > +0                                         
001798*----(KOD 51, 52, 54, 55, 57, 58, 66, 67, 80, 90)                         
001799       IF OBKR-SKRIVEN                                                    
001800         PERFORM IMS-ISRT-ORQM-WDQ101                                     
001801         ADD +1              TO OBKR-IDSEKVNR                             
001802       END-IF                                                             
001803       IF TILLKOMMANDE-RAD                                                
001804         MOVE PRAD-KVBEART   TO OBKR-KVBEART-TILLK                        
001805         COMPUTE OBKR-DIERS-KVOT =                                        
001806                              TILK-DIERS-TILLK(WS-INDEX-TILLK)            
001807                              / TILK-DIERS-ERS(WS-INDEX-TILLK)            
001808       END-IF                                                             
001809       MOVE SPAR-KDORDBEK     TO OBKR-KDORDBEK                            
001810       MOVE '4262SPAR'        TO OBKR-IDPGM                               
001811       MOVE 'S'               TO OBKR-SW                                  
001812     END-IF                                                               
001813     EJECT                                                                
001814     IF STOR-KDORDBEK > +0                                                
001815*----(KOD 70)                                                             
001816       IF OBKR-SKRIVEN                                                    
001817         PERFORM IMS-ISRT-ORQM-WDQ101                                     
001818         ADD +1              TO OBKR-IDSEKVNR                             
001819       END-IF                                                             
001820       IF TILLKOMMANDE-RAD                                                
001821         MOVE PRAD-KVBEART   TO OBKR-KVBEART-TILLK                        
001822         COMPUTE OBKR-DIERS-KVOT =                                        
001823                              TILK-DIERS-TILLK(WS-INDEX-TILLK)            
001824                              / TILK-DIERS-ERS(WS-INDEX-TILLK)            
001825       END-IF                                                             
001826       MOVE STOR-KDORDBEK     TO OBKR-KDORDBEK                            
001827       MOVE '4262STOR'        TO OBKR-IDPGM                               
001828       MOVE 'S'               TO OBKR-SW                                  
001829     END-IF                                                               
001830     EJECT                                                                
001831     IF OBKR-SKRIVEN                                                      
001832       MOVE WS-KVPREAVB TO OBKR-KVPREAVB                                  
001833******** TA BORT FRÅGAN FÖR ERSATT ARTIKEL                                
001834       IF KERS-KDORDBEK > 0 AND NOT TILLKOMMANDE-RAD                      
001835           PERFORM S05-DELETE-PRICE-Q-LINE                                
001836           INITIALIZE OBKR-DEAL-PR-LINE                                   
001837       END-IF                                                             
001838*************TL 030514                                                    
001839       PERFORM IMS-ISRT-ORQM-WDQ101                                       
001840       ADD +1 TO OBKR-IDSEKVNR                                            
001841     END-IF                                                               
001842                                                                          
001843     .                                                                    
001844     EJECT                                                                
001845 ECSA-REDIGERA-OBKR-RAD SECTION.                                          
001846                                                                          
001847     MOVE PRAD-IDORDER         TO OBKR-IDORDER                            
001848     MOVE ORFK-IDARTNR(WS-INDEX-MID)                                      
001849                               TO OBKR-IDARTNR                            
001850     IF NOT TILLKOMMANDE-RAD                                              
001851       MOVE OBKR-IDORDER      TO W-IDORDER-Q1-MIN                         
001852                                 W-IDORDER-Q1-MAX                         
001853       MOVE OBKR-IDARTNR      TO W-IDARTNR-Q1-MIN                         
001854                                 W-IDARTNR-Q1-MAX                         
001855       MOVE +1                TO W-IDLOPNR-Q1-MIN                         
001856                                 W-IDLOPNR-Q1-MAX                         
001857                                 W-IDSEKVNR-Q1-MIN                        
001858                                 W-IDSEKVNR-Q1-MAX                        
001859       PERFORM IMS-GU-ORQM-WDQ101                                         
001860       PERFORM UNTIL SEGMENT-SAKNAS                                       
001861          ADD +1              TO W-IDLOPNR-Q1-MIN                         
001862                                 W-IDLOPNR-Q1-MAX                         
001863          PERFORM IMS-GU-ORQM-WDQ101                                      
001864       END-PERFORM                                                        
001865       MOVE W-IDLOPNR-Q1-MIN  TO OBKR-IDLOPNR                             
001866       MOVE W-IDSEKVNR-Q1-MIN TO OBKR-IDSEKVNR                            
001867     END-IF                                                               
001868     MOVE WC-CDC-SE            TO OBKR-IDDC                               
001869                                                                          
001870     MOVE +0                   TO OBKR-KDORDBEK                           
001871     MOVE SPACE                TO OBKR-BEERS                              
001872     MOVE SPACE                TO OBKR-IDBIL                              
001873     MOVE PHUV-BEKUNDRF        TO OBKR-BEKUNDRF                           
001874     MOVE PRAD-BERADREF        TO OBKR-BERADREF                           
001875     MOVE SPACE                TO OBKR-BEVOLREF                           
001876     MOVE +0                   TO OBKR-IDKAMPRF                           
001877     MOVE +0                   TO OBKR-DIERS-KVOT                         
001878     MOVE NEJ                  TO OBKR-FLAKPLOC                           
001879     MOVE PRAD-FLINVEST        TO OBKR-FLINVEST                           
001880     MOVE NEJ                  TO OBKR-FLOBOK                             
001881     MOVE JA                   TO OBKR-FLOBTRAN                           
001882     MOVE NEJ                  TO OBKR-FLOBPRT                            
001883     MOVE PRAD-FLPRTILL        TO OBKR-FLPRTILL                           
001884     MOVE PRAD-FLRESTN         TO OBKR-FLRESTN                            
001885     MOVE ORFK-FLSLATT(WS-INDEX-MID)                                      
001886                               TO OBKR-FLSLATT                            
001887     IF TILLKOMMANDE-RAD                                                  
001888       MOVE JA                 TO OBKR-FLTILLK                            
001889     ELSE                                                                 
001890       MOVE NEJ                TO OBKR-FLTILLK                            
001891     END-IF                                                               
001892     IF ORFK-KDORDBEK(WS-INDEX-MID) = 59                                  
001893       MOVE PRAD-REKSIFFR      TO OBKR-REKSIFFR                           
001894     ELSE                                                                 
001895        MOVE ORFK-REKSIFFR(WS-INDEX-MID)                                  
001896                               TO OBKR-REKSIFFR                           
001897     END-IF                                                               
001898     IF TILLKOMMANDE-RAD                                                  
001899       MOVE TILK-IDARTNR-TILLK(WS-INDEX-TILLK)                            
001900                               TO OBKR-IDARTNR-TILLK                      
001901       MOVE TILK-REKSIFFR-TILLK(WS-INDEX-TILLK)                           
001902                               TO OBKR-REKSIFFR-TILLK                     
001903     ELSE                                                                 
001904       MOVE +0                TO OBKR-IDARTNR-TILLK                       
001905       MOVE +0                TO OBKR-REKSIFFR-TILLK                      
001906     END-IF                                                               
001907     MOVE PHUV-IDGMTREF        TO OBKR-IDGMTREF                           
001908     MOVE '0000000   '         TO OBKR-IDKUNDRF-RO                        
001909     MOVE PRAD-IDLEVNR         TO OBKR-IDLEVNR                            
001910     MOVE +0                   TO OBKR-IDLOPNR-RO                         
001911     MOVE 'PROF'               TO OBKR-IDSYSTEM                           
001912     MOVE +2                   TO OBKR-KDDSP                              
001913     IF NOT TILLKOMMANDE-RAD                                              
001914        MOVE AREG-KDERS        TO OBKR-KDERS                              
001915     END-IF                                                               
001916     MOVE PRAD-KDKVBRYT        TO OBKR-KDKVBRYT                           
001917     MOVE PRAD-KDPRTYP         TO OBKR-KDPRTYP                            
001918     MOVE PRAD-KDTPOTYP        TO OBKR-KDTPOTYP                           
001919     MOVE +2                   TO OBKR-KDVRINFO                           
001920     MOVE +0                   TO OBKR-KVANNANT                           
001921     MOVE +0                   TO OBKR-KVAVBART                           
001922     MOVE PRAD-KVBEART         TO OBKR-KVBEART                            
001923     MOVE PRAD-KVBEART-Q       TO OBKR-KVBEART-Q                          
001924     MOVE +0                   TO OBKR-KVBEART-TILLK                      
001925     MOVE +0                   TO OBKR-KVPREAVB                           
001926     MOVE +0                   TO OBKR-KVPRERO                            
001927     IF ALLT-OK                                                           
001928        MOVE KVAN-KVQPACK-UT   TO OBKR-KVQPACK                            
001929     ELSE                                                                 
001930        MOVE +3                TO OBKR-KVQPACK                            
001931     END-IF                                                               
001932     MOVE +0                   TO OBKR-KVRO                               
001933     MOVE +0                   TO OBKR-KVSLATT                            
001934     MOVE PRAD-PRARTNTO        TO OBKR-PRARTNTO                           
001935     MOVE PRAD-DEAL-PR-LINE    TO OBKR-DEAL-PR-LINE                       
001936     MOVE PRAD-PRBPRIS         TO OBKR-PRBPRIS                            
001937     MOVE +1                   TO OBKR-RERF-RAD                           
001938     MOVE AREG-TIDISPIN        TO OBKR-TIDISPIN                           
001939     MOVE PHUV-TIREGDAT        TO OBKR-TIORDREG                           
001940     MOVE PRAD-TIPRIS          TO OBKR-TIPRIS                             
001941     MOVE PRAD-TIREGDAT        TO OBKR-TIREGDAT                           
001942     MOVE PRAD-TIREGTID        TO OBKR-TIREGTID                           
001943     MOVE +0                   TO OBKR-TIRODAT                            
001944     MOVE PRAD-TIREGDAT        TO WS-AAMMDD-9KOMPL                        
001945     IF WS-AAMMDD-9KOMPL(1:2) < 50                                        
001946       MOVE 20                 TO WS-SEKEL-9KOMPL                         
001947     ELSE                                                                 
001948       MOVE 19                 TO WS-SEKEL-9KOMPL                         
001949     END-IF                                                               
001950     COMPUTE OBKR-TITIREGD-9KOMPL = 999999999 - WS-TITIREGD-9KOMPL        
001951     MOVE +0                   TO OBKR-TITPO                              
001952     MOVE PHUV-TIREGDAT        TO WS-AAMMDD-9KOMPL                        
001953     IF WS-AAMMDD-9KOMPL(1:2) < 50                                        
001954       MOVE 20                 TO WS-SEKEL-9KOMPL                         
001955     ELSE                                                                 
001956       MOVE 19                 TO WS-SEKEL-9KOMPL                         
001957     END-IF                                                               
001958     COMPUTE OBKR-TITIORDD-9KOMPL = 999999999 - WS-TITIREGD-9KOMPL        
001959     MOVE PHUV-KDFRAKT         TO OBKR-KDFRAKT                            
001960     MOVE PHUV-KDORDKL         TO OBKR-KDORDKL                            
001961                                                                          
001962     MOVE SPACE                TO OBKR-KDORDTYP-LDC                       
001963     MOVE ZERO                 TO OBKR-TIREPDAT                           
001964     MOVE SPACE                TO OBKR-IDKUNDRF-WIP                       
001965     MOVE ZERO                 TO OBKR-TIDLEVDAT                          
001966     MOVE +0                   TO OBKR-PRAVCOST                           
001967     MOVE PRAD-KDVALISO        TO OBKR-KDVALISO                           
001968     .                                                                    
001969     EJECT                                                                
001970 ECSC-OBKR-FRAN-TILLK-TAB SECTION.                                        
001971                                                                          
001972     MOVE +1                   TO WS-INDEX-TILLK                          
001973     PERFORM UNTIL WS-INDEX-TILLK > WS-INDEX-TILLK-MAX                    
001974       IF TILK-FLTILLK-X(WS-INDEX-TILLK) = JA                             
001975         IF OBKR-SKRIVEN                                                  
001976           PERFORM IMS-ISRT-ORQM-WDQ101                                   
001977           ADD +1              TO OBKR-IDSEKVNR                           
001978         END-IF                                                           
001979         MOVE KERS-KDORDBEK  TO OBKR-KDORDBEK                             
001980         MOVE '4262KER4'     TO OBKR-IDPGM                                
001981         MOVE TILK-IDARTNR-TILLK(WS-INDEX-TILLK)                          
001982                             TO OBKR-IDARTNR-TILLK                        
001983         MOVE TILK-REKSIFFR-TILLK(WS-INDEX-TILLK)                         
001984                             TO OBKR-REKSIFFR-TILLK                       
001985         MOVE TILK-KVBEART(WS-INDEX-TILLK)                                
001986                             TO OBKR-KVBEART-TILLK                        
001987         COMPUTE OBKR-DIERS-KVOT =                                        
001988                 TILK-DIERS-TILLK(WS-INDEX-TILLK) /                       
001989                 TILK-DIERS-ERS(WS-INDEX-TILLK)                           
001990         MOVE TILK-BEERS(WS-INDEX-TILLK)                                  
001991                             TO OBKR-BEERS                                
001992                                                                          
001993         MOVE 'S'            TO OBKR-SW                                   
001994       END-IF                                                             
001995       ADD +1                 TO WS-INDEX-TILLK                           
001996     END-PERFORM                                                          
001997     .                                                                    
001998     EJECT                                                                
001999 ECV-SKRIV-PRAD SECTION.                                                  
002000                                                                          
002001     PERFORM IMS-ISRT-PROD-WDE901                                         
002002     PERFORM UNTIL SEGMENT-FINNS                                          
002003       ADD +1                    TO PRAD-IDLOPNR                          
002004       PERFORM IMS-ISRT-PROD-WDE901                                       
002005     END-PERFORM                                                          
002006                                                                          
002007     PERFORM ECVA-ADDERA-TILL-PHUV                                        
002008                                                                          
002009     .                                                                    
002010     EJECT                                                                
002011 ECVA-ADDERA-TILL-PHUV SECTION.                                           
002012                                                                          
002013     IF AREG-IDFKNGRP = 2101 OR 2102                                      
002014       ADD +1 TO WS-KVMOTOR                                               
002015     ELSE                                                                 
002016       IF AREG-IDFKNGRP = 8001 OR 8002                                    
002017         ADD +1 TO WS-KVKAROSS                                            
002018       END-IF                                                             
002019     END-IF                                                               
002020                                                                          
002021     IF DIST79-DEALER-PRICE                                               
002022       COMPUTE WS-SUORDV-LOC = PRAD-PRARTNTO-LOC * PRAD-KVBEART-Q         
002023       COMPUTE WS-SUORDV-LOCPREL = PRAD-PRARTNTO-LOCPREL *                
002024                                   PRAD-KVBEART-Q                         
002025     ELSE                                                                 
002026       COMPUTE WS-SUORDV = PRAD-PRARTNTO * PRAD-KVBEART-Q                 
002027     END-IF                                                               
002028     ADD WS-SUORDV           TO WS-SUORDV-RAKN                            
002029     ADD WS-SUORDV-LOC       TO WS-SUORDV-RAKN-LOC                        
002030     ADD WS-SUORDV-LOCPREL   TO WS-SUORDV-RAKN-LOCPREL                    
002031     COMPUTE WS-SA-VLARTNTO = (PRAD-VLARTNTO * PRAD-KVBEART-Q)            
002032     ADD WS-SA-VLARTNTO      TO WS-SA-VLARTNTO-RAKN                       
002033     COMPUTE WS-SA-VKART = (PRAD-VKART * PRAD-KVBEART-Q)                  
002034     ADD WS-SA-VKART         TO WS-SA-VKART-RAKN                          
002035                                                                          
002036     .                                                                    
002037     EJECT                                                                
002038 ED-LAES-TILLK-DATA SECTION.                                              
002039                                                                          
002040     MOVE TILK-IDARTNR-TILLK(WS-INDEX-TILLK)                              
002041                               TO AREG-IDARTNR                            
002042                                                                          
002043     CALL W411AREG USING AREG-W411AREG                                    
002044                         AREG-WDK6-PCB                                    
002045                         AREG-WDK7-PCB                                    
002046     .                                                                    
002047     EJECT                                                                
002048 F-UPPDATERA-PHUV SECTION.                                                
002049                                                                          
002050     ADD WS-SUORDV-RAKN         TO PHUV-SUORDV                            
002051     ADD WS-SUORDV-RAKN-LOC     TO PHUV-SUORDV-LOC                        
002052     ADD WS-SUORDV-RAKN-LOCPREL TO PHUV-SUORDV-LOCPREL                    
002053     COMPUTE WS-VLORDBTO ROUNDED = WS-SA-VLARTNTO-RAKN / 1000000          
002054     ADD WS-VLORDBTO TO PHUV-VLORDBTO                                     
002055     COMPUTE WS-VKORDNTO ROUNDED = WS-SA-VKART-RAKN / 1000                
002056     ADD WS-VKORDNTO TO PHUV-VKORDNTO                                     
002057     ADD WS-KVMOTOR  TO PHUV-KVMOTOR                                      
002058     ADD WS-KVKAROSS TO PHUV-KVKAROSS                                     
002059     ACCEPT PHUV-TIUPPDAT FROM DATE                                       
002060     ACCEPT PHUV-TIUPPTID FROM TIME                                       
002061     PERFORM IMS-REPL-PROC-WDE801                                         
002062                                                                          
002063     .                                                                    
002064     EJECT                                                                
002065 G-HOPPA-TILL-SVARSBILD SECTION.                                          
002066                                                                          
002067     MOVE MFS-KDMFSFOR           TO 4263-SPRAK                            
002068     IF MID-KDTRTYP = 'V'                                                 
002069        MOVE 'W4T263V '          TO 4263-TRANSKOD                         
002070        MOVE ALL '+'             TO 4263-IDDISTR-IN                       
002071                                    4263-IDKUNDNR-IN                      
002072                                    4263-IDORDNR-IN                       
002073        MOVE WS-IDDISTR          TO 4263-IDDISTR-UT                       
002074        MOVE WS-IDKUNDNR         TO 4263-IDKUNDNR-UT                      
002075        MOVE WS-IDORDNR          TO 4263-IDORDNR-UT                       
002076        PERFORM IMS-INSERT-4263V-MSG                                      
002077     ELSE                                                                 
002078       MOVE WS-IDDISTR             TO 4263-IDDISTR-IN                     
002079       MOVE WS-IDKUNDNR            TO 4263-IDKUNDNR-IN                    
002080       MOVE WS-IDORDNR             TO 4263-IDORDNR-IN                     
002081       PERFORM IMS-INSERT-4263-MSG                                        
002082     END-IF                                                               
002083     MOVE ALL '+'                TO 4263-IDARTNR-IN                       
002084     MOVE JA                     TO HOPP-TILL-4263                        
002085     .                                                                    
002086     EJECT                                                                
002087 H-VISA-TOM-SIDA SECTION.                                                 
002088                                                                          
002089     MOVE +1 TO WS-INDEX                                                  
002090     PERFORM UNTIL WS-INDEX > WS-INDEX-MID-MAX                            
002091       MOVE MFS-RENSA-FAELT  TO  MOD-IDARTNR-006(WS-INDEX)                
002092                                 MOD-KVBEART(WS-INDEX)                    
002093                                 MOD-PRARTNTO(WS-INDEX)                   
002094                                 MOD-KDKVBRYT(WS-INDEX)                   
002095                                 MOD-FLINVEST(WS-INDEX)                   
002096                                 MOD-BERADREF(WS-INDEX)                   
002097       ADD  +1 TO WS-INDEX                                                
002098     END-PERFORM                                                          
002099     MOVE MFS-ADD-SAETT-CURSOR   TO MOD-IDARTNR-ATTR(1)                   
002100     .                                                                    
002101     EJECT                                                                
002102 I-SKICKA-PRISFRAGA SECTION.                                              
002103                                                                          
002104     MOVE 1                      TO 3039-REQU-IDMSGVER                    
002105     MOVE SPACE                  TO 3039-REQU-KDPGMACT                    
002106     MOVE 'W4026200'             TO 3039-REQU-IDUSER                      
002107                                                                          
002108     MOVE SPACE                  TO 3039-MID-IDBUNDLE                     
002109     MOVE PHUV-IDDISTR           TO 3039-MID-IDDISTR                      
002110     MOVE PHUV-IDKUNDNR          TO 3039-MID-IDKUNDNR                     
002111     MOVE PHUV-IDKUNDRF          TO 3039-MID-IDBUNDLE                     
002112     MOVE ZERO                   TO 3039-MID-IDPRQUES                     
002113                                                                          
002114     PERFORM S04-SKICKA-OPEN                                              
002115     PERFORM S04-SKICKA-MEDDELANDE                                        
002116     PERFORM S04-SKICKA-CLOSE                                             
002117                                                                          
002118     .                                                                    
002119     EJECT                                                                
002120 Z-FINIT-INSERT-MSG SECTION.                                              
002121                                                                          
002122     IF MED-IDMFSFEL NOT = SPACE                                          
002123       CALL WMEDKONV USING MED-WMEDAREA                                   
002124       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
002125     END-IF                                                               
002126                                                                          
002127     IF NOT ALLT-OK AND NYCKEL-OK                                         
002128       PERFORM MFS-ROER-EJ-BILD                                           
002129     END-IF                                                               
002130                                                                          
002131     IF STARTAD-AV-DISPATCHEN                                             
002132        MOVE MED-IDMFSFEL             TO MSG-KOM-IDMFSMED                 
002133                                                                          
002134        IF MSG-KOM-IDMFSMED = SPACE                                       
002135           MOVE OK-BEHANDLAD      TO MSG-KOM-IDMFSMED                     
002136        END-IF                                                            
002137                                                                          
002138        PERFORM IMS-INSERT-DISP-MSG                                       
002139     ELSE                                                                 
002140       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
002141       PERFORM IMS-INSERT-MSG                                             
002142     END-IF                                                               
002143     .                                                                    
002144     EJECT                                                                
002145 S01-RENSA-TILLK-TAB SECTION.                                             
002146                                                                          
002147     MOVE +1              TO WS-INDEX-TILLK                               
002148     PERFORM UNTIL WS-INDEX-TILLK > WS-INDEX-TILLK-MAX                    
002149       MOVE +0           TO TILK-IDARTNR(WS-INDEX-TILLK)                  
002150       MOVE +0           TO TILK-IDARTNR-TILLK(WS-INDEX-TILLK)            
002151       MOVE SPACE        TO TILK-BEERS(WS-INDEX-TILLK)                    
002152       MOVE +0           TO TILK-DIERS-ERS(WS-INDEX-TILLK)                
002153       MOVE +0           TO TILK-DIERS-TILLK(WS-INDEX-TILLK)              
002154       MOVE SPACE        TO TILK-FLFINLV(WS-INDEX-TILLK)                  
002155       MOVE SPACE        TO TILK-FLPRTILL(WS-INDEX-TILLK)                 
002156       MOVE SPACE        TO TILK-FLTILLK-X(WS-INDEX-TILLK)                
002157       MOVE +0           TO TILK-KDERS(WS-INDEX-TILLK)                    
002158       MOVE SPACE        TO TILK-KDPRTYP(WS-INDEX-TILLK)                  
002159       MOVE +0           TO TILK-KVBEART(WS-INDEX-TILLK)                  
002160       MOVE +0           TO TILK-PRARTNTO(WS-INDEX-TILLK)                 
002161                            TILK-PRARTNTO-LOC(WS-INDEX-TILLK)             
002162                            TILK-PRARTNTO-LOCPREL                         
002163                                             (WS-INDEX-TILLK)             
002164       MOVE +0           TO TILK-TIPRIS(WS-INDEX-TILLK)                   
002165       MOVE +0           TO TILK-REKSIFFR-TILLK(WS-INDEX-TILLK)           
002166       ADD +1            TO WS-INDEX-TILLK                                
002167     END-PERFORM                                                          
002168     MOVE +1              TO WS-INDEX-TILLK                               
002169     .                                                                    
002170     EJECT                                                                
002171                                                                          
002172 S04-SKICKA-OPEN SECTION.                                                 
002173                                                                          
002174     MOVE 'OPEN'                     TO SEND-KDFUNC                       
002175     MOVE 'CARPARTS.PULS.PRQRY' TO SEND-ADDISPABS                         
002176     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-OPEN-AREA                 
002177                                                                          
002178     IF SEND-KDRC > 0                                                     
002179       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
002180       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
002181       DELIMITED BY SIZE INTO FELTEXT                                     
002182       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
002183     END-IF                                                               
002184     .                                                                    
002185     SKIP3                                                                
002186 S04-SKICKA-MEDDELANDE SECTION.                                           
002187                                                                          
002188     MOVE 'PUT'                      TO SEND-KDFUNC                       
002189     MOVE LENGTH OF SEND-AREA        TO SEND-KVDLEN                       
002190     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-KVDLEN SEND-AREA          
002191                                                                          
002192     IF SEND-KDRC > 0                                                     
002193       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
002194       STRING 'WZ01SEND GET ERROR RC=' KDRC-DISPLAY                       
002195       DELIMITED BY SIZE INTO FELTEXT                                     
002196       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
002197     END-IF                                                               
002198     .                                                                    
002199     SKIP3                                                                
002200 S04-SKICKA-CLOSE SECTION.                                                
002201                                                                          
002202     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
002203     CALL WZ01SEND USING SEND-CONTROL-AREA                                
002204                                                                          
002205     IF SEND-KDRC > 0                                                     
002206       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
002207       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
002208       DELIMITED BY SIZE INTO FELTEXT                                     
002209       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
002210     END-IF                                                               
002211     .                                                                    
002212     EJECT                                                                
002213                                                                          
002214 S05-DELETE-PRICE-Q-LINE SECTION.                                         
002215                                                                          
002216     IF DIST79-DEALER-PRICE                                               
002217       IF OBKR-IDPRQUES > ZERO                                            
002218         INITIALIZE PRQU-W335PRQU                                         
002219         MOVE OBKR-IDDISTR       TO PRQU-IDDISTR                          
002220         MOVE OBKR-IDKUNDNR      TO PRQU-IDKUNDNR                         
002221         MOVE OBKR-IDKUNDRF      TO PRQU-IDKUNDRF                         
002222         MOVE OBKR-IDPRQUES      TO PRQU-IDPRQUES                         
002223         MOVE 4                  TO PRQU-KDCALL                           
002224         CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                 
002225                                            PRQU-WDC7-PCB                 
002226                                            PRQU-SJKO-WDK6-PCB            
002227       END-IF                                                             
002228     END-IF                                                               
002229     .                                                                    
002230                                                                          
002240 MFS-RENSA-MOD-RADER SECTION.                                             
002250                                                                          
002251     MOVE +1 TO WS-INDEX                                                  
002252     PERFORM UNTIL WS-INDEX > WS-INDEX-MID-MAX                            
002253       MOVE MFS-RENSA-FAELT  TO  MOD-IDARTNR-006(WS-INDEX)                
002254                                 MOD-KVBEART(WS-INDEX)                    
002255                                 MOD-PRARTNTO(WS-INDEX)                   
002256                                 MOD-KDKVBRYT(WS-INDEX)                   
002257                                 MOD-FLINVEST(WS-INDEX)                   
002258                                 MOD-BERADREF(WS-INDEX)                   
002259       ADD  +1 TO WS-INDEX                                                
002260     END-PERFORM                                                          
002261     .                                                                    
002262     EJECT                                                                
002263                                                                          
002264 MFS-ROER-EJ-BILD SECTION.                                                
002265                                                                          
002266     MOVE +1 TO WS-INDEX                                                  
002267     PERFORM UNTIL WS-INDEX > WS-INDEX-MID-MAX                            
002268       MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-006(WS-INDEX)                
002269                                 MOD-KVBEART(WS-INDEX)                    
002270                                 MOD-PRARTNTO(WS-INDEX)                   
002271                                 MOD-KDKVBRYT(WS-INDEX)                   
002272                                 MOD-FLINVEST(WS-INDEX)                   
002273                                 MOD-BERADREF(WS-INDEX)                   
002274       ADD  +1 TO WS-INDEX                                                
002275     END-PERFORM                                                          
002276     .                                                                    
002277     EJECT                                                                
002278* --- IMS SEKTIONER ---                                                   
002279                                                                          
002280 IMS-GET-MSG SECTION.                                                     
002281                                                                          
002282     MOVE '  QC' TO GODK-STATUSKODER                                      
002283     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
002284     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
002285     PERFORM IMS-STATUSKONTROLL                                           
002286     .                                                                    
002287     SKIP3                                                                
002288 IMS-GN-MSG SECTION.                                                      
002289                                                                          
002290     MOVE '  QD'   TO GODK-STATUSKODER                                    
002291     CALL CBLTDLI USING GN MSG-PCB KOM-IO-AREA                            
002292     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
002293     PERFORM IMS-STATUSKONTROLL                                           
002294     .                                                                    
002295     SKIP3                                                                
002296 IMS-INSERT-DISP-MSG SECTION.                                             
002297                                                                          
002298     MOVE '  '  TO GODK-STATUSKODER                                       
002299     CALL CBLTDLI USING ISRT DISP-PCB KOM-IO-AREA                         
002300     MOVE DISP-STATUS-CODE TO STATUS-WS                                   
002301     PERFORM IMS-STATUSKONTROLL                                           
002302     .                                                                    
002303     EJECT                                                                
002304 IMS-INSERT-MSG SECTION.                                                  
002305                                                                          
002306     IF ENGLISH-TEXT                                                      
002307       MOVE 'N' TO MFS-KDHUVOMR                                           
002308     END-IF                                                               
002309     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
002310     MOVE SPACE TO GODK-STATUSKODER                                       
002311     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
002312     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
002313     PERFORM IMS-STATUSKONTROLL                                           
002314     .                                                                    
002315     SKIP3                                                                
002316 IMS-INSERT-4263-MSG SECTION.                                             
002317                                                                          
002318     IF ENGLISH-TEXT                                                      
002319       MOVE 'N' TO MFS-KDHUVOMR                                           
002320     END-IF                                                               
002321     MOVE LOW-VALUE TO 4263-Z1 4263-Z2                                    
002322     MOVE SPACE TO GODK-STATUSKODER                                       
002323     CALL CBLTDLI USING ISRT 4263-PCB 4263-MSG-IO-AREA                    
002324     MOVE 4263-STATUS-CODE TO STATUS-WS                                   
002325     PERFORM IMS-STATUSKONTROLL                                           
002326     .                                                                    
002327     SKIP3                                                                
002328 IMS-INSERT-4263V-MSG SECTION.                                            
002329                                                                          
002330     IF ENGLISH-TEXT                                                      
002331       MOVE 'N' TO MFS-KDHUVOMR                                           
002332     END-IF                                                               
002333     MOVE LOW-VALUE TO 4263-Z1 4263-Z2                                    
002334     MOVE SPACE TO GODK-STATUSKODER                                       
002335     CALL CBLTDLI USING ISRT 4263V-PCB 4263-MSG-IO-AREA                   
002336     MOVE 4263V-STATUS-CODE TO STATUS-WS                                  
002337     PERFORM IMS-STATUSKONTROLL                                           
002338     .                                                                    
002339     EJECT                                                                
002340 IMS-GHU-PROC-WDE801 SECTION.                                             
002341                                                                          
002342     STRING 'WLPROC01(WDE801KY =' W-WDE801KY-X ')'                        
002343          DELIMITED BY SIZE INTO SSA1                                     
002344     MOVE '  GE'               TO GODK-STATUSKODER                        
002345     CALL CBLTDLI USING GHU PROC-PCB DLI-IO-AREA-PHUV SSA1                
002346     MOVE PROC-STATUS-CODE     TO STATUS-WS                               
002347     PERFORM IMS-STATUSKONTROLL                                           
002348     .                                                                    
002349     SKIP3                                                                
002350 IMS-REPL-PROC-WDE801 SECTION.                                            
002351                                                                          
002352     MOVE '    '             TO GODK-STATUSKODER                          
002353     CALL CBLTDLI USING REPL PROC-PCB DLI-IO-AREA-PHUV                    
002354     MOVE PROC-STATUS-CODE     TO STATUS-WS                               
002355     PERFORM IMS-STATUSKONTROLL                                           
002356     .                                                                    
002357     EJECT                                                                
002358 IMS-ISRT-PROD-WDE901 SECTION.                                            
002359                                                                          
002360     MOVE 'WLPROD01'         TO SSA1                                      
002361     MOVE '  II'             TO GODK-STATUSKODER                          
002362     CALL CBLTDLI USING ISRT PROD-PCB DLI-IO-AREA-PRAD SSA1               
002363     MOVE PROD-STATUS-CODE     TO STATUS-WS                               
002364     PERFORM IMS-STATUSKONTROLL                                           
002365     .                                                                    
002366     EJECT                                                                
002367 IMS-GU-ORQM-WDQ101 SECTION.                                              
002368                                                                          
002369     STRING 'WLORQM01(WDQ101KY >' W-WDQ101KY-MIN-X                        
002370                    '&WDQ101KY <' W-WDQ101KY-MAX-X ')'                    
002371          DELIMITED BY SIZE INTO SSA1                                     
002372     MOVE '  GE'             TO GODK-STATUSKODER                          
002373     CALL CBLTDLI USING GU   ORQM-PCB DLI-IO-AREA-OBKR SSA1               
002374     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
002375     PERFORM IMS-STATUSKONTROLL                                           
002376     .                                                                    
002377     SKIP3                                                                
002378 IMS-ISRT-ORQM-WDQ101 SECTION.                                            
002379                                                                          
002380     MOVE 'WLORQM01 '          TO SSA1                                    
002381     MOVE '    '               TO GODK-STATUSKODER                        
002382     CALL CBLTDLI USING ISRT ORQM-PCB DLI-IO-AREA-OBKR SSA1               
002383     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
002384     PERFORM IMS-STATUSKONTROLL                                           
002385     .                                                                    
002386     EJECT                                                                
002387 IMS-GU-01-ARTM-WDK901 SECTION.                                           
002388                                                                          
002389     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
002390          DELIMITED BY SIZE INTO SSA1                                     
002391     MOVE '  GE'               TO GODK-STATUSKODER                        
002392     CALL CBLTDLI USING GU ARTM-PCB DLI-IO-AREA-ART SSA1                  
002393     MOVE ARTM-STATUS-CODE     TO STATUS-WS                               
002394     PERFORM IMS-STATUSKONTROLL                                           
002395     .                                                                    
002396     SKIP3                                                                
002397 IMS-GU-WDB201 SECTION.                                                   
002398                                                                          
002399     STRING 'WDB201  (IDGMT    =' W-IDGMT-WDB2-X ')'                      
002400          DELIMITED BY SIZE INTO SSA1                                     
002401     MOVE '  GE'               TO GODK-STATUSKODER                        
002402     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-AREA-KUND SSA1                 
002403     MOVE WDB2-STATUS-CODE     TO STATUS-WS                               
002404     PERFORM IMS-STATUSKONTROLL                                           
002405     .                                                                    
002406     SKIP3                                                                
002407 IMS-GET-WDB201 SECTION.                                                  
002408                                                                          
002409     STRING 'WDB201  (IDGMT   >=' W-IDGMT-MIN-X                           
002410                    '&IDGMT   <=' W-IDGMT-MAX-X ')'                       
002411            DELIMITED BY SIZE INTO SSA1                                   
002412     MOVE '  GE'               TO GODK-STATUSKODER                        
002413     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-AREA-KUND SSA1                 
002414     MOVE WDB2-STATUS-CODE     TO STATUS-WS                               
002415     PERFORM IMS-STATUSKONTROLL                                           
002416     .                                                                    
002417     SKIP2                                                                
002418 IMS-GU-WDB101 SECTION.                                                   
002419                                                                          
002420     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
002421          DELIMITED BY SIZE INTO SSA1                                     
002422     MOVE '  GE'               TO GODK-STATUSKODER                        
002423     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-AREA-WDB101 SSA1               
002424     MOVE WDB1-STATUS-CODE     TO STATUS-WS                               
002425     PERFORM IMS-STATUSKONTROLL                                           
002426     .                                                                    
002427     SKIP2                                                                
002428 IMS-STATUSKONTROLL SECTION.                                              
002429                                                                          
002430     SET STATUS-IX TO 1                                                   
002431     SEARCH GODK-STATUS                                                   
002432       AT END CALL FELLOG                                                 
002433       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
002434     END-SEARCH                                                           
002435     .                                                                    
002436     EJECT                                                                
002437*    -COPY WY2000P1                                                       
