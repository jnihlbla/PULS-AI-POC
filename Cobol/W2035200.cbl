000001 PROCESS DYNAM                                                            
000002 ID DIVISION.                                                             
000003 PROGRAM-ID.     W2035200.                                                
000004 AUTHOR.         STEFAN ANDREASSON, FRONTEC.                              
000005 DATE-WRITTEN.   96/08/27                                                 
000006 DATE-COMPILED.                                                           
000007                                                                          
000008*    FUNKTION:                                                            
000009*        MANUELLA KÖP FRÅN CDC TILL NDC I USA OCH KANADA                  
000010*                                                                         
000011*        ORDERFÖRSLAG FRÅN REFILLSYSTEMET KOMMER UPP PÅ                   
000012*        DENNA BILD VILKET MAN KAN ACCEPTERA, ÄNDRA ELLER                 
000013*        FÖRKASTA                                                         
000014*        DESSUTOM KAN MAN LÄGGA MANUELLA ORDERBESTÄLLNINGAR HÄR           
000015*                                                                         
000016*        ENTER :    ANVÄNDS VID SIMULERING                                
000017*        PF 11 :    UPPDATERING                                           
000018*                   1) ETT ORDERFÖRSLAG MED KVANTITET IFYLLD              
000019*                      KOMMER ATT GENERERA EN ORDER                       
000020*                   2) ETT ORDERFÖRSLAG MED NOLL I KVANTITET              
000021*                      KOMMER ATT TAS BORT FRÅN WDE3 (FYSISKT)            
000022*                   3) VID EN NY BESTÄLLNING KOMMER EN POST PÅ            
000023*                      WDE3 ATT LÄGGAS UPP VILKET KOMMER ATT              
000024*                      GENERERA EN ORDER                                  
000025*                   4) DATAELEMENTEN KVPB-REF, FLREFBEO                   
000026*                      SAMT TEARTNOT KOMMER ATT UPPDATERAS                
000027*                      SÄTTS MANUELL PROGNOS PÅ                           
000028*                      PASSIV ARTIKEL AKTIVERAS DEN                       
000029*                                                                         
000030*        PF 7          BLÄDDRING BAKÅT                                    
000031*        PF 8          BLÄDDRING FRAMÅT (ETT ORDERFÖRSLAG LIGGER          
000032*                      KVAR PÅ WDE3 SOM ICKE BEHANDLAT OM MAN EJ          
000033*                      UPPDATERAR MED PF11 FÖRST, MED KVANT = 0)          
000034*                                                                         
000035*        PROGRAMMET UPPDATERAR WDK7                                       
000036*                              WLUSEA (WDP7)                              
000037*                              WLORDL (WDE3)                              
000038*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
000039*                              WLORD  (WDA5)                              
000040*                              WLARTC (WDK6)                              
000041*                                     (WDN6)                              
000042*                                     (WDD7)                              
000043*                              WLARTM (WDK9)                              
000044*                              WDL7 + WDL4                                
000045*                                      WDB6                               
000046*    INDATA.                                                              
000047*        TRANSAKTION: W2T352                                              
000048*        MID:         W2I35201                                            
000049*                                                                         
000050*    UTDATA.                                                              
000051*        MOD:         W2O35201                                            
000052*                                                                         
000053*   ÄNDRINGAR:                                                            
000054*        03-05-15. TILLAGT FUNKTION FÖR ATT BEGRÄNSA INFORMATION          
000055*                  FÖR USER VARS SEC-IDLEVNR PÅ USER-BASEN                
000056*                  INTE ÄR LIKA MED HUVUDLEVERANTÖREN.                    
000057*                  ( SEC-IDLEVNR = SPACE, FÅR SE ALLT )    /C.E.          
000058*                                                                         
000059*   CHANGELOG:                                                            
000060*        06-10-16. CHANGED THE SCREEN TO ACCPET DC AS INPUT AND           
000061*                  DISPLAY PROPOSALS AS PER THE DC GROUP ON 2365          
000062*        07-OCT-2021:STORY 2224095 CHK KVDISP-SEND-DC FOR                 
000063*                    KDERS >= 10 FOR EXTENDED DC'S AND 7*,4*,5*           
000064*                    SENDING DC'S                                         
000069                                                                          
000070     SKIP3                                                                
000071 ENVIRONMENT DIVISION.                                                    
000072     EJECT                                                                
000073 DATA DIVISION.                                                           
000074 WORKING-STORAGE SECTION.                                                 
000075*    -COPY WY2000W1                                                       
000076     SKIP3                                                                
000077 77  IDPGM                       PIC X(08)   VALUE 'W2035200'.            
000078                                                                          
000079*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
000080 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
000081                                                                          
000082 77  YES                         PIC X       VALUE 'Y'.                   
000083 77  JA                          PIC X       VALUE 'J'.                   
000084 77  NEJ                         PIC X       VALUE 'N'.                   
000085 77  FL-PRARTBES                 PIC X(1)    VALUE 'N'.                   
000086 77  WS-PRARTBES-PR              PIC S9(7)V99 COMP-3.                     
000087 77  W-IDDC-SPAR                 PIC X(2)    VALUE SPACE.                 
000088                                                                          
000089*01  -COPY WWDCKONS                                                       
000090*                                                                         
000091*01  -COPY WWDC99                                                         
000092*                                                                         
000093*01  -COPY WWDCLAND                                                       
000094                                                                          
000095 77  AKTIV                       PIC X       VALUE 'A'.                   
000096 77  PASSIV                      PIC X       VALUE 'P'.                   
000097 77  DEFINITIV                   PIC S9      VALUE +1 COMP-3.             
000098 77  MOD-IX                      PIC 9(3)    VALUE ZERO.                  
000099 77  IX                          PIC 9(3)    VALUE ZERO.                  
000100 77  IX2                         PIC 9(3)    VALUE ZERO.                  
000101 77  INDX                        PIC S9(3)   VALUE ZERO.                  
000102 77  IX-DC                       PIC 9(3)    VALUE ZERO.                  
000103 77  IX-DC-N                     PIC 9(3)    VALUE ZERO.                  
000104 77  IX-DC-MAX                   PIC 9(3)    VALUE ZERO.                  
000105 77  IX-VV                       PIC 9(2)    VALUE ZERO.                  
000106 77  IX-CD                       PIC 9(3)    VALUE ZERO.                  
000107 77  WS-KDARBTYP-SEC             PIC X(8)    VALUE SPACE.                 
000108 77  SPRAK-IX                    PIC 9(3)    VALUE ZERO.                  
000109 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
000110 77  DAGENS-DATUM-SEKEL          PIC 9(8)    VALUE ZERO.                  
000111 77  SW-KVPB-SEP                 PIC X       VALUE ' '.                   
000112     88  SW-KVPB-SEP-JA                      VALUE 'J'.                   
000113     88  SW-KVPB-SEP-NEJ                     VALUE 'N'.                   
000114 77  SW-KVPB-PLAN                PIC X       VALUE ' '.                   
000115     88  KVPB-PLAN-UPD-JA                    VALUE 'J'.                   
000116     88  KVPB-PLAN-UPD-NEJ                   VALUE 'N'.                   
000117 77  SW-K712-SEGMENT             PIC X       VALUE 'N'.                   
000118     88  K712-EXIST                          VALUE 'J'.                   
000119     88  KVPB-MISSING                        VALUE 'N'.                   
000120 77  SW-LYNK-PART                PIC X       VALUE 'N'.                   
000121     88  LYNK-PART                           VALUE 'J'.                   
000122                                                                          
000123*                                                                         
000124 01  DAGENS-PER                  PIC 9(4)   VALUE ZERO.                   
000125 01  DAG-PER REDEFINES DAGENS-PER.                                        
000126         05 DAGENS-AA            PIC 9(2).                                
000127         05 DAGENS-PP            PIC 9(2).                                
000128 77  DAGENS-AAR                  PIC 9(4)    VALUE ZERO.                  
000129 77  DAGENS-VECKA                PIC 9(2)    VALUE ZERO.                  
000130                                                                          
000131 01  WS.                                                                  
000132  05 WS-TEMFSINF.                                                         
000133    10 WS-TEMFSINF-SOURCE        PIC X(7)    VALUE SPACE.                 
000134    10 FILLER                    PIC X(2)    VALUE SPACE.                 
000135    10 FILLER                    OCCURS 4.                                
000136      15 WS-TEMFSINF-NDC         PIC X(7)    VALUE SPACE.                 
000137      15 FILLER                  PIC X       VALUE SPACE.                 
000138    10 WS-TEMFSINF-TEXT          PIC X(13)   VALUE SPACE.                 
000139                                                                          
000140  05 WS-TEMF-RED-OS.                                                      
000141    10 WS-TEMF-TEXT-OS           PIC X(3)    VALUE SPACE.                 
000142    10 WS-TEMF-OS-NDC            OCCURS 4                                 
000143                                 PIC X(3)    VALUE SPACE.                 
000144  05 WS-TEMF-RED-INVBAL.                                                  
000145    10 FILLER                    PIC X(5)    VALUE 'INVB '.               
000146    10 WS-TEMF-UTRSALDO          PIC -(6)9.                               
000147    10 FILLER                    PIC X       VALUE SPACE.                 
000148                                                                          
000149  05 WS-TEMF-UTRSALDO-NUM        PIC S9(7).                               
000150                                                                          
000151  05 WS-ANT-VV                   PIC  9(2)   VALUE ZERO.                  
000152  05 WS-TIAAVV.                                                           
000153    10 WS-AAR                    PIC  9(2)   VALUE ZERO.                  
000154    10 WS-VV                     PIC  9(2)   VALUE ZERO.                  
000155  05 TIAAVV REDEFINES WS-TIAAVV PIC 9(4).                                 
000156  05 WS-TIAAPER.                                                          
000157    10 TIAA                      PIC  9(2)   VALUE ZERO.                  
000158    10 PER                       PIC  9(2)   VALUE ZERO.                  
000159  05    TIAAPER REDEFINES WS-TIAAPER PIC 9(4).                            
000160  05 WS-FOM-TOM.                                                          
000161    10 WS-FOM                    PIC  X(2)   VALUE ZERO.                  
000162    10 WS-STRECK                 PIC  X(1)   VALUE '-'.                   
000163    10 WS-TOM                    PIC  X(2)   VALUE ZERO.                  
000164*                                                                         
000165*   WS-TABELL ÄR EN RULLANDE TABELL DÄR                                   
000166*   IX = 1 ÄR DAGENS PERIOD ETT ÅR TILLBAKA                               
000167*   IX = 12 ÄR FÖRRA PERIODEN                                             
000168*                                                                         
000169  05 WS-TABELL    OCCURS 12.                                              
000170    10 WS-PER                    PIC  9(2)   VALUE ZERO.                  
000171    10 WS-FORSTA-V               PIC  9(2)   VALUE ZERO.                  
000172    10 WS-SISTA-V                PIC  9(2)   VALUE ZERO.                  
000173    10 WS-KVOI                   PIC S9(7)   VALUE ZERO.                  
000174  05 WS-BALANCE                  PIC S9(7)   VALUE ZERO.                  
000175  05 WS-TRANSF-BALANCE           PIC S9(7)   VALUE ZERO.                  
000176  05 WS-REST                     PIC S9(7)   VALUE ZERO.                  
000177  05 WS-SLASK                    PIC S9(7)   VALUE ZERO.                  
000178  05 WS-KDERS                    PIC 9(3)    VALUE ZERO.                  
000179  05 WS-FOREG-AAR                PIC  9(4)   VALUE ZERO.                  
000180  05 WS-KVROS                    PIC S9(7)   VALUE ZERO.                  
000181  05 WS-ANTAL-POSTER             PIC S9(7)   VALUE ZERO.                  
000182  05 WS-ANTAL-VECKOR             PIC S9(3)   VALUE ZERO.                  
000183  05 WS-KVOI-SUM                 PIC S9(7)   VALUE ZERO.                  
000184  05 WS-KVAVIS                   PIC S9(9)   VALUE ZERO.                  
000185  05 WS-KVART-TOT-C1             PIC S9(9)   VALUE ZERO  COMP-3.          
000186  05 WS-KVQPACK-3                PIC S9(5)   VALUE ZERO  COMP-3.          
000187  05 WS-ANTAL                    PIC 9(9)    VALUE ZERO.                  
000188  05 WS-DAPRLIST                 PIC 9(8)    VALUE ZERO.                  
000189  05 WS-SPARA-IDARTNR            PIC 9(9)    VALUE ZERO.                  
000190  05 WS-PRMATRL                  PIC S9(7)V9(2)                           
000191                                             VALUE ZERO  COMP-3.          
000192  05 WS-AVER-COST-NUM            PIC 9(7)V9(2)                            
000193                                             VALUE ZERO.                  
000194  05 WS-VKART                    PIC S9(7)   VALUE ZERO COMP-3.           
000195  05 WS-VLARTNTO                 PIC S9(8)V9(1) VALUE ZERO COMP-3.        
000196  05 WS-AVER-COST-RED            PIC Z(4)9.9(2).                          
000197  05 WS-KR-VIKT                  PIC S9(9)V9(2)                           
000198                                             VALUE ZERO.                  
000199  05 WS-KR-VIKT-RED              PIC  9(9)   VALUE ZERO.                  
000200  05 WS-KR-VOLYM                 PIC S9(9)V9(2)                           
000201                                             VALUE ZERO.                  
000202  05 WS-KR-VOLYM-RED             PIC  9(9)   VALUE ZERO.                  
000203  05 WS-KVAKS                    PIC S9(9)V9(2)                           
000204                                             VALUE ZERO.                  
000205  05 WS-KVPB                     PIC S9(9)V9(2)                           
000206                                             VALUE ZERO.                  
000207  05 WS-TOTAL-ANTAL              PIC 9(9)    VALUE ZERO.                  
000208  05 WS-INDEX                    PIC S9V9(2) VALUE ZERO.                  
000209  05 WS-DASPSEA                  PIC 9(7)    VALUE ZERO.                  
000210  05 WS-OSAKERHET                PIC 9(2)V9  VALUE ZERO.                  
000211  05 WS-SIMIX-SUM                PIC S9(2)V9(2)                           
000212                                             VALUE ZERO.                  
000213  05 WS-SIMIX                    PIC S9(2)V9(2)                           
000214                                             VALUE ZERO.                  
000215  05 WS-SIMIX-X                  PIC X(4).                                
000216  05 WS-SIMIX-N                  REDEFINES WS-SIMIX-X                     
000217                                 PIC 9.9(2).                              
000218  05 WS-SUPERWEEK                OCCURS 4                                 
000219                                 PIC S9(9)V9(2)                           
000220                                             VALUE ZERO.                  
000221  05 WS-AVAILABLE                PIC S9(7)   VALUE ZERO.                  
000222  05 WS-ORDERED                  PIC  9(7)   VALUE ZERO.                  
000223  05 WS-KVAKS-SDC                PIC  9(7)   VALUE ZERO.                  
000224  05 WS-PURCHQTY                 OCCURS 4                                 
000225                                 PIC  9(7)   VALUE ZERO.                  
000226  05 W-PURCHQTY                  OCCURS 4                                 
000227                                 PIC  9(7)   VALUE ZERO.                  
000228  05 WS-PURCHQTY-SIM             OCCURS 4                                 
000229                                 PIC  9(7)   VALUE ZERO.                  
000230  05 WS-RED-PURCHQTY             PIC Z(6)9   VALUE ZERO.                  
000231  05 WS-PURCHQTY-DC1             PIC  9(7)   VALUE ZERO.                  
000232  05 WS-PURCHQTY-DC2             PIC  9(7)   VALUE ZERO.                  
000233  05 WS-PURCHQTY-DC3             PIC  9(7)   VALUE ZERO.                  
000234  05 WS-PURCHQTY-DC4             PIC  9(7)   VALUE ZERO.                  
000235  05 WS-KVPB-REF-SLAG            OCCURS 4    PIC S9(6)V9(1)               
000236                                             VALUE ZERO COMP-3.           
000237  05 WS-KVPB-REF                 OCCURS 4                                 
000238                                 PIC  9(6)V9 VALUE ZERO.                  
000239  05 WS-KVPBREOI                 OCCURS 4                                 
000240                                 PIC  9(6)V9 VALUE ZERO.                  
000241  05 WS-KVPB-TOT                 OCCURS 4                                 
000242                                 PIC  9(6)V9 VALUE ZERO.                  
000243  05 WS-KVPBREOI-SLAG            PIC  9(6)V9 VALUE ZERO.                  
000244  05 WS-RED-KVPB-REF             PIC Z(4)9.9 VALUE ZERO.                  
000245  05 WS-RED-KVPBREOI             PIC Z(4)9.9 VALUE ZERO.                  
000246  05 WS-OVERLAGER                OCCURS 4                                 
000247                                 PIC X       VALUE 'N'.                   
000248  05 WS-ANT-REVIEW               PIC S9(5)   VALUE ZERO.                  
000249  05 WS-KTRL-PRIO                PIC 9(2)    VALUE ZERO.                  
000250  05 WS-FLSIM                    PIC X       VALUE SPACES.                
000251  05 WS-COUNT                    PIC 9       VALUE ZERO.                  
000252  05 WS-RESTKVANT                OCCURS 4                                 
000253                                 PIC S9(7)   VALUE ZERO.                  
000254  05 WS-FLAGGA-FCD               PIC X       VALUE SPACE.                 
000255  05 WS-FLAGGA-FCD-UPD           OCCURS 4                                 
000256                                 PIC X       VALUE SPACE.                 
000257  05 WS-IDREFTYP                 PIC X       VALUE SPACE.                 
000258  05 WS-KVOKS-TOT                PIC S9(7)   VALUE ZERO COMP-3.           
000259  05 WS-HELTAL-BEST              PIC S9(7)   VALUE ZERO COMP-3.           
000260  05 WS-HELTAL-SALDO             PIC S9(7)   VALUE ZERO COMP-3.           
000261  05 WS-SALDO                    PIC S9(7)   VALUE ZERO COMP-3.           
000262  05 WS-DC-TABELL.                                                        
000263   10  WS-DC-1                   PIC X(2)    VALUE SPACES.                
000264   10  WS-DC-2                   PIC X(2)    VALUE SPACES.                
000265   10  WS-DC-3                   PIC X(2)    VALUE SPACES.                
000266   10  WS-DC-4                   PIC X(2)    VALUE SPACES.                
000267  05 FILLER REDEFINES            WS-DC-TABELL.                            
000268   10  WS-DC-NR                  OCCURS 4                                 
000269                                 PIC X(2).                                
000270  05 WS-SENDING-DC.                                                       
000271   10  WS-SENDING-DC-1           PIC X(2)    VALUE SPACES.                
000272   10  WS-SENDING-DC-2           PIC X(2)    VALUE SPACES.                
000273   10  WS-SENDING-DC-3           PIC X(2)    VALUE SPACES.                
000274   10  WS-SENDING-DC-4           PIC X(2)    VALUE SPACES.                
000275  05 FILLER REDEFINES            WS-SENDING-DC.                           
000276   10  WS-SENDING-DC-NR          OCCURS 4                                 
000277                                 PIC X(2).                                
000278  05 WS-IDDISTR                  OCCURS 4                                 
000279                                 PIC 9(4)    VALUE ZERO.                  
000280  05 WS-IDDISTR-B6               OCCURS 4                                 
000281                                 PIC 9(4)    VALUE ZERO.                  
000282  05 WS-IDDISTR-2-DEL            OCCURS 4                                 
000283                                 PIC 9(4)    VALUE ZERO.                  
000284  05 WS-IDKUNDNR                 OCCURS 4                                 
000285                                 PIC S9(7)   VALUE ZERO COMP-3.           
000286  05 WS-IDDC-FROM                OCCURS 4                                 
000287                                 PIC X(2)    VALUE SPACE.                 
000288  05 WS-REF-KDREFTXT             OCCURS 4                                 
000289                                 PIC 9(2)    VALUE ZERO.                  
000290                                                                          
000291  05 WS-IDLEVNR-8                PIC X(8)    VALUE SPACE.                 
000292  05 WS-KDARBTYP-X3              PIC X(3)    VALUE SPACE.                 
000293  05 WS-CDC-11                   PIC X(2)    VALUE '11'.                  
000294  05 WS-IDDC-REF                 PIC X(2)    VALUE '  '.                  
000295  05 WS-IDDC-REF-TEST            PIC X(2)    VALUE '  '.                  
000296  05 W-K6-IDDC-REF               PIC X(2)    VALUE '  '.                  
000297  05 WS-FLREFERAL                PIC X       VALUE 'N'.                   
000298  05 ws-where                    PIC X       VALUE space.                 
000299  05 WS-KVDISP-SEND-DC           PIC S9(7)   VALUE ZERO COMP-3.           
000300                                                                          
000301  05 WS-REAIRCO                  PIC S9(6)V9(1) VALUE ZERO COMP-3.        
000302  05 WS-PRFRAKT                  PIC S9(7)      VALUE ZERO COMP-3.        
000303  05 WS-AIR-COST-SEK             PIC 9(7) VALUE ZERO.                     
000304*********************************************************                 
000305*    WS-MSGI-AREA-2352                                                    
000306*           ANVÄNDS FÖR ATT SPARA PÅ NYCKELDATABASEN WDP7                 
000307*           (I MSGI-SPAR-AREA)                                            
000308*********************************************************                 
000309  05 WS-MSGI-AREA-2352.                                                   
000310    10 WS-MSGI-IDTRANS-2352      PIC X(4)    VALUE '2352'.                
000311    10 WS-MSGI-IDARTNR-ENTER     PIC  9(9)   VALUE ZERO.                  
000312    10 WS-MSGI-ORDER-ENTER       PIC X       VALUE SPACE.                 
000313    10 WS-MSGI-IDARTNR-PF7       PIC  9(9)   VALUE ZERO.                  
000314    10 WS-MSGI-ORDER-PF7         PIC X       VALUE SPACE.                 
000315    10 WS-MSGI-IDTYPE            PIC X       VALUE SPACE.                 
000316                                                                          
000317*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
000318                                                                          
000319  05 WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
000320  05 WS-IDARTNR-NUM              REDEFINES WS-IDARTNR                     
000321                                 PIC 9(9).                                
000322  05 IDDC-WS                     PIC X(2)    VALUE SPACE.                 
000323  05 IDDC-WS-NUM                 REDEFINES IDDC-WS                        
000324                                 PIC 9(2).                                
000325  05 WS-IDPERSON-BUY             PIC X(3)    VALUE SPACE.                 
000326  05 WS-IDPERSON-BUY-NUM         REDEFINES WS-IDPERSON-BUY                
000327                                 PIC 9(3).                                
000328  05 WS-IDPERSON-BUY-RED         PIC Z(2)9.                               
000329  05 WS-IDTYPE                   PIC X       VALUE SPACE.                 
000330  05 WS-STATUS                   PIC X       VALUE SPACE.                 
000331  05 WS-DISPLAY                  PIC X(30)   VALUE SPACE.                 
000332  05 WS-SEND-IDLEVNR             PIC X(5)    VALUE SPACE.                 
000333                                                                          
000334  05 FILLER                      PIC X(16)   VALUE                        
000335                                             'WS-IMS-SEKTION'.            
000336  05 WS-IMS-SEKTION              PIC X(24)   VALUE SPACE.                 
000337  05 FILLER                      PIC X(16)   VALUE                        
000338                                             'WS-DB2-SEKTION'.            
000339  05 WS-DB2-SEKTION              PIC X(24)   VALUE SPACE.                 
000340  05 WS-MEDD-ERS.                                                         
000341    07 WS-MEDD-ERSKOD            PIC 9(2)    VALUE ZERO.                  
000342    07 FILLER                    PIC X(1)    VALUE SPACE.                 
000343    07 WS-MEDD-TEXT              PIC X(23)   VALUE SPACE.                 
000344  05 WS-FLERSATT-X2X3            PIC X(1)    VALUE SPACE.                 
000345     EJECT                                                                
000346 01 NYCKLAR-TP4TRAN.                                                      
000347     03 WS-IDDC-SEND             PIC X(2)    VALUE SPACE.                 
000348     03 WS-IDDC-REC              PIC X(2)    VALUE SPACE.                 
000349                                                                          
000350 77  SW-TRANSF                   PIC X       VALUE 'N'.                   
000351     88  SW-TRANSF-JA                        VALUE 'J'.                   
000352     88  SW-TRANSF-NEJ                       VALUE 'N'.                   
000353                                                                          
000354 77  SW-TRAEFF                   PIC X       VALUE 'J'.                   
000355     88  SW-TRAEFF-JA                        VALUE 'J'.                   
000356     88  SW-TRAEFF-NEJ                       VALUE 'N'.                   
000357                                                                          
000358 77  SW-SEASON                   PIC X       VALUE ' '.                   
000359     88  SW-SEASON-JA                        VALUE 'J'.                   
000360     88  SW-SEASON-NEJ                       VALUE 'N'.                   
000361                                                                          
000362 77  SW-KTRL-ERS                 PIC X       VALUE ' '.                   
000363     88  SW-KTRL-ERS-JA                      VALUE 'J'.                   
000364     88  SW-KTRL-ERS-NEJ                     VALUE 'N'.                   
000365                                                                          
000366 77  SW-HAEMTA-INPUT-FAELT       PIC X       VALUE 'J'.                   
000367     88  SW-HAEMTA-INPUT-FAELT-JA            VALUE 'J'.                   
000368     88  SW-HAEMTA-INPUT-FAELT-NEJ           VALUE 'N'.                   
000369                                                                          
000370 77  SW-KVPB-REF-INPUT           PIC X       VALUE 'N'.                   
000371     88  SW-KVPB-REF-INPUT-JA                VALUE 'J'.                   
000372     88  SW-KVPB-REF-INPUT-NEJ               VALUE 'N'.                   
000373                                                                          
000374 77  VALID-IDDC-SW               PIC X       VALUE 'N'.                   
000375     88  VALID-IDDC-JA                       VALUE 'J'.                   
000376     88  VALID-IDDC-NEJ                      VALUE 'N'.                   
000377                                                                          
000378 77  CRITICAL-AIR-SW             PIC X       VALUE 'N'.                   
000379     88  CRITICAL-AIR-JA                     VALUE 'J'.                   
000380     88  CRITICAL-AIR-NEJ                    VALUE 'N'.                   
000381                                                                          
000382 77  INDATA-SW                   PIC X       VALUE 'J'.                   
000383     88  INDATA-OK                           VALUE 'J'.                   
000384     88  INDATA-FEL                          VALUE 'N'.                   
000385                                                                          
000386 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
000387     88  NYCKLAR-OK                          VALUE 'J'.                   
000388     88  NYCKLAR-FEL                         VALUE 'N'.                   
000389                                                                          
000390 77  SIM-INDEX-SW                PIC X       VALUE 'N'.                   
000391     88  SIM-INDEX-JA                        VALUE 'J'.                   
000392     88  SIM-INDEX-NEJ                       VALUE 'N'.                   
000393                                                                          
000394 77  SIM-ANTAL-SW                PIC X       VALUE 'N'.                   
000395     88  SIM-ANTAL-JA                        VALUE 'J'.                   
000396     88  SIM-ANTAL-NEJ                       VALUE 'N'.                   
000397                                                                          
000398 77  UPD-REQ-SW                  PIC X       VALUE 'N'.                   
000399     88  UPD-REQ-JA                          VALUE 'J'.                   
000400     88  UPD-REQ-NEJ                         VALUE 'N'.                   
000401                                                                          
000402 77  WDK7-FINNS-SW               PIC X       VALUE 'N'.                   
000403     88  WDK7-FINNS                          VALUE 'J'.                   
000404     88  WDK7-FINNS-EJ                       VALUE 'N'.                   
000405                                                                          
000406 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
000407     88  EGEN-MID                            VALUE '2352'.                
000408     88  GODK-MID                            VALUE '2351' '2352'          
000409                                                   '2353' '2354'          
000410                                                   '2355' '2356'          
000411                                                   '2357' '2358'          
000412                                                   '2359' '6322'.         
000413     88  HELP-MID                            VALUE '0551'.                
000414                                                                          
000415 77  SECURITY-SW                 PIC X       VALUE 'N'.                   
000416     88  PASSED-SECURITY-CHECK               VALUE 'J'.                   
000417     88  BLOCKED-SECURITY-CHECK              VALUE 'N'.                   
000418                                                                          
000419     EJECT                                                                
000420 01  FILLER                      PIC X(16) VALUE 'REFILLFÖRSLAG'.         
000421     SKIP3                                                                
000422*01  -COPY W271RTXT                                                       
000423     EJECT                                                                
000424*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
000425 01  GENERELLA-SUBPROGRAM.                                                
000426     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
000427     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
000428     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000429     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000430     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
000431     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
000432     03  W271REFL                PIC X(8)    VALUE 'W271REFL'.            
000433     03  W271UTIL                PIC X(8)    VALUE 'W271UTIL'.            
000434     03  W271UTUP                PIC X(8)    VALUE 'W271UTUP'.            
000435     03  W272UTUP                PIC X(8)    VALUE 'W272UTUP'.            
000436     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
000437     EJECT                                                                
000438*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
000439*01 -COPY WMEDAREA                                                        
000440     EJECT                                                                
000441*    --- COPYTEXT TILL SUBPROGRAM WDECEDIT                                
000442*01  -COPY WDECAREA                                                       
000443     EJECT                                                                
000444*    --- PARAMETRAR TILL W271REFL                                         
000445*01 -COPY W271REFL                                                        
000446     EJECT                                                                
000447*    --- PARAMETRAR TILL W271UTIL                                         
000448*01 -COPY W271UTIL                                                        
000449     EJECT                                                                
000450*    --- PARAMETRAR TILL W271UTUP                                         
000451*01 -COPY W271UTUP       -PRE W271-                                       
000452     EJECT                                                                
000453*    --- PARAMETRAR TILL W272UTUP                                         
000454*01 -COPY W272UTUP       -PRE W272-                                       
000455     EJECT                                                                
000456 01  MESSAGE-CODES.                                                       
000457     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
000458     03  CONFLICT                PIC X(3)    VALUE '002'.                 
000459     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
000460     03  URVAL-SAKNAS            PIC X(3)    VALUE '005'.                 
000461     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
000462     03  UPD-NOT-ALLOWED         PIC X(3)    VALUE '007'.                 
000463     03  ERR-NOT-REGISTERED      PIC X(3)    VALUE '010'.                 
000464     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
000465     03  ARTIKEL-SAKNAS          PIC X(3)    VALUE '017'.                 
000466     03  ARTIKEL-UTGANGEN        PIC X(3)    VALUE '018'.                 
000467     03  INF-UPDATE-NOT-DONE     PIC X(3)    VALUE '034'.                 
000468     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
000469     03  ARTIKEL-ERSATT          PIC X(3)    VALUE '220'.                 
000470     03  ARTIKEL-EJ-AKTIV        PIC X(3)    VALUE '244'.                 
000471     03  PRIS-SAKNAS             PIC X(3)    VALUE '301'.                 
000472     03  ARTIKEL-SAKNAS-SDC      PIC X(3)    VALUE '305'.                 
000473     03  DIREKTLEV               PIC X(3)    VALUE '306'.                 
000474     03  EJ-GODK-REFILL          PIC X(3)    VALUE '307'.                 
000475     03  ERR-HIGH-AIR-COST       PIC X(3)    VALUE '369'.                 
000476     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
000477     03  ERR-NOT-AUTHORIZED      PIC X(3)    VALUE '405'.                 
000478     03  NOT-REFILL-PART         PIC X(3)    VALUE '957'.                 
000479                                                                          
000480                                                                          
000481                                                                          
000482 01  MEDDELANDE.                                                          
000483     03  MED-1                  PIC X(30)                                 
000484         VALUE 'TYPE : A,B,C OR L             '.                          
000485     03  MED-2                  PIC X(30)                                 
000486         VALUE 'STATUS : R OR N               '.                          
000487     03  MED-3                  PIC X(30)                                 
000488         VALUE 'FORECAST WRONG                '.                          
000489     03  MED-4                  PIC X(30)                                 
000490         VALUE 'PURCHQTY WRONG                '.                          
000491     03  MED-5                  PIC X(30)                                 
000492         VALUE 'AUT REFILL ORDERING WRONG     '.                          
000493     03  MED-6                  PIC X(30)                                 
000494         VALUE 'CAN NOT UPDATE WITH NEW KEY   '.                          
000495     03  MED-7                  PIC X(30)                                 
000496         VALUE 'OT: A,B OR L                  '.                          
000497     03  MED-8                  PIC X(30)                                 
000498         VALUE 'CONFLICT OT/VENDOR            '.                          
000499     03  MED-9                  PIC X(30)                                 
000500         VALUE 'NO VALID PRICE                '.                          
000501     03  MED-10                 PIC X(30)                                 
000502         VALUE 'UNEVEN MULTIPEL OF Q1         '.                          
000503     03  MED-11                 PIC X(30)                                 
000504         VALUE 'DC NOT A VALID TRANSFER       '.                          
000505     03  MED-12                 PIC X(30)                                 
000506         VALUE 'PURCHQTY WRONG TRANSFER       '.                          
000507     03  MED-13                 PIC X(30)                                 
000508         VALUE 'TRANSFER NOT ALLOWED          '.                          
000509     03  MED-14                 PIC X(30)                                 
000510         VALUE 'MIXED OT NOT ALLOWED          '.                          
000511     03  MED-15                 PIC X(30)                                 
000512         VALUE 'NOT ALLOWED FOR LOCAL PARTS   '.                          
000513     03  MED-16                 PIC X(30)                                 
000514         VALUE 'NDC-NA ONLY                   '.                          
000515     03  MED-17                 PIC X(30)                                 
000516         VALUE 'PART MARKED AS AIRFREIGHT ONLY'.                          
000517     03  MED-18                 PIC X(30)                                 
000518         VALUE 'DISTRICT NOT FOUND            '.                          
000519     03  MED-19                  PIC X(30)                                
000520         VALUE 'LYNK & CO PART                '.                          
000521     03  MED-20                 PIC X(30)                                 
000522         VALUE 'PART LOCKED FOR AIRFREIGHT    '.                          
000523                                                                          
000524     EJECT                                                                
000525*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
000526*01  -COPY WDATAREA                                                       
000527     EJECT                                                                
000528*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
000529*                                                                         
000530 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
000531     SKIP3                                                                
000532*01 -COPY WMSGINIT                                                        
000533     EJECT                                                                
000534*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
000535*                                                                         
000536 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
000537     SKIP3                                                                
000538*01  MID -COPY W2I35201                                                   
000539     EJECT                                                                
000540*    --- VID HOPP FRÅN 2351 ANVÄNDS W2I35101                              
000541*    ---                                                                  
000542*01  MID -COPY W2I35101                                                   
000543     EJECT                                                                
000544 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
000545     SKIP3                                                                
000546*01  -COPY WMSGAREA                                                       
000547     EJECT                                                                
000548     03  MOD REDEFINES MSG-AREA.                                          
000549*      05  -COPY W2O35201                                                 
000550     EJECT                                                                
000551 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
000552     SKIP3                                                                
000553*01  -COPY WMFSAREA                                                       
000554     EJECT                                                                
000555 01  FILLER                      PIC X(16)  VALUE 'SQLCA-AREA'.           
000556       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
000557                                                                          
000558 01  FILLER                      PIC X(16)  VALUE 'SQLCODE-WS'.           
000559 01  DB2-WS.                                                              
000560     03  SQLCODE-WS              PIC 9(3)   VALUE ZERO.                   
000561         88  CURSOR-OK                      VALUE 000.                    
000562         88  LINES-FOUND                    VALUE 000.                    
000563         88  LINES-MISSING                  VALUE 100.                    
000564         88  RESOURCE-WRONG                 VALUE 904.                    
000565     03  GOOD-SQLCODECODES.                                               
000566         05  GOOD-SQLCODE OCCURS 5                                        
000567             INDEXED BY SQLCODE-IX PIC 9(3).                              
000568 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
000569     EJECT                                                                
000570*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
000571*                                                                         
000572     EJECT                                                                
000573 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000574     SKIP3                                                                
000575 01  NYCKLAR-TILL-DLI.                                                    
000576     03  W-IDARTNR-X.                                                     
000577         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
000578     03  W-IDDC-X.                                                        
000579         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
000580     03  W-IDDCREF-X.                                                     
000581         05  W-IDDC-REF          PIC X(2)    VALUE SPACE.                 
000582     03  W-IDLAND-X.                                                      
000583         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
000584     03  W-IDDC-B6-X.                                                     
000585         05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
000586     03  W-IDDC-B616-X.                                                   
000587         05  W-IDDC-B616         PIC X(2)    VALUE SPACE.                 
000588     03  W-IDDC-K7-MIN-X.                                                 
000589         05  W-IDDC-K7-MIN       PIC X(2)    VALUE SPACE.                 
000590     03  W-IDDC-K7-MAX-X.                                                 
000591         05  W-IDDC-K7-MAX       PIC X(2)    VALUE SPACE.                 
000592     03  W-IDUSER-X.                                                      
000593         05  W-IDUSER            PIC X(8)    VALUE SPACE.                 
000594     03  W-IDSKYLT-X.                                                     
000595         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
000596     03  W-IDLEVNR-21-X.                                                  
000597         05  W-IDLEVNR-21        PIC X(5)    VALUE LOW-VALUE.             
000598     03  W-DAPRLIST-21-N.                                                 
000599         05  W-DAPRLIST-21       PIC 9(8)    VALUE ZERO.                  
000600                                                                          
000601     03  W-KDNOTTYP-X.                                                    
000602         05  W-KDNOTTYP           PIC  S9(01)  COMP-3 VALUE ZERO.         
000603                                                                          
000604     03  W-IDDC-TP5-X.                                                    
000605         05  W-IDDC-TP5          PIC X(2)  VALUE SPACES.                  
000606                                                                          
000607     03 W-WDE301KY-X.                                                     
000608         05  W-IDDC-301          PIC X(2)  VALUE SPACE.                   
000609         05  W-IDPERSON-BUY      PIC S9(3) VALUE ZERO COMP-3.             
000610         05  W-KDREFTYP          PIC X     VALUE SPACE.                   
000611         05  W-IDARTNR-301       PIC S9(9) VALUE ZERO COMP-3.             
000612         05  W-IDDISTR           PIC S9(5) VALUE ZERO COMP-3.             
000613                                                                          
000614     03 W-WDE301KY-DEL.                                                   
000615         05  W-IDDC-301-DEL      PIC X(2)  VALUE SPACE.                   
000616         05  W-IDPERSON-BUY-DEL  PIC S9(3) VALUE ZERO COMP-3.             
000617         05  W-KDREFTYP-DEL      PIC X     VALUE SPACE.                   
000618         05  W-IDARTNR-301-DEL   PIC S9(9) VALUE ZERO COMP-3.             
000619         05  W-IDDISTR-DEL       PIC S9(5) VALUE ZERO COMP-3.             
000620                                                                          
000621     03 W-WDE301KY-MIN-X.                                                 
000622         05  W-IDDC-301-MIN      PIC X(2)  VALUE SPACE.                   
000623         05  W-IDPERSON-BUY-301-MIN                                       
000624                                 PIC S9(3) VALUE ZERO COMP-3.             
000625         05  W-KDREFTYP-301-MIN  PIC X     VALUE SPACE.                   
000626         05  W-IDARTNR-301-MIN   PIC S9(9) VALUE ZERO COMP-3.             
000627         05  W-IDDISTR-301-MIN   PIC S9(5) VALUE ZERO COMP-3.             
000628                                                                          
000629     03 W-WDE301KY-MAX-X.                                                 
000630         05  W-IDDC-301-MAX      PIC X(2)  VALUE HIGH-VALUE.              
000631         05  W-IDPERSON-BUY-301-MAX                                       
000632                                 PIC S9(3) VALUE +999 COMP-3.             
000633         05  W-KDREFTYP-301-MAX  PIC X     VALUE HIGH-VALUE.              
000634         05  W-IDARTNR-301-MAX   PIC S9(9)                                
000635                                         VALUE +999999999 COMP-3.         
000636         05  W-IDDISTR-301-MAX   PIC S9(5) VALUE +99999 COMP-3.           
000637                                                                          
000638     03 W-WDE3A1KY-MIN-X.                                                 
000639         05  W-KDREFTYP-MIN      PIC X     VALUE SPACE.                   
000640         05  W-IDPERSON-BUY-MIN  PIC S9(3) VALUE ZERO COMP-3.             
000641         05  W-IDARTNR-MIN       PIC S9(9) VALUE ZERO COMP-3.             
000642         05  W-IDDC-MIN          PIC X(2)  VALUE SPACE.                   
000643                                                                          
000644     03 W-WDE3A1KY-MAX-X.                                                 
000645         05  W-KDREFTYP-MAX      PIC X     VALUE HIGH-VALUE.              
000646         05  W-IDPERSON-BUY-MAX  PIC S9(3) VALUE +999 COMP-3.             
000647         05  W-IDARTNR-MAX       PIC S9(9)                                
000648                                          VALUE +999999999 COMP-3.        
000649         05  W-IDDC-MAX          PIC X(2)  VALUE HIGH-VALUE.              
000650                                                                          
000651                                                                          
000652     03  W-WDA5A1KY-MIN.                                                  
000653         05  W-IDARTNR-N3-MIN     PIC S9(9) COMP-3   VALUE ZERO.          
000654         05  FILLER               PIC X(35)          VALUE SPACE.         
000655                                                                          
000656     03  W-WDA5A1KY-MAX.                                                  
000657         05  W-IDARTNR-N3-MAX     PIC S9(9) COMP-3   VALUE ZERO.          
000658         05  FILLER               PIC X(35)          VALUE SPACE.         
000659                                                                          
000660     03  W-WDA501KY.                                                      
000661         05  W-IDDISTR-N2         PIC S9(5) COMP-3   VALUE ZERO.          
000662         05  W-IDKUNDNR-N2        PIC S9(7) COMP-3   VALUE ZERO.          
000663         05  W-IDKUNDRF-N2.                                               
000664             07  W-IDORDNR-N2     PIC 9(5)           VALUE ZERO.          
000665             07  FILLER           PIC X(5)           VALUE SPACE.         
000666         05  W-IDARTNR-N2         PIC S9(9) COMP-3   VALUE ZERO.          
000667         05  W-IDLOPNR-N2         PIC S9(3) COMP-3   VALUE ZERO.          
000668                                                                          
000669   03  W-WDN611KY-X.                                                      
000670     05  W-IDFORDON              PIC S9(2)   VALUE ZERO  COMP-3.          
000671     05  W-TIOMBRYT-1            PIC S9(7)   VALUE ZERO  COMP-3.          
000672     SKIP2                                                                
000673                                                                          
000674   03  W-WDD7A1KY-MIN.                                                    
000675     05  W-IDARTNR-MIN7           PIC S9(9)  COMP-3 VALUE ZERO.           
000676     05  FILLER                   PIC S9(9)  COMP-3 VALUE ZERO.           
000677     05  FILLER                   PIC S9(3)  COMP-3 VALUE ZERO.           
000678                                                                          
000679   03  W-WDD7A1KY-MAX.                                                    
000680     05  W-IDARTNR-MAX7    PIC S9(9)  COMP-3 VALUE ZERO.                  
000681     05  FILLER            PIC S9(9)  COMP-3 VALUE +999999999.            
000682     05  FILLER            PIC S9(3)  COMP-3 VALUE +999.                  
000683                                                                          
000684   03  W-IDARTNR-TILLK-X.                                                 
000685     05  W-IDARTNR-TILLK         PIC S9(9)   COMP-3.                      
000686   03  W-IDARTNR-ERS-LOW-X.                                               
000687     05  W-IDARTNR-ERS-LOW       PIC S9(9)   COMP-3  VALUE ZERO.          
000688   03  W-IDARTNR-ERS-HIGH-X.                                              
000689     05  W-IDARTNR-ERS-HIGH      PIC S9(9)   COMP-3                       
000690                                  VALUE +999999999.                       
000691                                                                          
000692   03  W-W6D1HSEQ-X.                                                      
000693     05  W-IDARTNR-HSEQ  PIC S9(9)   VALUE ZERO  COMP-3.                  
000694                                                                          
000695   03  W-KDSEGKEY-X.                                                      
000696         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
000697                                                                          
000698   03  W-IDLEVNR-X.                                                       
000699         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
000700                                                                          
000701     SKIP2                                                                
000702*    --- STATUS-KOD FRÅN IMS                                              
000703 01  STATUS-WS                   PIC XX.                                  
000704     88  SEGMENT-FINNS                       VALUE '  '.                  
000705     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
000706     88  SEGMENT-SAKNAS                      VALUE 'GE'                   
000707                                                   'GB'.                  
000708     SKIP2                                                                
000709 01  GODK-STATUSKODER.                                                    
000710     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000711     SKIP3                                                                
000712 01  SSA1                        PIC X(128).                              
000713 01  SSA2                        PIC X(128).                              
000714 01  SSA3                        PIC X(128).                              
000715     EJECT                                                                
000716*    --- IMS FUNKTIONSKODER                                               
000717*01  -COPY W0003                                                          
000718     EJECT                                                                
000719*    ---  DLI INPUT-OUTPUT AREA                                           
000720 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK701'.             
000721     SKIP3                                                                
000722 01  DLI-IO-AREA-WDK701.                                                  
000723*        05  -COPY WDK701                                                 
000724     EJECT                                                                
000725 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK711'.             
000726     SKIP3                                                                
000727 01  DLI-IO-AREA-WDK711.                                                  
000728*        05  -COPY WDK711                                                 
000729     EJECT                                                                
000730 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK712'.             
000731     SKIP3                                                                
000732 01  DLI-IO-AREA-WDK712.                                                  
000733*        05  -COPY WDK712                                                 
000734     EJECT                                                                
000735 01  FILLER                  PIC X(24)                                    
000736                                 VALUE 'DLI-IO-WDK711-TRANS'.             
000737     SKIP3                                                                
000738 01  DLI-IO-AREA-WDK711-TRANS.                                            
000739*        05  -COPY WDK711    -PRE TRANS-                                  
000740     EJECT                                                                
000741 01  FILLER                  PIC X(24)                                    
000742                                 VALUE 'DLI-IO-WDK727-FUTUR'.             
000743     SKIP3                                                                
000744 01  DLI-IO-AREA-WDK727-FUTUR.                                            
000745*        05  -COPY WDK727    -PRE FUTUR-                                  
000746     EJECT                                                                
000747 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDL711'.             
000748     SKIP3                                                                
000749 01  DLI-IO-AREA-WDL711.                                                  
000750*        05  -COPY WDL711                                                 
000751     EJECT                                                                
000752 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDL411'.             
000753     SKIP3                                                                
000754 01  DLI-IO-AREA-WDL411.                                                  
000755*        05  -COPY WDL411                                                 
000756     EJECT                                                                
000757 01  FILLER                  PIC X(16) VALUE 'DLI-IO-BENA01'.             
000758     SKIP3                                                                
000759 01  DLI-IO-AREA-BENA01.                                                  
000760*        05  -COPY WDD301  -PRE BENA01-                                   
000761     EJECT                                                                
000762 01  FILLER                  PIC X(16) VALUE 'DLI-IO-BENA11'.             
000763     SKIP3                                                                
000764 01  DLI-IO-AREA-BENA11.                                                  
000765*        05  -COPY WDD311  -PRE BENA11-                                   
000766     EJECT                                                                
000767 01  FILLER                  PIC X(16) VALUE 'DLI-IO-ORDL01'.             
000768     SKIP3                                                                
000769 01  DLI-IO-AREA-ORDL01.                                                  
000770*        05  -COPY WDE301                                                 
000771     EJECT                                                                
000772 01  FILLER                  PIC X(16) VALUE 'DLI-IO-ORDW01'.             
000773     SKIP3                                                                
000774 01  DLI-IO-AREA-ORDW01.                                                  
000775*        05  -COPY WDE3A1    -PRE ORDW-                                   
000776     EJECT                                                                
000777 01  FILLER                  PIC X(16) VALUE 'DLI-IO-ORDP01'.             
000778 01  DLI-IO-AREA-ORDP01.                                                  
000779*  03    WDA501 -COPY WDA501                                              
000780     EJECT                                                                
000781 01  FILLER                  PIC X(16) VALUE 'DLI-IO-ORDQ01'.             
000782 01  DLI-IO-AREA-ORDQ01.                                                  
000783*  03    WDA5A1 -COPY WDA5A1                                              
000784     EJECT                                                                
000785 01  FILLER                  PIC X(16) VALUE 'DLI-IO-ARTC01'.             
000786     SKIP3                                                                
000787 01  DLI-IO-AREA-ARTC01.                                                  
000788*        05  -COPY WDK601                                                 
000789     EJECT                                                                
000790 01  FILLER                  PIC X(16) VALUE 'DLI-IO-ARTC11'.             
000791     SKIP3                                                                
000792 01  DLI-IO-AREA-ARTC11.                                                  
000793*        05  -COPY WDK611                                                 
000794     EJECT                                                                
000795 01  FILLER                  PIC X(16) VALUE 'DLI-IO-ARTC11'.             
000796     SKIP3                                                                
000797 01  DLI-IO-AREA-ARTC21.                                                  
000798*        05  -COPY WDK621                                                 
000799     EJECT                                                                
000800 01  FILLER                  PIC X(16) VALUE 'DLI-IO-ARTC25'.             
000801     SKIP3                                                                
000802 01  DLI-IO-AREA-ARTC25.                                                  
000803*        05  -COPY WDK625                                                 
000804     EJECT                                                                
000805 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-KATN01'.            
000806     SKIP3                                                                
000807 01  DLI-IO-AREA-KATN01.                                                  
000808*  03    WLKATN01 -COPY WDN601 -PRE KATN-                                 
000809     EJECT                                                                
000810 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-KATN11'.            
000811     SKIP3                                                                
000812 01  DLI-IO-AREA-KATN11.                                                  
000813*  03    WLKATN11 -COPY WDN611 -PRE KATN-                                 
000814     EJECT                                                                
000815 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-ERSA01'.            
000816     SKIP3                                                                
000817 01  DLI-IO-AREA-ERSA01.                                                  
000818*  03  WLERSA01 -COPY WDD701  -PRE ERSA01-                                
000819     EJECT                                                                
000820 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-ERSA11'.            
000821     SKIP3                                                                
000822 01  DLI-IO-AREA-ERSA11.                                                  
000823*  03  WLERSA11 -COPY WDD702  -PRE ERSA11-                                
000824     EJECT                                                                
000825 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-ERSB01'.            
000826     SKIP3                                                                
000827 01  DLI-IO-AREA-ERSB01.                                                  
000828*  03  WLERSB01 -COPY WDD7A1  -PRE ERSB01-                                
000829     EJECT                                                                
000830 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-INLA11'.            
000831     SKIP3                                                                
000832 01  DLI-IO-AREA-INLA11.                                                  
000833*  03  W6INLA11 -COPY W6D111 -PRE INLA-                                   
000834     EJECT                                                                
000835 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-ARTM01'.            
000836     SKIP3                                                                
000837 01  DLI-IO-AREA-ARTM01.                                                  
000838*  03  WLARTM01 -COPY WDK901  -PRE ARTM-                                  
000839     EJECT                                                                
000840 01  FILLER                  PIC X(24) VALUE 'DLI-IO-LEVA01'.             
000841 01  DLI-IO-LEVA01.                                                       
000842*    03  -COPY WDF101                                                     
000843     EJECT                                                                
000844                                                                          
000845 01  FILLER                  PIC X(24) VALUE 'DLI-IO-LEVA16'.             
000846 01  DLI-IO-LEVA16.                                                       
000847*    03  -COPY WDF116                                                     
000848     EJECT                                                                
000849                                                                          
000850 01  FILLER                  PIC X(16)   VALUE 'WDB601 AREA'.             
000851 01   DLI-IO-AREA-B601.                                                   
000852*     03  -COPY WDB601                                                    
000853     EJECT                                                                
000854                                                                          
000855 01  FILLER                  PIC X(16)   VALUE 'WDB616 AREA'.             
000856 01   DLI-IO-AREA-B616.                                                   
000857*     03  -COPY WDB616 -PRE B616-                                         
000858     EJECT                                                                
000859                                                                          
000860 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDL611'.           
000861 01  DLI-IO-AREA-WDL611.                                                  
000862*  03  WDL611 -COPY WDL611                                                
000863     EJECT                                                                
000864                                                                          
000865 01  FILLER                      PIC X(16)  VALUE 'TP5IDDC-AREA'.         
000866                                                                          
000867*01  -COPY TP5IDDC -PRE TP5IDDC-                                          
000868     EJECT                                                                
000869 01  FILLER                      PIC X(16)  VALUE 'TP4TRAN-AREA'.         
000870                                                                          
000871*01  -COPY TP4TRAN -PRE TP4TRAN-                                          
000872     EJECT                                                                
000873     EXEC SQL INCLUDE TP5IDDC END-EXEC.                                   
000874     EJECT                                                                
000875     EXEC SQL INCLUDE TP4TRAN END-EXEC.                                   
000876     EJECT                                                                
000877 LINKAGE SECTION.                                                         
000878                                                                          
000879*01  -COPY W0009   -PRE MSG-                                              
000880     EJECT                                                                
000881*01  -COPY W0008  -PRE  USEA-                                             
000882     05  FILLER                          PIC X.                           
000883     EJECT                                                                
000884*01  -COPY W0008  -PRE  WDK7-                                             
000885     05  FILLER                          PIC X.                           
000886     EJECT                                                                
000887*01  -COPY W0008  -PRE  WDL7-                                             
000888     05  FILLER                          PIC X.                           
000889     EJECT                                                                
000890*01  -COPY W0008  -PRE  WDL4-                                             
000891     05  FILLER                          PIC X.                           
000892     EJECT                                                                
000893*01  -COPY W0008  -PRE  BENA-                                             
000894     05  FILLER                          PIC X.                           
000895     EJECT                                                                
000896*01  -COPY W0008  -PRE ORDL-                                              
000897     05  FILLER                          PIC X.                           
000898     EJECT                                                                
000899*01  -COPY W0008  -PRE ORDW-                                              
000900     05  FILLER                          PIC X.                           
000901     EJECT                                                                
000902*01  -COPY W0008  -PRE ORDP-                                              
000903     05  FILLER                          PIC X.                           
000904     EJECT                                                                
000905*01  -COPY W0008  -PRE ORDQ-                                              
000906     05  FILLER                          PIC X.                           
000907     EJECT                                                                
000908*01  -COPY W0008  -PRE ARTC-                                              
000909     05  FILLER                          PIC X.                           
000910     EJECT                                                                
000911*01  -COPY W0008  -PRE KATN-                                              
000912     05  FILLER                          PIC X.                           
000913     EJECT                                                                
000914*01  -COPY W0008  -PRE ERSA-                                              
000915     05  FILLER                          PIC X.                           
000916     EJECT                                                                
000917*01  -COPY W0008  -PRE ERSB-                                              
000918     05  FILLER                          PIC X.                           
000919     EJECT                                                                
000920*01  -COPY W0008  -PRE INLA-                                              
000921     05  FILLER                          PIC X.                           
000922     EJECT                                                                
000923*01  -COPY W0008  -PRE ARTM-                                              
000924     05  FILLER                          PIC X.                           
000925     EJECT                                                                
000926*01  -COPY W0008  -PRE  LEVA-                                             
000927     05  FILLER                          PIC X.                           
000928     EJECT                                                                
000929*01  -COPY W0008  -PRE  WDK7F-                                            
000930     05  FILLER                          PIC X.                           
000931     EJECT                                                                
000932*01  -COPY W0008      -PRE WDB6-                                          
000933     05  FILLER                          PIC X.                           
000934     EJECT                                                                
000935*01  -COPY W0008      -PRE WDB6-GN-                                       
000936     05  FILLER                          PIC X.                           
000937     EJECT                                                                
000938*****W271REFL**********                                                   
000939 01  REFL1-2501-PCB                      PIC X.                           
000940 01  REFL1-WDB6-PCB                      PIC X.                           
000941 01  REFL1-WDK7-PCB                      PIC X.                           
000942 01  REFL1-UTIL-WDK6-PCB                 PIC X.                           
000943 01  REFL1-UTIL-WDK7-PCB                 PIC X.                           
000944 01  REFL1-UTIL-WDB6-PCB                 PIC X.                           
000945     EJECT                                                                
000946*****W271UTIL**********                                                   
000947 01  UTIL-WDK6-PCB                       PIC X.                           
000948 01  UTIL-WDK7-PCB                       PIC X.                           
000949 01  UTIL-WDB6-PCB                       PIC X.                           
000950     EJECT                                                                
000951*****W271UTUP**********                                                   
000952 01  UTUP1-WDK7-PCB                      PIC X.                           
000953 01  UTUP1-WDB6-PCB                      PIC X.                           
000954 01  UTUP1-UTIL-WDK6-PCB                 PIC X.                           
000955 01  UTUP1-UTIL-WDK7-PCB                 PIC X.                           
000956 01  UTUP1-UTIL-WDB6-PCB                 PIC X.                           
000957*****W272UTUP**********                                                   
000958 01  U2-WDK6-PCB                         PIC X.                           
000959 01  U2-WDB6-PCB                         PIC X.                           
000960 01  U2-PBTO-W222-WDK6-PCB               PIC X.                           
000961 01  U2-PBTO-W222-WDK7-PCB               PIC X.                           
000962 01  U2-PBTO-W222-ARTM-PCB               PIC X.                           
000963 01  U2-PBTO-W222-REFL1-2501-PCB         PIC X.                           
000964 01  U2-PBTO-W222-REFL1-WDB6R-PCB        PIC X.                           
000965 01  U2-PBTO-W222-REFL1-WDK7R-PCB        PIC X.                           
000966 01  U2-PBTO-W222-REFL1-UTIL-K6-PCB      PIC X.                           
000967 01  U2-PBTO-W222-REFL1-UTIL-K7-PCB      PIC X.                           
000968 01  U2-PBTO-W222-REFL1-UTIL-B6-PCB      PIC X.                           
000969 01  U2-PBTO-W222-WDB6-PCB               PIC X.                           
000970 01  U2-PBTO-W222-WDD7-PCB               PIC X.                           
000971 01  U2-PBTO-W222-WDK7E-PCB              PIC X.                           
000972 01  U2-PBTO-W222-UTUP1-WDK7-PCB         PIC X.                           
000973 01  U2-PBTO-W222-UTUP1-WDB6-PCB         PIC X.                           
000974 01  U2-PBTO-W222-UTUP1-UTIL-K6-PCB      PIC X.                           
000975 01  U2-PBTO-W222-UTUP1-UTIL-K7-PCB      PIC X.                           
000976 01  U2-PBTO-W222-UTUP1-UTIL-B6-PCB      PIC X.                           
000977 01  U2-REFL2-2501-PCB                   PIC X.                           
000978 01  U2-REFL2-WDB6-PCB                   PIC X.                           
000979 01  U2-REFL2-UTIL-WDK6-PCB              PIC X.                           
000980 01  U2-REFL2-UTIL-WDK7-PCB              PIC X.                           
000981 01  U2-REFL2-UTIL-WDB6-PCB              PIC X.                           
000982 01  U2-W222-WDK6-PCB                    PIC X.                           
000983 01  U2-W222-WDK7-PCB                    PIC X.                           
000984 01  U2-W222-ARTM-PCB                    PIC X.                           
000985 01  U2-W222-2501-PCB                    PIC X.                           
000986 01  U2-W222-WDB6R-PCB                   PIC X.                           
000987 01  U2-W222-WDK7R-PCB                   PIC X.                           
000988 01  U2-W222-WDB6-PCB                    PIC X.                           
000989 01  U2-W222-WDD7-PCB                    PIC X.                           
000990 01  U2-W222-WDK7E-PCB                   PIC X.                           
000991 01  U2-W222-UTIL-WDK6-PCB               PIC X.                           
000992 01  U2-W222-UTIL-WDK7-PCB               PIC X.                           
000993 01  U2-W222-UTIL-WDB6-PCB               PIC X.                           
000994 01  U2-W222-UTUP1-WDK7-PCB              PIC X.                           
000995 01  U2-W222-UTUP1-WDB6-PCB              PIC X.                           
000996 01  U2-W222-UTUP1-UTIL-WDK6-PCB         PIC X.                           
000997 01  U2-W222-UTUP1-UTIL-WDK7-PCB         PIC X.                           
000998 01  U2-W222-UTUP1-UTIL-WDB6-PCB         PIC X.                           
000999     EJECT                                                                
001000                                                                          
001001 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB WDK7-PCB                      
001002     WDL7-PCB WDL4-PCB BENA-PCB ORDL-PCB ORDW-PCB                         
001003     ORDP-PCB ORDQ-PCB                                                    
001004     ARTC-PCB KATN-PCB ERSA-PCB ERSB-PCB INLA-PCB ARTM-PCB                
001005     LEVA-PCB WDK7F-PCB                                                   
001006     WDB6-PCB WDB6-GN-PCB                                                 
001007     REFL1-2501-PCB                                                       
001008     REFL1-WDB6-PCB                                                       
001009     REFL1-WDK7-PCB                                                       
001010     REFL1-UTIL-WDK6-PCB                                                  
001011     REFL1-UTIL-WDK7-PCB                                                  
001012     REFL1-UTIL-WDB6-PCB                                                  
001013     UTIL-WDK6-PCB                                                        
001014     UTIL-WDK7-PCB                                                        
001015     UTIL-WDB6-PCB                                                        
001016     UTUP1-WDK7-PCB                                                       
001017     UTUP1-WDB6-PCB                                                       
001018     UTUP1-UTIL-WDK6-PCB                                                  
001019     UTUP1-UTIL-WDK7-PCB                                                  
001020     UTUP1-UTIL-WDB6-PCB                                                  
001021     U2-WDK6-PCB                                                          
001022     U2-WDB6-PCB                                                          
001023     U2-PBTO-W222-WDK6-PCB                                                
001024     U2-PBTO-W222-WDK7-PCB                                                
001025     U2-PBTO-W222-ARTM-PCB                                                
001026     U2-PBTO-W222-REFL1-2501-PCB                                          
001027     U2-PBTO-W222-REFL1-WDB6R-PCB                                         
001028     U2-PBTO-W222-REFL1-WDK7R-PCB                                         
001029     U2-PBTO-W222-REFL1-UTIL-K6-PCB                                       
001030     U2-PBTO-W222-REFL1-UTIL-K7-PCB                                       
001031     U2-PBTO-W222-REFL1-UTIL-B6-PCB                                       
001032     U2-PBTO-W222-WDB6-PCB                                                
001033     U2-PBTO-W222-WDD7-PCB                                                
001034     U2-PBTO-W222-WDK7E-PCB                                               
001035     U2-PBTO-W222-UTUP1-WDK7-PCB                                          
001036     U2-PBTO-W222-UTUP1-WDB6-PCB                                          
001037     U2-PBTO-W222-UTUP1-UTIL-K6-PCB                                       
001038     U2-PBTO-W222-UTUP1-UTIL-K7-PCB                                       
001039     U2-PBTO-W222-UTUP1-UTIL-B6-PCB                                       
001040     U2-REFL2-2501-PCB                                                    
001041     U2-REFL2-WDB6-PCB                                                    
001042     U2-REFL2-UTIL-WDK6-PCB                                               
001043     U2-REFL2-UTIL-WDK7-PCB                                               
001044     U2-REFL2-UTIL-WDB6-PCB                                               
001045     U2-W222-WDK6-PCB                                                     
001046     U2-W222-WDK7-PCB                                                     
001047     U2-W222-ARTM-PCB                                                     
001048     U2-W222-2501-PCB                                                     
001049     U2-W222-WDB6R-PCB                                                    
001050     U2-W222-WDK7R-PCB                                                    
001051     U2-W222-WDB6-PCB                                                     
001052     U2-W222-WDD7-PCB                                                     
001053     U2-W222-WDK7E-PCB                                                    
001054     U2-W222-UTIL-WDK6-PCB                                                
001055     U2-W222-UTIL-WDK7-PCB                                                
001056     U2-W222-UTIL-WDB6-PCB                                                
001057     U2-W222-UTUP1-WDK7-PCB                                               
001058     U2-W222-UTUP1-WDB6-PCB                                               
001059     U2-W222-UTUP1-UTIL-WDK6-PCB                                          
001060     U2-W222-UTUP1-UTIL-WDK7-PCB                                          
001061     U2-W222-UTUP1-UTIL-WDB6-PCB.                                         
001062                                                                          
001063 MAIN SECTION.                                                            
001064     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB WDK7-PCB                      
001065     WDL7-PCB WDL4-PCB BENA-PCB ORDL-PCB ORDW-PCB                         
001066     ORDP-PCB ORDQ-PCB                                                    
001067     ARTC-PCB KATN-PCB ERSA-PCB ERSB-PCB INLA-PCB ARTM-PCB                
001068     LEVA-PCB WDK7F-PCB                                                   
001069     WDB6-PCB WDB6-GN-PCB                                                 
001070     REFL1-2501-PCB                                                       
001071     REFL1-WDB6-PCB                                                       
001072     REFL1-WDK7-PCB                                                       
001073     REFL1-UTIL-WDK6-PCB                                                  
001074     REFL1-UTIL-WDK7-PCB                                                  
001075     REFL1-UTIL-WDB6-PCB                                                  
001076     UTIL-WDK6-PCB                                                        
001077     UTIL-WDK7-PCB                                                        
001078     UTIL-WDB6-PCB                                                        
001079     UTUP1-WDK7-PCB                                                       
001080     UTUP1-WDB6-PCB                                                       
001081     UTUP1-UTIL-WDK6-PCB                                                  
001082     UTUP1-UTIL-WDK7-PCB                                                  
001083     UTUP1-UTIL-WDB6-PCB                                                  
001084     U2-WDK6-PCB                                                          
001085     U2-WDB6-PCB                                                          
001086     U2-PBTO-W222-WDK6-PCB                                                
001087     U2-PBTO-W222-WDK7-PCB                                                
001088     U2-PBTO-W222-ARTM-PCB                                                
001089     U2-PBTO-W222-REFL1-2501-PCB                                          
001090     U2-PBTO-W222-REFL1-WDB6R-PCB                                         
001091     U2-PBTO-W222-REFL1-WDK7R-PCB                                         
001092     U2-PBTO-W222-REFL1-UTIL-K6-PCB                                       
001093     U2-PBTO-W222-REFL1-UTIL-K7-PCB                                       
001094     U2-PBTO-W222-REFL1-UTIL-B6-PCB                                       
001095     U2-PBTO-W222-WDB6-PCB                                                
001096     U2-PBTO-W222-WDD7-PCB                                                
001097     U2-PBTO-W222-WDK7E-PCB                                               
001098     U2-PBTO-W222-UTUP1-WDK7-PCB                                          
001099     U2-PBTO-W222-UTUP1-WDB6-PCB                                          
001100     U2-PBTO-W222-UTUP1-UTIL-K6-PCB                                       
001101     U2-PBTO-W222-UTUP1-UTIL-K7-PCB                                       
001102     U2-PBTO-W222-UTUP1-UTIL-B6-PCB                                       
001103     U2-REFL2-2501-PCB                                                    
001104     U2-REFL2-WDB6-PCB                                                    
001105     U2-REFL2-UTIL-WDK6-PCB                                               
001106     U2-REFL2-UTIL-WDK7-PCB                                               
001107     U2-REFL2-UTIL-WDB6-PCB                                               
001108     U2-W222-WDK6-PCB                                                     
001109     U2-W222-WDK7-PCB                                                     
001110     U2-W222-ARTM-PCB                                                     
001111     U2-W222-2501-PCB                                                     
001112     U2-W222-WDB6R-PCB                                                    
001113     U2-W222-WDK7R-PCB                                                    
001114     U2-W222-WDB6-PCB                                                     
001115     U2-W222-WDD7-PCB                                                     
001116     U2-W222-WDK7E-PCB                                                    
001117     U2-W222-UTIL-WDK6-PCB                                                
001118     U2-W222-UTIL-WDK7-PCB                                                
001119     U2-W222-UTIL-WDB6-PCB                                                
001120     U2-W222-UTUP1-WDK7-PCB                                               
001121     U2-W222-UTUP1-WDB6-PCB                                               
001122     U2-W222-UTUP1-UTIL-WDK6-PCB                                          
001123     U2-W222-UTUP1-UTIL-WDK7-PCB                                          
001124     U2-W222-UTUP1-UTIL-WDB6-PCB.                                         
001125                                                                          
001126     PERFORM IMS-GET-MSG                                                  
001127     IF SEGMENT-FINNS                                                     
001128       PERFORM A-INIT                                                     
001129       PERFORM B-KOLLA-NYCKLAR                                            
001130       IF NYCKLAR-OK                                                      
001131         IF MFS-UPDATE OR                                                 
001132            MFS-UPD-V                                                     
001133           PERFORM I-KOLLA-INPUT                                          
001134           PERFORM L-KOLLA-INPUT-NYCKLAR                                  
001135           IF INDATA-OK                                                   
001136             PERFORM H-UPD-WDK6-WDK7                                      
001137             IF WS-IDREFTYP = 'A'                                         
001138             OR WS-IDREFTYP = 'C'                                         
001139             OR WS-IDREFTYP = 'B'                                         
001140             OR WS-IDREFTYP = 'L'                                         
001141             OR SW-TRANSF-JA                                              
001142               PERFORM N-UPD-WDE3-WDK7                                    
001143             END-IF                                                       
001144                                                                          
001145             IF SW-TRANSF-JA                                              
001146               MOVE 'TRANSFER ISSUED' TO MOD-TEMFSINF                     
001147             ELSE                                                         
001148               MOVE INF-UPDATE-DONE                                       
001149                               TO MED-IDMFSINF                            
001150               CALL WMEDKONV USING MED-WMEDAREA                           
001151               MOVE MED-TEMFSINF                                          
001152                               TO MOD-TEMFSINF                            
001153             END-IF                                                       
001154                                                                          
001155           ELSE                                                           
001156*   MOD-FÄLT SOM ÄVEN ÄR INFÄLT SKA INTE SKRIVAS ÖVER                     
001157*   I F-HAEMTA-INFO                                                       
001158             MOVE NEJ        TO SW-HAEMTA-INPUT-FAELT                     
001159             MOVE 'PROPOSAL NOT REVIEWED'                                 
001160                             TO MOD-ORDERSTATUS                           
001161             MOVE INF-UPDATE-NOT-DONE                                     
001162                             TO MED-IDMFSINF                              
001163             CALL WMEDKONV USING MED-WMEDAREA                             
001164             MOVE MED-TEMFSINF                                            
001165                             TO MOD-TEMFSINF                              
001166           END-IF                                                         
001167         ELSE                                                             
001168*   INITIERA MOD-PURCHQTY                                                 
001169           MOVE ZERO         TO WS-RED-PURCHQTY                           
001170           MOVE +1           TO IX-DC                                     
001171           PERFORM UNTIL IX-DC                                            
001172                              > IX-DC-MAX                                 
001173             MOVE WS-RED-PURCHQTY                                         
001174                             TO MOD-PURCHQTY (IX-DC)                      
001175             ADD +1          TO IX-DC                                     
001176           END-PERFORM                                                    
001177           IF MFS-FIRST                                                   
001178             PERFORM C-FOEREG-SIDA                                        
001179           ELSE                                                           
001180             IF MFS-NEXT                                                  
001181               PERFORM D-NAESTA-SIDA                                      
001182             ELSE                                                         
001183               PERFORM E-SAMMA-SIDA                                       
001184             END-IF                                                       
001185           END-IF                                                         
001186         END-IF                                                           
001187*   UPPDATERA BILDEN                                                      
001188         IF W-IDARTNR > ZERO                                              
001189           PERFORM S1-SECURITY-CHECK-PARTNO                               
001190           PERFORM F-HAEMTA-INFO                                          
001191         ELSE                                                             
001192           MOVE MFS-RENSA-FAELT                                           
001193                             TO MOD-IDARTNR-UT                            
001194                                MOD-IDREFTYP-UT                           
001195                                MOD-ORDERSTATUS                           
001196         END-IF                                                           
001197         PERFORM J-FYLL-I-ANT-REVIEW                                      
001198                                                                          
001199         MOVE W-IDARTNR      TO WS-MSGI-IDARTNR-ENTER                     
001200         IF MOD-ORDERSTATUS = 'REVIEWED'                                  
001201            MOVE 'R'         TO WS-MSGI-ORDER-ENTER                       
001202         ELSE                                                             
001203            MOVE 'N'         TO WS-MSGI-ORDER-ENTER                       
001204         END-IF                                                           
001205         IF WS-MSGI-IDARTNR-ENTER = WS-MSGI-IDARTNR-PF7                   
001206           MOVE ZERO            TO WS-MSGI-IDARTNR-PF7                    
001207           MOVE SPACE           TO WS-MSGI-ORDER-PF7                      
001208         END-IF                                                           
001209* ---    UPPDATERA MSGI-SPAR-AREA                                         
001210         MOVE '002'             TO MSGI-KDCALL                            
001211         MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                      
001212         MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                            
001213         MOVE '2352'            TO MSGI-IDTRANS                           
001214         MOVE WS-MSGI-AREA-2352 TO MSGI-SPAR-AREA                         
001215         CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                       
001216                                                                          
001217* ---    UPPDATERA MSGI-IDARTNR                                           
001218         MOVE ALL '+'        TO MSGI-WMSGINIT                             
001219         MOVE '001'          TO MSGI-KDCALL                               
001220         MOVE MSG-LTERM-NAME TO MSGI-IDLTERM-USER                         
001221         MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                            
001222         MOVE '2352'         TO MSGI-IDTRANS                              
001223         MOVE W-IDARTNR      TO WS-IDARTNR-NUM                            
001224         MOVE WS-IDARTNR     TO MSGI-IDARTNR                              
001225         CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                       
001226                                                                          
001227       END-IF                                                             
001228                                                                          
001229       IF BLOCKED-SECURITY-CHECK                                          
001230*        --- DETTA VÄRDE SÄTTS I SEKTION S1- DÄR KONTROLL GÖRS PÅ         
001231*            IFALL USER FÅR SE INFO OM VISS ARTIKEL                       
001232         PERFORM MFS-RENSA-FAELT-IN                                       
001233         PERFORM MFS-RENSA-FAELT-UT                                       
001234         PERFORM MFS-RENSA-OBEHOERIGA-FAELT-IN                            
001235         PERFORM MFS-STAENG-OBEHOERIGA-FAELT-IN                           
001236       END-IF                                                             
001237                                                                          
001238       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O35201 + 4                      
001239       PERFORM IMS-INSERT-MSG                                             
001240     END-IF                                                               
001241                                                                          
001242     MOVE ZERO TO RETURN-CODE                                             
001243     GOBACK                                                               
001244     .                                                                    
001245     EJECT                                                                
001246 A-INIT SECTION.                                                          
001247                                                                          
001248     IF MSG-DUBBLA-TRANSKODER                                             
001249       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
001250       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
001251       IF MFS-IDTRANS = '2351'                                            
001252         MOVE MSG-INDATA-MINUS-2-TRANSKODER                               
001253                             TO MID-W2I35101                              
001254       ELSE                                                               
001255         MOVE MSG-INDATA-MINUS-2-TRANSKODER                               
001256                             TO MID-W2I35201                              
001257       END-IF                                                             
001258     ELSE                                                                 
001259       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
001260       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
001261       IF MFS-IDTRANS = '2351'                                            
001262         MOVE MSG-INDATA-MINUS-1-TRANSKOD                                 
001263                             TO MID-W2I35101                              
001264       ELSE                                                               
001265         MOVE MSG-INDATA-MINUS-1-TRANSKOD                                 
001266                             TO MID-W2I35201                              
001267       END-IF                                                             
001268     END-IF                                                               
001269                                                                          
001270     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
001271     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
001272     MOVE MFS-IDTRANS TO W-IDTRANS                                        
001273                                                                          
001274     MOVE LOW-VALUE TO MSG-AREA                                           
001275     MOVE 'W2O352N1' TO MFS-IDMOD                                         
001276     MOVE '2352' TO MOD-IDTRANS                                           
001277     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
001278                                                                          
001279     IF EGEN-MID OR HELP-MID                                              
001280       CONTINUE                                                           
001281     ELSE                                                                 
001282       MOVE SPACE TO MFS-KDTRTYP                                          
001283     END-IF                                                               
001284                                                                          
001285     MOVE +2      TO SPRAK-IX                                             
001286     MOVE 'GB ' TO MED-IDSKYLT                                            
001287                                                                          
001288     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-DATUM-SEKEL                
001289                                                                          
001290     ACCEPT DAGENS-DATUM FROM DATE                                        
001291                                                                          
001292     MOVE 'AAMMDD'           TO DAT-KDDATFORM                             
001293     MOVE DAGENS-DATUM       TO DAT-I-TIDATUM                             
001294                                                                          
001295     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
001296                     DAT-O-TIDATUM DAT-KDSVAR                             
001297                                                                          
001298     IF DAT-KDSVAR-OK                                                     
001299****             HÄMTA SEKELSIFFROR                                       
001300                                                                          
001301       MOVE DAT-TISEKEL       TO DAGENS-AAR(1:2)                          
001302       MOVE DAT-TIAARP        TO DAGENS-PER                               
001303       MOVE DAT-TIVV          TO DAGENS-VECKA                             
001304                                                                          
001305     ELSE                                                                 
001306         STRING ' FEL FRÅN DATUMRUTIN WDATKONV ' STATUS-WS                
001307         DELIMITED BY SIZE INTO FELTEXT                                   
001308         CALL FELLOG                                                      
001309     END-IF                                                               
001310                                                                          
001311     MOVE DAGENS-DATUM(1:2)   TO DAGENS-AAR(3:2)                          
001312                                                                          
001313     MOVE 'AARP  '           TO DAT-KDDATFORM                             
001314     MOVE DAT-TIAARP         TO DAT-I-TIDATUM                             
001315                                                                          
001316     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
001317                     DAT-O-TIDATUM DAT-KDSVAR                             
001318                                                                          
001319     IF DAT-KDSVAR-OK                                                     
001320****             RÄKNA UT VECKA I AKTUELL PERIOD                          
001321                                                                          
001322       COMPUTE WS-ANTAL-VECKOR = DAGENS-VECKA - DAT-TIVV + 1              
001323                                                                          
001324     ELSE                                                                 
001325         STRING ' FEL FRÅN DATUMRUTIN WDATKONV ' STATUS-WS                
001326         DELIMITED BY SIZE INTO FELTEXT                                   
001327         CALL FELLOG                                                      
001328     END-IF                                                               
001329                                                                          
001330     MOVE +4                  TO IX-DC-MAX                                
001331                                                                          
001332*****   INPUT/OUTPUT FÄLTEN INITERAS                                      
001333     PERFORM MFS-LAES-IN-IGEN                                             
001334*    TEXT 'NO PROPOSAL' SKRIVS I BILDEN OM EJ TRÄFF                       
001335     MOVE 'NO PROPOSAL'      TO MOD-ORDERSTATUS                           
001336     .                                                                    
001337     EJECT                                                                
001338 B-KOLLA-NYCKLAR SECTION.                                                 
001339                                                                          
001340     MOVE MFS-RENSA-FAELT    TO MOD-IDARTNR-IN                            
001341                                MOD-IDPERSON-BUY-IN                       
001342                                MOD-IDSTATUS-IN                           
001343                                MOD-IDTYPE-IN                             
001344                                MOD-IDREFTYP-IN                           
001345     MOVE ALL '+'            TO MSGI-WMSGINIT                             
001346     MOVE '001'              TO MSGI-KDCALL                               
001347     MOVE MSG-LTERM-NAME     TO MSGI-IDLTERM-USER                         
001348     MOVE MSG-SIGNON-USERID  TO MSGI-IDUSER                               
001349     MOVE '2352'             TO MSGI-IDTRANS                              
001350     IF EGEN-MID                                                          
001351     OR (MID-IDARTNR-IN NUMERIC                                           
001352     AND MID-IDARTNR-IN > ZERO)                                           
001353        MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                              
001354        MOVE MID-IDDC-IN     TO MSGI-IDDC-KEY                             
001355     END-IF                                                               
001356     MOVE SPACE              TO MSGI-SPAR-AREA                            
001357     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
001358     MOVE MSGI-IDARTNR TO WS-IDARTNR                                      
001359     INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO                   
001360                                                                          
001361     MOVE MSGI-IDDC-KEY      TO IDDC-WS                                   
001362                                W-IDDC-B6                                 
001363     INSPECT IDDC-WS REPLACING LEADING SPACE BY ZERO                      
001364*                                                                         
001365     PERFORM IMS-GU-WDB601                                                
001366                                                                          
001367     IF EGEN-MID                                                          
001368                                                                          
001369       IF MSGI-SPAR-AREA(1:4) = '2352'                                    
001370         MOVE MSGI-SPAR-AREA TO WS-MSGI-AREA-2352                         
001371       END-IF                                                             
001372       MOVE JA TO NYCKLAR-SW                                              
001373                                                                          
001374                                                                          
001375*      -- KONTROLL AV IDARTNR                                             
001376                                                                          
001377       IF WS-IDARTNR NUMERIC                                              
001378         MOVE WS-IDARTNR-NUM                                              
001379                             TO W-IDARTNR-MIN                             
001380                                W-IDARTNR-301                             
001381                                W-IDARTNR                                 
001382       ELSE                                                               
001383         MOVE NEJ            TO NYCKLAR-SW                                
001384       END-IF                                                             
001385                                                                          
001386       MOVE WS-IDARTNR       TO MOD-IDARTNR-UT                            
001387                                                                          
001388       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
001389       INSPECT MOD-IDARTNR-UT REPLACING LEADING '+'  BY SPACE             
001390                                                                          
001391*      -- KONTROLL AV IDDC                                                
001392                                                                          
001393       MOVE IDDC-WS          TO MOD-IDDC-UT                               
001394                                                                          
001395       IF DCS-NDC-NA                                                      
001396          MOVE IDDC-WS       TO W-IDDC-TP5                                
001397       ELSE                                                               
001398          MOVE NEJ           TO NYCKLAR-SW                                
001399       END-IF                                                             
001400                                                                          
001401       INSPECT MOD-IDDC-UT REPLACING LEADING ZERO BY SPACE                
001402       INSPECT MOD-IDDC-UT REPLACING LEADING '+'  BY SPACE                
001403                                                                          
001404*      -- KONTROLL AV IDPERSON-BUY                                        
001405                                                                          
001406       IF MID-IDPERSON-BUY-IN = ALL '+'                                   
001407         INSPECT MID-IDPERSON-BUY-UT                                      
001408                                   REPLACING LEADING '+' BY SPACE         
001409         MOVE MID-IDPERSON-BUY-UT                                         
001410                             TO WS-IDPERSON-BUY                           
001411       ELSE                                                               
001412           MOVE MID-IDPERSON-BUY-IN                                       
001413                             TO WS-IDPERSON-BUY                           
001414       END-IF                                                             
001415       INSPECT WS-IDPERSON-BUY REPLACING LEADING SPACE BY ZERO            
001416       IF WS-IDPERSON-BUY-NUM NUMERIC                                     
001417         MOVE WS-IDPERSON-BUY-NUM                                         
001418                               TO W-IDPERSON-BUY                          
001419                                  WS-IDPERSON-BUY-RED                     
001420                                                                          
001421         MOVE WS-IDPERSON-BUY-RED                                         
001422                               TO MOD-IDPERSON-BUY-UT                     
001423         INSPECT MOD-IDPERSON-BUY-UT REPLACING LEADING '+'                
001424                                     BY SPACE                             
001425       ELSE                                                               
001426         MOVE NEJ TO NYCKLAR-SW                                           
001427       END-IF                                                             
001428                                                                          
001429*      -- KONTROLL AV STATUS                                              
001430                                                                          
001431       IF MID-IDSTATUS-IN = ALL '+'                                       
001432         INSPECT MID-IDSTATUS-UT REPLACING LEADING '+' BY SPACE           
001433         MOVE MID-IDSTATUS-UT  TO WS-STATUS                               
001434       ELSE                                                               
001435         MOVE MID-IDSTATUS-IN  TO WS-STATUS                               
001436       END-IF                                                             
001437                                                                          
001438       IF WS-STATUS = 'R'                                                 
001439       OR WS-STATUS = 'N'                                                 
001440         IF WS-STATUS = 'R'                                               
001441           MOVE 'REVIEWED'   TO MOD-IDSTATUS-UT                           
001442         END-IF                                                           
001443         IF WS-STATUS = 'N'                                               
001444           MOVE 'NOT REVIEWED'                                            
001445                             TO MOD-IDSTATUS-UT                           
001446         END-IF                                                           
001447       ELSE                                                               
001448         IF MID-IDSTATUS-IN NOT = ALL '+'                                 
001449           MOVE NEJ TO NYCKLAR-SW                                         
001450           MOVE MED-2        TO MOD-TEMFSINF                              
001451         END-IF                                                           
001452       END-IF                                                             
001453       INSPECT MOD-IDSTATUS-UT REPLACING LEADING '+'  BY SPACE            
001454                                                                          
001455                                                                          
001456*      -- KONTROLL AV TYP                                                 
001457*      -- TYP ANVÄNDS FÖR SÖKNING AV REFILLORDERFÖRSLAG                   
001458                                                                          
001459       IF MID-IDTYPE-IN = ALL '+'                                         
001460         INSPECT MID-IDTYPE-UT REPLACING LEADING '+' BY SPACE             
001461         IF MID-IDTYPE-UT = 'AIRCR'                                       
001462           MOVE 'C'          TO WS-IDTYPE                                 
001463         ELSE                                                             
001464           MOVE MID-IDTYPE-UT                                             
001465                             TO WS-IDTYPE                                 
001466         END-IF                                                           
001467       ELSE                                                               
001468         MOVE MID-IDTYPE-IN  TO WS-IDTYPE                                 
001469                                MID-IDREFTYP-UT                           
001470       END-IF                                                             
001471                                                                          
001472       IF WS-IDTYPE = 'A'                                                 
001473       OR WS-IDTYPE = 'C'                                                 
001474       OR WS-IDTYPE = 'B'                                                 
001475       OR WS-IDTYPE = 'L'                                                 
001476       OR SW-TRANSF-JA                                                    
001477         IF WS-IDTYPE = 'A'                                               
001478           MOVE 'AIR'        TO MOD-IDTYPE-UT                             
001479         END-IF                                                           
001480         IF WS-IDTYPE = 'C'                                               
001481           MOVE 'AIRCR'      TO MOD-IDTYPE-UT                             
001482         END-IF                                                           
001483         IF WS-IDTYPE = 'B'                                               
001484           MOVE 'BOAT'       TO MOD-IDTYPE-UT                             
001485         END-IF                                                           
001486         IF WS-IDTYPE = 'L'                                               
001487           MOVE 'LOCAL'      TO MOD-IDTYPE-UT                             
001488         END-IF                                                           
001489         IF SW-TRANSF-JA                                                  
001490           MOVE 'TRANS'      TO MOD-IDTYPE-UT                             
001491         END-IF                                                           
001492       ELSE                                                               
001493         IF MID-IDTYPE-IN NOT = ALL '+'                                   
001494           MOVE NEJ          TO NYCKLAR-SW                                
001495           MOVE MED-1        TO MOD-TEMFSINF                              
001496         END-IF                                                           
001497       END-IF                                                             
001498       INSPECT MOD-IDTYPE-UT REPLACING LEADING '+'  BY SPACE              
001499                                                                          
001500*      -- KONTROLL AV IDREFTYP                                            
001501*      -- IDREFTYP ANVÄNDS FÖR ATT ANGE VILKEN ORDERTYP                   
001502*      -- SOM SKA SKAPAS                                                  
001503                                                                          
001504       IF MID-IDREFTYP-IN = ALL '+'                                       
001505         IF MID-IDREFTYP-UT = 'AIR  '                                     
001506         OR MID-IDREFTYP-UT = 'AIRCR'                                     
001507         OR MID-IDREFTYP-UT = 'BOAT '                                     
001508         OR MID-IDREFTYP-UT = 'LOCAL'                                     
001509           IF MID-IDREFTYP-UT = 'AIR  '                                   
001510             MOVE 'A'        TO WS-IDREFTYP                               
001511           END-IF                                                         
001512           IF MID-IDREFTYP-UT = 'AIRCR'                                   
001513             MOVE 'C'        TO WS-IDREFTYP                               
001514           END-IF                                                         
001515           IF MID-IDREFTYP-UT = 'BOAT '                                   
001516             MOVE 'B'        TO WS-IDREFTYP                               
001517           END-IF                                                         
001518           IF MID-IDREFTYP-UT = 'LOCAL'                                   
001519             MOVE 'L'        TO WS-IDREFTYP                               
001520           END-IF                                                         
001521         ELSE                                                             
001522           MOVE WS-IDTYPE    TO WS-IDREFTYP                               
001523         END-IF                                                           
001524       ELSE                                                               
001525         MOVE MID-IDREFTYP-IN                                             
001526                             TO WS-IDREFTYP                               
001527       END-IF                                                             
001528       IF WS-IDREFTYP = 'B'                                               
001529       OR WS-IDREFTYP = 'A'                                               
001530       OR WS-IDREFTYP = 'C'                                               
001531       OR WS-IDREFTYP = 'L'                                               
001532       OR SW-TRANSF-JA                                                    
001533         IF WS-IDREFTYP = 'B'                                             
001534           MOVE 'BOAT'       TO MOD-IDREFTYP-UT                           
001535         END-IF                                                           
001536         IF WS-IDREFTYP = 'A'                                             
001537           MOVE 'AIR'        TO MOD-IDREFTYP-UT                           
001538         END-IF                                                           
001539         IF WS-IDREFTYP = 'C'                                             
001540           MOVE 'AIRCR'      TO MOD-IDREFTYP-UT                           
001541         END-IF                                                           
001542         IF WS-IDREFTYP = 'L'                                             
001543           MOVE 'LOCAL'      TO MOD-IDREFTYP-UT                           
001544         END-IF                                                           
001545       ELSE                                                               
001546         IF MID-IDREFTYP-IN NOT = ALL '+'                                 
001547           MOVE NEJ          TO NYCKLAR-SW                                
001548           MOVE MED-7        TO MOD-TEMFSINF                              
001549         END-IF                                                           
001550       END-IF                                                             
001551       INSPECT MOD-IDREFTYP-UT REPLACING LEADING '+'  BY SPACE            
001552                                                                          
001553                                                                          
001554     ELSE                                                                 
001555       IF  MFS-IDTRANS = '2351'                                           
001556                                                                          
001557         MOVE +1             TO IX                                        
001558         PERFORM UNTIL IX > +13                                           
001559         OR MID-SELECT-2351 (IX) NOT = '+'                                
001560           ADD +1            TO IX                                        
001561         END-PERFORM                                                      
001562                                                                          
001563         MOVE ZERO           TO WS-IDPERSON-BUY                           
001564         MOVE SPACE          TO WS-IDTYPE                                 
001565                                WS-IDREFTYP                               
001566                                                                          
001567         IF IX > +13                                                      
001568           CONTINUE                                                       
001569         ELSE                                                             
001570           INSPECT MID-IDPERSON-2351 (IX) REPLACING                       
001571                                     LEADING SPACE BY ZERO                
001572             IF MID-IDTYPE-2351-IN = '+'                                  
001573               IF MID-IDTYPE-2351-UT = 'AIR  '                            
001574                 MOVE 'A'    TO WS-IDTYPE                                 
001575                                WS-IDREFTYP                               
001576               END-IF                                                     
001577               IF MID-IDTYPE-2351-UT = 'AIRCR'                            
001578                 MOVE 'C'    TO WS-IDTYPE                                 
001579                                WS-IDREFTYP                               
001580               END-IF                                                     
001581               IF MID-IDTYPE-2351-UT = 'BOAT '                            
001582                 MOVE 'B'    TO WS-IDTYPE                                 
001583                                WS-IDREFTYP                               
001584               END-IF                                                     
001585               IF MID-IDTYPE-2351-UT = 'LOCAL'                            
001586                 MOVE 'L'    TO WS-IDTYPE                                 
001587                                WS-IDREFTYP                               
001588               END-IF                                                     
001589             ELSE                                                         
001590               MOVE MID-IDTYPE-2351-IN                                    
001591                             TO WS-IDTYPE                                 
001592                                WS-IDREFTYP                               
001593             END-IF                                                       
001594             MOVE MID-IDPERSON-2351 (IX)                                  
001595                             TO WS-IDPERSON-BUY                           
001596         END-IF                                                           
001597                                                                          
001598         MOVE '7'            TO MFS-IDPFK                                 
001599         MOVE SPACE          TO MFS-KDTRTYP                               
001600                                                                          
001601         MOVE 'N'            TO WS-STATUS                                 
001602         INSPECT WS-IDPERSON-BUY REPLACING                                
001603                                     LEADING SPACE BY ZERO                
001604         IF WS-IDPERSON-BUY-NUM NUMERIC                                   
001605           MOVE WS-IDPERSON-BUY-NUM                                       
001606                             TO W-IDPERSON-BUY                            
001607                                WS-IDPERSON-BUY-RED                       
001608                                                                          
001609           MOVE WS-IDPERSON-BUY-RED                                       
001610                             TO MOD-IDPERSON-BUY-UT                       
001611           INSPECT MOD-IDPERSON-BUY-UT REPLACING                          
001612                                     LEADING '+'      BY SPACE            
001613         ELSE                                                             
001614           MOVE NEJ TO NYCKLAR-SW                                         
001615         END-IF                                                           
001616                                                                          
001617*      -- KONTROLL AV IDDC                                                
001618         IF DCS-NDC-NA                                                    
001619            MOVE IDDC-WS     TO W-IDDC-TP5                                
001620         ELSE                                                             
001621            MOVE NEJ         TO NYCKLAR-SW                                
001622         END-IF                                                           
001623         MOVE IDDC-WS        TO MOD-IDDC-UT                               
001624                                                                          
001625         IF WS-IDTYPE = 'A'                                               
001626         OR WS-IDTYPE = 'C'                                               
001627         OR WS-IDTYPE = 'B'                                               
001628         OR WS-IDTYPE = 'L'                                               
001629         OR SW-TRANSF-JA                                                  
001630           IF WS-IDTYPE = 'A'                                             
001631             MOVE 'AIR'      TO MOD-IDTYPE-UT                             
001632                                MOD-IDREFTYP-UT                           
001633           END-IF                                                         
001634           IF WS-IDTYPE = 'C'                                             
001635             MOVE 'AIRCR'    TO MOD-IDTYPE-UT                             
001636                                MOD-IDREFTYP-UT                           
001637           END-IF                                                         
001638           IF WS-IDTYPE = 'B'                                             
001639             MOVE 'BOAT'     TO MOD-IDTYPE-UT                             
001640                                MOD-IDREFTYP-UT                           
001641           END-IF                                                         
001642           IF WS-IDTYPE = 'L'                                             
001643             MOVE 'LOCAL'    TO MOD-IDTYPE-UT                             
001644                                MOD-IDREFTYP-UT                           
001645           END-IF                                                         
001646         ELSE                                                             
001647           MOVE NEJ TO NYCKLAR-SW                                         
001648         END-IF                                                           
001649         INSPECT MOD-IDTYPE-UT REPLACING                                  
001650                                   LEADING '+' BY SPACE                   
001651                                                                          
001652         MOVE 'N'            TO WS-STATUS                                 
001653         MOVE 'NOT REVIEWED'                                              
001654                             TO MOD-IDSTATUS-UT                           
001655                                                                          
001656       ELSE                                                               
001657*      -- KONTROLL AV IDARTNR                                             
001658                                                                          
001659         IF WS-IDARTNR NUMERIC                                            
001660           MOVE WS-IDARTNR-NUM                                            
001661                               TO W-IDARTNR-MIN                           
001662                                  W-IDARTNR-301                           
001663                                  W-IDARTNR                               
001664                                  MID-IDARTNR-IN                          
001665         ELSE                                                             
001666           MOVE NEJ            TO NYCKLAR-SW                              
001667         END-IF                                                           
001668                                                                          
001669         MOVE WS-IDARTNR       TO MOD-IDARTNR-UT                          
001670                                                                          
001671         INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE           
001672         INSPECT MOD-IDARTNR-UT REPLACING LEADING '+' BY SPACE            
001673                                                                          
001674*      -- KONTROLL AV IDDC                                                
001675         IF DCS-NDC-NA                                                    
001676            MOVE IDDC-WS       TO W-IDDC-TP5                              
001677         ELSE                                                             
001678            MOVE NEJ           TO NYCKLAR-SW                              
001679         END-IF                                                           
001680         MOVE IDDC-WS          TO MOD-IDDC-UT                             
001681                                                                          
001682         IF MSGI-SPAR-AREA(1:4) = '2352'                                  
001683           MOVE MSGI-SPAR-AREA                                            
001684                               TO WS-MSGI-AREA-2352                       
001685           IF WS-MSGI-IDTYPE = 'A'                                        
001686           OR WS-MSGI-IDTYPE = 'C'                                        
001687           OR WS-MSGI-IDTYPE = 'B'                                        
001688           OR WS-MSGI-IDTYPE = 'L'                                        
001689             IF WS-MSGI-IDTYPE = 'A'                                      
001690               MOVE 'AIR'      TO MOD-IDTYPE-UT                           
001691                                  MOD-IDREFTYP-UT                         
001692             END-IF                                                       
001693             IF WS-MSGI-IDTYPE = 'C'                                      
001694               MOVE 'AIRCR'    TO MOD-IDTYPE-UT                           
001695                                  MOD-IDREFTYP-UT                         
001696             END-IF                                                       
001697             IF WS-MSGI-IDTYPE = 'B'                                      
001698               MOVE 'BOAT'     TO MOD-IDTYPE-UT                           
001699                                  MOD-IDREFTYP-UT                         
001700             END-IF                                                       
001701             IF WS-MSGI-IDTYPE = 'L'                                      
001702               MOVE 'LOCAL'    TO MOD-IDTYPE-UT                           
001703                                  MOD-IDREFTYP-UT                         
001704             END-IF                                                       
001705             MOVE WS-MSGI-IDTYPE                                          
001706                               TO MID-IDTYPE-IN                           
001707                                  MID-IDREFTYP-IN                         
001708                                  WS-IDTYPE                               
001709                                  WS-IDREFTYP                             
001710                                  W-KDREFTYP-DEL                          
001711           ELSE                                                           
001712             MOVE '+'          TO MID-IDTYPE-IN                           
001713                                  MID-IDREFTYP-IN                         
001714           END-IF                                                         
001715         ELSE                                                             
001716           MOVE '+'            TO MID-IDTYPE-IN                           
001717                                  MID-IDREFTYP-IN                         
001718         END-IF                                                           
001719                                                                          
001720         MOVE 'N'              TO WS-STATUS                               
001721         MOVE 'NOT REVIEWED'                                              
001722                               TO MOD-IDSTATUS-UT                         
001723                                                                          
001724       END-IF                                                             
001725                                                                          
001726                                                                          
001727     END-IF                                                               
001728                                                                          
001729*--  KONTROLL AV KDARBTYP                                                 
001730     IF MSGI-KDARBTYP-SEC(1:3) = 'ESC'                                    
001731       MOVE 'ESC' TO WS-KDARBTYP-SEC                                      
001732     ELSE                                                                 
001733       IF MSGI-KDARBTYP-SEC(1:3) = 'LOC'                                  
001734         MOVE 'LOC' TO WS-KDARBTYP-SEC                                    
001735       ELSE                                                               
001736         MOVE 'FEL' TO WS-KDARBTYP-SEC                                    
001737       END-IF                                                             
001738     END-IF                                                               
001739                                                                          
001740     IF NYCKLAR-OK                                                        
001741        PERFORM S04-GET-DCGROUP-ALL-DC                                    
001742        IF NYCKLAR-OK                                                     
001743           MOVE +1           TO IX-DC                                     
001744           PERFORM UNTIL IX-DC  > IX-DC-MAX                               
001745                                                                          
001746             IF WS-DC-NR (IX-DC)  > SPACES                                
001747                MOVE WS-DC-NR (IX-DC)                                     
001748                             TO W-IDDC                                    
001749                                W-IDDC-B6                                 
001750                PERFORM S06-GET-REFILL-DISTRIKT                           
001751             ELSE                                                         
001752                MOVE ZEROES  TO WS-IDDISTR       (IX-DC)                  
001753                                WS-IDDISTR-B6    (IX-DC)                  
001754                                WS-IDDISTR-2-DEL (IX-DC)                  
001755                MOVE SPACES  TO WS-SENDING-DC-NR (IX-DC)                  
001756             END-IF                                                       
001757             ADD +1          TO IX-DC                                     
001758           END-PERFORM                                                    
001759        END-IF                                                            
001760                                                                          
001761     END-IF                                                               
001762                                                                          
001763***LYNK PARTS ONLY IN EUROPE                                              
001764     MOVE NEJ     TO SW-LYNK-PART                                         
001765     PERFORM IMS-GU-WDK6-ARTC01                                           
001766     IF SEGMENT-FINNS                                                     
001767       IF  ART-KDPRODSL > 30                                              
001768       AND ART-KDPRODSL < 40                                              
001769         MOVE NEJ          TO NYCKLAR-SW                                  
001770         MOVE JA           TO SW-LYNK-PART                                
001771       END-IF                                                             
001772     END-IF                                                               
001773                                                                          
001774     IF NYCKLAR-FEL                                                       
001775       IF LYNK-PART                                                       
001776         MOVE MED-19          TO MOD-TEMFSFEL                             
001777       ELSE                                                               
001778         MOVE ERR-WRONG-KEY   TO MED-IDMFSFEL                             
001779         CALL WMEDKONV USING MED-WMEDAREA                                 
001780         MOVE MED-MFSFEL      TO MOD-TEMFSFEL                             
001781       END-IF                                                             
001782       PERFORM MFS-RENSA-FAELT-IN                                         
001783       PERFORM MFS-RENSA-FAELT-UT                                         
001784     ELSE                                                                 
001785       IF WS-IDPERSON-BUY NUMERIC                                         
001786         MOVE WS-IDPERSON-BUY                                             
001787                             TO W-IDPERSON-BUY                            
001788         IF WS-IDPERSON-BUY > ZERO                                        
001789           MOVE WS-IDPERSON-BUY-NUM                                       
001790                             TO W-IDPERSON-BUY-MIN                        
001791                                W-IDPERSON-BUY-MAX                        
001792         END-IF                                                           
001793       ELSE                                                               
001794         MOVE ZERO           TO WS-IDPERSON-BUY                           
001795                                W-IDPERSON-BUY                            
001796       END-IF                                                             
001797       MOVE WS-IDREFTYP      TO W-KDREFTYP-MIN                            
001798                                W-KDREFTYP-MAX                            
001799                                                                          
001800       MOVE WS-IDTYPE        TO WS-MSGI-IDTYPE                            
001801                                W-KDREFTYP-DEL                            
001802     END-IF                                                               
001803     .                                                                    
001804     EJECT                                                                
001805 C-FOEREG-SIDA SECTION.                                                   
001806     MOVE NEJ                TO SW-TRAEFF                                 
001807     PERFORM CA-BLAEDDRA-BAK                                              
001808     IF SW-TRAEFF-JA                                                      
001809       CONTINUE                                                           
001810     ELSE                                                                 
001811       MOVE URVAL-SAKNAS     TO MED-IDMFSFEL                              
001812       CALL WMEDKONV USING MED-WMEDAREA                                   
001813       MOVE MED-TEMFSFEL     TO MOD-TEMFSFEL                              
001814       IF EGEN-MID                                                        
001815         MOVE ZERO           TO W-IDARTNR                                 
001816       END-IF                                                             
001817       PERFORM MFS-RENSA-FAELT-IN                                         
001818       PERFORM MFS-RENSA-FAELT-UT                                         
001819     END-IF                                                               
001820     .                                                                    
001821     EJECT                                                                
001822 CA-BLAEDDRA-BAK SECTION.                                                 
001823                                                                          
001824     MOVE NEJ                TO SW-TRAEFF                                 
001825****   BLÄDDRA TILLBAKA TILL FÖREGÅENDE ARTIKEL *******                   
001826****   (OM DET FINNS NÅGON)                     *******                   
001827     MOVE WS-MSGI-IDARTNR-PF7                                             
001828                             TO W-IDARTNR                                 
001829                                W-IDARTNR-301                             
001830                                W-IDARTNR-MIN                             
001831     MOVE WS-DC-NR(1)        TO W-IDDC-MIN                                
001832     MOVE WS-DC-NR(IX-DC-MAX)                                             
001833                             TO W-IDDC-MAX                                
001834                                                                          
001835     PERFORM IMS-GU-WDE3-ORDW01-MIN-MAX                                   
001836     IF ORDW-SEQA-IDDC NOT = DCS-IDDC                                     
001837        MOVE ORDW-SEQA-IDDC  TO W-IDDC-B6                                 
001838        PERFORM IMS-GU-WDB601                                             
001839     END-IF                                                               
001840     PERFORM S05-CHECK-IDDC                                               
001841                                                                          
001842     PERFORM UNTIL SEGMENT-SAKNAS                                         
001843     OR (ORDW-SEQA-IDARTNR = WS-MSGI-IDARTNR-PF7                          
001844     AND VALID-IDDC-JA                                                    
001845     AND DCS-NDC-NA                                                       
001846     AND  ORDW-SEQA-IDPERSON-BUY = WS-IDPERSON-BUY-NUM                    
001847     AND  ORDW-SEQA-KDREFTYP = WS-IDTYPE                                  
001848     AND   ((WS-MSGI-ORDER-PF7 = 'N'                                      
001849        AND ORDW-SEQA-KDREFORS = 'P')                                     
001850       OR (WS-MSGI-ORDER-PF7   = 'R'                                      
001851        AND ORDW-SEQA-KDREFORS = 'O')))                                   
001852          PERFORM IMS-GN-WDE3-ORDW01-MIN-MAX                              
001853          IF ORDW-SEQA-IDDC NOT = DCS-IDDC                                
001854             MOVE ORDW-SEQA-IDDC     TO W-IDDC-B6                         
001855             PERFORM IMS-GU-WDB601                                        
001856          END-IF                                                          
001857                                                                          
001858     END-PERFORM                                                          
001859                                                                          
001860     IF SEGMENT-FINNS                                                     
001861*       --- CHECK IDLEVNR-SECURITY                                        
001862        MOVE ORDW-SEQA-IDARTNR TO W-IDARTNR                               
001863        PERFORM S1-SECURITY-CHECK-PARTNO                                  
001864     END-IF                                                               
001865                                                                          
001866     IF SEGMENT-FINNS                                                     
001867       PERFORM K-FYLL-I-NYCKEL-FAELT                                      
001868                                                                          
001869     ELSE                                                                 
001870       MOVE ZERO             TO W-IDARTNR                                 
001871                                W-IDARTNR-301                             
001872                                W-IDARTNR-MIN                             
001873       PERFORM IMS-GU-WDE3-ORDW01-MIN-MAX                                 
001874       IF ORDW-SEQA-IDDC NOT = DCS-IDDC                                   
001875          MOVE ORDW-SEQA-IDDC     TO W-IDDC-B6                            
001876          PERFORM IMS-GU-WDB601                                           
001877       END-IF                                                             
001878       PERFORM S05-CHECK-IDDC                                             
001879                                                                          
001880       PERFORM UNTIL SEGMENT-SAKNAS                                       
001881       OR (ORDW-SEQA-IDPERSON-BUY = WS-IDPERSON-BUY-NUM                   
001882       AND VALID-IDDC-JA                                                  
001883       AND DCS-NDC-NA                                                     
001884       AND (ORDW-SEQA-KDREFTYP = WS-IDTYPE                                
001885       AND ((WS-STATUS  = 'N'                                             
001886        AND ORDW-SEQA-KDREFORS = 'P')                                     
001887       OR (WS-STATUS    = 'R'                                             
001888        AND ORDW-SEQA-KDREFORS = 'O'))))                                  
001889                                                                          
001890           PERFORM IMS-GN-WDE3-ORDW01-MIN-MAX                             
001891           IF ORDW-SEQA-IDDC NOT = DCS-IDDC                               
001892              MOVE ORDW-SEQA-IDDC     TO W-IDDC-B6                        
001893              PERFORM IMS-GU-WDB601                                       
001894           END-IF                                                         
001895           PERFORM S05-CHECK-IDDC                                         
001896       END-PERFORM                                                        
001897                                                                          
001898       IF SEGMENT-FINNS                                                   
001899*         --- CHECK IDLEVNR-SECURITY                                      
001900          MOVE ORDW-SEQA-IDARTNR TO W-IDARTNR                             
001901          PERFORM S1-SECURITY-CHECK-PARTNO                                
001902       END-IF                                                             
001903                                                                          
001904       IF SEGMENT-FINNS                                                   
001905         PERFORM K-FYLL-I-NYCKEL-FAELT                                    
001906       END-IF                                                             
001907     END-IF                                                               
001908***                                                                       
001909***  REINITIALIZING DB POINTER TO FIRST SEGMENT FOUND                     
001910***                                                                       
001911     IF SEGMENT-FINNS                                                     
001912        MOVE ORDW-SEQA-KDREFTYP         TO W-KDREFTYP-MIN                 
001913        MOVE ORDW-SEQA-IDPERSON-BUY     TO W-IDPERSON-BUY-MIN             
001914        MOVE ORDW-SEQA-IDARTNR          TO W-IDARTNR-MIN                  
001915        MOVE WS-DC-NR (1)               TO W-IDDC-MIN                     
001916        PERFORM IMS-GU-WDE3-ORDW01-MIN-MAX                                
001917*                                                                         
001918        PERFORM UNTIL SEGMENT-SAKNAS                                      
001919        OR NOT (WS-SPARA-IDARTNR = ORDW-SEQA-IDARTNR                      
001920        AND DCS-NDC-NA)                                                   
001921**********      LÄS ALLA REFILLPOSTER FÖR AKTUELL                         
001922**********      ARTNR FÖR NDC MED RÄTT KDREFTYP                           
001923**********      SÖKT STATUS ÄR REDAN FRAMLÄST                             
001924          IF ORDW-SEQA-KDREFTYP = WS-IDTYPE                               
001925            MOVE +1             TO IX-DC                                  
001926            PERFORM UNTIL IX-DC  > IX-DC-MAX                              
001927              IF ORDW-SEQA-IDDC  = WS-DC-NR (IX-DC)                       
001928                                                                          
001929                 MOVE ORDW-SEQA-IDWDE301                                  
001930                                TO W-WDE301KY-X                           
001931                 PERFORM IMS-GU-WDE3-ORDL01                               
001932                                                                          
001933                 PERFORM M-UPD-MOD-FAELT                                  
001934              END-IF                                                      
001935              ADD +1            TO IX-DC                                  
001936            END-PERFORM                                                   
001937          END-IF                                                          
001938                                                                          
001939          PERFORM IMS-GN-WDE3-ORDW01-MIN-MAX                              
001940          IF ORDW-SEQA-IDDC NOT = DCS-IDDC                                
001941             MOVE ORDW-SEQA-IDDC     TO W-IDDC-B6                         
001942             PERFORM IMS-GU-WDB601                                        
001943          END-IF                                                          
001944          PERFORM S05-CHECK-IDDC                                          
001945        END-PERFORM                                                       
001946     END-IF                                                               
001947     .                                                                    
001948     EJECT                                                                
001949 D-NAESTA-SIDA SECTION.                                                   
001950     MOVE WS-IDPERSON-BUY    TO W-IDPERSON-BUY-MIN                        
001951     MOVE WS-IDTYPE          TO W-KDREFTYP-MIN                            
001952     MOVE WS-IDARTNR         TO W-IDARTNR-MIN                             
001953     MOVE NEJ                TO SW-TRAEFF                                 
001954     PERFORM DA-BLAEDDRA-FRAM                                             
001955     IF SW-TRAEFF-JA                                                      
001956       CONTINUE                                                           
001957     ELSE                                                                 
001958       MOVE URVAL-SAKNAS     TO MED-IDMFSFEL                              
001959       CALL WMEDKONV USING MED-WMEDAREA                                   
001960       MOVE MED-TEMFSFEL     TO MOD-TEMFSFEL                              
001961       MOVE ZERO             TO W-IDARTNR                                 
001962       PERFORM MFS-RENSA-FAELT-IN                                         
001963       PERFORM MFS-RENSA-FAELT-UT                                         
001964     END-IF                                                               
001965     .                                                                    
001966     EJECT                                                                
001967 DA-BLAEDDRA-FRAM SECTION.                                                
001968                                                                          
001969     PERFORM IMS-GU-WDE3-ORDW01-PF8                                       
001970     IF ORDW-SEQA-IDDC NOT = DCS-IDDC                                     
001971        MOVE ORDW-SEQA-IDDC     TO W-IDDC-B6                              
001972        PERFORM IMS-GU-WDB601                                             
001973     END-IF                                                               
001974     PERFORM S05-CHECK-IDDC                                               
001975                                                                          
001976     PERFORM UNTIL SEGMENT-SAKNAS                                         
001977     OR  (ORDW-SEQA-IDPERSON-BUY = WS-IDPERSON-BUY-NUM                    
001978     AND VALID-IDDC-JA                                                    
001979     AND DCS-NDC-NA                                                       
001980     AND  ORDW-SEQA-IDARTNR > W-IDARTNR                                   
001981     AND (ORDW-SEQA-KDREFTYP = WS-IDTYPE                                  
001982     AND ((WS-STATUS        = 'N'                                         
001983     AND ORDW-SEQA-KDREFORS = 'P')                                        
001984     OR      (WS-STATUS     = 'R'                                         
001985     AND ORDW-SEQA-KDREFORS = 'O'))))                                     
001986                                                                          
001987         PERFORM IMS-GN-WDE3-ORDW01-PF8                                   
001988         IF ORDW-SEQA-IDDC NOT = DCS-IDDC                                 
001989            MOVE ORDW-SEQA-IDDC     TO W-IDDC-B6                          
001990            PERFORM IMS-GU-WDB601                                         
001991         END-IF                                                           
001992         PERFORM S05-CHECK-IDDC                                           
001993     END-PERFORM                                                          
001994                                                                          
001995     IF SEGMENT-FINNS                                                     
001996*       --- CHECK IDLEVNR-SECURITY                                        
001997        MOVE ORDW-SEQA-IDARTNR TO W-IDARTNR                               
001998        PERFORM S1-SECURITY-CHECK-PARTNO                                  
001999     END-IF                                                               
002000                                                                          
002001     IF SEGMENT-FINNS                                                     
002002       PERFORM K-FYLL-I-NYCKEL-FAELT                                      
002003     ELSE                                                                 
002004       MOVE ZERO             TO W-IDARTNR                                 
002005     END-IF                                                               
002006     MOVE WS-MSGI-IDARTNR-ENTER                                           
002007                             TO WS-MSGI-IDARTNR-PF7                       
002008     MOVE WS-MSGI-ORDER-ENTER                                             
002009                             TO WS-MSGI-ORDER-PF7                         
002010                                                                          
002011***                                                                       
002012***  REINITIALIZING DB POINTER TO FIRST SEGMENT FOUND                     
002013***                                                                       
002014     IF SEGMENT-FINNS                                                     
002015        MOVE ORDW-SEQA-KDREFTYP         TO W-KDREFTYP-MIN                 
002016        MOVE ORDW-SEQA-IDPERSON-BUY     TO W-IDPERSON-BUY-MIN             
002017        MOVE ORDW-SEQA-IDARTNR          TO W-IDARTNR-MIN                  
002018        MOVE WS-DC-NR (1)               TO W-IDDC-MIN                     
002019        PERFORM IMS-GU-WDE3-ORDW01-MIN-MAX                                
002020*                                                                         
002021        PERFORM UNTIL SEGMENT-SAKNAS                                      
002022        OR NOT (WS-SPARA-IDARTNR = ORDW-SEQA-IDARTNR                      
002023        AND DCS-NDC-NA)                                                   
002024**********      LÄS ALLA REFILLPOSTER FÖR AKTUELL                         
002025**********      ARTNR FÖR NDC MED RÄTT KDREFTYP                           
002026**********      SÖKT STATUS ÄR REDAN FRAMLÄST                             
002027          IF ORDW-SEQA-KDREFTYP = WS-IDTYPE                               
002028            MOVE +1             TO IX-DC                                  
002029            PERFORM UNTIL IX-DC  > IX-DC-MAX                              
002030              IF ORDW-SEQA-IDDC  = WS-DC-NR (IX-DC)                       
002031                                                                          
002032                 MOVE ORDW-SEQA-IDWDE301                                  
002033                                TO W-WDE301KY-X                           
002034                 PERFORM IMS-GU-WDE3-ORDL01                               
002035                                                                          
002036                 PERFORM M-UPD-MOD-FAELT                                  
002037              END-IF                                                      
002038              ADD +1            TO IX-DC                                  
002039            END-PERFORM                                                   
002040          END-IF                                                          
002041                                                                          
002042          PERFORM IMS-GN-WDE3-ORDW01-PF8                                  
002043          IF ORDW-SEQA-IDDC NOT = DCS-IDDC                                
002044             MOVE ORDW-SEQA-IDDC     TO W-IDDC-B6                         
002045             PERFORM IMS-GU-WDB601                                        
002046          END-IF                                                          
002047          PERFORM S05-CHECK-IDDC                                          
002048        END-PERFORM                                                       
002049     END-IF                                                               
002050     .                                                                    
002051     EJECT                                                                
002052 E-SAMMA-SIDA SECTION.                                                    
002053                                                                          
002054     IF MID-IDARTNR-IN       = ALL '+'                                    
002055     AND MID-IDDC-IN         = ALL '+'                                    
002056     AND MID-IDTYPE-IN       = ALL '+'                                    
002057     AND MID-IDPERSON-BUY-IN = ALL '+'                                    
002058     AND MID-IDSTATUS-IN     = ALL '+'                                    
002059     AND MID-IDREFTYP-IN     = ALL '+'                                    
002060       MOVE +1               TO IX-DC                                     
002061       PERFORM UNTIL IX-DC   >  IX-DC-MAX                                 
002062         IF  MID-KVPB-REF (IX-DC) = ALL '+'                               
002063         OR  MID-KVPBREOI (IX-DC) = ALL '+'                               
002064             CONTINUE                                                     
002065         ELSE                                                             
002066             MOVE JA         TO SW-KVPB-REF-INPUT                         
002067         END-IF                                                           
002068         ADD +1              TO IX-DC                                     
002069       END-PERFORM                                                        
002070*----     INGEN INFO LIGGER I BILDEN                                      
002071*----     SKA SKRIVAS UT I F-HAEMTA                                       
002072*----                                                                     
002073       IF SW-KVPB-REF-INPUT-JA                                            
002074          MOVE NEJ           TO SW-HAEMTA-INPUT-FAELT                     
002075       END-IF                                                             
002076                                                                          
002077       IF MID-INPUT      NOT  = ALL '+'                                   
002078         PERFORM EB-SIMULERA                                              
002079         PERFORM I-KOLLA-INPUT                                            
002080         PERFORM EE-BERAKNA-REFPKT                                        
002081         MOVE +1             TO IX-DC                                     
002082         PERFORM UNTIL  IX-DC > IX-DC-MAX                                 
002083           MOVE WS-PURCHQTY (IX-DC)                                       
002084                             TO WS-PURCHQTY-SIM (IX-DC)                   
002085           ADD +1            TO IX-DC                                     
002086         END-PERFORM                                                      
002087       END-IF                                                             
002088                                                                          
002089     ELSE                                                                 
002090                                                                          
002091       IF (MID-IDARTNR-IN NOT = ALL '+'                                   
002092       AND MID-IDTYPE-IN  NOT = ALL '+')                                  
002093       OR MID-IDREFTYP-IN NOT = ALL '+'                                   
002094                                                                          
002095         PERFORM EC-SOEK-NY-ARTIKEL-TYP                                   
002096                                                                          
002097         IF SW-TRAEFF-JA                                                  
002098           CONTINUE                                                       
002099         ELSE                                                             
002100           PERFORM MFS-RENSA-FAELT-IN                                     
002101           PERFORM MFS-RENSA-FAELT-UT                                     
002102           MOVE URVAL-SAKNAS TO MED-IDMFSFEL                              
002103           CALL WMEDKONV USING MED-WMEDAREA                               
002104           MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                              
002105         END-IF                                                           
002106                                                                          
002107       ELSE                                                               
002108                                                                          
002109           IF MID-IDARTNR-IN NOT = ALL '+'                                
002110           OR MID-IDDC-IN    NOT = ALL '+'                                
002111           AND MID-IDTYPE-IN    = ALL '+'                                 
002112           AND MID-IDREFTYP-IN  = ALL '+'                                 
002113                                                                          
002114             PERFORM ED-SOEK-NY-ARTIKEL                                   
002115                                                                          
002116             IF SW-TRAEFF-JA                                              
002117               CONTINUE                                                   
002118             ELSE                                                         
002119               PERFORM MFS-RENSA-FAELT-IN                                 
002120               PERFORM MFS-RENSA-FAELT-UT                                 
002121               MOVE URVAL-SAKNAS TO MED-IDMFSFEL                          
002122               CALL WMEDKONV  USING MED-WMEDAREA                          
002123               MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                          
002124             END-IF                                                       
002125                                                                          
002126           ELSE                                                           
002127                                                                          
002128             IF MID-IDPERSON-BUY-IN NOT = ALL '+'                         
002129             OR MID-IDTYPE-IN   NOT = ALL '+'                             
002130             OR MID-IDSTATUS-IN NOT = ALL '+'                             
002131                                                                          
002132               PERFORM EA-NY-BUY-TYPE-LEV-STAT                            
002133                                                                          
002134               IF SW-TRAEFF-JA                                            
002135                 CONTINUE                                                 
002136               ELSE                                                       
002137                 PERFORM MFS-RENSA-FAELT-IN                               
002138                 PERFORM MFS-RENSA-FAELT-UT                               
002139                 MOVE URVAL-SAKNAS TO MED-IDMFSFEL                        
002140                 CALL WMEDKONV  USING MED-WMEDAREA                        
002141                 MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                        
002142               END-IF                                                     
002143                                                                          
002144                                                                          
002145             END-IF                                                       
002146           END-IF                                                         
002147       END-IF                                                             
002148     END-IF                                                               
002149                                                                          
002150     .                                                                    
002151     EJECT                                                                
002152                                                                          
002153 EA-NY-BUY-TYPE-LEV-STAT SECTION.                                         
002154                                                                          
002155     MOVE NEJ                TO SW-TRAEFF                                 
002156     MOVE ZERO               TO W-IDARTNR                                 
002157     MOVE MFS-RENSA-FAELT    TO MOD-IDARTNR-UT                            
002158     PERFORM IMS-GU-WDE3-ORDW01-MIN-MAX                                   
002159     IF ORDW-SEQA-IDDC NOT = DCS-IDDC                                     
002160        MOVE ORDW-SEQA-IDDC     TO W-IDDC-B6                              
002161        PERFORM IMS-GU-WDB601                                             
002162     END-IF                                                               
002163     PERFORM S05-CHECK-IDDC                                               
002164                                                                          
002165     PERFORM UNTIL SEGMENT-SAKNAS                                         
002166     OR  (ORDW-SEQA-KDREFTYP = WS-IDTYPE                                  
002167       AND VALID-IDDC-JA                                                  
002168       AND DCS-NDC-NA                                                     
002169       AND ((WS-STATUS         = 'N'                                      
002170       AND  ORDW-SEQA-KDREFORS = 'P')                                     
002171       OR  (WS-STATUS          = 'R'                                      
002172       AND  ORDW-SEQA-KDREFORS = 'O')))                                   
002173            PERFORM IMS-GN-WDE3-ORDW01-MIN-MAX                            
002174            IF ORDW-SEQA-IDDC NOT = DCS-IDDC                              
002175               MOVE ORDW-SEQA-IDDC     TO W-IDDC-B6                       
002176               PERFORM IMS-GU-WDB601                                      
002177            END-IF                                                        
002178                                                                          
002179     END-PERFORM                                                          
002180                                                                          
002181     IF SEGMENT-FINNS                                                     
002182*       --- CHECK IDLEVNR-SECURITY                                        
002183        MOVE ORDW-SEQA-IDARTNR TO W-IDARTNR                               
002184        PERFORM S1-SECURITY-CHECK-PARTNO                                  
002185     END-IF                                                               
002186                                                                          
002187     IF SEGMENT-FINNS                                                     
002188       PERFORM K-FYLL-I-NYCKEL-FAELT                                      
002189     END-IF                                                               
002190                                                                          
002191***                                                                       
002192***  REINITIALIZING DB POINTER TO FIRST SEGMENT FOUND                     
002193***                                                                       
002194     IF SEGMENT-FINNS                                                     
002195        MOVE ORDW-SEQA-KDREFTYP         TO W-KDREFTYP-MIN                 
002196        MOVE ORDW-SEQA-IDPERSON-BUY     TO W-IDPERSON-BUY-MIN             
002197        MOVE ORDW-SEQA-IDARTNR          TO W-IDARTNR-MIN                  
002198        MOVE WS-DC-NR (1)               TO W-IDDC-MIN                     
002199        PERFORM IMS-GU-WDE3-ORDW01-MIN-MAX                                
002200*                                                                         
002201        PERFORM UNTIL SEGMENT-SAKNAS                                      
002202        OR NOT (WS-SPARA-IDARTNR = ORDW-SEQA-IDARTNR                      
002203        AND DCS-NDC-NA)                                                   
002204          IF ORDW-SEQA-KDREFTYP = WS-IDTYPE                               
002205                                                                          
002206            MOVE +1             TO IX-DC                                  
002207            PERFORM UNTIL IX-DC  > IX-DC-MAX                              
002208              IF ORDW-SEQA-IDDC  = WS-DC-NR (IX-DC)                       
002209                                                                          
002210                 MOVE ORDW-SEQA-IDWDE301                                  
002211                                TO W-WDE301KY-X                           
002212                 PERFORM IMS-GU-WDE3-ORDL01                               
002213                                                                          
002214                 PERFORM M-UPD-MOD-FAELT                                  
002215              END-IF                                                      
002216              ADD +1            TO IX-DC                                  
002217            END-PERFORM                                                   
002218          END-IF                                                          
002219                                                                          
002220          PERFORM IMS-GN-WDE3-ORDW01-MIN-MAX                              
002221          IF ORDW-SEQA-IDDC NOT = DCS-IDDC                                
002222             MOVE ORDW-SEQA-IDDC     TO W-IDDC-B6                         
002223             PERFORM IMS-GU-WDB601                                        
002224          END-IF                                                          
002225          PERFORM S05-CHECK-IDDC                                          
002226        END-PERFORM                                                       
002227     END-IF                                                               
002228     .                                                                    
002229     EJECT                                                                
002230 EB-SIMULERA SECTION.                                                     
002231                                                                          
002232                                                                          
002233     MOVE NEJ                TO SW-TRAEFF                                 
002234     MOVE WS-MSGI-IDARTNR-ENTER                                           
002235                             TO W-IDARTNR                                 
002236                                W-IDARTNR-301                             
002237                                W-IDARTNR-MIN                             
002238     PERFORM IMS-GU-WDE3-ORDW01-MIN-MAX                                   
002239     IF ORDW-SEQA-IDDC NOT = DCS-IDDC                                     
002240        MOVE ORDW-SEQA-IDDC  TO W-IDDC-B6                                 
002241        PERFORM IMS-GU-WDB601                                             
002242     END-IF                                                               
002243     PERFORM S05-CHECK-IDDC                                               
002244                                                                          
002245     PERFORM UNTIL SEGMENT-SAKNAS                                         
002246     OR (ORDW-SEQA-IDARTNR = W-IDARTNR-MIN                                
002247     AND VALID-IDDC-JA                                                    
002248     AND DCS-NDC-NA                                                       
002249     AND ORDW-SEQA-KDREFTYP = WS-IDREFTYP)                                
002250        PERFORM IMS-GN-WDE3-ORDW01-MIN-MAX                                
002251        IF ORDW-SEQA-IDDC NOT = DCS-IDDC                                  
002252           MOVE ORDW-SEQA-IDDC     TO W-IDDC-B6                           
002253           PERFORM IMS-GU-WDB601                                          
002254        END-IF                                                            
002255        PERFORM S05-CHECK-IDDC                                            
002256                                                                          
002257     END-PERFORM                                                          
002258                                                                          
002259     IF SEGMENT-FINNS                                                     
002260*       --- CHECK IDLEVNR-SECURITY                                        
002261        MOVE ORDW-SEQA-IDARTNR TO W-IDARTNR                               
002262        PERFORM S1-SECURITY-CHECK-PARTNO                                  
002263     END-IF                                                               
002264                                                                          
002265     IF SEGMENT-FINNS                                                     
002266       PERFORM K-FYLL-I-NYCKEL-FAELT                                      
002267     END-IF                                                               
002268                                                                          
002269     PERFORM UNTIL SEGMENT-SAKNAS                                         
002270     OR NOT (WS-SPARA-IDARTNR = ORDW-SEQA-IDARTNR                         
002271     AND VALID-IDDC-JA                                                    
002272     AND DCS-NDC-NA)                                                      
002273       IF ORDW-SEQA-KDREFTYP  = WS-IDREFTYP                               
002274                                                                          
002275         MOVE +1             TO IX-DC                                     
002276         PERFORM UNTIL IX-DC  > IX-DC-MAX                                 
002277           IF ORDW-SEQA-IDDC  = WS-DC-NR (IX-DC)                          
002278                                                                          
002279              MOVE ORDW-SEQA-IDWDE301                                     
002280                             TO W-WDE301KY-X                              
002281              PERFORM IMS-GU-WDE3-ORDL01                                  
002282                                                                          
002283              MOVE REF-KDREFTXT                                           
002284                             TO WS-REF-KDREFTXT (IX-DC)                   
002285           END-IF                                                         
002286           ADD +1            TO IX-DC                                     
002287         END-PERFORM                                                      
002288         IF REF-KDREFORS = 'P'                                            
002289            MOVE 'PROPOSAL NOT REVIEWED'                                  
002290                             TO MOD-ORDERSTATUS                           
002291         ELSE                                                             
002292            MOVE 'REVIEWED'  TO MOD-ORDERSTATUS                           
002293         END-IF                                                           
002294                                                                          
002295         IF REF-KDREFTYP = 'B'                                            
002296           MOVE 'BOAT'       TO MOD-IDREFTYP-UT                           
002297         END-IF                                                           
002298         IF REF-KDREFTYP = 'A'                                            
002299           MOVE 'AIR'        TO MOD-IDREFTYP-UT                           
002300         END-IF                                                           
002301         IF REF-KDREFTYP = 'C'                                            
002302           MOVE 'AIRCR'      TO MOD-IDREFTYP-UT                           
002303         END-IF                                                           
002304         IF REF-KDREFTYP = 'L'                                            
002305           MOVE 'LOCAL'      TO MOD-IDREFTYP-UT                           
002306         END-IF                                                           
002307       END-IF                                                             
002308                                                                          
002309       PERFORM IMS-GN-WDE3-ORDW01-MIN-MAX                                 
002310       IF ORDW-SEQA-IDDC NOT  = DCS-IDDC                                  
002311          MOVE ORDW-SEQA-IDDC     TO W-IDDC-B6                            
002312          PERFORM IMS-GU-WDB601                                           
002313       END-IF                                                             
002314       PERFORM S05-CHECK-IDDC                                             
002315     END-PERFORM                                                          
002316     .                                                                    
002317     EJECT                                                                
002318                                                                          
002319 EC-SOEK-NY-ARTIKEL-TYP SECTION.                                          
002320                                                                          
002321     MOVE NEJ                TO SW-TRAEFF                                 
002322*    TEXT 'NO PROPOSAL' SKRIVS I BILDEN OM EJ TRÄFF                       
002323     MOVE 'NO PROPOSAL'      TO MOD-ORDERSTATUS                           
002324     MOVE HIGH-VALUE         TO W-KDREFTYP-MAX                            
002325     MOVE W-IDARTNR          TO W-IDARTNR-MIN                             
002326     MOVE WS-DC-NR (1)       TO W-IDDC-MIN                                
002327     MOVE WS-DC-NR (IX-DC-MAX)                                            
002328                             TO W-IDDC-MAX                                
002329     IF WS-IDREFTYP = 'C'                                                 
002330        MOVE 'A'             TO W-KDREFTYP-MIN                            
002331     END-IF                                                               
002332                                                                          
002333     PERFORM IMS-GU-WDE3-ORDW01-MIN-MAX                                   
002334     IF ORDW-SEQA-IDDC NOT = DCS-IDDC                                     
002335        MOVE ORDW-SEQA-IDDC  TO W-IDDC-B6                                 
002336        PERFORM IMS-GU-WDB601                                             
002337     END-IF                                                               
002338     PERFORM S05-CHECK-IDDC                                               
002339                                                                          
002340     PERFORM UNTIL SEGMENT-SAKNAS                                         
002341     OR (ORDW-SEQA-IDARTNR = W-IDARTNR-MIN                                
002342     AND VALID-IDDC-JA                                                    
002343     AND DCS-NDC-NA                                                       
002344     AND (ORDW-SEQA-KDREFTYP = WS-IDREFTYP                                
002345     OR  (ORDW-SEQA-KDREFTYP = 'A'                                        
002346     AND  WS-IDREFTYP = 'C')                                              
002347     OR  (ORDW-SEQA-KDREFTYP = 'C'                                        
002348     AND  WS-IDREFTYP = 'A')))                                            
002349                                                                          
002350       PERFORM IMS-GN-WDE3-ORDW01-MIN-MAX                                 
002351       IF ORDW-SEQA-IDDC NOT = DCS-IDDC                                   
002352          MOVE ORDW-SEQA-IDDC     TO W-IDDC-B6                            
002353          PERFORM IMS-GU-WDB601                                           
002354       END-IF                                                             
002355       PERFORM S05-CHECK-IDDC                                             
002356     END-PERFORM                                                          
002357                                                                          
002358     IF SEGMENT-FINNS                                                     
002359*       --- CHECK IDLEVNR-SECURITY                                        
002360        MOVE ORDW-SEQA-IDARTNR TO W-IDARTNR                               
002361        PERFORM S1-SECURITY-CHECK-PARTNO                                  
002362     END-IF                                                               
002363                                                                          
002364     IF SEGMENT-FINNS                                                     
002365       PERFORM K-FYLL-I-NYCKEL-FAELT                                      
002366       MOVE ORDW-SEQA-IDARTNR                                             
002367                             TO W-IDARTNR                                 
002368                                W-IDARTNR-301                             
002369                                W-IDARTNR-MIN                             
002370                                                                          
002371       PERFORM UNTIL SEGMENT-SAKNAS                                       
002372       OR NOT (WS-SPARA-IDARTNR = ORDW-SEQA-IDARTNR                       
002373       AND DCS-NDC-NA)                                                    
002374                                                                          
002375**********      LÄS ALLA REFILLPOSTER FÖR AKTUELL                         
002376**********      ARTNR FÖR NDC MED RÄTT KDREFTYP                           
002377**********      SÖKT STATUS ÄR REDAN FRAMLÄST                             
002378         IF ORDW-SEQA-KDREFTYP = WS-IDREFTYP                              
002379         OR (ORDW-SEQA-KDREFTYP = 'A'                                     
002380         AND WS-IDREFTYP = 'C')                                           
002381         OR (ORDW-SEQA-KDREFTYP = 'C'                                     
002382         AND WS-IDREFTYP = 'A')                                           
002383           MOVE +1           TO IX-DC                                     
002384           PERFORM UNTIL IX-DC  > IX-DC-MAX                               
002385             IF ORDW-SEQA-IDDC  = WS-DC-NR (IX-DC)                        
002386                                                                          
002387                MOVE ORDW-SEQA-IDWDE301                                   
002388                             TO W-WDE301KY-X                              
002389                PERFORM IMS-GU-WDE3-ORDL01                                
002390                                                                          
002391                PERFORM M-UPD-MOD-FAELT                                   
002392             END-IF                                                       
002393             ADD +1          TO IX-DC                                     
002394           END-PERFORM                                                    
002395         END-IF                                                           
002396                                                                          
002397         PERFORM IMS-GN-WDE3-ORDW01-MIN-MAX                               
002398         IF ORDW-SEQA-IDDC NOT = DCS-IDDC                                 
002399            MOVE ORDW-SEQA-IDDC     TO W-IDDC-B6                          
002400            PERFORM IMS-GU-WDB601                                         
002401         END-IF                                                           
002402         PERFORM S05-CHECK-IDDC                                           
002403       END-PERFORM                                                        
002404     ELSE                                                                 
002405       MOVE WS-DC-NR (1)       TO W-IDDC                                  
002406       PERFORM IMS-GU-WDK7-WDK711                                         
002407       IF SEGMENT-FINNS                                                   
002408          MOVE SLAG-IDPERSON-BUY                                          
002409                               TO WS-IDPERSON-BUY-NUM                     
002410                                  WS-IDPERSON-BUY-RED                     
002411          MOVE WS-IDPERSON-BUY-RED                                        
002412                               TO MOD-IDPERSON-BUY-UT                     
002413       END-IF                                                             
002414     END-IF                                                               
002415     .                                                                    
002416     EJECT                                                                
002417 ED-SOEK-NY-ARTIKEL SECTION.                                              
002418                                                                          
002419     MOVE NEJ                TO SW-TRAEFF                                 
002420*    TEXT 'NO PROPOSAL' SKRIVS I BILDEN OM EJ TRÄFF                       
002421     MOVE 'NO PROPOSAL'      TO MOD-ORDERSTATUS                           
002422     MOVE LOW-VALUE          TO W-WDE301KY-MIN-X                          
002423     MOVE HIGH-VALUE         TO W-WDE301KY-MAX-X                          
002424     MOVE WS-DC-NR (1)       TO W-IDDC-301-MIN                            
002425     MOVE WS-DC-NR (IX-DC-MAX)                                            
002426                             TO W-IDDC-301-MAX                            
002427     PERFORM IMS-GU-WDE3-ORDL01-MIN-MAX                                   
002428     IF REF-IDDC NOT = DCS-IDDC                                           
002429        MOVE REF-IDDC        TO W-IDDC-B6                                 
002430        PERFORM IMS-GU-WDB601                                             
002431     END-IF                                                               
002432     PERFORM S05-CHECK-IDDC                                               
002433                                                                          
002434     MOVE 9                  TO WS-KTRL-PRIO                              
002435                                                                          
002436     PERFORM UNTIL SEGMENT-SAKNAS                                         
002437     OR WS-KTRL-PRIO = 1                                                  
002438       IF  REF-IDARTNR = W-IDARTNR                                        
002439       AND VALID-IDDC-JA                                                  
002440       AND DCS-NDC-NA                                                     
002441         IF (REF-KDREFTYP = 'A' OR 'C')                                   
002442         AND REF-KDREFORS = 'O'                                           
002443         AND WS-KTRL-PRIO > 1                                             
002444*------    HÖGST PRIORITET SPARAS                                         
002445*------                                                                   
002446           MOVE REF-KDREFTYP TO WS-IDREFTYP                               
002447           MOVE REF-IDPERSON-BUY                                          
002448                             TO WS-IDPERSON-BUY-NUM                       
002449           MOVE 1            TO WS-KTRL-PRIO                              
002450                                                                          
002451         ELSE                                                             
002452           IF REF-KDREFTYP = 'B'                                          
002453           AND REF-KDREFORS = 'O'                                         
002454           AND WS-KTRL-PRIO > 2                                           
002455*------    HÖGST PRIORITET SPARAS                                         
002456*------                                                                   
002457             MOVE REF-KDREFTYP                                            
002458                             TO WS-IDREFTYP                               
002459             MOVE REF-IDPERSON-BUY                                        
002460                             TO WS-IDPERSON-BUY-NUM                       
002461             MOVE 2          TO WS-KTRL-PRIO                              
002462                                                                          
002463           ELSE                                                           
002464             IF REF-KDREFTYP = 'T'                                        
002465             AND REF-KDREFORS = 'O'                                       
002466             AND WS-KTRL-PRIO > 3                                         
002467*------    HÖGST PRIORITET SPARAS                                         
002468*------                                                                   
002469               MOVE REF-KDREFTYP                                          
002470                             TO WS-IDREFTYP                               
002471               MOVE REF-IDPERSON-BUY                                      
002472                             TO WS-IDPERSON-BUY-NUM                       
002473               MOVE 3        TO WS-KTRL-PRIO                              
002474                                                                          
002475             ELSE                                                         
002476               IF REF-KDREFTYP = 'L'                                      
002477               AND REF-KDREFORS = 'O'                                     
002478               AND WS-KTRL-PRIO > 4                                       
002479*------      HÖGST PRIORITET SPARAS                                       
002480*------                                                                   
002481                 MOVE REF-KDREFTYP                                        
002482                               TO WS-IDREFTYP                             
002483                 MOVE REF-IDPERSON-BUY                                    
002484                               TO WS-IDPERSON-BUY-NUM                     
002485                 MOVE 4        TO WS-KTRL-PRIO                            
002486                                                                          
002487               ELSE                                                       
002488                 IF (REF-KDREFTYP = 'A' OR 'C')                           
002489                 AND REF-KDREFORS = 'P'                                   
002490                 AND WS-KTRL-PRIO > 5                                     
002491*------      HÖGST PRIORITET SPARAS                                       
002492*------                                                                   
002493                   MOVE REF-KDREFTYP                                      
002494                               TO WS-IDREFTYP                             
002495                   MOVE REF-IDPERSON-BUY                                  
002496                               TO WS-IDPERSON-BUY-NUM                     
002497                   MOVE 5      TO WS-KTRL-PRIO                            
002498                                                                          
002499                 ELSE                                                     
002500                   IF REF-KDREFTYP = 'B'                                  
002501                   AND REF-KDREFORS = 'P'                                 
002502                   AND WS-KTRL-PRIO > 6                                   
002503*------      HÖGST PRIORITET SPARAS                                       
002504*------                                                                   
002505                     MOVE REF-KDREFTYP                                    
002506                               TO WS-IDREFTYP                             
002507                     MOVE REF-IDPERSON-BUY                                
002508                               TO WS-IDPERSON-BUY-NUM                     
002509                     MOVE 6    TO WS-KTRL-PRIO                            
002510                                                                          
002511                   ELSE                                                   
002512                     IF REF-KDREFTYP = 'T'                                
002513                     AND REF-KDREFORS = 'P'                               
002514                     AND WS-KTRL-PRIO > 7                                 
002515*------      HÖGST PRIORITET SPARAS                                       
002516*------                                                                   
002517                       MOVE REF-KDREFTYP                                  
002518                               TO WS-IDREFTYP                             
002519                       MOVE REF-IDPERSON-BUY                              
002520                               TO WS-IDPERSON-BUY-NUM                     
002521                       MOVE 7  TO WS-KTRL-PRIO                            
002522                                                                          
002523                     ELSE                                                 
002524                       IF REF-KDREFTYP = 'L'                              
002525                       AND REF-KDREFORS = 'P'                             
002526                       AND WS-KTRL-PRIO > 8                               
002527*------        HÖGST PRIORITET SPARAS                                     
002528*------                                                                   
002529                         MOVE REF-KDREFTYP                                
002530                                 TO WS-IDREFTYP                           
002531                         MOVE REF-IDPERSON-BUY                            
002532                                 TO WS-IDPERSON-BUY-NUM                   
002533                         MOVE 8  TO WS-KTRL-PRIO                          
002534                                                                          
002535                       END-IF                                             
002536                     END-IF                                               
002537                   END-IF                                                 
002538                 END-IF                                                   
002539               END-IF                                                     
002540             END-IF                                                       
002541           END-IF                                                         
002542         END-IF                                                           
002543       END-IF                                                             
002544       PERFORM IMS-GN-WDE3-ORDL01-MIN-MAX                                 
002545       IF REF-IDDC NOT = DCS-IDDC                                         
002546          MOVE REF-IDDC        TO W-IDDC-B6                               
002547          PERFORM IMS-GU-WDB601                                           
002548       END-IF                                                             
002549       PERFORM S05-CHECK-IDDC                                             
002550     END-PERFORM                                                          
002551                                                                          
002552     IF WS-KTRL-PRIO NOT = 9                                              
002553*---    TRÄFF PÅ SÖKT ARTIKEL                                             
002554*---                                                                      
002555       MOVE WS-IDREFTYP      TO W-KDREFTYP-MIN                            
002556                                W-KDREFTYP-MAX                            
002557       MOVE WS-IDPERSON-BUY-NUM                                           
002558                             TO W-IDPERSON-BUY-MIN                        
002559                                W-IDPERSON-BUY-MAX                        
002560       MOVE W-IDARTNR        TO W-IDARTNR-MIN                             
002561                                W-IDARTNR-MAX                             
002562       MOVE WS-DC-NR (1)     TO W-IDDC-MIN                                
002563       MOVE WS-DC-NR (IX-DC-MAX)                                          
002564                              TO W-IDDC-MAX                               
002565       PERFORM IMS-GU-WDE3-ORDW01-MIN-MAX                                 
002566       IF ORDW-SEQA-IDDC   NOT = DCS-IDDC                                 
002567          MOVE ORDW-SEQA-IDDC TO W-IDDC-B6                                
002568          PERFORM IMS-GU-WDB601                                           
002569       END-IF                                                             
002570       PERFORM S05-CHECK-IDDC                                             
002571                                                                          
002572       PERFORM UNTIL SEGMENT-SAKNAS                                       
002573       OR (ORDW-SEQA-IDARTNR = W-IDARTNR                                  
002574       AND VALID-IDDC-JA                                                  
002575       AND DCS-NDC-NA                                                     
002576       AND ORDW-SEQA-KDREFTYP = WS-IDREFTYP)                              
002577*------    LÄS FRAM TILL SÖKT ARTIKEL/REFTYP                              
002578*------                                                                   
002579         PERFORM IMS-GN-WDE3-ORDW01-MIN-MAX                               
002580         IF ORDW-SEQA-IDDC   NOT = DCS-IDDC                               
002581            MOVE ORDW-SEQA-IDDC TO W-IDDC-B6                              
002582            PERFORM IMS-GU-WDB601                                         
002583         END-IF                                                           
002584         PERFORM S05-CHECK-IDDC                                           
002585       END-PERFORM                                                        
002586                                                                          
002587       IF SEGMENT-FINNS                                                   
002588*         --- CHECK IDLEVNR-SECURITY                                      
002589          MOVE REF-IDARTNR TO W-IDARTNR                                   
002590          PERFORM S1-SECURITY-CHECK-PARTNO                                
002591       END-IF                                                             
002592                                                                          
002593       IF SEGMENT-FINNS                                                   
002594         PERFORM K-FYLL-I-NYCKEL-FAELT                                    
002595         MOVE ORDW-SEQA-IDPERSON-BUY                                      
002596                             TO WS-IDPERSON-BUY-NUM                       
002597       ELSE                                                               
002598*------    DETTA FALL SKA INTE INTRÄFFA                                   
002599*------                                                                   
002600         MOVE 'GE'           TO STATUS-WS                                 
002601       END-IF                                                             
002602     ELSE                                                                 
002603       MOVE WS-DC-NR (1)     TO W-IDDC                                    
002604       PERFORM IMS-GU-WDK7-WDK711                                         
002605       IF SEGMENT-FINNS                                                   
002606                                                                          
002607          MOVE SLAG-IDPERSON-BUY                                          
002608                               TO WS-IDPERSON-BUY-NUM                     
002609                                  WS-IDPERSON-BUY-RED                     
002610          MOVE WS-IDPERSON-BUY-RED                                        
002611                               TO MOD-IDPERSON-BUY-UT                     
002612       END-IF                                                             
002613       MOVE 'GE'               TO STATUS-WS                               
002614     END-IF                                                               
002615                                                                          
002616     PERFORM UNTIL SEGMENT-SAKNAS                                         
002617     OR NOT (WS-SPARA-IDARTNR = ORDW-SEQA-IDARTNR                         
002618     AND DCS-NDC-NA)                                                      
002619                                                                          
002620**********      LÄS ALLA REFILLPOSTER FÖR AKTUELL                         
002621**********      ARTNR FÖR NDC MED RÄTT KDREFTYP                           
002622**********      SÖKT STATUS ÄR REDAN FRAMLÄST                             
002623       IF  ORDW-SEQA-KDREFTYP = WS-IDREFTYP                               
002624         MOVE +1             TO IX-DC                                     
002625         PERFORM UNTIL IX-DC  > IX-DC-MAX                                 
002626           IF ORDW-SEQA-IDDC  = WS-DC-NR (IX-DC)                          
002627                                                                          
002628              MOVE ORDW-SEQA-IDWDE301                                     
002629                             TO W-WDE301KY-X                              
002630              PERFORM IMS-GU-WDE3-ORDL01                                  
002631                                                                          
002632              PERFORM M-UPD-MOD-FAELT                                     
002633           END-IF                                                         
002634           ADD +1            TO IX-DC                                     
002635         END-PERFORM                                                      
002636       END-IF                                                             
002637                                                                          
002638       PERFORM IMS-GN-WDE3-ORDW01-MIN-MAX                                 
002639       IF ORDW-SEQA-IDDC  NOT = DCS-IDDC                                  
002640          MOVE ORDW-SEQA-IDDC  TO W-IDDC-B6                               
002641          PERFORM IMS-GU-WDB601                                           
002642       END-IF                                                             
002643       PERFORM S05-CHECK-IDDC                                             
002644     END-PERFORM                                                          
002645     .                                                                    
002646     EJECT                                                                
002647 EE-BERAKNA-REFPKT SECTION.                                               
002648                                                                          
002649     MOVE +1                 TO IX-DC                                     
002650     PERFORM UNTIL IX-DC > IX-DC-MAX                                      
002651                                                                          
002652       IF WS-KVPB-TOT (IX-DC) > ZERO                                      
002653                                                                          
002654         MOVE WS-DC-NR (IX-DC)                                            
002655                             TO W-IDDC                                    
002656         PERFORM IMS-GU-WDK7-WDK711                                       
002657         IF SEGMENT-FINNS                                                 
002658                                                                          
002659           MOVE SLAG-TIREFPKT   TO TMP1-YYMMDD                            
002660           MOVE DAGENS-DATUM    TO TMP2-YYMMDD                            
002661           PERFORM WY2000P1                                               
002662           IF TMP1-YYMMDD   >= TMP2-YYMMDD                                
002663                                                                          
002664*                                                                         
002665*---   KVREFPKT FRÅN BASEN GÄLLER PGA MANUELLT DATUM ÄR SATT              
002666             MOVE SLAG-KVREFPKT                                           
002667                                TO MOD-KVREFPKT (IX-DC)                   
002668             PERFORM IMS-GU-WDK727                                        
002669             IF SEGMENT-FINNS                                             
002670               IF FUTUR-PROG-KVPB-JUST(1) > ZERO                          
002671                 MOVE MFS-ADD-LYS-UPP-FAELT                               
002672                                TO MOD-KVREFPKT-ATTR (IX-DC)              
002673               END-IF                                                     
002674             END-IF                                                       
002675           ELSE                                                           
002676             PERFORM S03-GET-BESPRIS                                      
002677                                                                          
002678***          THIS SIMULATES WHEN ENTER IS PRESSED                         
002679***          ALWAYS PASS YES TO FLSIM AND NO FOR UPDATES                  
002680             MOVE JA            TO WS-FLSIM                               
002681                                                                          
002682             PERFORM S90-CALL-W271REFL                                    
002683                                                                          
002684             MOVE REFL-KVREFPKT                                           
002685                                TO MOD-KVREFPKT (IX-DC)                   
002686                                                                          
002687             PERFORM IMS-GU-WDK727                                        
002688             IF SEGMENT-FINNS                                             
002689               IF FUTUR-PROG-KVPB-JUST(1) > ZERO                          
002690                 MOVE MFS-ADD-LYS-UPP-FAELT                               
002691                                TO MOD-KVREFPKT-ATTR (IX-DC)              
002692               END-IF                                                     
002693             END-IF                                                       
002694           END-IF                                                         
002695         ELSE                                                             
002696           MOVE ZERO            TO MOD-KVREFPKT (IX-DC)                   
002697         END-IF                                                           
002698       ELSE                                                               
002699         MOVE ZERO              TO MOD-KVREFPKT (IX-DC)                   
002700       END-IF                                                             
002701                                                                          
002702       ADD +1                   TO IX-DC                                  
002703     END-PERFORM                                                          
002704     .                                                                    
002705     EJECT                                                                
002706 F-HAEMTA-INFO SECTION.                                                   
002707                                                                          
002708     PERFORM IMS-GU-WDK6-ARTC01                                           
002709     IF SEGMENT-FINNS                                                     
002710       MOVE ART-IDFKNGRP     TO MOD-IDFKNGRP                              
002711       MOVE ART-KDPRODSL     TO MOD-KDPRODSL                              
002712       MOVE ART-TIFINLV      TO MOD-TIFINLV                               
002713       MOVE ART-TIURPROD     TO MOD-TIURPROD                              
002714                                                                          
002715       PERFORM IMS-GNP-WDK6-ARTC11                                        
002716       IF SEGMENT-FINNS                                                   
002717         PERFORM FC-DISPLAY-IDDC-GROUP                                    
002718         MOVE CLAG-KDERS     TO MOD-KDERS                                 
002719                                WS-KDERS                                  
002720         MOVE CLAG-REDIRLEV  TO MOD-REDIRLEV                              
002721         MOVE CLAG-KVQPACK-0 TO MOD-KVQPACK-0                             
002722         MOVE CLAG-KVQPACK-1 TO MOD-KVQPACK-1                             
002723         PERFORM S03-GET-BESPRIS                                          
002724         MOVE WS-PRMATRL     TO MOD-PRMATRL                               
002725*                                                                         
002726         MOVE CLAG-VKART     TO WS-VKART                                  
002727         MOVE CLAG-VLARTNTO  TO WS-VLARTNTO                               
002728*                                                                         
002729         PERFORM S07-GET-WDK712-LOCAL-INFO                                
002730         MOVE WS-KVQPACK-3   TO MOD-KVQPACK-3                             
002731                                                                          
002732         PERFORM IMS-GU-ARTM01                                            
002733         IF SEGMENT-FINNS                                                 
002734           COMPUTE WS-KVOKS-TOT = ARTM-ART-KVOKS-BULK +                   
002735                                   ARTM-ART-KVOKS-DAG +                   
002736                                   ARTM-ART-KVOKS-VOR                     
002737         ELSE                                                             
002738           MOVE ZERO         TO WS-KVOKS-TOT                              
002739         END-IF                                                           
002740                                                                          
002741         PERFORM IMS-GU-WDK7-WDK711                                       
002742         MOVE SLAG-IDDC-REF  TO WS-IDDC                                   
002743         IF CDC OR SEGMENT-SAKNAS                                         
002744           COMPUTE WS-AVAILABLE ROUNDED =                                 
002745                   CLAG-KVLS - CLAG-KVRESS - WS-KVOKS-TOT                 
002746           MOVE    CLAG-KVROS                                             
002747                              TO WS-KVROS                                 
002748           PERFORM FD-HAEMTA-KVPB                                         
002749           COMPUTE WS-KVAKS ROUNDED =                                     
002750                   CLAG-KVAKS-CDC +                                       
002751                   CLAG-KVAKS-T                                           
002752         ELSE                                                             
002753           IF SLAG-IDDC-REF = SPACE                                       
002754**to show info on localy sourced part on the right side                   
002755             MOVE SLAG-IDDC                                               
002756                               TO W-IDDC                                  
002757           ELSE                                                           
002758             MOVE SLAG-IDDC-REF                                           
002759                               TO W-IDDC                                  
002760           END-IF                                                         
002761           PERFORM IMS-GU-WDK7-WDK711                                     
002762           IF SEGMENT-FINNS                                               
002763             COMPUTE WS-AVAILABLE ROUNDED =                               
002764                     SLAG-KVLS - SLAG-KVRESS - SLAG-KVOKS-BULK -          
002765                     SLAG-KVOKS-DAG                                       
002766             COMPUTE WS-KVROS ROUNDED =                                   
002767                     SLAG-KVROS-BULK + SLAG-KVROS-DAG                     
002768             COMPUTE WS-KVPB = SLAG-KVPB-REF + SLAG-KVPBREOI              
002769             MOVE  SLAG-KVAKS-SDC                                         
002770                               TO WS-KVAKS                                
002771           ELSE                                                           
002772             MOVE ZERO TO    WS-AVAILABLE                                 
002773                             WS-KVROS                                     
002774                             WS-KVPB                                      
002775                             WS-KVAKS                                     
002776           END-IF                                                         
002777         END-IF                                                           
002778         MOVE WS-AVAILABLE   TO MOD-AVAIL                                 
002779         MOVE WS-KVAKS       TO MOD-KVAKS-CDC                             
002780         MOVE WS-KVROS       TO MOD-KVROS-CDC                             
002781         MOVE WS-KVPB        TO MOD-KVPB-CDC                              
002782         IF CLAG-KVUTRS > ZERO                                            
002783           MOVE CLAG-KVUTRS  TO WS-TEMF-UTRSALDO-NUM                      
002784           MOVE WS-TEMF-UTRSALDO-NUM                                      
002785                             TO WS-TEMF-UTRSALDO                          
002786           MOVE WS-TEMF-RED-INVBAL                                        
002787                             TO WS-TEMFSINF-TEXT                          
002788         END-IF                                                           
002789         MOVE MFS-RENSA-FAELT                                             
002790                             TO MOD-REPLACES                              
002791         IF ART-FLERS = JA                                                
002792            MOVE ART-IDARTNR TO W-IDARTNR-MIN7                            
002793                                W-IDARTNR-MAX7                            
002794            PERFORM IMS-GU-WDD7-ERSB01-MINMAX                             
002795            IF SEGMENT-FINNS                                              
002796               IF ERSB01-ERS-IDARTNR NOT = ZERO                           
002797                  MOVE ERSB01-ERS-IDARTNR                                 
002798                             TO MOD-REPLACES                              
002799                  INSPECT MOD-REPLACES REPLACING                          
002800                                        LEADING ZERO BY SPACE             
002801                  PERFORM IMS-GN-WDD7-ERSB01-MINMAX                       
002802                  IF SEGMENT-FINNS                                        
002803                  AND ERSB01-ERS-IDARTNR NOT = ZERO                       
002804                    MOVE 'VARIOUS'                                        
002805                             TO MOD-REPLACES                              
002806                  END-IF                                                  
002807               END-IF                                                     
002808            END-IF                                                        
002809         END-IF                                                           
002810                                                                          
002811         IF SW-HAEMTA-INPUT-FAELT-JA                                      
002812           MOVE +5           TO W-KDNOTTYP                                
002813           PERFORM IMS-GNP-WDK6-ARTC25                                    
002814           IF SEGMENT-FINNS                                               
002815             MOVE NOT-TEARTNOT TO MOD-COMMENT                             
002816           ELSE                                                           
002817             MOVE SPACE      TO MOD-COMMENT                               
002818           END-IF                                                         
002819         END-IF                                                           
002820         MOVE MFS-ADD-LAES-IN-FAELT                                       
002821                             TO MOD-COMMENT-ATTR                          
002822                                                                          
002823         PERFORM S01-LAES-WDA5-ENTER                                      
002824         MOVE +1             TO IX-DC                                     
002825         PERFORM UNTIL IX-DC  > IX-DC-MAX                                 
002826           MOVE WS-RESTKVANT (IX-DC)                                      
002827                             TO MOD-KVROS-NDC-CDC (IX-DC)                 
002828           ADD +1            TO IX-DC                                     
002829         END-PERFORM                                                      
002830                                                                          
002831         IF WS-KDERS                    >  0                              
002832           IF WS-KDERS                  <  29                             
002833             IF MOD-TEMFSFEL = SPACE                                      
002834               MOVE ARTIKEL-ERSATT                                        
002835                             TO MED-IDMFSFEL                              
002836               CALL WMEDKONV USING MED-WMEDAREA                           
002837               MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                          
002838             END-IF                                                       
002839           ELSE                                                           
002840             MOVE ARTIKEL-UTGANGEN                                        
002841                             TO MED-IDMFSFEL                              
002842             CALL WMEDKONV USING MED-WMEDAREA                             
002843             MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                            
002844           END-IF                                                         
002845         END-IF                                                           
002846                                                                          
002847       ELSE                                                               
002848         MOVE ARTIKEL-SAKNAS TO MED-IDMFSFEL                              
002849         CALL WMEDKONV USING MED-WMEDAREA                                 
002850         MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                
002851         PERFORM MFS-RENSA-FAELT-IN                                       
002852         PERFORM MFS-RENSA-FAELT-UT                                       
002853       END-IF                                                             
002854     ELSE                                                                 
002855       MOVE ARTIKEL-SAKNAS   TO MED-IDMFSFEL                              
002856       CALL WMEDKONV USING MED-WMEDAREA                                   
002857       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
002858       PERFORM MFS-RENSA-FAELT-IN                                         
002859       PERFORM MFS-RENSA-FAELT-UT                                         
002860     END-IF                                                               
002861                                                                          
002862     MOVE MFS-RENSA-FAELT    TO MOD-REPL-BY                               
002863     PERFORM IMS-GU-WDD7-ERSA01                                           
002864     IF SEGMENT-FINNS                                                     
002865        PERFORM IMS-GNP-WDD7-ERSA11                                       
002866        IF SEGMENT-FINNS                                                  
002867           MOVE ERSA11-IDARTNR-TILLK                                      
002868                             TO MOD-REPL-BY                               
002869           INSPECT MOD-REPL-BY REPLACING LEADING ZERO BY SPACE            
002870           PERFORM IMS-GNP-WDD7-ERSA11                                    
002871           IF SEGMENT-FINNS                                               
002872             MOVE 'VARIOUS'  TO MOD-REPL-BY                               
002873           END-IF                                                         
002874        END-IF                                                            
002875     END-IF                                                               
002876                                                                          
002877                                                                          
002878     MOVE SPACE              TO MOD-BEART                                 
002879     PERFORM IMS-GU-WDD3-BENA01-BSEQ                                      
002880     IF SEGMENT-FINNS                                                     
002881       MOVE 'USA'            TO W-IDSKYLT                                 
002882       PERFORM IMS-GNP-WDD3-BENA11                                        
002883       IF SEGMENT-FINNS                                                   
002884         MOVE BENA11-TEXT-BEART                                           
002885                             TO MOD-BEART                                 
002886       END-IF                                                             
002887     END-IF                                                               
002888                                                                          
002889*                                                                         
002890     MOVE IX-DC-MAX          TO IX-DC                                     
002891     PERFORM UNTIL IX-DC  NOT > ZERO                                      
002892       MOVE WS-DC-NR (IX-DC) TO W-IDDC                                    
002893       PERFORM IMS-GU-WDK7-WDK711                                         
002894       IF SEGMENT-FINNS                                                   
002895          PERFORM FB-BEHANDLA-DC                                          
002896       END-IF                                                             
002897       COMPUTE  IX-DC         = IX-DC - 1                                 
002898     END-PERFORM                                                          
002899                                                                          
002900**To get Air Freight cost factor                                          
002901*WDB601 KEY - RECEIVING DC                                                
002902*WDB616 KEY - REFILL DC                                                   
002903     MOVE MOD-IDDC-UT   TO W-IDDC-B6-X                                    
002904     MOVE +1 TO IX                                                        
002905*TO GET CORRESPONDING REFILL DC                                           
002906     PERFORM UNTIL IX > IX-DC-MAX                                         
002907     OR  MOD-IDDC-GROUP (IX) = MOD-IDDC-UT                                
002908         ADD +1            TO IX                                          
002909     END-PERFORM                                                          
002910*                                                                         
002911     IF IX <= IX-DC-MAX                                                   
002912        IF MOD-IDDC-GROUP (IX) = MOD-IDDC-UT                              
002913           MOVE MOD-IDDC-REF (IX) TO W-IDDC-B616-X                        
002914           PERFORM S10-GET-AIR-FREIGHT                                    
002915        END-IF                                                            
002916     END-IF                                                               
002917**To calculate Air Freight cost                                           
002918     COMPUTE WS-KR-VIKT ROUNDED                                           
002919                          = WS-VKART * WS-REAIRCO / 1000                  
002920     COMPUTE WS-KR-VOLYM ROUNDED =                                        
002921             WS-VLARTNTO * WS-REAIRCO * 167 / 1000000                     
002922                                                                          
002923     COMPUTE WS-KR-VIKT-RED ROUNDED = WS-KR-VIKT * 1                      
002924     COMPUTE WS-KR-VOLYM-RED ROUNDED = WS-KR-VOLYM * 1                    
002925                                                                          
002926*     ---- VISA DET DYRASTE ALTERNATIVET                                  
002927     IF WS-KR-VIKT-RED > WS-KR-VOLYM-RED                                  
002928        MOVE WS-KR-VIKT-RED                                               
002929                       TO MOD-AIR-COST                                    
002930     ELSE                                                                 
002931        MOVE WS-KR-VOLYM-RED                                              
002932                       TO MOD-AIR-COST                                    
002933     END-IF                                                               
002934**                                                                        
002935     MOVE +1                 TO IX-DC                                     
002936     PERFORM UNTIL IX-DC      > IX-DC-MAX                                 
002937       MOVE WS-DC-NR (IX-DC) TO W-IDDC                                    
002938       PERFORM IMS-GU-WDL711                                              
002939       IF SEGMENT-FINNS                                                   
002940          PERFORM IMS-GU-WDL411                                           
002941          IF SEGMENT-SAKNAS                                               
002942******** IF PART/DC FROM WDL7 IS MISSING IN WDL4 THEN L411-AREA           
002943******** MUST BE ZEROED BCZ SOME ITEMS ARE USED IN COMPUTES BELOW         
002944            INITIALIZE OIHD-WDL411                                        
002945          END-IF                                                          
002946          PERFORM FA-BEHANDLA-ORDERINGGANG                                
002947       END-IF                                                             
002948       ADD +1                TO IX-DC                                     
002949     END-PERFORM                                                          
002950                                                                          
002951     MOVE +1                 TO IX                                        
002952     PERFORM IMS-GU-WDN6-KATN01                                           
002953     IF SEGMENT-FINNS                                                     
002954       PERFORM IMS-GNP-WDN6-KATN11                                        
002955     END-IF                                                               
002956     PERFORM UNTIL SEGMENT-SAKNAS                                         
002957     OR              IX > 3                                               
002958       MOVE KATN-KAT-BEMASTER (1:3)                                       
002959                             TO MOD-MODEL (IX)                            
002960       PERFORM IMS-GNP-WDN6-KATN11                                        
002961       ADD +1                TO IX                                        
002962     END-PERFORM                                                          
002963                                                                          
002964     MOVE W-IDARTNR          TO W-IDARTNR-HSEQ                            
002965     MOVE ZERO               TO WS-KVAVIS                                 
002966     MOVE MSGI-IDDC-KEY      TO W-IDDC                                    
002967     PERFORM IMS-GU-WDK7-WDK711                                           
002968     MOVE SLAG-IDDC-REF      TO WS-IDDC                                   
002969*                                                                         
002970     IF CDC                                                               
002971        IF CLAG-IDDC-REF = SPACE                                          
002972          PERFORM IMS-GU-INLA11-W6D1SEQ                                   
002973          PERFORM UNTIL SEGMENT-SAKNAS                                    
002974            MOVE INLA-ART-IDDC  TO W-IDDC-B6                              
002975            PERFORM IMS-GU-WDB601                                         
002976            IF (DCS-CDC                                                   
002977            OR DCS-CDC-TR)                                                
002978            AND INLA-ART-IDLOPNRM = ZERO                                  
002979                MOVE INLA-ART-KVAVIS                                      
002980                                  TO WS-KVAVIS                            
002981                IF INLA-ART-FLFEL = NEJ                                   
002982                   ADD WS-KVAVIS TO WS-KVAVIS                             
002983                END-IF                                                    
002984            END-IF                                                        
002985            PERFORM IMS-GN-INLA11-W6D1SEQ                                 
002986          END-PERFORM                                                     
002987        ELSE                                                              
002988          COMPUTE WS-KVAVIS        = CLAG-KVAKS-PAV +                     
002989                                     CLAG-KVBEART                         
002990        END-IF                                                            
002991     ELSE                                                                 
002992       IF SLAG-IDDC-REF = SPACES                                          
002993         PERFORM IMS-GU-INLA11-W6D1SEQ                                    
002994         PERFORM UNTIL SEGMENT-SAKNAS                                     
002995           IF INLA-ART-IDDC        = W-IDDC                               
002996           AND INLA-ART-IDLOPNRM = ZERO                                   
002997           AND INLA-ART-FLFEL      = NEJ                                  
002998               ADD INLA-ART-KVAVIS                                        
002999                                 TO WS-KVAVIS                             
003000           END-IF                                                         
003001           PERFORM IMS-GN-INLA11-W6D1SEQ                                  
003002         END-PERFORM                                                      
003003       ELSE                                                               
003004         MOVE SLAG-IDDC-REF       TO W-IDDC                               
003005         PERFORM IMS-GU-WDK7-WDK711                                       
003006         IF SEGMENT-FINNS                                                 
003007           IF SLAG-IDDC-REF = SPACE                                       
003008             PERFORM IMS-GU-INLA11-W6D1SEQ                                
003009             PERFORM UNTIL SEGMENT-SAKNAS                                 
003010               IF INLA-ART-IDDC    = W-IDDC                               
003011               AND INLA-ART-IDLOPNRM = ZERO                               
003012               AND INLA-ART-FLFEL = NEJ                                   
003013                   ADD INLA-ART-KVAVIS                                    
003014                                     TO WS-KVAVIS                         
003015               END-IF                                                     
003016               PERFORM IMS-GN-INLA11-W6D1SEQ                              
003017             END-PERFORM                                                  
003018           ELSE                                                           
003019             COMPUTE WS-KVAVIS ROUNDED                                    
003020                                = SLAG-KVBEART + SLAG-KVAKS-PAV           
003021           END-IF                                                         
003022         END-IF                                                           
003023       END-IF                                                             
003024     END-IF                                                               
003025     MOVE WS-KVAVIS          TO MOD-KVAVIS                                
003026*                                                                         
003027     MOVE +1                 TO IX-DC                                     
003028     PERFORM UNTIL IX-DC      > IX-DC-MAX                                 
003029       IF  WS-DC-NR (IX-DC)   > SPACES                                    
003030          MOVE WS-DC-NR (IX-DC)                                           
003031                             TO W-IDDC                                    
003032                                WS-IDDC                                   
003033          PERFORM IMS-GU-WDK7-WDK711                                      
003034          IF SEGMENT-FINNS                                                
003035             IF (SLAG-IDDC-REF = SPACE) AND NDC-US                        
003036                MOVE '  PURCH'                                            
003037                             TO WS-TEMFSINF-NDC   (IX-DC)                 
003038                MOVE MFS-STAENG-FAELT                                     
003039                             TO MOD-KVPB-REF-ATTR (IX-DC)                 
003040             END-IF                                                       
003041          END-IF                                                          
003042       END-IF                                                             
003043       ADD +1                TO IX-DC                                     
003044     END-PERFORM                                                          
003045*                                                                         
003046*    CHECK SOURCING MARKET FOR THE PART                                   
003047     PERFORM FE-CHECK-ART-SOURCE                                          
003048*                                                                         
003049     IF INDATA-OK                                                         
003050     AND NOT MFS-UPDATE                                                   
003051     AND NOT MFS-UPD-V                                                    
003052       MOVE WS-TEMFSINF      TO MOD-TEMFSINF                              
003053     END-IF                                                               
003054*                                                                         
003055*     MFS-ALFA-FAELT-FEL FLYTTAS TILL ATTRIBUTET MOD-PURCHQTY-ATTR        
003056*     ENBART FÖR ATT FÅ UPPLYST FÄLT PLUS CURSORPLACERING                 
003057*     INTE FÖR ATT DET ÄR NÅGOT FEL                                       
003058*                                                                         
003059     MOVE IX-DC-MAX            TO IX-DC                                   
003060     PERFORM UNTIL IX-DC NOT    > ZERO                                    
003061       IF WS-PURCHQTY (IX-DC)   > ZERO                                    
003062         IF WS-TEMFSINF-NDC (IX-DC) = 'TRANSF?'                           
003063            MOVE MFS-ALFA-FAELT-FEL                                       
003064                               TO MOD-IDDC-FROM-ATTR (IX-DC)              
003065                                  MOD-PURCHQTY-ATTR  (IX-DC)              
003066         ELSE                                                             
003067            MOVE MFS-ALFA-FAELT-FEL                                       
003068                               TO MOD-PURCHQTY-ATTR  (IX-DC)              
003069         END-IF                                                           
003070       END-IF                                                             
003071       COMPUTE IX-DC            = IX-DC - 1                               
003072     END-PERFORM                                                          
003073                                                                          
003074*     EFTER UPPDATERING SKA CURSOR STÅ PÅ FÄLTET IDARTNR-IN               
003075*                                                                         
003076     IF MFS-UPDATE OR MFS-UPD-V                                           
003077       MOVE MFS-ADD-SAETT-CURSOR                                          
003078                             TO MOD-IDARTNR-IN-ATTR                       
003079     END-IF                                                               
003080*                                                                         
003081     MOVE +1                 TO IX-DC                                     
003082     PERFORM UNTIL IX-DC      > 4                                         
003083       IF WS-DC-NR (IX-DC)    > SPACES                                    
003084          CONTINUE                                                        
003085       ELSE                                                               
003086          PERFORM MFS-CLOSE-BLANK-DC-FIELD-IN                             
003087       END-IF                                                             
003088       ADD +1                TO IX-DC                                     
003089     END-PERFORM                                                          
003090     .                                                                    
003091     EJECT                                                                
003092 FA-BEHANDLA-ORDERINGGANG     SECTION.                                    
003093                                                                          
003094     PERFORM FAA-HAMTA-VV-I-PER                                           
003095                                                                          
003096*    -- OBS 4-STÄLLIGT ÅR                                                 
003097     MOVE DAGENS-AAR         TO MOD-IAAR                                  
003098     SUBTRACT 1 FROM DAGENS-AAR GIVING WS-FOREG-AAR                       
003099     MOVE WS-FOREG-AAR       TO MOD-FOREG-AAR                             
003100                                                                          
003101     MOVE ZERO               TO WS-KVOI-SUM                               
003102     MOVE +1                 TO IX                                        
003103     PERFORM UNTIL IX > +12                                               
003104       ADD OIHD-KVOI (1, IX) TO WS-KVOI-SUM                               
003105       ADD OIHD-KVOI-REFILL (1, IX)                                       
003106                             TO WS-KVOI-SUM                               
003107       ADD +1                TO IX                                        
003108     END-PERFORM                                                          
003109     MOVE WS-KVOI-SUM        TO MOD-KVOI-FOREG-AAR (IX-DC)                
003110                                                                          
003111     MOVE ZERO               TO WS-KVOI-SUM                               
003112     MOVE +1                 TO IX                                        
003113                                                                          
003114*  --- HÄMTA ALLA VECKOR TOM FÖRRA PERIODEN                               
003115                                                                          
003116     IF DAGENS-PP > 1                                                     
003117        PERFORM UNTIL IX > WS-SISTA-V (12)                                
003118          ADD DC-KVOI-RULL (IX) TO WS-KVOI-SUM                            
003119          ADD DC-KVOI-REF-RULL (IX)                                       
003120                             TO WS-KVOI-SUM                               
003121          ADD +1             TO IX                                        
003122        END-PERFORM                                                       
003123     END-IF                                                               
003124     MOVE WS-KVOI-SUM        TO MOD-KVOI-IAAR (IX-DC)                     
003125                                                                          
003126                                                                          
003127*  --- FYLL PÅ TABELLEN MED OI                                            
003128                                                                          
003129     MOVE +1                 TO IX                                        
003130     PERFORM UNTIL IX        >  12                                        
003131       MOVE WS-FORSTA-V(IX)  TO IX-VV                                     
003132       PERFORM UNTIL IX-VV   >  WS-SISTA-V(IX)                            
003133         ADD DC-KVOI-RULL(IX-VV)                                          
003134                             TO WS-KVOI(IX)                               
003135         ADD DC-KVOI-REF-RULL(IX-VV)                                      
003136                             TO WS-KVOI(IX)                               
003137         ADD +1              TO IX-VV                                     
003138       END-PERFORM                                                        
003139       ADD +1                TO IX                                        
003140     END-PERFORM                                                          
003141                                                                          
003142*    --- FLYTTA UT TABELLEN TILL MOD:EN,                                  
003143*    --- ENDAST DE SENASTE FEM PERIODERNA VISAS                           
003144                                                                          
003145     MOVE +10                TO IX                                        
003146     MOVE +1                 TO MOD-IX                                    
003147     PERFORM UNTIL IX > +12                                               
003148       MOVE WS-PER(IX)       TO MOD-TIPP(MOD-IX)                          
003149       INSPECT MOD-TIPP(MOD-IX) REPLACING LEADING ZERO BY SPACE           
003150       MOVE WS-FORSTA-V(IX)                                               
003151                             TO WS-FOM                                    
003152       MOVE WS-SISTA-V(IX)   TO WS-TOM                                    
003153       MOVE WS-FOM-TOM       TO MOD-TIVV-FOM-TOM(MOD-IX)                  
003154       MOVE WS-KVOI(IX)      TO MOD-KVOI-RULL(MOD-IX, IX-DC)              
003155       MOVE ZERO             TO WS-KVOI(IX)                               
003156       ADD +1                TO IX                                        
003157                                MOD-IX                                    
003158     END-PERFORM                                                          
003159                                                                          
003160*    --- LÄGG UT KVOI FÖR AKTUELL PERIOD                                  
003161*                                                                         
003162                                                                          
003163     MOVE +1                 TO IX                                        
003164     MOVE ZERO               TO WS-KVOI-SUM                               
003165     PERFORM UNTIL IX > +5                                                
003166       ADD DC-KVOI-INNEV(IX)    TO WS-KVOI-SUM                            
003167       ADD DC-KVOI-PP-INNEV(IX) TO WS-KVOI-SUM                            
003168       ADD DC-KVOI-REF-INNEV(IX)                                          
003169                                TO WS-KVOI-SUM                            
003170       ADD +1                   TO IX                                     
003171     END-PERFORM                                                          
003172     MOVE WS-ANTAL-VECKOR       TO MOD-VECKA                              
003173     MOVE WS-KVOI-SUM           TO MOD-KVOI-INNEV (IX-DC)                 
003174     .                                                                    
003175     EJECT                                                                
003176                                                                          
003177 FAA-HAMTA-VV-I-PER SECTION.                                              
003178                                                                          
003179     MOVE +1                 TO IX                                        
003180     MOVE DAGENS-PER         TO WS-TIAAPER                                
003181     IF TIAA = 00                                                         
003182       MOVE 99 TO TIAA                                                    
003183     ELSE                                                                 
003184       SUBTRACT 1 FROM TIAA                                               
003185     END-IF                                                               
003186                                                                          
003187*    --- TA FRAM HUR MÅNGA VECKOR DET VAR FÖREGÅENDE ÅR                   
003188     MOVE TIAA               TO WS-AAR                                    
003189     MOVE 53                 TO WS-VV                                     
003190     MOVE 'AAVV  '           TO DAT-KDDATFORM                             
003191     MOVE TIAAVV             TO DAT-I-TIDATUM                             
003192     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
003193                         DAT-O-TIDATUM DAT-KDSVAR                         
003194     IF DAT-KDSVAR-OK                                                     
003195       MOVE 53               TO WS-ANT-VV                                 
003196     ELSE                                                                 
003197       MOVE 52               TO WS-ANT-VV                                 
003198     END-IF                                                               
003199                                                                          
003200*    --- FYLL I VECKONR FÖR PERIODERNA                                    
003201                                                                          
003202     MOVE 'AARP  '           TO DAT-KDDATFORM                             
003203     MOVE TIAAPER            TO DAT-I-TIDATUM                             
003204     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
003205                         DAT-O-TIDATUM DAT-KDSVAR                         
003206     IF DAT-KDSVAR-OK                                                     
003207       IF PER                = 1                                          
003208         MOVE 1              TO WS-PER(IX)                                
003209                                WS-FORSTA-V(IX)                           
003210       ELSE                                                               
003211         MOVE PER            TO WS-PER(IX)                                
003212         MOVE DAT-TIVV       TO WS-FORSTA-V(IX)                           
003213       END-IF                                                             
003214     ELSE                                                                 
003215       MOVE 'FELAKTIGT DATUM - DATKONV2' TO FELTEXT                       
003216       CALL FELLOG                                                        
003217     END-IF                                                               
003218                                                                          
003219     PERFORM UNTIL IX        >  12                                        
003220       ADD +1                TO PER                                       
003221       IF PER                >  12                                        
003222         IF TIAA = 99                                                     
003223           MOVE ZERO         TO TIAA                                      
003224         ELSE                                                             
003225           ADD +1            TO TIAA                                      
003226         END-IF                                                           
003227         MOVE 01             TO PER                                       
003228       END-IF                                                             
003229                                                                          
003230       MOVE TIAAPER          TO DAT-I-TIDATUM                             
003231       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
003232                           DAT-O-TIDATUM DAT-KDSVAR                       
003233       IF DAT-KDSVAR-OK                                                   
003234         IF PER              =  1                                         
003235           MOVE WS-ANT-VV    TO WS-SISTA-V(IX)                            
003236         ELSE                                                             
003237           COMPUTE WS-SISTA-V(IX) = DAT-TIVV - 1                          
003238         END-IF                                                           
003239         ADD +1              TO IX                                        
003240         IF IX               <= 12                                        
003241           MOVE PER          TO WS-PER(IX)                                
003242           IF PER            =  1                                         
003243             MOVE +1         TO WS-FORSTA-V(IX)                           
003244           ELSE                                                           
003245             MOVE DAT-TIVV   TO WS-FORSTA-V(IX)                           
003246           END-IF                                                         
003247         END-IF                                                           
003248       ELSE                                                               
003249         MOVE 'FELAKTIGT DATUM - DATKONV3' TO FELTEXT                     
003250         CALL FELLOG                                                      
003251       END-IF                                                             
003252     END-PERFORM                                                          
003253*    --- OM VECKOR I FÖRSTA OCH SISTA PERIODEN ÖVERLAPPAR,                
003254*    --- RÄTTA I FÖRSTA (DVS DEN ÄLDSTA) PERIODEN.                        
003255     IF WS-FORSTA-V(1)        = WS-SISTA-V(12)                            
003256     OR WS-FORSTA-V(1)        = WS-SISTA-V(12) - 1                        
003257       COMPUTE WS-FORSTA-V(1) = WS-SISTA-V(12) + 1                        
003258     END-IF                                                               
003259     .                                                                    
003260     EJECT                                                                
003261 FB-BEHANDLA-DC    SECTION.                                               
003262                                                                          
003263     IF SW-HAEMTA-INPUT-FAELT-JA                                          
003264       IF SLAG-FLFLYG      = JA                                           
003265       AND MSGI-IDLAND-SPR = 'GB'                                         
003266         MOVE YES            TO MOD-FLFLYG      (IX-DC)                   
003267       ELSE                                                               
003268         MOVE SLAG-FLFLYG    TO MOD-FLFLYG      (IX-DC)                   
003269       END-IF                                                             
003270       MOVE MFS-ADD-LAES-IN-FAELT                                         
003271                             TO MOD-FLFLYG-ATTR (IX-DC)                   
003272*                                                                         
003273       MOVE SLAG-KVPB-REF    TO WS-RED-KVPB-REF                           
003274                                WS-KVPB-REF     (IX-DC)                   
003275       MOVE SLAG-KVPBREOI    TO WS-RED-KVPBREOI                           
003276                                WS-KVPBREOI     (IX-DC)                   
003277       COMPUTE WS-KVPB-TOT (IX-DC)                                        
003278                                = WS-KVPB-REF (IX-DC)                     
003279                                + WS-KVPBREOI (IX-DC)                     
003280       MOVE WS-RED-KVPB-REF  TO MOD-KVPB-REF    (IX-DC)                   
003281       MOVE WS-RED-KVPBREOI  TO MOD-KVPBREOI    (IX-DC)                   
003282       MOVE MFS-ADD-LAES-IN-FAELT                                         
003283                             TO MOD-KVPB-REF-ATTR (IX-DC)                 
003284                                MOD-KVPBREOI-ATTR (IX-DC)                 
003285*                                                                         
003286       IF SLAG-FLREFBEO = JA                                              
003287       AND MSGI-IDLAND-SPR = 'GB'                                         
003288         MOVE YES            TO MOD-FLREFBEO    (IX-DC)                   
003289       ELSE                                                               
003290         MOVE SLAG-FLREFBEO  TO MOD-FLREFBEO    (IX-DC)                   
003291       END-IF                                                             
003292       MOVE MFS-ADD-LAES-IN-FAELT                                         
003293                             TO MOD-FLREFBEO-ATTR (IX-DC)                 
003294       MOVE SLAG-KVREFPKT    TO MOD-KVREFPKT    (IX-DC)                   
003295       PERFORM IMS-GU-WDK727                                              
003296       IF SEGMENT-FINNS                                                   
003297         IF FUTUR-PROG-KVPB-JUST(1) > ZERO                                
003298           MOVE MFS-ADD-LYS-UPP-FAELT                                     
003299                             TO MOD-KVREFPKT-ATTR (IX-DC)                 
003300         END-IF                                                           
003301       END-IF                                                             
003302     END-IF                                                               
003303                                                                          
003304     IF SLAG-ADLAGOMR-CD = ZERO                                           
003305        MOVE NEJ             TO MOD-FLCDART (IX-DC)                       
003306     ELSE                                                                 
003307        IF MSGI-IDLAND-SPR = 'GB'                                         
003308          MOVE YES           TO MOD-FLCDART (IX-DC)                       
003309        ELSE                                                              
003310          MOVE JA            TO MOD-FLCDART (IX-DC)                       
003311        END-IF                                                            
003312     END-IF                                                               
003313     MOVE MFS-ADD-LYS-UPP-FAELT                                           
003314                             TO MOD-FLCDART-ATTR (IX-DC)                  
003315                                                                          
003316     MOVE SLAG-IDDC          TO WS-IDDC                                   
003317     MOVE NEJ                TO SW-K712-SEGMENT                           
003318     IF NDC                                                               
003319       PERFORM S08-SEARCH-IDLAND                                          
003320       PERFORM IMS-GU-WDK712                                              
003321       IF SEGMENT-FINNS                                                   
003322         MOVE JA             TO SW-K712-SEGMENT                           
003323       END-IF                                                             
003324     END-IF                                                               
003325                                                                          
003326     IF K712-EXIST                                                        
003327       MOVE LART-FLREFERAL   TO WS-FLREFERAL                              
003328     ELSE                                                                 
003329       MOVE NEJ              TO WS-FLREFERAL                              
003330     END-IF                                                               
003331     IF SLAG-FLORDSP = JA                                                 
003332       IF WS-FLREFERAL = JA                                               
003333         MOVE 'T'            TO MOD-FREEZECODE (IX-DC)                    
003334       ELSE                                                               
003335         MOVE 'F'            TO MOD-FREEZECODE (IX-DC)                    
003336       END-IF                                                             
003337     ELSE                                                                 
003338       IF SLAG-FLSPBULK = JA                                              
003339         IF WS-FLREFERAL = JA                                             
003340           MOVE 'T'          TO MOD-FREEZECODE (IX-DC)                    
003341         ELSE                                                             
003342           MOVE 'P'          TO MOD-FREEZECODE (IX-DC)                    
003343         END-IF                                                           
003344       ELSE                                                               
003345         IF WS-FLREFERAL = JA                                             
003346           MOVE 'R'          TO MOD-FREEZECODE (IX-DC)                    
003347         ELSE                                                             
003348           MOVE '-'          TO MOD-FREEZECODE (IX-DC)                    
003349         END-IF                                                           
003350       END-IF                                                             
003351     END-IF                                                               
003352*                                                                         
003353     COMPUTE WS-BALANCE ROUNDED = SLAG-KVLS -                             
003354                 SLAG-KVROS-BULK - SLAG-KVROS-DAG -                       
003355                 SLAG-KVOKS-BULK - SLAG-KVOKS-DAG                         
003356     MOVE WS-BALANCE         TO MOD-BALANCE (IX-DC)                       
003357     MOVE SLAG-IDDC-REF      TO MOD-IDDC-REF (IX-DC)                      
003358     MOVE SLAG-KVAKS-SDC     TO MOD-KVAKS-SDC (IX-DC)                     
003359     COMPUTE WS-ORDERED ROUNDED = SLAG-KVBEART + SLAG-KVAKS-PAV           
003360     MOVE WS-ORDERED         TO MOD-ORDERED (IX-DC)                       
003361     COMPUTE WS-KVROS ROUNDED = SLAG-KVROS-BULK + SLAG-KVROS-DAG          
003362     MOVE WS-KVROS           TO MOD-KVROS (IX-DC)                         
003363     COMPUTE WS-SUPERWEEK (IX-DC) ROUNDED =                               
003364           (SLAG-KVLS +                                                   
003365            SLAG-KVAKS-SDC + SLAG-KVAKS-PAV + SLAG-KVBEART -              
003366            SLAG-KVOKS-BULK - SLAG-KVOKS-DAG -                            
003367            SLAG-KVROS-BULK - SLAG-KVROS-DAG +                            
003368            WS-PURCHQTY-SIM (IX-DC)) /                                    
003369           (WS-KVPB-TOT (IX-DC) / 4.33)                                   
003370                  ON SIZE ERROR                                           
003371                      MOVE +999   TO WS-SUPERWEEK (IX-DC)                 
003372     END-COMPUTE                                                          
003373     IF WS-SUPERWEEK (IX-DC) > +999                                       
003374       MOVE +999             TO WS-SUPERWEEK (IX-DC)                      
003375     END-IF                                                               
003376                                                                          
003377     IF WS-SUPERWEEK (IX-DC) < ZERO                                       
003378       MOVE ZERO             TO WS-SUPERWEEK (IX-DC)                      
003379     END-IF                                                               
003380                                                                          
003381     IF WS-BALANCE > ZERO                                                 
003382     OR WS-KVPB-TOT (IX-DC) > ZERO                                        
003383       CONTINUE                                                           
003384     ELSE                                                                 
003385       MOVE ZERO             TO WS-SUPERWEEK (IX-DC)                      
003386     END-IF                                                               
003387                                                                          
003388     MOVE WS-SUPERWEEK (IX-DC)                                            
003389                             TO MOD-SUPERWEEK (IX-DC)                     
003390     IF SLAG-KDLEVSP > ZERO                                               
003391       MOVE 'F'              TO MOD-QUALBLOCK (IX-DC)                     
003392     ELSE                                                                 
003393       IF SLAG-KVSPARR-KVAL > ZERO                                        
003394         MOVE 'P'            TO MOD-QUALBLOCK (IX-DC)                     
003395       ELSE                                                               
003396         MOVE '-'            TO MOD-QUALBLOCK (IX-DC)                     
003397       END-IF                                                             
003398     END-IF                                                               
003399                                                                          
003400*        DC = 41, 42, 43, 44 ELLER 45                                     
003401*        AVERAGE COST FÖR NDC-US TAS FRÅN                                 
003402*        DC 41 OM DET FINNS I ANNAT FALL                                  
003403*        DC 42 ELLER DC 43 ELLER DC 44 ELLER 45                           
003404     MOVE W-IDDC             TO W-IDDC-B6                                 
003405     PERFORM IMS-GU-WDB601                                                
003406     MOVE +1                 TO IX                                        
003407     MOVE NEJ                TO SW-SEASON                                 
003408     PERFORM UNTIL IX > +12                                               
003409     OR SW-SEASON-JA                                                      
003410       IF SLAG-RESEASON (IX) NOT = +1.00                                  
003411         MOVE JA             TO SW-SEASON                                 
003412       END-IF                                                             
003413       ADD +1                TO IX                                        
003414     END-PERFORM                                                          
003415     IF SW-SEASON = JA                                                    
003416       MOVE 'Y'              TO MOD-SEASON (IX-DC)                        
003417     ELSE                                                                 
003418       MOVE 'N'              TO MOD-SEASON (IX-DC)                        
003419     END-IF                                                               
003420     MOVE SLAG-KVROS-DAG     TO MOD-CRIT-KVROS-NDC-CDC (IX-DC)            
003421                                                                          
003422     IF SLAG-FLPB-FLYTT = JA                                              
003423       MOVE 'REPL'           TO WS-TEMFSINF-NDC (IX-DC)                   
003424     END-IF                                                               
003425                                                                          
003426     IF  WS-REF-KDREFTXT (IX-DC) > ZERO                                   
003427       MOVE +1               TO IX                                        
003428       PERFORM UNTIL IX > REF-TEXT-TABMAX                                 
003429       OR WS-REF-KDREFTXT (IX-DC) = REF-TEXT-KDREFTEXT (IX)               
003430         ADD +1              TO IX                                        
003431       END-PERFORM                                                        
003432       IF IX > REF-TEXT-TABMAX                                            
003433         CONTINUE                                                         
003434       ELSE                                                               
003435         IF WS-REF-KDREFTXT (IX-DC) = 70                                  
003436           CONTINUE                                                       
003437         ELSE                                                             
003438           MOVE REF-TEXT (IX) TO WS-TEMFSINF-NDC (IX-DC)                  
003439         END-IF                                                           
003440       END-IF                                                             
003441     END-IF                                                               
003442     .                                                                    
003443     EJECT                                                                
003444                                                                          
003445 FC-DISPLAY-IDDC-GROUP SECTION.                                           
003446                                                                          
003447     MOVE +1                    TO IX-DC                                  
003448     PERFORM UNTIL     IX-DC     > IX-DC-MAX                              
003449       MOVE WS-DC-NR (IX-DC)    TO MOD-IDDC-GROUP (IX-DC)                 
003450       ADD +1                   TO IX-DC                                  
003451     END-PERFORM                                                          
003452     .                                                                    
003453     EJECT                                                                
003454                                                                          
003455 FD-HAEMTA-KVPB SECTION.                                                  
003456                                                                          
003457*  ----  CLAG-KVPB ÄR PROGNOS FÖR EN 6-VECKORS PERIOD                     
003458*  ----  DETTA GÖRS OM OCH VISAS SOM EN MÅNADSPERIOD                      
003459                                                                          
003460     COMPUTE WS-KVPB ROUNDED =                                            
003461               (CLAG-KVPB-SATS + CLAG-KVPB-SEP + CLAG-KVPB-TPO)           
003462                                                                          
003463     PERFORM IMS-GN-WDB601                                                
003464     PERFORM UNTIL SEGMENT-SAKNAS                                         
003465       IF DCS-CDC OR DCS-DDC                                              
003466          CONTINUE                                                        
003467       ELSE                                                               
003468          MOVE DCS-IDDC          TO W-IDDC                                
003469          PERFORM IMS-GU-WDK7-WDK711                                      
003470          IF SEGMENT-FINNS                                                
003471*  ----  SLAG-KVPB ÄR PROGNOS FÖR EN MÅNADSPERIOD                         
003472*            IF SLAG-IDDC-REF = '11'                                      
003473               ADD SLAG-KVPB-REF TO WS-KVPB                               
003474*            END-IF                                                       
003475          END-IF                                                          
003476       END-IF                                                             
003477       PERFORM IMS-GN-WDB601                                              
003478     END-PERFORM                                                          
003479     .                                                                    
003480     EJECT                                                                
003481 FE-CHECK-ART-SOURCE SECTION.                                             
003482                                                                          
003483*  ----  CHECK MARKET WHERE PART IS SOURCED                               
003484*  ----  FOR CHECKING SOURCE MARKET CALL W271UTIL WITH KDCAL 001          
003485     INITIALIZE  UTIL-W271UTIL                                            
003486     MOVE 001                   TO UTIL-KDCALL                            
003487     MOVE W-IDARTNR             TO UTIL-IDARTNR                           
003488                                                                          
003489     CALL W271UTIL USING UTIL-W271UTIL                                    
003490                         UTIL-WDK6-PCB                                    
003491                         UTIL-WDK7-PCB                                    
003492                         UTIL-WDB6-PCB                                    
003493                                                                          
003494     IF UTIL-KDSVAR-OK                                                    
003495        MOVE UTIL-TEXT          TO WS-TEMFSINF-SOURCE                     
003496     END-IF                                                               
003497     .                                                                    
003498     EJECT                                                                
003499 H-UPD-WDK6-WDK7 SECTION.                                                 
003500                                                                          
003501     PERFORM IMS-GU-WDK6-ARTC11                                           
003502     MOVE +1                 TO IX-DC                                     
003503     PERFORM UNTIL IX-DC      > IX-DC-MAX                                 
003504       MOVE WS-DC-NR (IX-DC) TO W-IDDC                                    
003505       PERFORM S03-GET-BESPRIS                                            
003506       PERFORM IMS-GHU-WDK7-WDK711                                        
003507       IF SEGMENT-FINNS                                                   
003508          PERFORM HA-UPPDATERA-WDK7                                       
003509          PERFORM IMS-REPL-WDK7-WDK711                                    
003510       END-IF                                                             
003511       IF KVPB-PLAN-UPD-JA                                                
003512          PERFORM HC-RECALCULATE-PBPLAN                                   
003513          MOVE 'N'   TO SW-KVPB-PLAN                                      
003514       END-IF                                                             
003515       PERFORM HB-UPD-REFL1-OUTPUT                                        
003516       ADD +1                TO IX-DC                                     
003517     END-PERFORM                                                          
003518*                                                                         
003519     MOVE +5                 TO W-KDNOTTYP                                
003520     PERFORM IMS-GHU-WDK6-ARTC25                                          
003521     IF SEGMENT-FINNS                                                     
003522       MOVE +5               TO NOT-KDNOTTYP                              
003523       MOVE MID-COMMENT      TO NOT-TEARTNOT                              
003524       PERFORM IMS-REPL-WDK6-ARTC25                                       
003525     ELSE                                                                 
003526       MOVE +5               TO NOT-KDNOTTYP                              
003527       MOVE MID-COMMENT      TO NOT-TEARTNOT                              
003528       PERFORM IMS-ISRT-WDK6-ARTC25                                       
003529     END-IF                                                               
003530     .                                                                    
003531     EJECT                                                                
003532 HA-UPPDATERA-WDK7 SECTION.                                               
003533                                                                          
003534     IF WS-KVPB-TOT (IX-DC) > ZERO                                        
003535       IF WS-KVPB-REF (IX-DC) > SLAG-KVPB-REF                             
003536       OR WS-KVPBREOI (IX-DC) > SLAG-KVPBREOI                             
003537          MOVE NEJ           TO SLAG-FLREFNYO                             
003538       END-IF                                                             
003539     END-IF                                                               
003540                                                                          
003541* --                                                                      
003542     IF MID-FLREFBEO (IX-DC)  = ALL '+'                                   
003543        CONTINUE                                                          
003544     ELSE                                                                 
003545        IF MID-FLREFBEO (IX-DC) = YES                                     
003546          MOVE JA            TO SLAG-FLREFBEO                             
003547        ELSE                                                              
003548          MOVE MID-FLREFBEO (IX-DC)                                       
003549                             TO SLAG-FLREFBEO                             
003550        END-IF                                                            
003551     END-IF                                                               
003552                                                                          
003553     IF WS-KVPB-REF (IX-DC)   = SLAG-KVPB-REF                             
003554       CONTINUE                                                           
003555     ELSE                                                                 
003556       MOVE DAGENS-DATUM     TO SLAG-TIREFMPB                             
003557       MOVE JA               TO SW-KVPB-SEP                               
003558     END-IF                                                               
003559                                                                          
003560     IF WS-KVPBREOI (IX-DC)   = SLAG-KVPBREOI                             
003561       CONTINUE                                                           
003562     ELSE                                                                 
003563       MOVE DAGENS-DATUM     TO SLAG-TIPBREOI                             
003564       MOVE JA               TO SW-KVPB-SEP                               
003565     END-IF                                                               
003566                                                                          
003567     MOVE SLAG-IDDC          TO WS-IDDC                                   
003568                                W-IDDC-B6                                 
003569     IF WS-FLAGGA-FCD-UPD (IX-DC) > SPACES                                
003570*      To exclude LOCK logic for the new buyer steering rule              
003571       PERFORM IMS-GU-WDB601                                              
003572       IF DCS-KDDCSTYR-BUY > ZERO                                         
003573*                                                                         
003574          IF WS-FLAGGA-FCD-UPD (IX-DC) = JA                               
003575             MOVE 9             TO SLAG-IDREFTAB                          
003576             MOVE 'J'           TO SLAG-FLTABUPD                          
003577          ELSE                                                            
003578            IF (WS-FLAGGA-FCD-UPD (IX-DC) = NEJ                           
003579            AND SLAG-FLFLYG = JA)                                         
003580            OR (WS-FLAGGA-FCD-UPD (IX-DC) = 'S'                           
003581            AND SLAG-FLFLYG = JA)                                         
003582              MOVE ZERO         TO SLAG-IDREFTAB                          
003583              MOVE 'N'          TO SLAG-FLTABUPD                          
003584            END-IF                                                        
003585          END-IF                                                          
003586       END-IF                                                             
003587       IF NDC-NA                                                          
003588          MOVE WS-FLAGGA-FCD-UPD (IX-DC)                                  
003589                             TO SLAG-FLFLYG                               
003590       END-IF                                                             
003591     END-IF                                                               
003592                                                                          
003593     MOVE WS-KVPB-REF (IX-DC)                                             
003594                             TO SLAG-KVPB-REF                             
003595                                SLAG-KVPB-HIST                            
003596     MOVE WS-KVPBREOI (IX-DC)                                             
003597                             TO SLAG-KVPBREOI                             
003598                                SLAG-KVPBREOI-HIST                        
003599                                                                          
003600     IF SW-KVPB-SEP-JA                                                    
003601       IF SLAG-IDDC-REF = WS-CDC-11                                       
003602*****   IF REFILLED FROM CDC AND WE CHANGE FORECAST WE NEED TO            
003603*****   UDPATE CREF-KVPB-PLAN IF PART IS REFILLED TO CDC FROM             
003604*****   ANOTHER DC                                                        
003605         IF CLAG-IDDC-REF NOT = SPACE                                     
003606           MOVE JA          TO SW-KVPB-PLAN                               
003607         END-IF                                                           
003608       END-IF                                                             
003609     END-IF                                                               
003610     .                                                                    
003611     EJECT                                                                
003612*                                                                         
003613 HB-UPD-REFL1-OUTPUT SECTION.                                             
003614                                                                          
003615***  THIS SECTION CALLS W271REFL TO GET THE CALULATED                     
003616***  REFILLING POINT AND REFILLING QUANTITY                               
003617***  CAN BE USED FOR UPDATE ON WDK7 OR JUST SIMULATION                    
003618***                                                                       
003619     PERFORM IMS-GHU-WDK7-WDK711                                          
003620     IF SEGMENT-FINNS                                                     
003621***     FOR UPDATES SIMULATION FLAG FLSIM SHOULD BE NO                    
003622        MOVE NEJ                 TO WS-FLSIM                              
003623        PERFORM S90-CALL-W271REFL                                         
003624*                                                                         
003625        IF  SLAG-IDDC-REF = SPACE                                         
003626        AND NDC-US                                                        
003627            MOVE 'A'             TO SLAG-IDREFTAB                         
003628        END-IF                                                            
003629*                                                                         
003630        MOVE SLAG-TIREFPKT       TO TMP1-YYMMDD                           
003631        MOVE DAGENS-DATUM        TO TMP2-YYMMDD                           
003632        PERFORM WY2000P1                                                  
003633        IF TMP1-YYMMDD >= TMP2-YYMMDD                                     
003634                                                                          
003635*                                                                         
003636*--- INGEN UPPDATERING AV KVREFPKT PGA MANUELLT DATUM ÄR SATT             
003637          CONTINUE                                                        
003638        ELSE                                                              
003639          MOVE REFL-KVREFPKT     TO SLAG-KVREFPKT                         
003640        END-IF                                                            
003641                                                                          
003642        MOVE SLAG-TIREFPAF       TO TMP1-YYMMDD                           
003643        MOVE DAGENS-DATUM        TO TMP2-YYMMDD                           
003644        PERFORM WY2000P1                                                  
003645        IF TMP1-YYMMDD >= TMP2-YYMMDD                                     
003646*                                                                         
003647*--- INGEN UPPDATERING AV KVREFBER PGA MANUELLT DATUM ÄR SATT             
003648          CONTINUE                                                        
003649        ELSE                                                              
003650          MOVE REFL-KVREFBER     TO SLAG-KVREFBER                         
003651        END-IF                                                            
003652        MOVE REFL-KVREFOVL       TO SLAG-KVREFOVL                         
003653*                                                                         
003654        IF  SLAG-KDREFSTA         = PASSIV                                
003655        AND (SLAG-KVPB-REF        > ZERO                                  
003656        OR  SLAG-KVPBREOI         > ZERO)                                 
003657          MOVE AKTIV             TO SLAG-KDREFSTA                         
003658          MOVE DAGENS-DATUM      TO SLAG-TIREFSTA                         
003659        END-IF                                                            
003660*                                                                         
003661        PERFORM IMS-REPL-WDK7-WDK711                                      
003662     END-IF                                                               
003663     .                                                                    
003664     EJECT                                                                
003665*                                                                         
003666 HC-RECALCULATE-PBPLAN SECTION.                                           
003667                                                                          
003668*--- THIS SECTION CALLS UTILITY PROGRAM W272UTUP TO UPDATE                
003669*--- KVPB-PLAN IN WDK6. ITS MANDATORY TO CALL THE UTILITY                 
003670*--- USING CORRECT KDCALL VALUE.                                          
003671                                                                          
003672     INITIALIZE W272-UTUP-W272UTUP                                        
003673     MOVE 1                     TO W272-UTUP-KDCALL                       
003674     MOVE W-IDARTNR             TO W272-UTUP-IDARTNR                      
003675     MOVE W-IDDC                TO W272-UTUP-IDDC                         
003676     IF CLAG-IDDC-REF = SPACE                                             
003677       CALL FELLOG                                                        
003678     ELSE                                                                 
003679       MOVE CLAG-IDDC-REF       TO W272-UTUP-IDDC-REF                     
003680     END-IF                                                               
003681                                                                          
003682     CALL W272UTUP USING W272-UTUP-W272UTUP                               
003683                   U2-WDK6-PCB                                            
003684                   U2-WDB6-PCB                                            
003685                   U2-PBTO-W222-WDK6-PCB                                  
003686                   U2-PBTO-W222-WDK7-PCB                                  
003687                   U2-PBTO-W222-ARTM-PCB                                  
003688                   U2-PBTO-W222-REFL1-2501-PCB                            
003689                   U2-PBTO-W222-REFL1-WDB6R-PCB                           
003690                   U2-PBTO-W222-REFL1-WDK7R-PCB                           
003691                   U2-PBTO-W222-WDB6-PCB                                  
003692                   U2-PBTO-W222-WDD7-PCB                                  
003693                   U2-PBTO-W222-WDK7E-PCB                                 
003694                   U2-PBTO-W222-REFL1-UTIL-K6-PCB                         
003695                   U2-PBTO-W222-REFL1-UTIL-K7-PCB                         
003696                   U2-PBTO-W222-REFL1-UTIL-B6-PCB                         
003697                   U2-PBTO-W222-UTUP1-WDK7-PCB                            
003698                   U2-PBTO-W222-UTUP1-WDB6-PCB                            
003699                   U2-PBTO-W222-UTUP1-UTIL-K6-PCB                         
003700                   U2-PBTO-W222-UTUP1-UTIL-K7-PCB                         
003701                   U2-PBTO-W222-UTUP1-UTIL-B6-PCB                         
003702                   U2-REFL2-2501-PCB                                      
003703                   U2-REFL2-WDB6-PCB                                      
003704                   U2-REFL2-UTIL-WDK6-PCB                                 
003705                   U2-REFL2-UTIL-WDK7-PCB                                 
003706                   U2-REFL2-UTIL-WDB6-PCB                                 
003707                   U2-W222-WDK6-PCB                                       
003708                   U2-W222-WDK7-PCB                                       
003709                   U2-W222-ARTM-PCB                                       
003710                   U2-W222-2501-PCB                                       
003711                   U2-W222-WDB6R-PCB                                      
003712                   U2-W222-WDK7R-PCB                                      
003713                   U2-W222-WDB6-PCB                                       
003714                   U2-W222-WDD7-PCB                                       
003715                   U2-W222-WDK7E-PCB                                      
003716                   U2-W222-UTIL-WDK6-PCB                                  
003717                   U2-W222-UTIL-WDK7-PCB                                  
003718                   U2-W222-UTIL-WDB6-PCB                                  
003719                   U2-W222-UTUP1-WDK7-PCB                                 
003720                   U2-W222-UTUP1-WDB6-PCB                                 
003721                   U2-W222-UTUP1-UTIL-WDK6-PCB                            
003722                   U2-W222-UTUP1-UTIL-WDK7-PCB                            
003723                   U2-W222-UTUP1-UTIL-WDB6-PCB                            
003724                                                                          
003725     IF W272-UTUP-KDSVAR-OK                                               
003726        CONTINUE                                                          
003727     ELSE                                                                 
003728        DISPLAY 'W272UTUP-ERROR :' W272-UTUP-TEXT                         
003729        CALL FELLOG                                                       
003730     END-IF                                                               
003731     .                                                                    
003732     EJECT                                                                
003733*                                                                         
003734 I-KOLLA-INPUT SECTION.                                                   
003735                                                                          
003736     PERFORM IMS-GU-WDK6-ARTC11                                           
003737     IF SEGMENT-FINNS                                                     
003738       MOVE NEJ              TO SW-KTRL-ERS                               
003739       MOVE CLAG-IDDC-REF    TO W-K6-IDDC-REF                             
003740       MOVE +1               TO IX-DC                                     
003741       PERFORM UNTIL IX-DC > IX-DC-MAX                                    
003742         IF MID-PURCHQTY (IX-DC) = ALL '+'                                
003743           MOVE NEJ          TO SW-KTRL-ERS                               
003744         ELSE                                                             
003745           INSPECT MID-PURCHQTY (IX-DC)                                   
003746                          REPLACING LEADING SPACE BY ZERO                 
003747           IF  MID-PURCHQTY (IX-DC) NUMERIC                               
003748           AND MID-PURCHQTY (IX-DC) > ZERO                                
003749             MOVE JA         TO SW-KTRL-ERS                               
003750             EVALUATE MID-IDDC-FROM (IX-DC)                               
003751               WHEN   WS-DC-NR (1)                                        
003752                 MOVE MID-PURCHQTY (IX-DC) TO W-PURCHQTY (IX-DC)          
003753                 ADD W-PURCHQTY (IX-DC)    TO WS-PURCHQTY-DC1             
003754               WHEN   WS-DC-NR (2)                                        
003755                 MOVE MID-PURCHQTY (IX-DC) TO W-PURCHQTY (IX-DC)          
003756                 ADD W-PURCHQTY (IX-DC)    TO WS-PURCHQTY-DC2             
003757               WHEN   WS-DC-NR (3)                                        
003758                 MOVE MID-PURCHQTY (IX-DC) TO W-PURCHQTY (IX-DC)          
003759                 ADD W-PURCHQTY (IX-DC)    TO WS-PURCHQTY-DC3             
003760               WHEN   WS-DC-NR (4)                                        
003761                 MOVE MID-PURCHQTY (IX-DC) TO W-PURCHQTY (IX-DC)          
003762                 ADD W-PURCHQTY (IX-DC)    TO WS-PURCHQTY-DC4             
003763             END-EVALUATE                                                 
003764           ELSE                                                           
003765             MOVE NEJ                      TO SW-KTRL-ERS                 
003766           END-IF                                                         
003767         END-IF                                                           
003768*2224095 CHK FOR KVDISP-SEND-DC FOR KDERS >= 10                           
003769         IF SW-KTRL-ERS-JA                                                
003770          IF CLAG-KDERS               < 10                                
003771            IF CLAG-PRARTSTD = ZERO                                       
003772              MOVE PRIS-SAKNAS                                            
003773                             TO MED-IDMFSFEL                              
003774              CALL WMEDKONV USING MED-WMEDAREA                            
003775              MOVE MED-TEMFSFEL                                           
003776                             TO MOD-TEMFSFEL                              
003777              MOVE NEJ          TO INDATA-SW                              
003778            END-IF                                                        
003779          ELSE                                                            
003780            MOVE WS-DC-NR (IX-DC)    TO WS-IDDC                           
003781            MOVE WS-SENDING-DC-NR (IX-DC)                                 
003782                                     TO  WS-IDDC-REF-TEST                 
003783            IF ((NDC-CN OR NDC-NA)                                        
003784            AND (WS-IDDC-REF-TEST NOT = SPACES))                          
003785               IF CLAG-KDERS NOT = +52                                    
003786                   PERFORM S09-GET-WDK711-SEND-DC                         
003787                   IF WS-KVDISP-SEND-DC > 0                               
003788                       CONTINUE                                           
003789                   ELSE                                                   
003790                       MOVE ARTIKEL-ERSATT                                
003791                                        TO MED-IDMFSFEL                   
003792                       CALL WMEDKONV USING MED-WMEDAREA                   
003793                       MOVE MED-TEMFSFEL                                  
003794                                        TO MOD-TEMFSFEL                   
003795                       MOVE NEJ         TO INDATA-SW                      
003796                   END-IF                                                 
003797               ELSE                                                       
003798                   MOVE ARTIKEL-UTGANGEN                                  
003799                                        TO MED-IDMFSFEL                   
003800                   CALL WMEDKONV USING MED-WMEDAREA                       
003801                   MOVE MED-TEMFSFEL    TO MOD-TEMFSFEL                   
003802                   MOVE NEJ             TO INDATA-SW                      
003803               END-IF                                                     
003804            ELSE                                                          
003805                IF CLAG-KDERS              = +29 OR +52                   
003806                  MOVE ARTIKEL-UTGANGEN                                   
003807                                   TO MED-IDMFSFEL                        
003808                  CALL WMEDKONV USING MED-WMEDAREA                        
003809                  MOVE MED-TEMFSFEL                                       
003810                                   TO MOD-TEMFSFEL                        
003811                  MOVE NEJ           TO INDATA-SW                         
003812                ELSE                                                      
003813                  MOVE ARTIKEL-ERSATT                                     
003814                                   TO MED-IDMFSFEL                        
003815                  CALL WMEDKONV USING MED-WMEDAREA                        
003816                  MOVE MED-TEMFSFEL                                       
003817                                   TO MOD-TEMFSFEL                        
003818                  MOVE NEJ           TO INDATA-SW                         
003819                END-IF                                                    
003820            END-IF                                                        
003821          END-IF                                                          
003822         END-IF                                                           
003823                                                                          
003824         ADD +1              TO IX-DC                                     
003825       END-PERFORM                                                        
003826     ELSE                                                                 
003827       MOVE ARTIKEL-SAKNAS   TO MED-IDMFSFEL                              
003828       CALL WMEDKONV USING MED-WMEDAREA                                   
003829       MOVE MED-TEMFSFEL     TO MOD-TEMFSFEL                              
003830       MOVE NEJ              TO INDATA-SW                                 
003831     END-IF                                                               
003832                                                                          
003833     MOVE +1                 TO IX-DC                                     
003834     PERFORM UNTIL IX-DC > IX-DC-MAX                                      
003835       IF MID-KVPB-REF (IX-DC) = ALL '+'                                  
003836         MOVE MFS-ADD-LAES-IN-FAELT                                       
003837                             TO MOD-KVPB-REF-ATTR (IX-DC)                 
003838       ELSE                                                               
003839         MOVE MID-KVPB-REF (IX-DC)                                        
003840                             TO DEC-IDFRIDATA                             
003841                                MOD-KVPB-REF (IX-DC)                      
003842         MOVE 6              TO DEC-KVHELTAL                              
003843         MOVE 1              TO DEC-KVDECIMAL                             
003844         CALL WDECEDIT USING DEC-WDECAREA                                 
003845         IF DEC-KDSVAR-OK                                                 
003846           MOVE MFS-ADD-LAES-IN-FAELT                                     
003847                             TO MOD-KVPB-REF-ATTR (IX-DC)                 
003848           MOVE DEC-IDEDITDATA                                            
003849                             TO WS-RED-KVPB-REF                           
003850                                WS-KVPB-REF (IX-DC)                       
003851           MOVE WS-RED-KVPB-REF                                           
003852                             TO MOD-KVPB-REF (IX-DC)                      
003853         ELSE                                                             
003854           MOVE MED-3        TO MOD-TEMFSFEL                              
003855           MOVE MFS-ADD-LAES-IN-FAELT-HI                                  
003856                             TO MOD-KVPB-REF-ATTR (IX-DC)                 
003857           MOVE NEJ          TO INDATA-SW                                 
003858           MOVE MID-KVPB-REF (IX-DC)                                      
003859                             TO MOD-KVPB-REF (IX-DC)                      
003860         END-IF                                                           
003861       END-IF                                                             
003862                                                                          
003863       IF MID-KVPBREOI (IX-DC) = ALL '+'                                  
003864         MOVE MFS-ADD-LAES-IN-FAELT                                       
003865                             TO MOD-KVPBREOI-ATTR (IX-DC)                 
003866       ELSE                                                               
003867         MOVE ZERO           TO WS-KVPBREOI-SLAG                          
003868         MOVE MID-KVPBREOI (IX-DC)                                        
003869                             TO DEC-IDFRIDATA                             
003870                                MOD-KVPBREOI (IX-DC)                      
003871         MOVE 6              TO DEC-KVHELTAL                              
003872         MOVE 1              TO DEC-KVDECIMAL                             
003873         CALL WDECEDIT USING DEC-WDECAREA                                 
003874         IF DEC-KDSVAR-OK                                                 
003875           MOVE MFS-ADD-LAES-IN-FAELT                                     
003876                             TO MOD-KVPBREOI-ATTR (IX-DC)                 
003877           MOVE DEC-IDEDITDATA                                            
003878                             TO WS-RED-KVPBREOI                           
003879                                WS-KVPBREOI (IX-DC)                       
003880           MOVE WS-RED-KVPBREOI                                           
003881                             TO MOD-KVPBREOI (IX-DC)                      
003882         ELSE                                                             
003883           MOVE MED-3        TO MOD-TEMFSFEL                              
003884           MOVE MFS-ADD-LAES-IN-FAELT-HI                                  
003885                             TO MOD-KVPBREOI-ATTR (IX-DC)                 
003886           MOVE NEJ          TO INDATA-SW                                 
003887           MOVE MID-KVPBREOI (IX-DC)                                      
003888                             TO MOD-KVPBREOI (IX-DC)                      
003889         END-IF                                                           
003890       END-IF                                                             
003891                                                                          
003892       IF WS-KVPB-REF (IX-DC) > ZERO                                      
003893         MOVE WS-DC-NR (IX-DC)                                            
003894                             TO W-IDDC                                    
003895                                WS-IDDC                                   
003896         PERFORM IMS-GU-WDK7-WDK711                                       
003897         IF SEGMENT-FINNS                                                 
003898           IF NDC-US                                                      
003899              IF SLAG-IDDC-REF > SPACES                                   
003900                 MOVE MFS-ADD-LAES-IN-FAELT                               
003901                             TO MOD-KVPB-REF-ATTR (IX-DC)                 
003902              ELSE                                                        
003903                 IF WS-KVPB-REF (IX-DC)                                   
003904                               = SLAG-KVPB-REF                            
003905                    CONTINUE                                              
003906                 ELSE                                                     
003907                    MOVE NOT-REFILL-PART                                  
003908                             TO MED-IDMFSFEL                              
003909                    CALL WMEDKONV                                         
003910                          USING MED-WMEDAREA                              
003911                    MOVE MED-TEMFSFEL                                     
003912                             TO MOD-TEMFSFEL                              
003913                    MOVE MFS-ADD-LAES-IN-FAELT-HI                         
003914                             TO MOD-KVPB-REF-ATTR (IX-DC)                 
003915                    MOVE NEJ TO INDATA-SW                                 
003916                 END-IF                                                   
003917              END-IF                                                      
003918           END-IF                                                         
003919         ELSE                                                             
003920           MOVE ARTIKEL-SAKNAS                                            
003921                             TO MED-IDMFSFEL                              
003922           CALL WMEDKONV USING MED-WMEDAREA                               
003923           MOVE MED-TEMFSFEL                                              
003924                             TO MOD-TEMFSFEL                              
003925           MOVE MFS-ADD-LAES-IN-FAELT-HI                                  
003926                             TO MOD-KVPB-REF-ATTR (IX-DC)                 
003927           MOVE NEJ          TO INDATA-SW                                 
003928         END-IF                                                           
003929       END-IF                                                             
003930                                                                          
003931       IF WS-KVPBREOI (IX-DC) > ZERO                                      
003932         MOVE WS-DC-NR (IX-DC)                                            
003933                             TO W-IDDC                                    
003934                                W-IDDC-REF                                
003935         PERFORM IMS-GU-WDK7-WDK711                                       
003936         IF SEGMENT-FINNS                                                 
003937           MOVE SLAG-KVPBREOI                                             
003938                             TO WS-KVPBREOI-SLAG                          
003939           MOVE MFS-ADD-LAES-IN-FAELT                                     
003940                             TO MOD-KVPBREOI-ATTR (IX-DC)                 
003941         ELSE                                                             
003942           MOVE ARTIKEL-SAKNAS                                            
003943                             TO MED-IDMFSFEL                              
003944           CALL WMEDKONV  USING MED-WMEDAREA                              
003945           MOVE MED-TEMFSFEL                                              
003946                             TO MOD-TEMFSFEL                              
003947           MOVE MFS-ADD-LAES-IN-FAELT-HI                                  
003948                             TO MOD-KVPBREOI-ATTR (IX-DC)                 
003949           MOVE NEJ          TO INDATA-SW                                 
003950         END-IF                                                           
003951*                                                                         
003952         IF WS-KVPBREOI (IX-DC)                                           
003953                          NOT = WS-KVPBREOI-SLAG                          
003954            IF W-IDDC-REF     = W-K6-IDDC-REF                             
003955               CONTINUE                                                   
003956            ELSE                                                          
003957              PERFORM IMS-GU-WDK711-IDDCREF                               
003958              IF SEGMENT-FINNS                                            
003959                 CONTINUE                                                 
003960              ELSE                                                        
003961                MOVE UPD-NOT-ALLOWED                                      
003962                               TO MED-IDMFSFEL                            
003963                CALL WMEDKONV                                             
003964                            USING MED-WMEDAREA                            
003965                MOVE MED-TEMFSFEL                                         
003966                               TO MOD-TEMFSFEL                            
003967                MOVE MFS-ADD-LAES-IN-FAELT-HI                             
003968                               TO MOD-KVPBREOI-ATTR (IX-DC)               
003969                MOVE NEJ       TO INDATA-SW                               
003970              END-IF                                                      
003971            END-IF                                                        
003972         END-IF                                                           
003973       END-IF                                                             
003974       COMPUTE WS-KVPB-TOT (IX-DC)                                        
003975                                = WS-KVPB-REF (IX-DC)                     
003976                                + WS-KVPBREOI (IX-DC)                     
003977                                                                          
003978       IF  WS-KVPB-TOT (IX-DC)   > ZERO                                   
003979       AND INDATA-OK                                                      
003980         PERFORM IMS-GU-WDK7-WDK711                                       
003981         IF SEGMENT-FINNS                                                 
003982           IF WS-KVPB-REF (IX-DC) > ZERO                                  
003983             MOVE MFS-ADD-LAES-IN-FAELT                                   
003984                               TO MOD-KVPB-REF-ATTR (IX-DC)               
003985           END-IF                                                         
003986           IF WS-KVPBREOI (IX-DC) > ZERO                                  
003987             MOVE MFS-ADD-LAES-IN-FAELT                                   
003988                               TO MOD-KVPBREOI-ATTR (IX-DC)               
003989           END-IF                                                         
003990         ELSE                                                             
003991           CONTINUE                                                       
003992         END-IF                                                           
003993       END-IF                                                             
003994                                                                          
003995       IF MID-PURCHQTY (IX-DC) = ALL '+'                                  
003996         MOVE ZERO           TO WS-RED-PURCHQTY                           
003997                                WS-PURCHQTY (IX-DC)                       
003998         MOVE WS-RED-PURCHQTY                                             
003999                             TO MOD-PURCHQTY (IX-DC)                      
004000       ELSE                                                               
004001         INSPECT MID-PURCHQTY (IX-DC)                                     
004002                          REPLACING LEADING SPACE BY ZERO                 
004003         IF MID-PURCHQTY (IX-DC) NUMERIC                                  
004004           MOVE MID-PURCHQTY (IX-DC)                                      
004005                             TO WS-RED-PURCHQTY                           
004006                                WS-PURCHQTY (IX-DC)                       
004007           MOVE WS-RED-PURCHQTY                                           
004008                             TO MOD-PURCHQTY (IX-DC)                      
004009                                                                          
004010           IF MID-PURCHQTY (IX-DC) > ZERO                                 
004011             MOVE WS-DC-NR (IX-DC)                                        
004012                               TO W-IDDC                                  
004013             IF (MID-IDDC-FROM (IX-DC) NOT = ALL '+' AND                  
004014                 MID-IDDC-FROM (IX-DC) NOT = SPACE)                       
004015               MOVE MID-IDDC-FROM (IX-DC)     TO WS-IDDC                  
004016               IF NDC-NA                                                  
004017                 MOVE 'J' TO SW-TRANSF                                    
004018               END-IF                                                     
004019             END-IF                                                       
004020             PERFORM IMS-GU-WDK7-WDK711                                   
004021             IF SEGMENT-FINNS                                             
004022               MOVE SLAG-IDDC-REF TO WS-IDDC-REF                          
004023               IF MFS-UPDATE AND (WS-IDREFTYP = 'A' OR                    
004024                                  WS-IDREFTYP = 'C')                      
004025                             AND                                          
004026                (MID-IDDC-FROM (IX-DC) = ALL '+' OR                       
004027                 MID-IDDC-FROM (IX-DC) = SPACE)                           
004028                  IF SLAG-FLFLYG = 'S'                                    
004029                    MOVE MED-20 TO MOD-TEMFSFEL                           
004030                    MOVE NEJ TO INDATA-SW                                 
004031                  ELSE                                                    
004032                    PERFORM IA-CHECK-AIR-COST                             
004033                  END-IF                                                  
004034               END-IF                                                     
004035               IF ((WS-IDREFTYP = 'A'                                     
004036               OR   WS-IDREFTYP = 'C'                                     
004037               OR   WS-IDREFTYP = 'B'                                     
004038               OR   SW-TRANSF-JA)                                         
004039               AND SLAG-IDDC-REF > SPACE)                                 
004040               OR  (WS-IDREFTYP = 'L'                                     
004041               AND SLAG-IDDC-REF = SPACE)                                 
004042                 MOVE MFS-ADD-LAES-IN-FAELT                               
004043                               TO MOD-PURCHQTY-ATTR (IX-DC)               
004044               ELSE                                                       
004045                 MOVE MED-8  TO MOD-TEMFSFEL                              
004046                 MOVE NEJ    TO INDATA-SW                                 
004047                 MOVE MFS-ADD-LAES-IN-FAELT-HI                            
004048                               TO MOD-PURCHQTY-ATTR (IX-DC)               
004049               END-IF                                                     
004050               IF WS-IDREFTYP = 'L'                                       
004051*-----                                                                    
004052*-----   FÖR LOKALA LEVERANSER MÅSTE                                      
004053*-----   DET FINNAS ETT GODKÄNT PRIS                                      
004054*-----                                                                    
004055                 PERFORM IMS-GU-WDK6-ARTC01                               
004056                 IF SEGMENT-FINNS                                         
004057                   COMPUTE W-DAPRLIST-21 =                                
004058                            99999999 - DAGENS-DATUM-SEKEL                 
004059                   MOVE SLAG-IDLEVNR   TO W-IDLEVNR-21                    
004060                   PERFORM IMS-GNP-WDK6-ARTC21                            
004061                   PERFORM UNTIL SEGMENT-SAKNAS OR                        
004062                       PRL-KDSTATUS-PR = 1                                
004063                     PERFORM IMS-GNP-WDK6-ARTC21                          
004064                   END-PERFORM                                            
004065                 END-IF                                                   
004066                                                                          
004067                 IF SEGMENT-FINNS                                         
004068***    GODKÄNT PRIS ***************                                       
004069                   CONTINUE                                               
004070                 ELSE                                                     
004071                   MOVE NEJ  TO INDATA-SW                                 
004072                   MOVE MED-9 TO MOD-TEMFSFEL                             
004073                   MOVE MFS-ADD-LAES-IN-FAELT-HI                          
004074                               TO MOD-PURCHQTY-ATTR (IX-DC)               
004075                 END-IF                                                   
004076***    LOCAL ORDER NOT ALLOWED HERE FOR USA                               
004077                 MOVE WS-DC-NR (IX-DC) TO WS-IDDC                         
004078                 IF NDC-US                                                
004079                    MOVE MED-8  TO MOD-TEMFSFEL                           
004080                    MOVE NEJ    TO INDATA-SW                              
004081                 END-IF                                                   
004082                                                                          
004083               ELSE                                                       
004084                 IF CLAG-KVQPACK-1 > ZERO                                 
004085***    KONTROLLERAR ATT KÖPET ÄR EN JÄMN                                  
004086***    MULTIPEL AV Q1                                                     
004087                   DIVIDE WS-PURCHQTY (IX-DC)                             
004088                             BY CLAG-KVQPACK-1                            
004089                             GIVING WS-SLASK                              
004090                             REMAINDER WS-REST                            
004091                 END-IF                                                   
004092                 IF WS-REST > ZERO                                        
004093                   MOVE NEJ  TO INDATA-SW                                 
004094                   MOVE MED-10                                            
004095                             TO MOD-TEMFSFEL                              
004096                   MOVE MFS-ADD-LAES-IN-FAELT-HI                          
004097                             TO MOD-PURCHQTY-ATTR (IX-DC)                 
004098                 END-IF                                                   
004099               END-IF                                                     
004100             ELSE                                                         
004101               MOVE ARTIKEL-SAKNAS                                        
004102                             TO MED-IDMFSFEL                              
004103               CALL WMEDKONV USING MED-WMEDAREA                           
004104               MOVE MED-TEMFSFEL                                          
004105                             TO MOD-TEMFSFEL                              
004106               MOVE MFS-ADD-LAES-IN-FAELT-HI                              
004107                             TO MOD-PURCHQTY-ATTR (IX-DC)                 
004108               MOVE NEJ      TO INDATA-SW                                 
004109             END-IF                                                       
004110           ELSE                                                           
004111             MOVE MFS-ADD-LAES-IN-FAELT                                   
004112                             TO MOD-PURCHQTY-ATTR (IX-DC)                 
004113           END-IF                                                         
004114         ELSE                                                             
004115           MOVE MED-4        TO MOD-TEMFSFEL                              
004116           MOVE MFS-ADD-LAES-IN-FAELT-HI                                  
004117                             TO MOD-PURCHQTY-ATTR (IX-DC)                 
004118           MOVE MID-PURCHQTY (IX-DC)                                      
004119                             TO MOD-PURCHQTY (IX-DC)                      
004120           MOVE NEJ          TO INDATA-SW                                 
004121         END-IF                                                           
004122       END-IF                                                             
004123                                                                          
004124       IF (MID-IDDC-FROM (IX-DC) NOT = ALL '+' AND                        
004125           MID-IDDC-FROM (IX-DC) NOT = SPACE)                             
004126       AND MID-PURCHQTY (IX-DC) NUMERIC                                   
004127       AND MID-PURCHQTY (IX-DC) > ZERO                                    
004128         MOVE MID-IDDC-FROM (IX-DC)     TO WS-IDDC                        
004129         IF NDC-NA                                                        
004130             MOVE 'J'    TO SW-TRANSF                                     
004131             MOVE MID-IDDC-FROM (IX-DC) TO W-IDDC-B6                      
004132             PERFORM IMS-GU-WDB601                                        
004133*** ENDAST NDC BEHANDLAS PÅ DENNA BILD                                    
004134             IF DCS-NDC-NA                                                
004135               PERFORM S02-KONTROLLERA-KDARBTYP                           
004136               IF INDATA-OK                                               
004137                 MOVE MID-IDDC-FROM (IX-DC)                               
004138                                   TO WS-IDDC-SEND                        
004139                                      MOD-IDDC-FROM (IX-DC)               
004140                                      WS-IDDC-FROM (IX-DC)                
004141                                      W-IDDC                              
004142                 MOVE WS-DC-NR (IX-DC)                                    
004143                                   TO WS-IDDC-REC                         
004144                 MOVE WS-KDARBTYP-SEC(1:3) TO WS-KDARBTYP-X3              
004145                                                                          
004146                 IF  WS-IDDC-SEND   = WS-IDDC-REF                         
004147                 AND SW-TRANSF-JA                                         
004148                     MOVE UPD-NOT-ALLOWED                                 
004149                                   TO MED-IDMFSFEL                        
004150                     CALL WMEDKONV                                        
004151                                USING MED-WMEDAREA                        
004152                     MOVE MED-TEMFSFEL                                    
004153                                   TO MOD-TEMFSFEL                        
004154                     MOVE NEJ      TO INDATA-SW                           
004155                     MOVE MFS-ADD-LAES-IN-FAELT-HI                        
004156                                   TO MOD-IDDC-FROM-ATTR (IX-DC)          
004157                 END-IF                                                   
004158*                                                                         
004159                 IF INDATA-OK                                             
004160                  IF WS-IDDC-SEND    = WS-CDC-11                          
004161                   MOVE MED-11     TO MOD-TEMFSFEL                        
004162                   MOVE MFS-ADD-LAES-IN-FAELT-HI                          
004163                                   TO MOD-IDDC-FROM-ATTR (IX-DC)          
004164                   MOVE NEJ        TO INDATA-SW                           
004165                  ELSE                                                    
004166*** KONTROLL OM MAN TRANSFER ÄR TILLÅTET MELLAN DC:N                      
004167                   PERFORM DB2-SELECT-TP4TRAN                             
004168                   IF LINES-FOUND                                         
004169                     MOVE TP4TRAN-IDDISTR TO WS-IDDISTR(IX-DC)            
004170                     MOVE TP4TRAN-IDKUNDNR TO WS-IDKUNDNR(IX-DC)          
004171                     MOVE MFS-NUM-FAELT-RAETT                             
004172                                   TO MOD-IDDC-FROM-ATTR (IX-DC)          
004173                     PERFORM IMS-GU-WDK711-TRANS                          
004174                     MOVE SPACE TO WS-SEND-IDLEVNR                        
004175                     IF SEGMENT-FINNS                                     
004176***moves idlevnr for sending dc, idlevnr from wdb6 read above             
004177                       MOVE DCS-IDLEVNR-DC TO WS-SEND-IDLEVNR             
004178                       COMPUTE WS-TRANSF-BALANCE ROUNDED =                
004179                                           TRANS-SLAG-KVLS                
004180                                         - TRANS-SLAG-KVROS-BULK          
004181                                         - TRANS-SLAG-KVROS-DAG           
004182                                         - TRANS-SLAG-KVOKS-BULK          
004183                                         - TRANS-SLAG-KVOKS-DAG           
004184*** KONTROLL SÅ ATT TOTALA SUMMAN MAN MATAT IN FÖR RESPEKTIVE             
004185*** TDC(TRANSFERERANDE) INTE ÄR STÖRRE ÄN BALANSEN FÖR TDC                
004186                       IF (WS-DC-NR (1) > SPACES                          
004187                       AND MID-IDDC-FROM (IX-DC) = WS-DC-NR (1)           
004188                       AND WS-PURCHQTY-DC1 > WS-TRANSF-BALANCE)           
004189                       OR (WS-DC-NR (2) > SPACES                          
004190                       AND MID-IDDC-FROM (IX-DC) = WS-DC-NR (2)           
004191                       AND WS-PURCHQTY-DC2 > WS-TRANSF-BALANCE)           
004192                       OR (WS-DC-NR (3) > SPACES                          
004193                       AND MID-IDDC-FROM (IX-DC) = WS-DC-NR (3)           
004194                       AND WS-PURCHQTY-DC3 > WS-TRANSF-BALANCE)           
004195                       OR (WS-DC-NR (4) > SPACES                          
004196                       AND MID-IDDC-FROM (IX-DC) = WS-DC-NR (4)           
004197                       AND WS-PURCHQTY-DC4 > WS-TRANSF-BALANCE)           
004198                         MOVE MED-12 TO MOD-TEMFSFEL                      
004199                         MOVE NEJ TO INDATA-SW                            
004200                         MOVE MFS-ADD-LAES-IN-FAELT-HI                    
004201                                     TO MOD-PURCHQTY-ATTR (IX-DC)         
004202                       END-IF                                             
004203                     ELSE                                                 
004204                       MOVE MED-12 TO MOD-TEMFSFEL                        
004205                       MOVE NEJ TO INDATA-SW                              
004206                       MOVE MFS-ADD-LAES-IN-FAELT-HI                      
004207                                     TO MOD-PURCHQTY-ATTR (IX-DC)         
004208                     END-IF                                               
004209                   ELSE                                                   
004210                     MOVE MED-11 TO MOD-TEMFSFEL                          
004211                     MOVE MFS-ADD-LAES-IN-FAELT-HI                        
004212                                   TO MOD-IDDC-FROM-ATTR (IX-DC)          
004213                     MOVE NEJ TO INDATA-SW                                
004214                   END-IF                                                 
004215                  END-IF                                                  
004216                 END-IF                                                   
004217               END-IF                                                     
004218             ELSE                                                         
004219               MOVE MED-11 TO MOD-TEMFSFEL                                
004220               MOVE MFS-ADD-LAES-IN-FAELT-HI                              
004221                             TO MOD-IDDC-FROM-ATTR (IX-DC)                
004222               MOVE NEJ TO INDATA-SW                                      
004223             END-IF                                                       
004224           IF SW-TRANSF-NEJ                                               
004225             MOVE WS-DC-NR (IX-DC) TO W-IDDC-B6                           
004226             PERFORM IMS-GU-WDB601                                        
004227             IF MID-IDDC-FROM (IX-DC) = DCS-IDDC-REF                      
004228               MOVE MFS-NUM-FAELT-RAETT                                   
004229                               TO MOD-IDDC-FROM-ATTR (IX-DC)              
004230             ELSE                                                         
004231               MOVE MED-11   TO MOD-TEMFSFEL                              
004232               MOVE MFS-ADD-LAES-IN-FAELT-HI                              
004233                               TO MOD-IDDC-FROM-ATTR (IX-DC)              
004234               MOVE NEJ TO INDATA-SW                                      
004235             END-IF                                                       
004236           END-IF                                                         
004237         ELSE                                                             
004238           MOVE MED-11       TO MOD-TEMFSFEL                              
004239           MOVE MFS-ADD-LAES-IN-FAELT-HI                                  
004240                           TO MOD-IDDC-FROM-ATTR (IX-DC)                  
004241           MOVE NEJ TO INDATA-SW                                          
004242         END-IF                                                           
004243       ELSE                                                               
004244***    KONTROLL NÄR MAN VILL NOLLA TRANSFERORDER                          
004245         IF (MID-IDDC-FROM (IX-DC) NOT = ALL '+' AND                      
004246             MID-IDDC-FROM (IX-DC) NOT = SPACE)                           
004247         AND MID-PURCHQTY (IX-DC) NUMERIC                                 
004248         AND MID-PURCHQTY (IX-DC) = ZERO                                  
004249           MOVE WS-DC-NR (IX-DC)   TO W-IDDC-301                          
004250                                      WS-IDDC-REC                         
004251           MOVE 'T'                TO W-KDREFTYP                          
004252           MOVE W-IDARTNR          TO W-IDARTNR-301                       
004253           MOVE MID-IDDC-FROM (IX-DC)                                     
004254                                   TO WS-IDDC-SEND                        
004255           MOVE WS-KDARBTYP-SEC(1:3) TO WS-KDARBTYP-X3                    
004256           PERFORM DB2-SELECT-TP4TRAN                                     
004257           IF LINES-FOUND                                                 
004258             MOVE TP4TRAN-IDDISTR  TO W-IDDISTR                           
004259           ELSE                                                           
004260             MOVE MED-11 TO MOD-TEMFSFEL                                  
004261             MOVE MFS-ADD-LAES-IN-FAELT-HI                                
004262                                   TO MOD-IDDC-FROM-ATTR (IX-DC)          
004263             MOVE NEJ              TO INDATA-SW                           
004264           END-IF                                                         
004265           PERFORM IMS-GU-WDE3-ORDL01-TRAN                                
004266                                                                          
004267           IF SEGMENT-FINNS                                               
004268             MOVE 'J'    TO SW-TRANSF                                     
004269           ELSE                                                           
004270             MOVE MED-12 TO MOD-TEMFSFEL                                  
004271             MOVE NEJ TO INDATA-SW                                        
004272             MOVE MFS-ADD-LAES-IN-FAELT-HI                                
004273                         TO MOD-PURCHQTY-ATTR (IX-DC)                     
004274             MOVE MFS-ADD-LAES-IN-FAELT-HI                                
004275                               TO MOD-IDDC-FROM-ATTR (IX-DC)              
004276           END-IF                                                         
004277         END-IF                                                           
004278       END-IF                                                             
004279                                                                          
004280       IF SW-TRANSF-JA                                                    
004281         IF ((MID-IDDC-FROM (1) = ALL '+' OR                              
004282              MID-IDDC-FROM (1) = SPACE)                                  
004283         AND WS-PURCHQTY (1) NOT = ZERO)                                  
004284         OR  ((MID-IDDC-FROM (2) = ALL '+' OR                             
004285              MID-IDDC-FROM (2) = SPACE)                                  
004286         AND WS-PURCHQTY (2) NOT = ZERO)                                  
004287         OR  ((MID-IDDC-FROM (3) = ALL '+' OR                             
004288              MID-IDDC-FROM (3) = SPACE)                                  
004289         AND WS-PURCHQTY (3) NOT = ZERO)                                  
004290         OR  ((MID-IDDC-FROM (4) = ALL '+' OR                             
004291              MID-IDDC-FROM (4) = SPACE)                                  
004292         AND WS-PURCHQTY (4) NOT = ZERO)                                  
004293           MOVE MED-14 TO MOD-TEMFSFEL                                    
004294           MOVE NEJ TO INDATA-SW                                          
004295           MOVE MFS-ADD-LAES-IN-FAELT-HI                                  
004296                             TO MOD-PURCHQTY-ATTR (IX-DC)                 
004297           MOVE MFS-ADD-LAES-IN-FAELT-HI                                  
004298                             TO MOD-IDDC-FROM-ATTR (IX-DC)                
004299         END-IF                                                           
004300       END-IF                                                             
004301                                                                          
004302       MOVE NEJ              TO WDK7-FINNS-SW                             
004303       IF MID-FLREFBEO (IX-DC) = ALL '+'                                  
004304         MOVE MFS-RENSA-FAELT                                             
004305                             TO MOD-FLREFBEO (IX-DC)                      
004306       ELSE                                                               
004307         MOVE MID-FLREFBEO (IX-DC)                                        
004308                             TO MOD-FLREFBEO (IX-DC)                      
004309         MOVE WS-DC-NR (IX-DC)                                            
004310                             TO W-IDDC                                    
004311         PERFORM IMS-GU-WDK7-WDK711                                       
004312         IF SEGMENT-FINNS                                                 
004313           MOVE JA           TO WDK7-FINNS-SW                             
004314           IF SLAG-IDDC-REF > SPACE                                       
004315*----                                                                     
004316*---- EJ LOKAL ARTIKEL                                                    
004317*----                                                                     
004318             IF MID-FLREFBEO (IX-DC) = JA                                 
004319             OR MID-FLREFBEO (IX-DC) = YES                                
004320             OR MID-FLREFBEO (IX-DC) = NEJ                                
004321             OR MID-FLREFBEO (IX-DC) = 'S'                                
004322               MOVE MFS-ADD-LAES-IN-FAELT                                 
004323                             TO MOD-FLREFBEO-ATTR (IX-DC)                 
004324             ELSE                                                         
004325               MOVE MED-5    TO MOD-TEMFSFEL                              
004326               MOVE NEJ      TO INDATA-SW                                 
004327               MOVE MFS-ADD-LAES-IN-FAELT-HI                              
004328                             TO MOD-FLREFBEO-ATTR (IX-DC)                 
004329             END-IF                                                       
004330           ELSE                                                           
004331*----                                                                     
004332*---- LOKAL ARTIKEL                                                       
004333*----                                                                     
004334             IF MID-FLREFBEO (IX-DC) = NEJ                                
004335               MOVE MFS-ADD-LAES-IN-FAELT                                 
004336                             TO MOD-FLREFBEO-ATTR (IX-DC)                 
004337             ELSE                                                         
004338               MOVE MED-5    TO MOD-TEMFSFEL                              
004339               MOVE NEJ      TO INDATA-SW                                 
004340               MOVE MFS-ADD-LAES-IN-FAELT-HI                              
004341                             TO MOD-FLREFBEO-ATTR (IX-DC)                 
004342             END-IF                                                       
004343           END-IF                                                         
004344         END-IF                                                           
004345       END-IF                                                             
004346       IF WDK7-FINNS                                                      
004347         IF MID-FLFLYG (IX-DC)       = ALL '+'                            
004348           IF WS-IDREFTYP = 'B'                                           
004349             IF SLAG-FLFLYG          = 'J'                                
004350               MOVE MED-17          TO MOD-TEMFSFEL                       
004351               MOVE NEJ             TO INDATA-SW                          
004352               MOVE MFS-ROER-EJ-FAELT                                     
004353                                    TO MOD-FLFLYG-ATTR (IX-DC)            
004354             END-IF                                                       
004355           ELSE                                                           
004356             IF WS-IDREFTYP = 'A' or 'C'                                  
004357               IF SLAG-FLFLYG        = 'S'                                
004358                 MOVE MED-20        TO MOD-TEMFSFEL                       
004359                 MOVE NEJ           TO INDATA-SW                          
004360                 MOVE MFS-ROER-EJ-FAELT                                   
004361                                      TO MOD-FLFLYG-ATTR (IX-DC)          
004362*                move 'a' to ws-where                                     
004363*                call fellog                                              
004364               END-IF                                                     
004365             END-IF                                                       
004366           END-IF                                                         
004367         ELSE                                                             
004368           MOVE MID-FLFLYG (IX-DC)  TO MOD-FLFLYG (IX-DC)                 
004369           IF MID-FLFLYG (IX-DC) = 'Y' OR 'J'                             
004370             MOVE MFS-ADD-LAES-IN-FAELT                                   
004371                                    TO MOD-FLFLYG-ATTR (IX-DC)            
004372             MOVE WS-DC-NR (IX-DC)  TO W-IDDC-B6                          
004373             PERFORM IMS-GU-WDB601                                        
004374             IF DCS-NDC-NA                                                
004375             AND (DCS-USA OR DCS-CANADA)                                  
004376               IF WS-IDREFTYP = 'B'                                       
004377               AND MID-PURCHQTY(IX-DC) > ZERO                             
004378                  MOVE NEJ          TO INDATA-SW                          
004379                  MOVE MFS-ROER-EJ-FAELT                                  
004380                                    TO MOD-FLFLYG-ATTR (IX-DC)            
004381               ELSE                                                       
004382                  IF SLAG-IDDC-REF > SPACE                                
004383                    MOVE JA         TO WS-FLAGGA-FCD-UPD (IX-DC)          
004384                    IF MSGI-IDLAND-SPR = 'GB'                             
004385                      MOVE YES      TO MOD-FLFLYG (IX-DC)                 
004386                    ELSE                                                  
004387                      MOVE JA       TO MOD-FLFLYG (IX-DC)                 
004388                    END-IF                                                
004389                  ELSE                                                    
004390                    MOVE MED-15     TO MOD-TEMFSFEL                       
004391                    MOVE NEJ        TO INDATA-SW                          
004392                    MOVE MFS-ADD-LAES-IN-FAELT-HI                         
004393                                    TO MOD-FLFLYG-ATTR (IX-DC)            
004394                  END-IF                                                  
004395               END-IF                                                     
004396             ELSE                                                         
004397               MOVE MED-16          TO MOD-TEMFSFEL                       
004398               MOVE NEJ             TO INDATA-SW                          
004399               MOVE MFS-ADD-LAES-IN-FAELT-HI                              
004400                                    TO MOD-FLFLYG-ATTR (IX-DC)            
004401             END-IF                                                       
004402           ELSE                                                           
004403             IF MID-FLFLYG (IX-DC) = NEJ                                  
004404             OR MID-FLFLYG (IX-DC) = 'S'                                  
004405*                                                                         
004406*              IF MID-FLFLYG (IX-DC) = 'S'                                
004407*                IF WS-IDREFTYP = 'A' OR 'C'                              
004408*                IF MID-PURCHQTY (IX-DC) > ZERO                           
004409*                  MOVE NEJ      TO INDATA-SW                             
004410*                  MOVE MED-20   TO MOD-TEMFSFEL                          
004411*                  MOVE MFS-ADD-LAES-IN-FAELT-HI                          
004412*                                   TO MOD-FLFLYG-ATTR (IX-DC)            
004413*                  move 'b' to ws-where                                   
004414*                  call fellog                                            
004415*                END-IF                                                   
004416*              END-IF                                                     
004417               IF MID-FLFLYG (IX-DC) = NEJ                                
004418                 MOVE NEJ           TO WS-FLAGGA-FCD-UPD (IX-DC)          
004419               ELSE                                                       
004420                 MOVE 'S'           TO WS-FLAGGA-FCD-UPD (IX-DC)          
004421               END-IF                                                     
004422               MOVE WS-FLAGGA-FCD-UPD (IX-DC)                             
004423                                    TO MOD-FLFLYG (IX-DC)                 
004424               MOVE MFS-ADD-LAES-IN-FAELT                                 
004425                                    TO MOD-FLFLYG-ATTR (IX-DC)            
004426             ELSE                                                         
004427               MOVE ERR-CORR-HILITE-FLDS                                  
004428                                    TO MED-IDMFSFEL                       
004429               CALL WMEDKONV     USING MED-WMEDAREA                       
004430               MOVE MED-TEMFSFEL    TO MOD-TEMFSFEL                       
004431               MOVE NEJ             TO INDATA-SW                          
004432               MOVE MFS-ADD-LAES-IN-FAELT-HI                              
004433                                    TO MOD-FLFLYG-ATTR (IX-DC)            
004434             END-IF                                                       
004435           END-IF                                                         
004436         END-IF                                                           
004437         IF  INDATA-OK                                                    
004438         AND NOT MFS-UPDATE                                               
004439            MOVE NEJ                  TO UPD-REQ-SW                       
004440            IF  MID-FLREFBEO (IX-DC)   = ALL '+'                          
004441            AND MID-FLFLYG   (IX-DC)   = ALL '+'                          
004442                CONTINUE                                                  
004443            ELSE                                                          
004444              IF ((MID-FLREFBEO (IX-DC)                                   
004445                                 NOT   = SLAG-FLREFBEO                    
004446              AND MID-FLREFBEO (IX-DC) = NEJ)                             
004447              OR (MID-FLREFBEO (IX-DC) = YES                              
004448                                                                          
004449              AND SLAG-FLREFBEO  NOT   = JA ))                            
004450                MOVE JA               TO UPD-REQ-SW                       
004451              END-IF                                                      
004452              IF ((MID-FLFLYG   (IX-DC)                                   
004453                                 NOT   = SLAG-FLFLYG                      
004454              AND MID-FLFLYG   (IX-DC) = NEJ)                             
004455              OR (MID-FLFLYG   (IX-DC)                                    
004456                                 NOT   = SLAG-FLFLYG                      
004457              AND MID-FLFLYG   (IX-DC) = 'S')                             
004458              OR (MID-FLFLYG   (IX-DC) = YES                              
004459                                                                          
004460              AND SLAG-FLFLYG    NOT   = JA ))                            
004461                MOVE JA               TO UPD-REQ-SW                       
004462              END-IF                                                      
004463              IF UPD-REQ-JA                                               
004464                MOVE INF-PRESS-PF11   TO MED-IDMFSFEL                     
004465                CALL WMEDKONV      USING MED-WMEDAREA                     
004466                MOVE MED-TEMFSFEL     TO MOD-TEMFSFEL                     
004467              END-IF                                                      
004468            END-IF                                                        
004469         END-IF                                                           
004470       END-IF                                                             
004471                                                                          
004472       ADD +1                TO IX-DC                                     
004473     END-PERFORM                                                          
004474                                                                          
004475     IF MID-COMMENT = ALL '+'                                             
004476       MOVE MFS-RENSA-FAELT  TO MOD-COMMENT                               
004477                                MID-COMMENT                               
004478     ELSE                                                                 
004479       MOVE MID-COMMENT      TO MOD-COMMENT                               
004480     END-IF                                                               
004481     MOVE MFS-ADD-LAES-IN-FAELT                                           
004482                             TO MOD-COMMENT-ATTR                          
004483     .                                                                    
004484     EJECT                                                                
004485******************************************************************        
004486*1.GET WEIGHT/VOLUME OF THE PART IN WDK712 OR WDK611                      
004487*2.GET AIR FREIGHT COST FACTOR & AIRCOST FROM WDB616                      
004488******************************************************************        
004489 IA-CHECK-AIR-COST   SECTION.                                             
004490                                                                          
004491     PERFORM S07A-GET-WEIGHT-VOL-WDK712                                   
004492*                                                                         
004493     MOVE ZEROES               TO WS-PRFRAKT                              
004494                                  WS-AIR-COST-SEK                         
004495     MOVE W-IDDC               TO W-IDDC-B6-X                             
004496     MOVE WS-IDDC-REF          TO W-IDDC-B616-X                           
004497     PERFORM S10-GET-AIR-FREIGHT                                          
004498                                                                          
004499     COMPUTE WS-KR-VIKT  ROUNDED                                          
004500                                 = WS-VKART * WS-REAIRCO / 1000           
004501     COMPUTE WS-KR-VOLYM ROUNDED =                                        
004502             WS-VLARTNTO * WS-REAIRCO * 167 / 1000000                     
004503                                                                          
004504     COMPUTE WS-KR-VIKT-RED  ROUNDED = WS-KR-VIKT * 1                     
004505     COMPUTE WS-KR-VOLYM-RED ROUNDED = WS-KR-VOLYM * 1                    
004506                                                                          
004507     IF WS-KR-VIKT-RED > WS-KR-VOLYM-RED                                  
004508        MOVE WS-KR-VIKT-RED    TO WS-AIR-COST-SEK                         
004509     ELSE                                                                 
004510        MOVE WS-KR-VOLYM-RED   TO WS-AIR-COST-SEK                         
004511     END-IF                                                               
004512                                                                          
004513     COMPUTE WS-AIR-COST-SEK =  WS-AIR-COST-SEK *                         
004514                                WS-PURCHQTY (IX-DC)                       
004515                                                                          
004516     IF WS-AIR-COST-SEK > WS-PRFRAKT                                      
004517        MOVE ERR-HIGH-AIR-COST TO MED-IDMFSFEL                            
004518        CALL WMEDKONV          USING MED-WMEDAREA                         
004519        MOVE MED-TEMFSFEL      TO MOD-TEMFSFEL                            
004520        MOVE NEJ               TO INDATA-SW                               
004521     END-IF                                                               
004522     .                                                                    
004523     EJECT                                                                
004524 J-FYLL-I-ANT-REVIEW SECTION.                                             
004525                                                                          
004526     MOVE ZERO               TO WS-ANT-REVIEW                             
004527                                W-IDARTNR-MIN                             
004528     MOVE WS-IDTYPE          TO W-KDREFTYP-MIN                            
004529                                W-KDREFTYP-MAX                            
004530                                                                          
004531     PERFORM IMS-GU-WDE3-ORDW01-MIN-MAX                                   
004532     IF ORDW-SEQA-IDDC NOT = DCS-IDDC                                     
004533        MOVE ORDW-SEQA-IDDC  TO W-IDDC-B6                                 
004534        PERFORM IMS-GU-WDB601                                             
004535     END-IF                                                               
004536     PERFORM S05-CHECK-IDDC                                               
004537                                                                          
004538     PERFORM UNTIL SEGMENT-SAKNAS                                         
004539                                                                          
004540         IF  ORDW-SEQA-KDREFTYP     = WS-IDTYPE                           
004541         AND VALID-IDDC-JA                                                
004542         AND DCS-NDC-NA                                                   
004543         AND ORDW-SEQA-IDPERSON-BUY = WS-IDPERSON-BUY-NUM                 
004544         AND ORDW-SEQA-KDREFORS     = 'P'                                 
004545            ADD +1           TO WS-ANT-REVIEW                             
004546            MOVE ORDW-SEQA-IDARTNR                                        
004547                             TO WS-SPARA-IDARTNR                          
004548                                                                          
004549            PERFORM UNTIL SEGMENT-SAKNAS                                  
004550            OR NOT (WS-SPARA-IDARTNR = ORDW-SEQA-IDARTNR                  
004551            AND VALID-IDDC-JA                                             
004552            AND DCS-NDC-NA)                                               
004553                                                                          
004554               PERFORM IMS-GN-WDE3-ORDW01-MIN-MAX                         
004555               IF ORDW-SEQA-IDDC NOT = DCS-IDDC                           
004556                  MOVE ORDW-SEQA-IDDC  TO W-IDDC-B6                       
004557                  PERFORM IMS-GU-WDB601                                   
004558               END-IF                                                     
004559               PERFORM S05-CHECK-IDDC                                     
004560            END-PERFORM                                                   
004561                                                                          
004562         ELSE                                                             
004563            PERFORM IMS-GN-WDE3-ORDW01-MIN-MAX                            
004564            IF ORDW-SEQA-IDDC NOT = DCS-IDDC                              
004565               MOVE ORDW-SEQA-IDDC  TO W-IDDC-B6                          
004566               PERFORM IMS-GU-WDB601                                      
004567            END-IF                                                        
004568            PERFORM S05-CHECK-IDDC                                        
004569         END-IF                                                           
004570                                                                          
004571     END-PERFORM                                                          
004572     MOVE WS-ANT-REVIEW      TO MOD-ANT-REVIEW                            
004573     .                                                                    
004574     EJECT                                                                
004575 K-FYLL-I-NYCKEL-FAELT SECTION.                                           
004576                                                                          
004577     MOVE JA                 TO SW-TRAEFF                                 
004578     MOVE ORDW-SEQA-IDARTNR  TO WS-SPARA-IDARTNR                          
004579                                W-IDARTNR                                 
004580     MOVE ORDW-SEQA-IDARTNR  TO MOD-IDARTNR-UT                            
004581     MOVE ORDW-SEQA-IDPERSON-BUY                                          
004582                             TO WS-IDPERSON-BUY-RED                       
004583     MOVE WS-IDPERSON-BUY-RED                                             
004584                             TO MOD-IDPERSON-BUY-UT                       
004585     IF ORDW-SEQA-KDREFTYP = 'A'                                          
004586        MOVE 'AIR'           TO MOD-IDTYPE-UT                             
004587                                WS-IDTYPE                                 
004588     END-IF                                                               
004589     IF ORDW-SEQA-KDREFTYP = 'C'                                          
004590        MOVE 'AIRCR'         TO MOD-IDTYPE-UT                             
004591        MOVE 'C'             TO WS-IDTYPE                                 
004592     END-IF                                                               
004593     IF ORDW-SEQA-KDREFTYP = 'B'                                          
004594        MOVE 'BOAT'          TO MOD-IDTYPE-UT                             
004595                                WS-IDTYPE                                 
004596     END-IF                                                               
004597     IF ORDW-SEQA-KDREFTYP = 'L'                                          
004598        MOVE 'LOCAL'         TO MOD-IDTYPE-UT                             
004599                                WS-IDTYPE                                 
004600     END-IF                                                               
004601     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
004602     .                                                                    
004603     EJECT                                                                
004604 L-KOLLA-INPUT-NYCKLAR SECTION.                                           
004605                                                                          
004606     IF NOT (MID-IDARTNR-IN       = ALL '+'                               
004607        AND  MID-IDDC-IN          = ALL '+'                               
004608        AND  MID-IDTYPE-IN        = ALL '+'                               
004609        AND  MID-IDPERSON-BUY-IN  = ALL '+'                               
004610        AND  MID-IDSTATUS-IN      = ALL '+')                              
004611                                                                          
004612       MOVE NEJ              TO INDATA-SW                                 
004613     END-IF                                                               
004614     .                                                                    
004615     EJECT                                                                
004616 M-UPD-MOD-FAELT SECTION.                                                 
004617                                                                          
004618     MOVE REF-KDREFTXT       TO WS-REF-KDREFTXT (IX-DC)                   
004619     IF REF-KDREFORS = 'P'                                                
004620        MOVE 'PROPOSAL NOT REVIEWED'                                      
004621                             TO MOD-ORDERSTATUS                           
004622        MOVE 'NOT REVIEWED'                                               
004623                             TO MOD-IDSTATUS-UT                           
004624        MOVE 'N'             TO WS-STATUS                                 
004625     ELSE                                                                 
004626        MOVE 'REVIEWED'      TO MOD-ORDERSTATUS                           
004627     END-IF                                                               
004628                                                                          
004629     IF REF-KDREFTYP = 'B'                                                
004630       MOVE 'BOAT'           TO MOD-IDREFTYP-UT                           
004631     END-IF                                                               
004632     IF REF-KDREFTYP = 'A'                                                
004633       MOVE 'AIR'            TO MOD-IDREFTYP-UT                           
004634     END-IF                                                               
004635     IF REF-KDREFTYP = 'C'                                                
004636       MOVE 'AIRCR'          TO MOD-IDREFTYP-UT                           
004637     END-IF                                                               
004638     IF REF-KDREFTYP = 'L'                                                
004639       MOVE 'LOCAL'          TO MOD-IDREFTYP-UT                           
004640     END-IF                                                               
004641     IF REF-KDREFTYP = 'T'                                                
004642       PERFORM DB2-SELECT-TP4TRAN-2                                       
004643       IF LINES-FOUND                                                     
004644         MOVE TP4TRAN-IDDC-SEND                                           
004645                             TO MOD-IDDC-FROM (IX-DC)                     
004646                                WS-IDDC-FROM (IX-DC)                      
004647       END-IF                                                             
004648     END-IF                                                               
004649                                                                          
004650     MOVE REF-KVBEART        TO WS-RED-PURCHQTY                           
004651                                WS-PURCHQTY (IX-DC)                       
004652     MOVE WS-RED-PURCHQTY    TO MOD-PURCHQTY (IX-DC)                      
004653     MOVE MFS-ADD-LAES-IN-FAELT                                           
004654                             TO MOD-PURCHQTY-ATTR (IX-DC)                 
004655     .                                                                    
004656     EJECT                                                                
004657 N-UPD-WDE3-WDK7 SECTION.                                                 
004658                                                                          
004659     MOVE +1                 TO IX-DC                                     
004660     PERFORM UNTIL IX-DC > IX-DC-MAX                                      
004661                                                                          
004662       IF SW-TRANSF-JA                                                    
004663         MOVE 'T'            TO   W-KDREFTYP-MIN                          
004664                                  W-KDREFTYP-MAX                          
004665       ELSE                                                               
004666         MOVE WS-IDREFTYP    TO   W-KDREFTYP-MIN                          
004667                                  W-KDREFTYP-MAX                          
004668       END-IF                                                             
004669                                                                          
004670       MOVE W-IDARTNR        TO W-IDARTNR-MIN                             
004671                                W-IDARTNR-MAX                             
004672                                                                          
004673       PERFORM IMS-GU-WDE3-ORDW01-MIN-MAX                                 
004674                                                                          
004675       PERFORM UNTIL SEGMENT-SAKNAS                                       
004676       OR (ORDW-SEQA-IDPERSON-BUY = W-IDPERSON-BUY-MIN                    
004677       AND ORDW-SEQA-KDREFTYP     = W-KDREFTYP-MIN                        
004678       AND ORDW-SEQA-IDARTNR      = W-IDARTNR-MIN                         
004679       AND ORDW-SEQA-IDDC         = WS-DC-NR (IX-DC))                     
004680                                                                          
004681           PERFORM IMS-GN-WDE3-ORDW01-MIN-MAX                             
004682       END-PERFORM                                                        
004683                                                                          
004684       IF SEGMENT-FINNS                                                   
004685                                                                          
004686         MOVE ORDW-SEQA-IDWDE301                                          
004687                             TO W-WDE301KY-X                              
004688         PERFORM IMS-GHU-WDE3-ORDL01                                      
004689                                                                          
004690         MOVE WS-DC-NR (IX-DC)                                            
004691                             TO W-IDDC                                    
004692         PERFORM IMS-GHU-WDK7-WDK711                                      
004693         IF REF-KDREFORS = 'O'                                            
004694           IF (MID-IDDC-FROM (IX-DC) NOT = ALL '+' AND                    
004695               MID-IDDC-FROM (IX-DC) NOT = SPACE)                         
004696             SUBTRACT REF-KVBEART                                         
004697                               FROM SLAG-KVBEART                          
004698             PERFORM NB-EV-CROSS-DOCKING                                  
004699           ELSE                                                           
004700             IF SW-TRANSF-NEJ                                             
004701               SUBTRACT REF-KVBEART                                       
004702                                 FROM SLAG-KVBEART                        
004703               PERFORM NB-EV-CROSS-DOCKING                                
004704             END-IF                                                       
004705           END-IF                                                         
004706         END-IF                                                           
004707         IF WS-PURCHQTY (IX-DC) > ZERO                                    
004708           ADD WS-PURCHQTY (IX-DC)                                        
004709                             TO SLAG-KVBEART                              
004710           MOVE DAGENS-DATUM TO SLAG-TIORDREG                             
004711         END-IF                                                           
004712                                                                          
004713         IF SW-TRANSF-JA                                                  
004714           MOVE 'T'          TO REF-KDREFTYP                              
004715         ELSE                                                             
004716           MOVE WS-IDREFTYP  TO REF-KDREFTYP                              
004717         END-IF                                                           
004718         IF WS-PURCHQTY (IX-DC) > ZERO                                    
004719           MOVE WS-PURCHQTY (IX-DC)                                       
004720                             TO REF-KVBEART                               
004721           MOVE 'O '         TO REF-KDREFORS                              
004722           IF SW-TRANSF-JA                                                
004723             MOVE WS-SEND-IDLEVNR TO REF-IDLEVNR                          
004724           ELSE                                                           
004725             MOVE SLAG-IDLEVNR TO REF-IDLEVNR                             
004726           END-IF                                                         
004727*          MOVE SLAG-IDLEVNR TO REF-IDLEVNR                               
004728                                                                          
004729           PERFORM NA-EV-CROSS-DOCKING                                    
004730           PERFORM IMS-REPL-WDE3-ORDL01                                   
004731         ELSE                                                             
004732           IF SW-TRANSF-JA                                                
004733             IF (MID-IDDC-FROM (IX-DC) NOT = ALL '+' AND                  
004734                 MID-IDDC-FROM (IX-DC) NOT = SPACE)                       
004735               PERFORM IMS-DLET-WDE3-ORDL01                               
004736*     --   OM MAN NOLLAT KÖPFÖRSLAGET SÄTTS FLREFNYO TILL JA              
004737               IF SLAG-IDDC-REF > SPACE                                   
004738                 MOVE JA    TO SLAG-FLREFNYO                              
004739               END-IF                                                     
004740             END-IF                                                       
004741           ELSE                                                           
004742             PERFORM IMS-DLET-WDE3-ORDL01                                 
004743*   --     OM MAN NOLLAT KÖPFÖRSLAGET SÄTTS FLREFNYO TILL JA              
004744             IF SLAG-IDDC-REF > SPACE                                     
004745                 MOVE JA    TO SLAG-FLREFNYO                              
004746             END-IF                                                       
004747           END-IF                                                         
004748         END-IF                                                           
004749         PERFORM IMS-REPL-WDK7-WDK711                                     
004750       ELSE                                                               
004751         IF WS-PURCHQTY (IX-DC) > ZERO                                    
004752                                                                          
004753           MOVE WS-IDREFTYP  TO REF-KDREFTYP                              
004754           MOVE WS-DC-NR (IX-DC)                                          
004755                             TO REF-IDDC                                  
004756                                W-IDDC                                    
004757           MOVE W-IDARTNR    TO REF-IDARTNR                               
004758           PERFORM IMS-GHU-WDK7-WDK711                                    
004759           ADD WS-PURCHQTY (IX-DC)                                        
004760                             TO SLAG-KVBEART                              
004761           MOVE DAGENS-DATUM TO SLAG-TIORDREG                             
004762           PERFORM IMS-REPL-WDK7-WDK711                                   
004763           MOVE SLAG-ADART   TO REF-ADART-SDC                             
004764           MOVE CLAG-ADART   TO REF-ADART-CDC                             
004765           MOVE WS-IDDISTR (IX-DC)                                        
004766                             TO REF-IDDISTR                               
004767           MOVE WS-IDDISTR-2-DEL (IX-DC)                                  
004768                             TO W-IDDISTR-DEL                             
004769           MOVE WS-PURCHQTY (IX-DC)                                       
004770                             TO REF-KVBEART                               
004771           MOVE 'O'          TO REF-KDREFORS                              
004772           MOVE SLAG-IDLEVNR TO REF-IDLEVNR                               
004773           MOVE WS-IDPERSON-BUY-NUM                                       
004774                             TO REF-IDPERSON-BUY                          
004775           MOVE ZERO         TO REF-IDKUNDNR                              
004776                                REF-KDREFTXT                              
004777                                REF-KDFRAKT                               
004778                                REF-KVBEART-CD                            
004779                                REF-ADLAGOMR-CD                           
004780                                REF-ADGANG-CD                             
004781                                REF-ADPLATS-CD                            
004782           IF SW-TRANSF-JA                                                
004783           AND (MID-IDDC-FROM (IX-DC) NOT = ALL '+' AND                   
004784                MID-IDDC-FROM (IX-DC) NOT = SPACE)                        
004785             MOVE WS-IDDISTR (IX-DC)                                      
004786                             TO REF-IDDISTR                               
004787             MOVE WS-IDKUNDNR (IX-DC)                                     
004788                             TO REF-IDKUNDNR                              
004789             MOVE 'T'        TO REF-KDREFTYP                              
004790             MOVE WS-SEND-IDLEVNR TO REF-IDLEVNR                          
004791             MOVE MID-IDDC-FROM (IX-DC)                                   
004792                             TO REF-IDDC-REF                              
004793           ELSE                                                           
004794             MOVE WS-IDDISTR (IX-DC)                                      
004795                             TO REF-IDDISTR                               
004796             MOVE ZERO       TO REF-IDKUNDNR                              
004797             MOVE SLAG-IDDC-REF                                           
004798                             TO REF-IDDC-REF                              
004799           END-IF                                                         
004800           PERFORM NA-EV-CROSS-DOCKING                                    
004801           PERFORM IMS-ISRT-WDE3-ORDL01                                   
004802           MOVE MFS-RENSA-FAELT  TO MOD-IDDC-FROM(IX-DC)                  
004803           IF SW-TRANSF-JA                                                
004804           AND (MID-IDDC-FROM (IX-DC) NOT = ALL '+' AND                   
004805                MID-IDDC-FROM (IX-DC) NOT = SPACE)                        
004806             MOVE W-IDARTNR           TO W-IDARTNR-301-DEL                
004807             MOVE WS-IDPERSON-BUY-NUM                                     
004808                                      TO W-IDPERSON-BUY-DEL               
004809             MOVE WS-DC-NR (IX-DC)    TO W-IDDC-301-DEL                   
004810             PERFORM IMS-GHU-WDE3-ORDL01-DEL                              
004811             IF SEGMENT-FINNS                                             
004812               IF REF-KDREFORS = 'P'                                      
004813                 PERFORM IMS-DLET-WDE3-ORDL01                             
004814               END-IF                                                     
004815             END-IF                                                       
004816           END-IF                                                         
004817         END-IF                                                           
004818       END-IF                                                             
004819       ADD +1                TO IX-DC                                     
004820     END-PERFORM                                                          
004821     MOVE 'REVIEWED'         TO MOD-ORDERSTATUS                           
004822     .                                                                    
004823     EJECT                                                                
004824                                                                          
004825 NA-EV-CROSS-DOCKING  SECTION.                                            
004826                                                                          
004827     IF SLAG-ADLAGOMR-CD > ZERO                                           
004828                                                                          
004829       MOVE 1                TO IX-CD                                     
004830       PERFORM UNTIL IX-CD > 4                                            
004831       OR SLAG-ADLAGOMR-CD = CLAG-ADLAGOMR-CD (IX-CD)                     
004832         ADD 1               TO IX-CD                                     
004833       END-PERFORM                                                        
004834                                                                          
004835       IF IX-CD > 4                                                       
004836*    SKA INTE KUNNA INTRÄFFA                                              
004837         CONTINUE                                                         
004838       ELSE                                                               
004839                                                                          
004840         IF (CLAG-KVLS-CD (IX-CD) - CLAG-KVRESS-CD (IX-CD))               
004841                             < CLAG-KVQPACK-3                             
004842           CONTINUE                                                       
004843         ELSE                                                             
004844           DIVIDE REF-KVBEART BY CLAG-KVQPACK-3                           
004845                                 GIVING WS-HELTAL-BEST                    
004846           COMPUTE WS-SALDO =                                             
004847                   CLAG-KVLS-CD (IX-CD) - CLAG-KVRESS-CD (IX-CD)          
004848           DIVIDE WS-SALDO       BY CLAG-KVQPACK-3                        
004849                                 GIVING WS-HELTAL-SALDO                   
004850           IF WS-HELTAL-BEST > WS-HELTAL-SALDO                            
004851             COMPUTE REF-KVBEART-CD =                                     
004852                             WS-HELTAL-SALDO * CLAG-KVQPACK-3             
004853           ELSE                                                           
004854             COMPUTE REF-KVBEART-CD =                                     
004855                             WS-HELTAL-BEST * CLAG-KVQPACK-3              
004856           END-IF                                                         
004857           MOVE CLAG-ADLAGOMR-CD (IX-CD)                                  
004858                             TO REF-ADLAGOMR-CD                           
004859           MOVE CLAG-ADGANG-CD (IX-CD)                                    
004860                             TO REF-ADGANG-CD                             
004861           MOVE CLAG-ADPLATS-CD (IX-CD)                                   
004862                             TO REF-ADPLATS-CD                            
004863                                                                          
004864           PERFORM IMS-GHU-WDK611                                         
004865           COMPUTE CLAG-KVRESS-CD (IX-CD) =                               
004866                   CLAG-KVRESS-CD (IX-CD) + REF-KVBEART-CD                
004867           PERFORM IMS-REPL-WDK6                                          
004868         END-IF                                                           
004869       END-IF                                                             
004870     END-IF                                                               
004871     .                                                                    
004872     EJECT                                                                
004873                                                                          
004874                                                                          
004875 NB-EV-CROSS-DOCKING  SECTION.                                            
004876                                                                          
004877     IF SLAG-ADLAGOMR-CD > ZERO                                           
004878                                                                          
004879       MOVE 1                TO IX-CD                                     
004880       PERFORM UNTIL IX-CD > 4                                            
004881       OR SLAG-ADLAGOMR-CD = CLAG-ADLAGOMR-CD (IX-CD)                     
004882         ADD 1               TO IX-CD                                     
004883       END-PERFORM                                                        
004884                                                                          
004885       IF IX-CD > 4                                                       
004886*    SKA INTE KUNNA INTRÄFFA                                              
004887         CONTINUE                                                         
004888       ELSE                                                               
004889                                                                          
004890         PERFORM IMS-GHU-WDK611                                           
004891         COMPUTE CLAG-KVRESS-CD (IX-CD) =                                 
004892                 CLAG-KVRESS-CD (IX-CD) - REF-KVBEART-CD                  
004893         PERFORM IMS-REPL-WDK6                                            
004894       END-IF                                                             
004895     END-IF                                                               
004896     .                                                                    
004897     EJECT                                                                
004898                                                                          
004899                                                                          
004900 S01-LAES-WDA5-ENTER SECTION.                                             
004901                                                                          
004902                                                                          
004903     MOVE LOW-VALUE          TO W-WDA5A1KY-MIN                            
004904     MOVE HIGH-VALUE         TO W-WDA5A1KY-MAX                            
004905                                                                          
004906     MOVE W-IDARTNR          TO W-IDARTNR-N3-MIN                          
004907                                W-IDARTNR-N3-MAX                          
004908     PERFORM IMS-GU-WDA5-ORDQ01                                           
004909                                                                          
004910     MOVE ZERO               TO WS-RESTKVANT (1)                          
004911                                WS-RESTKVANT (2)                          
004912                                WS-RESTKVANT (3)                          
004913                                WS-RESTKVANT (4)                          
004914     PERFORM UNTIL SEGMENT-SAKNAS                                         
004915                                                                          
004916        MOVE SEQA-IDDISTR    TO  W-IDDISTR-N2                             
004917        MOVE SEQA-IDKUNDNR   TO  W-IDKUNDNR-N2                            
004918        MOVE SEQA-IDKUNDRF   TO  W-IDKUNDRF-N2                            
004919        MOVE SEQA-IDARTNR    TO  W-IDARTNR-N2                             
004920        MOVE SEQA-IDLOPNR    TO  W-IDLOPNR-N2                             
004921                                                                          
004922        MOVE ZERO            TO IX-DC                                     
004923        MOVE +1              TO IX-DC-N                                   
004924        PERFORM UNTIL IX-DC-N > IX-DC-MAX                                 
004925          IF SEQA-IDDISTR     = WS-IDDISTR-B6 (IX-DC-N)                   
004926             MOVE IX-DC-N    TO IX-DC                                     
004927          END-IF                                                          
004928          ADD +1             TO IX-DC-N                                   
004929        END-PERFORM                                                       
004930                                                                          
004931        IF IX-DC > ZERO                                                   
004932          PERFORM IMS-GU-WDA5-ORDP01                                      
004933          IF SEGMENT-FINNS                                                
004934          AND RAD-KDSTARAD = '2'                                          
004935*  SUMMERA RESTORDERKVANTITET                                             
004936             ADD RAD-KVART   TO WS-RESTKVANT (IX-DC)                      
004937          END-IF                                                          
004938        END-IF                                                            
004939                                                                          
004940        PERFORM IMS-GN-WDA5-ORDQ01                                        
004941                                                                          
004942     END-PERFORM                                                          
004943     .                                                                    
004944     EJECT                                                                
004945 S1-SECURITY-CHECK-PARTNO SECTION.                                        
004946     SKIP2                                                                
004947*    --- CHECK IF USER IS GRANTED TO SEE PART-INFO                        
004948     PERFORM IMS-GU-WDK6-ARTC01                                           
004949     IF  SEGMENT-FINNS                                                    
004950       MOVE ART-IDLEVNR          TO WS-IDLEVNR-8                          
004951       IF MSGI-KDARBTYP-SEC-IDLEV = WS-IDLEVNR-8                          
004952       OR MSGI-KDARBTYP-SEC-IDLEV = SPACE OR LOW-VALUE                    
004953*        --- USER GRANTED                                                 
004954         SET PASSED-SECURITY-CHECK TO TRUE                                
004955       ELSE                                                               
004956         SET BLOCKED-SECURITY-CHECK TO TRUE                               
004957       END-IF                                                             
004958     END-IF                                                               
004959     .                                                                    
004960     EJECT                                                                
004961                                                                          
004962 S02-KONTROLLERA-KDARBTYP SECTION.                                        
004963                                                                          
004964     IF WS-KDARBTYP-SEC(1:3) = 'FEL'                                      
004965       MOVE MED-13              TO MOD-TEMFSINF                           
004966       MOVE NEJ                 TO INDATA-SW                              
004967     END-IF                                                               
004968     .                                                                    
004969     EJECT                                                                
004970 S03-GET-BESPRIS  SECTION.                                                
004971                                                                          
004972     MOVE W-IDDC                TO WS-IDDC                                
004973     IF NDC-NA                                                            
004974        PERFORM S08-SEARCH-IDLAND                                         
004975                                                                          
004976        PERFORM IMS-GU-WDK712                                             
004977        IF SEGMENT-FINNS                                                  
004978           MOVE LART-PRMATRL    TO WS-PRARTBES-PR                         
004979                                   WS-PRMATRL                             
004980        ELSE                                                              
004981           MOVE ZERO            TO WS-PRARTBES-PR                         
004982                                   WS-PRMATRL                             
004983        END-IF                                                            
004984     END-IF                                                               
004985     .                                                                    
004986     EJECT                                                                
004987 S04-GET-DCGROUP-ALL-DC SECTION.                                          
004988                                                                          
004989     PERFORM DB2-DCL-OPN-TP5IDDC-CRS                                      
004990     PERFORM DB2-FETCH-TP5IDDC-CRS                                        
004991     IF LINES-FOUND                                                       
004992        MOVE +1              TO IX-DC                                     
004993        PERFORM UNTIL IX-DC > IX-DC-MAX OR LINES-MISSING                  
004994          MOVE TP5IDDC-IDDC  TO WS-DC-NR (IX-DC)                          
004995          PERFORM DB2-FETCH-TP5IDDC-CRS                                   
004996          MOVE IX-DC         TO IX-DC-N                                   
004997          ADD +1             TO IX-DC                                     
004998        END-PERFORM                                                       
004999        MOVE IX-DC-N         TO IX-DC-MAX                                 
005000     ELSE                                                                 
005001        MOVE NEJ             TO NYCKLAR-SW                                
005002     END-IF                                                               
005003     PERFORM DB2-CLOSE-TP5IDDC-CRS                                        
005004     .                                                                    
005005     EJECT                                                                
005006 S05-CHECK-IDDC SECTION.                                                  
005007                                                                          
005008     MOVE NEJ                TO VALID-IDDC-SW                             
005009     MOVE +1                 TO IX-DC                                     
005010     PERFORM UNTIL IX-DC     >  IX-DC-MAX                                 
005011       IF DCS-IDDC           =  WS-DC-NR (IX-DC)                          
005012          MOVE JA            TO VALID-IDDC-SW                             
005013          MOVE IX-DC-MAX     TO IX-DC                                     
005014       END-IF                                                             
005015       ADD +1                TO IX-DC                                     
005016     END-PERFORM                                                          
005017     .                                                                    
005018     EJECT                                                                
005019 S06-GET-REFILL-DISTRIKT SECTION.                                         
005020                                                                          
005021     PERFORM IMS-GU-WDB601                                                
005022     MOVE DCS-IDDISTR-REFILL                                              
005023                             TO WS-IDDISTR       (IX-DC)                  
005024                                WS-IDDISTR-B6    (IX-DC)                  
005025                                WS-IDDISTR-2-DEL (IX-DC)                  
005026     PERFORM IMS-GU-WDK7-WDK711                                           
005027     IF SEGMENT-FINNS                                                     
005028        MOVE SLAG-IDDC-REF   TO WS-SENDING-DC-NR (IX-DC)                  
005029        IF SLAG-IDDC-REF = WC-CDC-SE OR WC-CDC-TR                         
005030           CONTINUE                                                       
005031        ELSE                                                              
005032           IF SLAG-IDDC-REF NOT = SPACE                                   
005033              MOVE SLAG-IDDC-REF                                          
005034                               TO W-IDDC-B616                             
005035              PERFORM IMS-GU-WDB616                                       
005036              IF SEGMENT-FINNS                                            
005037                 MOVE B616-REF-IDDISTR-REFILL                             
005038                               TO WS-IDDISTR       (IX-DC)                
005039                                  WS-IDDISTR-B6    (IX-DC)                
005040                                  WS-IDDISTR-2-DEL (IX-DC)                
005041              ELSE                                                        
005042                 MOVE NEJ      TO NYCKLAR-SW                              
005043                 MOVE MED-18   TO MOD-TEMFSINF                            
005044              END-IF                                                      
005045           END-IF                                                         
005046        END-IF                                                            
005047     END-IF                                                               
005048     .                                                                    
005049 S07-GET-WDK712-LOCAL-INFO SECTION.                                       
005050                                                                          
005051     MOVE SPACES                TO WS-IDDC                                
005052     MOVE CLAG-KVQPACK-3        TO WS-KVQPACK-3                           
005053*                                                                         
005054     MOVE MSGI-IDDC-KEY         TO W-IDDC                                 
005055     PERFORM IMS-GU-WDK7-WDK711                                           
005056     IF SEGMENT-FINNS                                                     
005057        IF SLAG-IDDC-REF = SPACE                                          
005058          MOVE SLAG-IDDC        TO WS-IDDC                                
005059        ELSE                                                              
005060          MOVE SLAG-IDDC-REF    TO WS-IDDC                                
005061        END-IF                                                            
005062        PERFORM S08-SEARCH-IDLAND                                         
005063                                                                          
005064        PERFORM IMS-GU-WDK712                                             
005065        IF SEGMENT-FINNS                                                  
005066           IF LART-KVQPACK-3      > ZERO                                  
005067              MOVE LART-KVQPACK-3                                         
005068                                 TO WS-KVQPACK-3                          
005069           END-IF                                                         
005070           IF LART-VKART          > ZERO                                  
005071              MOVE LART-VKART    TO WS-VKART                              
005072           END-IF                                                         
005073           IF LART-VLARTNTO       > ZERO                                  
005074              MOVE LART-VLARTNTO TO WS-VLARTNTO                           
005075           END-IF                                                         
005076        END-IF                                                            
005077     END-IF                                                               
005078     .                                                                    
005079     EJECT                                                                
005080                                                                          
005081 S07A-GET-WEIGHT-VOL-WDK712 SECTION.                                      
005082                                                                          
005083     MOVE CLAG-VKART         TO WS-VKART                                  
005084     MOVE CLAG-VLARTNTO      TO WS-VLARTNTO                               
005085                                                                          
005086     IF SLAG-IDDC-REF = SPACE                                             
005087       MOVE SLAG-IDDC        TO WS-IDDC                                   
005088     ELSE                                                                 
005089       MOVE SLAG-IDDC-REF    TO WS-IDDC                                   
005090     END-IF                                                               
005091     PERFORM S08-SEARCH-IDLAND                                            
005092                                                                          
005093     PERFORM IMS-GU-WDK712                                                
005094     IF SEGMENT-FINNS                                                     
005095        IF LART-VKART      > ZERO                                         
005096           MOVE LART-VKART    TO WS-VKART                                 
005097        END-IF                                                            
005098        IF LART-VLARTNTO   > ZERO                                         
005099           MOVE LART-VLARTNTO TO WS-VLARTNTO                              
005100        END-IF                                                            
005101     END-IF                                                               
005102     .                                                                    
005103     EJECT                                                                
005104                                                                          
005105 S08-SEARCH-IDLAND SECTION.                                               
005106                                                                          
005107     SEARCH ALL DC-LAND                                                   
005108       AT END                                                             
005109         MOVE SPACE          TO W-IDLAND                                  
005110       WHEN DCLAND-IDDC (DCLAND-IX) = WS-IDDC                             
005111         MOVE DCLAND-IDLANDX2(DCLAND-IX) TO W-IDLAND                      
005112     END-SEARCH                                                           
005113     .                                                                    
005114     EJECT                                                                
005115                                                                          
005116 S09-GET-WDK711-SEND-DC SECTION.                                          
005117                                                                          
005118     MOVE ZEROES                        TO WS-KVDISP-SEND-DC              
005119                                                                          
005120     IF WS-IDDC-REF-TEST > SPACES                                         
005121        MOVE WS-IDDC-REF-TEST           TO  W-IDDC                        
005122        PERFORM IMS-GU-WDK7-WDK711                                        
005123        IF SEGMENT-FINNS                                                  
005124           COMPUTE WS-KVDISP-SEND-DC  = SLAG-KVLS         +               
005125                                        SLAG-KVBEART      +               
005126                                        SLAG-KVAKS-SDC    +               
005127                                        SLAG-KVAKS-PAV    -               
005128                                        SLAG-KVOKS-DAG    -               
005129                                        SLAG-KVOKS-BULK   -               
005130                                        SLAG-KVROS-DAG    -               
005131                                        SLAG-KVROS-BULK   -               
005132                                        SLAG-KVSPARR-KVAL -               
005133                                        SLAG-KVRESS                       
005134                                                                          
005135        END-IF                                                            
005136     END-IF                                                               
005137     .                                                                    
005138     EJECT                                                                
005139 S10-GET-AIR-FREIGHT SECTION.                                             
005140                                                                          
005141     PERFORM IMS-GU-WDB616                                                
005142     IF SEGMENT-FINNS                                                     
005143        MOVE B616-REF-REAIRCO      TO WS-REAIRCO                          
005144        MOVE B616-REF-PRFRAKT      TO WS-PRFRAKT                          
005145     END-IF                                                               
005146     .                                                                    
005147     EJECT                                                                
005148                                                                          
005149 S90-CALL-W271REFL SECTION.                                               
005150                                                                          
005151     INITIALIZE  REFL-W271REFL                                            
005152                                                                          
005153     MOVE ZERO                TO REFL-NDC-KVDAGAR-TBT-DC                  
005154     MOVE W-IDDC              TO REFL-IDDC                                
005155     MOVE W-IDARTNR           TO REFL-IDARTNR                             
005156     MOVE SLAG-IDREFTAB       TO REFL-IDREFTAB                            
005157     MOVE SLAG-FLWILSON       TO REFL-FLWILSON                            
005158     MOVE WS-PRARTBES-PR      TO REFL-PRARTBES                            
005159     MOVE 1 TO IX                                                         
005160     PERFORM UNTIL IX > 12                                                
005161         MOVE SLAG-RESEASON(IX)                                           
005162                              TO REFL-RESEASON(IX)                        
005163         ADD 1                TO IX                                       
005164     END-PERFORM                                                          
005165                                                                          
005166     MOVE SLAG-IDLEVNR        TO REFL-IN-IDLEVNR-DC                       
005167     MOVE SLAG-IDDC-REF       TO REFL-IDDC-REF                            
005168     MOVE SLAG-FLFLYG         TO REFL-FLFLYG                              
005169                                                                          
005170     MOVE SLAG-TIREFPKT       TO TMP1-YYMMDD                              
005171     MOVE DAGENS-DATUM        TO TMP2-YYMMDD                              
005172     PERFORM WY2000P1                                                     
005173     IF TMP1-YYMMDD >= TMP2-YYMMDD                                        
005174       MOVE SLAG-KVREFPKT     TO REFL-IN-KVREFPKT                         
005175     ELSE                                                                 
005176       MOVE ZERO              TO REFL-IN-KVREFPKT                         
005177     END-IF                                                               
005178                                                                          
005179     MOVE SLAG-TIREFPAF       TO TMP1-YYMMDD                              
005180     MOVE DAGENS-DATUM        TO TMP2-YYMMDD                              
005181     PERFORM WY2000P1                                                     
005182     IF TMP1-YYMMDD >= TMP2-YYMMDD                                        
005183       MOVE SLAG-KVREFBER     TO REFL-IN-KVREFBER                         
005184     ELSE                                                                 
005185       MOVE ZERO              TO REFL-IN-KVREFBER                         
005186     END-IF                                                               
005187                                                                          
005188     IF SLAG-IDDC-REF > SPACE                                             
005189        MOVE ZERO             TO REFL-NDC-KVDAGAR-TBT-DC                  
005190     ELSE                                                                 
005191        IF SLAG-KVDAGAR-MANLT  > ZERO                                     
005192          MOVE SLAG-KVDAGAR-MANLT                                         
005193                              TO REFL-NDC-KVDAGAR-TBT-DC                  
005194        ELSE                                                              
005195          MOVE SLAG-IDLEVNR   TO W-IDLEVNR                                
005196          PERFORM IMS-GU-LEVA16                                           
005197          IF SEGMENT-FINNS                                                
005198             MOVE NDC-KVDAGAR-TBT                                         
005199                              TO REFL-NDC-KVDAGAR-TBT-DC                  
005200          ELSE                                                            
005201             MOVE 1           TO REFL-NDC-KVDAGAR-TBT-DC                  
005202          END-IF                                                          
005203        END-IF                                                            
005204     END-IF                                                               
005205*                                                                         
005206***  BELOW CHECK ONLY IF SIMUNATION IS REQUESTED AND NOT FOR UPD          
005207*                                                                         
005208     IF WS-FLSIM = JA                                                     
005209        MOVE JA                  TO REFL-IN-FLSIM                         
005210        MOVE WS-KVPB-REF (IX-DC) TO REFL-IN-KVPB-REF                      
005211        MOVE WS-KVPBREOI (IX-DC) TO REFL-IN-KVPBREOI                      
005212     END-IF                                                               
005213                                                                          
005214     PERFORM S100-CALL-W271UTUP                                           
005215                                                                          
005216     CALL W271REFL USING REFL-W271REFL                                    
005217                         REFL1-2501-PCB                                   
005218                         REFL1-WDB6-PCB                                   
005219                         REFL1-WDK7-PCB                                   
005220                         REFL1-UTIL-WDK6-PCB                              
005221                         REFL1-UTIL-WDK7-PCB                              
005222                         REFL1-UTIL-WDB6-PCB                              
005223                                                                          
005224                                                                          
005225     .                                                                    
005226     EJECT                                                                
005227                                                                          
005228 S100-CALL-W271UTUP SECTION.                                              
005229                                                                          
005230***  THIS SECTION CALLS W271UTUP TO GET LEAD TIME                         
005231***  ADJUSTED DEMAND - ( LEAD TIME ADJUSTED FROM CURRENT WEEK)            
005232*                                                                         
005233     INITIALIZE W271-UTUP-W271UTUP                                        
005234     MOVE W-IDARTNR                 TO W271-UTUP-IDARTNR                  
005235     MOVE W-IDDC                    TO W271-UTUP-IDDC                     
005236     MOVE SLAG-IDDC-REF             TO W271-UTUP-IDDC-REF                 
005237     MOVE DAGENS-DATUM              TO W271-UTUP-TIAAMMDD                 
005238     MOVE REFL-NDC-KVDAGAR-TBT-DC   TO W271-UTUP-LEADTIME                 
005239     MOVE 004                       TO W271-UTUP-KDCALL                   
005240*                                                                         
005241***  BELOW CHECK ONLY IF SIMUNATION IS REQUESTED AND NOT FOR UPD          
005242*                                                                         
005243     IF WS-FLSIM = JA                                                     
005244        MOVE JA                     TO W271-UTUP-FLSIM                    
005245        MOVE WS-KVPB-REF (IX-DC)    TO W271-UTUP-KVPB-REF                 
005246        MOVE WS-KVPBREOI (IX-DC)    TO W271-UTUP-KVPBREOI                 
005247     END-IF                                                               
005248*                                                                         
005249     CALL W271UTUP USING W271-UTUP-W271UTUP                               
005250                         UTUP1-WDK7-PCB                                   
005251                         UTUP1-WDB6-PCB                                   
005252                         UTUP1-UTIL-WDK6-PCB                              
005253                         UTUP1-UTIL-WDK7-PCB                              
005254                         UTUP1-UTIL-WDB6-PCB                              
005255     IF W271-UTUP-KDSVAR-OK                                               
005256        MOVE W271-UTUP-LEADTID-BEHOV TO REFL-IN-LEADTID-BEHOV             
005257     ELSE                                                                 
005258        MOVE 'FEL FRÅN W20352 S100-'                                      
005259                                    TO FELTEXT                            
005260        CALL FELLOG                                                       
005261     END-IF                                                               
005262     .                                                                    
005263     EJECT                                                                
005264                                                                          
005265 MFS-RENSA-FAELT-IN SECTION.                                              
005266                                                                          
005267*    --- ALLA INDATA-FÄLT                                                 
005268     MOVE MFS-RENSA-FAELT    TO MOD-COMMENT                               
005269                                                                          
005270     MOVE 1                  TO IX                                        
005271     PERFORM UNTIL IX > 4                                                 
005272       MOVE MFS-RENSA-FAELT  TO MOD-KVPB-REF (IX)                         
005273                                MOD-KVPBREOI (IX)                         
005274                                MOD-IDDC-FROM(IX)                         
005275                                MOD-PURCHQTY (IX)                         
005276                                MOD-FLREFBEO (IX)                         
005277                                MOD-FLFLYG (IX)                           
005278                                MOD-FREEZECODE (IX)                       
005279       ADD 1                  TO IX                                       
005280     END-PERFORM                                                          
005281     MOVE ZERO               TO WS-RED-PURCHQTY                           
005282     MOVE WS-RED-PURCHQTY    TO MOD-PURCHQTY (1)                          
005283                                MOD-PURCHQTY (2)                          
005284                                MOD-PURCHQTY (3)                          
005285                                MOD-PURCHQTY (4)                          
005286     .                                                                    
005287     EJECT                                                                
005288 MFS-RENSA-FAELT-UT SECTION.                                              
005289                                                                          
005290*    --- ALLA UTDATA-FÄLT                                                 
005291     MOVE MFS-RENSA-FAELT    TO MOD-ANT-REVIEW                            
005292                                MOD-FOREG-AAR                             
005293                                MOD-IAAR                                  
005294                                MOD-VECKA                                 
005295                                MOD-BEART                                 
005296                                MOD-IDFKNGRP                              
005297                                MOD-KDPRODSL                              
005298                                MOD-REPLACES                              
005299                                MOD-TIFINLV                               
005300                                MOD-REPL-BY                               
005301                                MOD-KDERS                                 
005302                                MOD-KVQPACK-0                             
005303                                MOD-KVQPACK-1                             
005304                                MOD-KVQPACK-3                             
005305                                MOD-PRMATRL                               
005306                                MOD-TIURPROD                              
005307                                MOD-AIR-COST                              
005308                                MOD-AVAIL                                 
005309                                MOD-KVAKS-CDC                             
005310                                MOD-KVROS-CDC                             
005311                                MOD-KVAVIS                                
005312                                MOD-KVPB-CDC                              
005313                                MOD-REDIRLEV                              
005314                                                                          
005315     MOVE 1                  TO IX                                        
005316     PERFORM UNTIL IX > 3                                                 
005317       MOVE MFS-RENSA-FAELT  TO MOD-MODEL (IX)                            
005318       ADD 1                 TO IX                                        
005319     END-PERFORM                                                          
005320                                                                          
005321     MOVE 1                  TO IX                                        
005322     PERFORM UNTIL IX > 4                                                 
005323       MOVE MFS-RENSA-FAELT  TO MOD-KVOI-FOREG-AAR (IX)                   
005324                                MOD-KVOI-IAAR (IX)                        
005325                                MOD-KVOI-INNEV (IX)                       
005326                                MOD-IDDC-REF (IX)                         
005327                                MOD-BALANCE (IX)                          
005328                                MOD-KVAKS-SDC (IX)                        
005329                                MOD-ORDERED (IX)                          
005330                                MOD-KVROS (IX)                            
005331                                MOD-SUPERWEEK (IX)                        
005332                                MOD-QUALBLOCK (IX)                        
005333                                MOD-SEASON (IX)                           
005334                                MOD-KVROS-NDC-CDC (IX)                    
005335                                MOD-CRIT-KVROS-NDC-CDC (IX)               
005336                                MOD-KVREFPKT (IX)                         
005337                                MOD-FLFLYG (IX)                           
005338       ADD 1                 TO IX                                        
005339     END-PERFORM                                                          
005340                                                                          
005341     MOVE 1                  TO IX                                        
005342     PERFORM UNTIL IX > 3                                                 
005343       MOVE MFS-RENSA-FAELT  TO MOD-TIPP (IX)                             
005344                                MOD-TIVV-FOM-TOM (IX)                     
005345       MOVE 1                TO IX2                                       
005346       PERFORM UNTIL IX2 > 4                                              
005347         MOVE MFS-RENSA-FAELT                                             
005348                             TO MOD-KVOI-RULL (IX, IX2)                   
005349         ADD 1               TO IX2                                       
005350       END-PERFORM                                                        
005351       ADD 1                 TO IX                                        
005352     END-PERFORM                                                          
005353     .                                                                    
005354     EJECT                                                                
005355 MFS-RENSA-OBEHOERIGA-FAELT-IN SECTION.                                   
005356     SKIP2                                                                
005357     MOVE MFS-RENSA-FAELT      TO MOD-PURCHQTY(1)                         
005358                                  MOD-PURCHQTY(2)                         
005359                                  MOD-PURCHQTY(3)                         
005360                                  MOD-PURCHQTY(4)                         
005361                                  MOD-KVPB-REF(1)                         
005362                                  MOD-KVPB-REF(2)                         
005363                                  MOD-KVPB-REF(3)                         
005364                                  MOD-KVPB-REF(4)                         
005365                                  MOD-KVPBREOI(1)                         
005366                                  MOD-KVPBREOI(2)                         
005367                                  MOD-KVPBREOI(3)                         
005368                                  MOD-KVPBREOI(4)                         
005369                                  MOD-FLREFBEO(1)                         
005370                                  MOD-FLREFBEO(2)                         
005371                                  MOD-FLREFBEO(3)                         
005372                                  MOD-FLREFBEO(4)                         
005373                                  MOD-IDDC-FROM-ATTR(1)                   
005374                                  MOD-IDDC-FROM-ATTR(2)                   
005375                                  MOD-IDDC-FROM-ATTR(3)                   
005376                                  MOD-IDDC-FROM-ATTR(4)                   
005377                                  MOD-FLCDART(1)                          
005378                                  MOD-FLCDART(2)                          
005379                                  MOD-FLCDART(3)                          
005380                                  MOD-FLCDART(4)                          
005381                                  MOD-COMMENT                             
005382                                  MOD-FLFLYG(1)                           
005383                                  MOD-FLFLYG(2)                           
005384                                  MOD-FLFLYG(3)                           
005385                                  MOD-FLFLYG(4)                           
005386     .                                                                    
005387     EJECT                                                                
005388 MFS-STAENG-OBEHOERIGA-FAELT-IN  SECTION.                                 
005389     SKIP2                                                                
005390     MOVE MFS-STAENG-FAELT TO MOD-PURCHQTY-ATTR(1)                        
005391                              MOD-PURCHQTY-ATTR(2)                        
005392                              MOD-PURCHQTY-ATTR(3)                        
005393                              MOD-PURCHQTY-ATTR(4)                        
005394                              MOD-KVPB-REF-ATTR(1)                        
005395                              MOD-KVPB-REF-ATTR(2)                        
005396                              MOD-KVPB-REF-ATTR(3)                        
005397                              MOD-KVPB-REF-ATTR(4)                        
005398                              MOD-KVPBREOI-ATTR(1)                        
005399                              MOD-KVPBREOI-ATTR(2)                        
005400                              MOD-KVPBREOI-ATTR(3)                        
005401                              MOD-KVPBREOI-ATTR(4)                        
005402                              MOD-FLREFBEO-ATTR(1)                        
005403                              MOD-FLREFBEO-ATTR(2)                        
005404                              MOD-FLREFBEO-ATTR(3)                        
005405                              MOD-FLREFBEO-ATTR(4)                        
005406                              MOD-IDDC-FROM-ATTR(1)                       
005407                              MOD-IDDC-FROM-ATTR(2)                       
005408                              MOD-IDDC-FROM-ATTR(3)                       
005409                              MOD-IDDC-FROM-ATTR(4)                       
005410                              MOD-FLCDART-ATTR(1)                         
005411                              MOD-FLCDART-ATTR(2)                         
005412                              MOD-FLCDART-ATTR(3)                         
005413                              MOD-FLCDART-ATTR(4)                         
005414                              MOD-COMMENT-ATTR                            
005415                              MOD-FLFLYG-ATTR(1)                          
005416                              MOD-FLFLYG-ATTR(2)                          
005417                              MOD-FLFLYG-ATTR(3)                          
005418                              MOD-FLFLYG-ATTR(4)                          
005419     .                                                                    
005420     EJECT                                                                
005421                                                                          
005422 MFS-CLOSE-BLANK-DC-FIELD-IN  SECTION.                                    
005423     SKIP2                                                                
005424     MOVE MFS-STAENG-FAELT TO MOD-PURCHQTY-ATTR  (IX-DC)                  
005425                              MOD-KVPB-REF-ATTR  (IX-DC)                  
005426                              MOD-KVPBREOI-ATTR  (IX-DC)                  
005427                              MOD-FLREFBEO-ATTR  (IX-DC)                  
005428                              MOD-IDDC-FROM-ATTR (IX-DC)                  
005429                              MOD-FLCDART-ATTR   (IX-DC)                  
005430                              MOD-FLFLYG-ATTR    (IX-DC)                  
005431     .                                                                    
005432     EJECT                                                                
005433                                                                          
005434 MFS-LAES-IN-IGEN SECTION.                                                
005435                                                                          
005436*    --- ALLA INDATA-FÄLT                                                 
005437                                                                          
005438     MOVE MFS-ADD-LAES-IN-FAELT                                           
005439                             TO MOD-IDARTNR-IN-ATTR                       
005440                                MOD-COMMENT-ATTR                          
005441     MOVE 1                  TO IX                                        
005442     PERFORM UNTIL IX > 4                                                 
005443       MOVE MFS-ADD-LAES-IN-FAELT                                         
005444                             TO MOD-KVPB-REF-ATTR (IX)                    
005445                                MOD-KVPBREOI-ATTR (IX)                    
005446                                MOD-IDDC-FROM-ATTR(IX)                    
005447                                MOD-PURCHQTY-ATTR (IX)                    
005448                                MOD-FLREFBEO-ATTR (IX)                    
005449                                MOD-FLFLYG-ATTR   (IX)                    
005450       ADD 1                 TO IX                                        
005451     END-PERFORM                                                          
005452     .                                                                    
005453     EJECT                                                                
005454* --- IMS SEKTIONER ---                                                   
005455     SKIP3                                                                
005456 IMS-GET-MSG SECTION.                                                     
005457                                                                          
005458     MOVE '  QC' TO GODK-STATUSKODER                                      
005459     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
005460     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
005461     PERFORM IMS-STATUSKONTROLL                                           
005462     .                                                                    
005463     SKIP3                                                                
005464 IMS-INSERT-MSG SECTION.                                                  
005465                                                                          
005466     IF ENGLISH-TEXT                                                      
005467       MOVE 'N' TO MFS-KDHUVOMR                                           
005468     END-IF                                                               
005469     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
005470     MOVE SPACE TO GODK-STATUSKODER                                       
005471     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
005472     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
005473     PERFORM IMS-STATUSKONTROLL                                           
005474     .                                                                    
005475     EJECT                                                                
005476 IMS-GU-WDK7-WDK701 SECTION.                                              
005477     MOVE 'IMS-GU-WDK7-WDK701        ' TO  WS-IMS-SEKTION                 
005478                                                                          
005479     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
005480          DELIMITED BY SIZE INTO SSA1                                     
005481     MOVE '  GE' TO GODK-STATUSKODER                                      
005482     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK701 SSA1               
005483     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
005484     PERFORM IMS-STATUSKONTROLL                                           
005485     .                                                                    
005486     SKIP3                                                                
005487 IMS-GU-WDK7-WDK711 SECTION.                                              
005488     MOVE 'IMS-GU-WDK7-WDK711        ' TO  WS-IMS-SEKTION                 
005489                                                                          
005490     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
005491          DELIMITED BY SIZE INTO SSA1                                     
005492     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
005493          DELIMITED BY SIZE INTO SSA2                                     
005494     MOVE '  GE' TO GODK-STATUSKODER                                      
005495     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK711 SSA1 SSA2          
005496     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
005497     PERFORM IMS-STATUSKONTROLL                                           
005498     .                                                                    
005499     SKIP3                                                                
005500 IMS-GU-WDK711-IDDCREF SECTION.                                           
005501     MOVE 'IMS-GU-WDK711-IDDCREF     ' TO  WS-IMS-SEKTION                 
005502                                                                          
005503     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
005504          DELIMITED BY SIZE INTO SSA1                                     
005505     STRING 'WDK711  (IDDCREF  =' W-IDDCREF-X ')'                         
005506          DELIMITED BY SIZE INTO SSA2                                     
005507     MOVE '  GE' TO GODK-STATUSKODER                                      
005508     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK711 SSA1 SSA2          
005509     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
005510     PERFORM IMS-STATUSKONTROLL                                           
005511     .                                                                    
005512     SKIP3                                                                
005513 IMS-GHU-WDK7-WDK711 SECTION.                                             
005514     MOVE 'IMS-GHU-WDK7-WDK711       ' TO  WS-IMS-SEKTION                 
005515                                                                          
005516     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
005517          DELIMITED BY SIZE INTO SSA1                                     
005518     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
005519          DELIMITED BY SIZE INTO SSA2                                     
005520     MOVE '  GE' TO GODK-STATUSKODER                                      
005521     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-AREA-WDK711 SSA1 SSA2         
005522     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
005523     PERFORM IMS-STATUSKONTROLL                                           
005524     .                                                                    
005525     SKIP3                                                                
005526 IMS-GHNP-WDK7-WDK711 SECTION.                                            
005527     MOVE 'IMS-GHNP-WDK7-WDK711      ' TO  WS-IMS-SEKTION                 
005528                                                                          
005529     STRING 'WDK711  (IDDC    >=' W-IDDC-K7-MIN-X                         
005530                    '&IDDC    <=' W-IDDC-K7-MAX-X ')'                     
005531          DELIMITED BY SIZE INTO SSA1                                     
005532     MOVE '  GE' TO GODK-STATUSKODER                                      
005533     CALL CBLTDLI USING GHNP WDK7-PCB DLI-IO-AREA-WDK711 SSA1             
005534     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
005535     PERFORM IMS-STATUSKONTROLL                                           
005536     .                                                                    
005537     SKIP3                                                                
005538 IMS-GU-WDK711-TRANS SECTION.                                             
005539     MOVE 'IMS-GU-WDK711-TRANS       ' TO  WS-IMS-SEKTION                 
005540                                                                          
005541     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
005542          DELIMITED BY SIZE INTO SSA1                                     
005543     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
005544          DELIMITED BY SIZE INTO SSA2                                     
005545     MOVE '  GE' TO GODK-STATUSKODER                                      
005546     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK711-TRANS              
005547                                    SSA1 SSA2                             
005548     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
005549     PERFORM IMS-STATUSKONTROLL                                           
005550     .                                                                    
005551     SKIP3                                                                
005552 IMS-REPL-WDK7-WDK711 SECTION.                                            
005553     MOVE 'IMS-REPL-WDK7-WDK711      ' TO  WS-IMS-SEKTION                 
005554                                                                          
005555     MOVE '  ' TO GODK-STATUSKODER                                        
005556     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-AREA-WDK711                  
005557     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
005558     PERFORM IMS-STATUSKONTROLL                                           
005559     .                                                                    
005560     EJECT                                                                
005561 IMS-GU-WDK712 SECTION.                                                   
005562     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
005563          DELIMITED BY SIZE INTO SSA1                                     
005564     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
005565          DELIMITED BY SIZE INTO SSA2                                     
005566     MOVE '  GE' TO GODK-STATUSKODER                                      
005567     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK712 SSA1 SSA2          
005568     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
005569     PERFORM IMS-STATUSKONTROLL                                           
005570     .                                                                    
005571     SKIP3                                                                
005572 IMS-GU-WDD3-BENA01-BSEQ SECTION.                                         
005573     MOVE 'IMS-GU-WDD3-BENA01-BSEQ   ' TO  WS-IMS-SEKTION                 
005574                                                                          
005575     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
005576          DELIMITED BY SIZE INTO SSA1                                     
005577     MOVE '  GE' TO GODK-STATUSKODER                                      
005578     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA-BENA01 SSA1               
005579     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
005580     PERFORM IMS-STATUSKONTROLL                                           
005581     .                                                                    
005582     SKIP3                                                                
005583 IMS-GNP-WDD3-BENA11 SECTION.                                             
005584     MOVE 'IMS-GNP-WDD3-BENA11       ' TO  WS-IMS-SEKTION                 
005585                                                                          
005586     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
005587          DELIMITED BY SIZE INTO SSA1                                     
005588     MOVE '  GE' TO GODK-STATUSKODER                                      
005589     CALL CBLTDLI USING GNP BENA-PCB DLI-IO-AREA-BENA11 SSA1              
005590     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
005591     PERFORM IMS-STATUSKONTROLL                                           
005592     .                                                                    
005593     EJECT                                                                
005594 IMS-GHU-WDE3-ORDL01 SECTION.                                             
005595     MOVE 'IMS-GHU-WDE3-ORDL01       ' TO  WS-IMS-SEKTION                 
005596                                                                          
005597     STRING 'WLORDL01(WDE301KY =' W-WDE301KY-X ')'                        
005598          DELIMITED BY SIZE INTO SSA1                                     
005599     MOVE '  ' TO GODK-STATUSKODER                                        
005600     CALL CBLTDLI USING GHU ORDL-PCB DLI-IO-AREA-ORDL01 SSA1              
005601     MOVE ORDL-STATUS-CODE TO STATUS-WS                                   
005602     PERFORM IMS-STATUSKONTROLL                                           
005603     .                                                                    
005604     SKIP3                                                                
005605 IMS-GU-WDE3-ORDL01 SECTION.                                              
005606     MOVE 'IMS-GU-WDE3-ORDL01        ' TO  WS-IMS-SEKTION                 
005607                                                                          
005608     STRING 'WLORDL01(WDE301KY =' W-WDE301KY-X ')'                        
005609          DELIMITED BY SIZE INTO SSA1                                     
005610     MOVE '  ' TO GODK-STATUSKODER                                        
005611     CALL CBLTDLI USING GU ORDL-PCB DLI-IO-AREA-ORDL01 SSA1               
005612     MOVE ORDL-STATUS-CODE TO STATUS-WS                                   
005613     PERFORM IMS-STATUSKONTROLL                                           
005614     .                                                                    
005615     SKIP3                                                                
005616 IMS-GU-WDE3-ORDL01-TRAN SECTION.                                         
005617     MOVE 'IMS-GU-WDE3-ORDL01-TRAN   ' TO  WS-IMS-SEKTION                 
005618                                                                          
005619     STRING 'WLORDL01(WDE301KY =' W-WDE301KY-X ')'                        
005620          DELIMITED BY SIZE INTO SSA1                                     
005621     MOVE '  GE' TO GODK-STATUSKODER                                      
005622     CALL CBLTDLI USING GU ORDL-PCB DLI-IO-AREA-ORDL01 SSA1               
005623     MOVE ORDL-STATUS-CODE TO STATUS-WS                                   
005624     PERFORM IMS-STATUSKONTROLL                                           
005625     .                                                                    
005626     SKIP3                                                                
005627 IMS-GHU-WDE3-ORDL01-DEL SECTION.                                         
005628     MOVE 'IMS-GU-WDE3-ORDL01        ' TO  WS-IMS-SEKTION                 
005629                                                                          
005630     STRING 'WLORDL01(WDE301KY =' W-WDE301KY-DEL ')'                      
005631          DELIMITED BY SIZE INTO SSA1                                     
005632     MOVE '  GE' TO GODK-STATUSKODER                                      
005633     CALL CBLTDLI USING GHU ORDL-PCB DLI-IO-AREA-ORDL01 SSA1              
005634     MOVE ORDL-STATUS-CODE TO STATUS-WS                                   
005635     PERFORM IMS-STATUSKONTROLL                                           
005636     .                                                                    
005637     SKIP3                                                                
005638 IMS-GU-WDE3-ORDW01-MIN-MAX SECTION.                                      
005639     MOVE 'IMS-GU-WDE3-ORDW01-MIN-MAX' TO  WS-IMS-SEKTION                 
005640                                                                          
005641     STRING 'WLORDW01(WDE3A1KY>=' W-WDE3A1KY-MIN-X                        
005642                    '&WDE3A1KY<=' W-WDE3A1KY-MAX-X ')'                    
005643          DELIMITED BY SIZE INTO SSA1                                     
005644     MOVE '  GE' TO GODK-STATUSKODER                                      
005645     CALL CBLTDLI USING GU ORDW-PCB DLI-IO-AREA-ORDW01 SSA1               
005646     MOVE ORDW-STATUS-CODE TO STATUS-WS                                   
005647     PERFORM IMS-STATUSKONTROLL                                           
005648     .                                                                    
005649     SKIP3                                                                
005650 IMS-GN-WDE3-ORDW01-MIN-MAX SECTION.                                      
005651     MOVE 'IMS-GN-WDE3-ORDW01-MIN-MAX' TO  WS-IMS-SEKTION                 
005652                                                                          
005653     STRING 'WLORDW01(WDE3A1KY>=' W-WDE3A1KY-MIN-X                        
005654                    '&WDE3A1KY<=' W-WDE3A1KY-MAX-X ')'                    
005655          DELIMITED BY SIZE INTO SSA1                                     
005656     MOVE '  GEGB' TO GODK-STATUSKODER                                    
005657     CALL CBLTDLI USING GN ORDW-PCB DLI-IO-AREA-ORDW01 SSA1               
005658     MOVE ORDW-STATUS-CODE TO STATUS-WS                                   
005659     PERFORM IMS-STATUSKONTROLL                                           
005660     .                                                                    
005661     SKIP3                                                                
005662 IMS-GU-WDE3-ORDL01-MIN-MAX SECTION.                                      
005663     MOVE 'IMS-GU-WDE3-ORDL01-MIN-MAX' TO  WS-IMS-SEKTION                 
005664                                                                          
005665     STRING 'WLORDL01(WDE301KY>=' W-WDE301KY-MIN-X                        
005666                    '&WDE301KY<=' W-WDE301KY-MAX-X ')'                    
005667          DELIMITED BY SIZE INTO SSA1                                     
005668     MOVE '  GE' TO GODK-STATUSKODER                                      
005669     CALL CBLTDLI USING GU ORDL-PCB DLI-IO-AREA-ORDL01 SSA1               
005670     MOVE ORDL-STATUS-CODE TO STATUS-WS                                   
005671     PERFORM IMS-STATUSKONTROLL                                           
005672     .                                                                    
005673     SKIP3                                                                
005674 IMS-GN-WDE3-ORDL01-MIN-MAX SECTION.                                      
005675     MOVE 'IMS-GN-WDE3-ORDL01-MIN-MAX' TO  WS-IMS-SEKTION                 
005676                                                                          
005677     STRING 'WLORDL01(WDE301KY>=' W-WDE301KY-MIN-X                        
005678                    '&WDE301KY<=' W-WDE301KY-MAX-X ')'                    
005679          DELIMITED BY SIZE INTO SSA1                                     
005680     MOVE '  GEGB' TO GODK-STATUSKODER                                    
005681     CALL CBLTDLI USING GN ORDL-PCB DLI-IO-AREA-ORDL01 SSA1               
005682     MOVE ORDL-STATUS-CODE TO STATUS-WS                                   
005683     PERFORM IMS-STATUSKONTROLL                                           
005684     .                                                                    
005685     SKIP3                                                                
005686 IMS-GU-WDE3-ORDW01-PF8 SECTION.                                          
005687     MOVE 'IMS-GU-WDE3-ORDW01-PF8    ' TO  WS-IMS-SEKTION                 
005688                                                                          
005689     STRING 'WLORDW01(WDE3A1KY >' W-WDE3A1KY-MIN-X                        
005690                    '&WDE3A1KY<=' W-WDE3A1KY-MAX-X ')'                    
005691          DELIMITED BY SIZE INTO SSA1                                     
005692     MOVE '  GE' TO GODK-STATUSKODER                                      
005693     CALL CBLTDLI USING GU ORDW-PCB DLI-IO-AREA-ORDW01 SSA1               
005694     MOVE ORDW-STATUS-CODE TO STATUS-WS                                   
005695     PERFORM IMS-STATUSKONTROLL                                           
005696     .                                                                    
005697     SKIP3                                                                
005698 IMS-GN-WDE3-ORDW01-PF8 SECTION.                                          
005699     MOVE 'IMS-GN-WDE3-ORDW01-PF8    ' TO  WS-IMS-SEKTION                 
005700                                                                          
005701     STRING 'WLORDW01(WDE3A1KY >' W-WDE3A1KY-MIN-X                        
005702                    '&WDE3A1KY<=' W-WDE3A1KY-MAX-X ')'                    
005703          DELIMITED BY SIZE INTO SSA1                                     
005704     MOVE '  GEGB' TO GODK-STATUSKODER                                    
005705     CALL CBLTDLI USING GN ORDW-PCB DLI-IO-AREA-ORDW01 SSA1               
005706     MOVE ORDW-STATUS-CODE TO STATUS-WS                                   
005707     PERFORM IMS-STATUSKONTROLL                                           
005708     .                                                                    
005709     SKIP3                                                                
005710 IMS-ISRT-WDE3-ORDL01 SECTION.                                            
005711     MOVE 'IMS-ISRT-WDE3-ORDL01      ' TO  WS-IMS-SEKTION                 
005712                                                                          
005713     MOVE 'WLORDL01 ' TO SSA1                                             
005714     MOVE '  ' TO GODK-STATUSKODER                                        
005715     CALL CBLTDLI USING ISRT ORDL-PCB DLI-IO-AREA-ORDL01 SSA1             
005716     MOVE ORDL-STATUS-CODE TO STATUS-WS                                   
005717     PERFORM IMS-STATUSKONTROLL                                           
005718     .                                                                    
005719     SKIP3                                                                
005720 IMS-REPL-WDE3-ORDL01 SECTION.                                            
005721     MOVE 'IMS-REPL-WDE3-ORDL01      ' TO  WS-IMS-SEKTION                 
005722                                                                          
005723     MOVE '  ' TO GODK-STATUSKODER                                        
005724     CALL CBLTDLI USING REPL ORDL-PCB DLI-IO-AREA-ORDL01                  
005725     MOVE ORDL-STATUS-CODE TO STATUS-WS                                   
005726     PERFORM IMS-STATUSKONTROLL                                           
005727     .                                                                    
005728     EJECT                                                                
005729 IMS-DLET-WDE3-ORDL01 SECTION.                                            
005730     MOVE 'IMS-DLET-WDE3-ORDL01      ' TO  WS-IMS-SEKTION                 
005731                                                                          
005732     MOVE '  ' TO GODK-STATUSKODER                                        
005733     CALL CBLTDLI USING DLET ORDL-PCB DLI-IO-AREA-ORDL01                  
005734     MOVE ORDL-STATUS-CODE TO STATUS-WS                                   
005735     PERFORM IMS-STATUSKONTROLL                                           
005736     .                                                                    
005737     SKIP3                                                                
005738 IMS-GU-WDA5-ORDQ01 SECTION.                                              
005739     MOVE 'IMS-GU-WDA5-ORDQ01        ' TO  WS-IMS-SEKTION                 
005740                                                                          
005741     STRING 'WLORDQ01(WDA5A1KY>=' W-WDA5A1KY-MIN                          
005742                    '&WDA5A1KY<=' W-WDA5A1KY-MAX  ')'                     
005743            DELIMITED BY SIZE INTO SSA1                                   
005744     MOVE '  GBGE' TO GODK-STATUSKODER                                    
005745     CALL CBLTDLI USING GU ORDQ-PCB DLI-IO-AREA-ORDQ01 SSA1               
005746     MOVE ORDQ-STATUS-CODE TO STATUS-WS                                   
005747     PERFORM IMS-STATUSKONTROLL                                           
005748     .                                                                    
005749     EJECT                                                                
005750 IMS-GN-WDA5-ORDQ01 SECTION.                                              
005751     MOVE 'IMS-GN-WDA5-ORDQ01        ' TO  WS-IMS-SEKTION                 
005752                                                                          
005753     STRING 'WLORDQ01(WDA5A1KY>=' W-WDA5A1KY-MIN                          
005754                    '&WDA5A1KY<=' W-WDA5A1KY-MAX  ')'                     
005755            DELIMITED BY SIZE INTO SSA1                                   
005756     MOVE '  GBGE' TO GODK-STATUSKODER                                    
005757     CALL CBLTDLI USING GN ORDQ-PCB DLI-IO-AREA-ORDQ01 SSA1               
005758     MOVE ORDQ-STATUS-CODE TO STATUS-WS                                   
005759     PERFORM IMS-STATUSKONTROLL                                           
005760     .                                                                    
005761     EJECT                                                                
005762 IMS-GU-WDA5-ORDP01 SECTION.                                              
005763     MOVE 'IMS-GU-WDA5-ORDP01        ' TO  WS-IMS-SEKTION                 
005764                                                                          
005765     STRING 'WLORDP01(WDA501KY =' W-WDA501KY ')'                          
005766            DELIMITED BY SIZE INTO SSA1                                   
005767     MOVE '  GE' TO GODK-STATUSKODER                                      
005768     CALL CBLTDLI USING GU ORDP-PCB DLI-IO-AREA-ORDP01 SSA1               
005769     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
005770     PERFORM IMS-STATUSKONTROLL                                           
005771     .                                                                    
005772     SKIP2                                                                
005773 IMS-GU-WDK6-ARTC01 SECTION.                                              
005774     MOVE 'IMS-GU-WDK6-ARTC01        ' TO  WS-IMS-SEKTION                 
005775     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
005776          DELIMITED BY SIZE INTO SSA1                                     
005777     MOVE '  GE' TO GODK-STATUSKODER                                      
005778     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-ARTC01 SSA1               
005779     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
005780     PERFORM IMS-STATUSKONTROLL                                           
005781     .                                                                    
005782     SKIP3                                                                
005783 IMS-GU-WDK6-ARTC11 SECTION.                                              
005784     MOVE 'IMS-GU-WDK6-ARTC11        ' TO  WS-IMS-SEKTION                 
005785     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
005786          DELIMITED BY SIZE INTO SSA1                                     
005787     MOVE 'WLARTC11'       TO SSA2                                        
005788     MOVE '  GE' TO GODK-STATUSKODER                                      
005789     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-ARTC11 SSA1 SSA2          
005790     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
005791     PERFORM IMS-STATUSKONTROLL                                           
005792     .                                                                    
005793     SKIP3                                                                
005794 IMS-GNP-WDK6-ARTC11 SECTION.                                             
005795     MOVE 'IMS-GNP-WDK6-ARTC11       ' TO  WS-IMS-SEKTION                 
005796     MOVE 'WLARTC11'       TO SSA1                                        
005797     MOVE '  GE' TO GODK-STATUSKODER                                      
005798     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA-ARTC11 SSA1              
005799     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
005800     PERFORM IMS-STATUSKONTROLL                                           
005801     .                                                                    
005802     EJECT                                                                
005803 IMS-GNP-WDK6-ARTC21-FIRST SECTION.                                       
005804     MOVE 'IMS-GNP-WDK6-ARTC21       ' TO  WS-IMS-SEKTION                 
005805     STRING 'WLARTC21*F(DAPRLIST>=' W-DAPRLIST-21-N                       
005806                    '&IDLEVNR  =' W-IDLEVNR-21-X ')'                      
005807          DELIMITED BY SIZE INTO SSA1                                     
005808     MOVE '  GE' TO GODK-STATUSKODER                                      
005809     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA-ARTC21 SSA1              
005810     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
005811     PERFORM IMS-STATUSKONTROLL                                           
005812     SKIP3                                                                
005813     .                                                                    
005814 IMS-GNP-WDK6-ARTC21 SECTION.                                             
005815     MOVE 'IMS-GNP-WDK6-ARTC21       ' TO  WS-IMS-SEKTION                 
005816     STRING 'WLARTC21(DAPRLIST>=' W-DAPRLIST-21-N                         
005817                    '&IDLEVNR  =' W-IDLEVNR-21-X ')'                      
005818          DELIMITED BY SIZE INTO SSA1                                     
005819     MOVE '  GE' TO GODK-STATUSKODER                                      
005820     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA-ARTC21 SSA1              
005821     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
005822     PERFORM IMS-STATUSKONTROLL                                           
005823     SKIP3                                                                
005824     .                                                                    
005825     EJECT                                                                
005826 IMS-GNP-WDK6-ARTC25 SECTION.                                             
005827     MOVE 'IMS-GNP-WDK6-ARTC25       ' TO  WS-IMS-SEKTION                 
005828     MOVE 'WLARTC11 ' TO SSA1                                             
005829     STRING 'WLARTC25(KDNOTTYP =' W-KDNOTTYP-X ')'                        
005830            DELIMITED BY SIZE INTO SSA2                                   
005831     MOVE '  GE' TO GODK-STATUSKODER                                      
005832     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA-ARTC25 SSA1 SSA2         
005833     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
005834     PERFORM IMS-STATUSKONTROLL                                           
005835     .                                                                    
005836     SKIP3                                                                
005837 IMS-GHU-WDK6-ARTC25 SECTION.                                             
005838     MOVE 'IMS-GHU-WDK6-ARTC25       ' TO  WS-IMS-SEKTION                 
005839     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
005840          DELIMITED BY SIZE INTO SSA1                                     
005841     MOVE 'WLARTC11 ' TO SSA2                                             
005842     STRING 'WLARTC25(KDNOTTYP =' W-KDNOTTYP-X ')'                        
005843            DELIMITED BY SIZE INTO SSA3                                   
005844     MOVE '  GE' TO GODK-STATUSKODER                                      
005845     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-AREA-ARTC25                   
005846                                     SSA1 SSA2 SSA3                       
005847     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
005848     PERFORM IMS-STATUSKONTROLL                                           
005849     .                                                                    
005850     SKIP3                                                                
005851 IMS-ISRT-WDK6-ARTC25 SECTION.                                            
005852     MOVE 'IMS-ISRT-WDK6-ARTC25      ' TO  WS-IMS-SEKTION                 
005853     SKIP2                                                                
005854     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
005855             DELIMITED BY SIZE INTO SSA1                                  
005856     MOVE 'WLARTC11(KDSEGKEY =1)' TO SSA2                                 
005857     MOVE 'WLARTC25 ' TO SSA3                                             
005858     MOVE '  ' TO GODK-STATUSKODER                                        
005859     CALL CBLTDLI USING ISRT ARTC-PCB                                     
005860                        DLI-IO-AREA-ARTC25 SSA1 SSA2 SSA3                 
005861     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
005862     PERFORM IMS-STATUSKONTROLL                                           
005863     .                                                                    
005864     SKIP3                                                                
005865 IMS-REPL-WDK6-ARTC25 SECTION.                                            
005866     MOVE 'IMS-REPL-WDK6-ARTC25      ' TO  WS-IMS-SEKTION                 
005867     SKIP2                                                                
005868     MOVE '  ' TO GODK-STATUSKODER                                        
005869     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA-ARTC25                  
005870     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
005871     PERFORM IMS-STATUSKONTROLL                                           
005872     .                                                                    
005873     SKIP3                                                                
005874 IMS-GU-WDN6-KATN01 SECTION.                                              
005875     MOVE 'IMS-GU-WDN6-KATN01        ' TO  WS-IMS-SEKTION                 
005876     STRING 'WLKATN01(IDARTNR  =' W-IDARTNR-X ')'                         
005877            DELIMITED BY SIZE INTO SSA1                                   
005878     MOVE '  GE' TO GODK-STATUSKODER                                      
005879     CALL CBLTDLI USING GU KATN-PCB DLI-IO-AREA-KATN01 SSA1               
005880     MOVE KATN-STATUS-CODE TO STATUS-WS                                   
005881     PERFORM IMS-STATUSKONTROLL                                           
005882     .                                                                    
005883     SKIP3                                                                
005884 IMS-GNP-WDN6-KATN11 SECTION.                                             
005885     MOVE 'IMS-GNP-WDN6-KATN11       ' TO  WS-IMS-SEKTION                 
005886     MOVE 'WLKATN11 ' TO SSA1                                             
005887     MOVE '  GE' TO GODK-STATUSKODER                                      
005888     CALL CBLTDLI USING GNP KATN-PCB DLI-IO-AREA-KATN11 SSA1              
005889     MOVE KATN-STATUS-CODE TO STATUS-WS                                   
005890     PERFORM IMS-STATUSKONTROLL                                           
005891     .                                                                    
005892     EJECT                                                                
005893 IMS-GU-WDD7-ERSA01 SECTION.                                              
005894     MOVE 'IMS-GU-WDD7-ERSA01        ' TO  WS-IMS-SEKTION                 
005895     STRING 'WLERSA01(IDARTNR  =' W-IDARTNR-X ')'                         
005896            DELIMITED BY SIZE INTO SSA1                                   
005897     MOVE '  GE' TO GODK-STATUSKODER                                      
005898     CALL CBLTDLI USING GU ERSA-PCB DLI-IO-AREA-ERSA01 SSA1               
005899     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
005900     PERFORM IMS-STATUSKONTROLL                                           
005901     .                                                                    
005902     SKIP2                                                                
005903 IMS-GNP-WDD7-ERSA11 SECTION.                                             
005904     MOVE 'IMS-GNP-WDD7-ERSA11       ' TO  WS-IMS-SEKTION                 
005905     STRING 'WLERSA11(FLTEXT   =N)'                                       
005906            DELIMITED BY SIZE INTO SSA1                                   
005907     MOVE '  GE' TO GODK-STATUSKODER                                      
005908     CALL CBLTDLI USING GNP ERSA-PCB DLI-IO-AREA-ERSA11 SSA1              
005909     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
005910     PERFORM IMS-STATUSKONTROLL                                           
005911     .                                                                    
005912     SKIP2                                                                
005913 IMS-GU-WDD7-ERSB01-MINMAX SECTION.                                       
005914     MOVE 'IMS-GU-WDD7-ERSB01-MINMAX ' TO  WS-IMS-SEKTION                 
005915     STRING 'WLERSB01(WDD7A1KY=>' W-WDD7A1KY-MIN                          
005916                    '&WDD7A1KY=<' W-WDD7A1KY-MAX ')'                      
005917            DELIMITED BY SIZE INTO SSA1                                   
005918     MOVE '  GE' TO GODK-STATUSKODER                                      
005919     CALL CBLTDLI USING GU ERSB-PCB DLI-IO-AREA-ERSB01 SSA1               
005920     MOVE ERSB-STATUS-CODE TO STATUS-WS                                   
005921     PERFORM IMS-STATUSKONTROLL                                           
005922     .                                                                    
005923     EJECT                                                                
005924 IMS-GN-WDD7-ERSB01-MINMAX SECTION.                                       
005925     MOVE 'IMS-GN-WDD7-ERSB01-MINMAX ' TO  WS-IMS-SEKTION                 
005926     STRING 'WLERSB01(WDD7A1KY=>' W-WDD7A1KY-MIN                          
005927                    '&WDD7A1KY=<' W-WDD7A1KY-MAX ')'                      
005928            DELIMITED BY SIZE INTO SSA1                                   
005929     MOVE '  GEGB' TO GODK-STATUSKODER                                    
005930     CALL CBLTDLI USING GN ERSB-PCB DLI-IO-AREA-ERSB01 SSA1               
005931     MOVE ERSB-STATUS-CODE TO STATUS-WS                                   
005932     PERFORM IMS-STATUSKONTROLL                                           
005933     .                                                                    
005934     EJECT                                                                
005935 IMS-GU-INLA11-W6D1SEQ SECTION.                                           
005936     MOVE 'IMS-GU-INLA11-W6D1SEQ     ' TO  WS-IMS-SEKTION                 
005937     STRING 'W6INLA11(W6D1HSEQ =' W-W6D1HSEQ-X ')'                        
005938            DELIMITED BY SIZE INTO SSA1                                   
005939     MOVE '  GE' TO GODK-STATUSKODER                                      
005940     CALL CBLTDLI USING GU INLA-PCB DLI-IO-AREA-INLA11 SSA1               
005941     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
005942     PERFORM IMS-STATUSKONTROLL                                           
005943     .                                                                    
005944     EJECT                                                                
005945 IMS-GN-INLA11-W6D1SEQ SECTION.                                           
005946     MOVE 'IMS-GN-INLA11-W6D1SEQ     ' TO  WS-IMS-SEKTION                 
005947     STRING 'W6INLA11(W6D1HSEQ =' W-W6D1HSEQ-X ')'                        
005948            DELIMITED BY SIZE INTO SSA1                                   
005949     MOVE '  GEGB' TO GODK-STATUSKODER                                    
005950     CALL CBLTDLI USING GN INLA-PCB DLI-IO-AREA-INLA11 SSA1               
005951     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
005952     PERFORM IMS-STATUSKONTROLL                                           
005953     .                                                                    
005954     EJECT                                                                
005955 IMS-GU-WDL711     SECTION.                                               
005956     MOVE 'IMS-GU-WDL711        ' TO  WS-IMS-SEKTION                      
005957     STRING 'WDL701  (IDARTNR  =' W-IDARTNR-X ')'                         
005958          DELIMITED BY SIZE INTO SSA1                                     
005959     STRING 'WDL711  (IDDC     =' W-IDDC-X ')'                            
005960          DELIMITED BY SIZE INTO SSA2                                     
005961     MOVE '  GE' TO GODK-STATUSKODER                                      
005962     CALL CBLTDLI USING GU WDL7-PCB DLI-IO-AREA-WDL711 SSA1 SSA2          
005963     MOVE WDL7-STATUS-CODE TO STATUS-WS                                   
005964     PERFORM IMS-STATUSKONTROLL                                           
005965     .                                                                    
005966     SKIP3                                                                
005967 IMS-GU-WDL411     SECTION.                                               
005968     MOVE 'IMS-GU-WDL411        ' TO  WS-IMS-SEKTION                      
005969     STRING 'WDL401  (IDARTNR  =' W-IDARTNR-X ')'                         
005970          DELIMITED BY SIZE INTO SSA1                                     
005971     STRING 'WDL411  (IDDC     =' W-IDDC-X ')'                            
005972          DELIMITED BY SIZE INTO SSA2                                     
005973     MOVE '  GE' TO GODK-STATUSKODER                                      
005974     CALL CBLTDLI USING GU WDL4-PCB DLI-IO-AREA-WDL411 SSA1 SSA2          
005975     MOVE WDL4-STATUS-CODE TO STATUS-WS                                   
005976     PERFORM IMS-STATUSKONTROLL                                           
005977     .                                                                    
005978     SKIP3                                                                
005979 IMS-GU-ARTM01         SECTION.                                           
005980     MOVE 'IMS-GU-ARTM01             ' TO  WS-IMS-SEKTION                 
005981     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
005982            DELIMITED BY SIZE INTO SSA1                                   
005983     MOVE '  GE' TO GODK-STATUSKODER                                      
005984     CALL CBLTDLI USING GU ARTM-PCB DLI-IO-AREA-ARTM01 SSA1               
005985     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
005986     PERFORM IMS-STATUSKONTROLL                                           
005987     .                                                                    
005988     SKIP3                                                                
005989 IMS-GU-LEVA16 SECTION.                                                   
005990     MOVE 'IMS-GU-LEVA16             ' TO  WS-IMS-SEKTION                 
005991                                                                          
005992     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
005993          DELIMITED BY SIZE INTO SSA1                                     
005994     STRING 'WLLEVA16(IDDC     =' W-IDDC-X ')'                            
005995          DELIMITED BY SIZE INTO SSA2                                     
005996     MOVE '  GE' TO GODK-STATUSKODER                                      
005997     CALL CBLTDLI USING GU LEVA-PCB DLI-IO-LEVA16 SSA1 SSA2               
005998     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
005999     PERFORM IMS-STATUSKONTROLL                                           
006000     .                                                                    
006001     EJECT                                                                
006002 IMS-GU-WDK727 SECTION.                                                   
006003     MOVE 'IMS-GU-WDK727 '  TO WS-IMS-SEKTION                             
006004                                                                          
006005     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
006006          DELIMITED BY SIZE INTO SSA1                                     
006007     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
006008          DELIMITED BY SIZE INTO SSA2                                     
006009     STRING 'WDK727  (KDSEGKEY =1)'                                       
006010          DELIMITED BY SIZE INTO SSA3                                     
006011     MOVE '  GE'            TO GODK-STATUSKODER                           
006012     CALL CBLTDLI USING GU WDK7F-PCB DLI-IO-AREA-WDK727-FUTUR             
006013                        SSA1 SSA2 SSA3                                    
006014     MOVE WDK7F-STATUS-CODE TO STATUS-WS                                  
006015     PERFORM IMS-STATUSKONTROLL                                           
006016     .                                                                    
006017     EJECT                                                                
006018                                                                          
006019 IMS-GHU-WDK611 SECTION.                                                  
006020     MOVE 'IMS-GHU-WDK611            ' TO  WS-IMS-SEKTION                 
006021     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
006022          DELIMITED BY SIZE INTO SSA1                                     
006023     MOVE 'WLARTC11(KDSEGKEY =1)' TO SSA2                                 
006024     MOVE '  GE' TO GODK-STATUSKODER                                      
006025     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-AREA-ARTC11 SSA1 SSA2         
006026     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
006027     PERFORM IMS-STATUSKONTROLL                                           
006028     .                                                                    
006029     EJECT                                                                
006030                                                                          
006031 IMS-REPL-WDK6 SECTION.                                                   
006032     MOVE 'IMS-REPL-WDK6             ' TO  WS-IMS-SEKTION                 
006033                                                                          
006034     MOVE '  ' TO GODK-STATUSKODER                                        
006035     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA-ARTC11                  
006036     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
006037     PERFORM IMS-STATUSKONTROLL                                           
006038     .                                                                    
006039     EJECT                                                                
006040                                                                          
006041 IMS-GU-WDB601    SECTION.                                                
006042     MOVE 'IMS-GU-WDB601             ' TO  WS-IMS-SEKTION                 
006043     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
006044          DELIMITED BY SIZE INTO SSA1                                     
006045     MOVE '  GE' TO GODK-STATUSKODER                                      
006046     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
006047     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
006048     PERFORM IMS-STATUSKONTROLL                                           
006049     .                                                                    
006050     EJECT                                                                
006051                                                                          
006052 IMS-GN-WDB601    SECTION.                                                
006053     MOVE 'IMS-GN-WDB601             ' TO  WS-IMS-SEKTION                 
006054     MOVE 'WDB601 ' TO SSA1                                               
006055     MOVE '  GB' TO GODK-STATUSKODER                                      
006056     CALL CBLTDLI USING GN WDB6-GN-PCB DLI-IO-AREA-B601 SSA1              
006057     MOVE WDB6-GN-STATUS-CODE    TO STATUS-WS                             
006058     PERFORM IMS-STATUSKONTROLL                                           
006059     .                                                                    
006060     EJECT                                                                
006061 IMS-GU-WDB616    SECTION.                                                
006062     MOVE 'IMS-GU-WDB616             ' TO  WS-IMS-SEKTION                 
006063     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
006064          DELIMITED BY SIZE INTO SSA1                                     
006065     STRING 'WDB616  (IDDCREF  =' W-IDDC-B616-X ')'                       
006066          DELIMITED BY SIZE INTO SSA2                                     
006067     MOVE '  GE' TO GODK-STATUSKODER                                      
006068     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B616 SSA1 SSA2            
006069     MOVE WDB6-STATUS-CODE       TO STATUS-WS                             
006070     PERFORM IMS-STATUSKONTROLL                                           
006071     .                                                                    
006072     EJECT                                                                
006073 IMS-STATUSKONTROLL SECTION.                                              
006074                                                                          
006075     SET STATUS-IX TO 1                                                   
006076     SEARCH GODK-STATUS                                                   
006077       AT END                                                             
006078         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
006079         DELIMITED BY SIZE INTO FELTEXT                                   
006080         CALL FELLOG                                                      
006081       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
006082         CONTINUE                                                         
006083     END-SEARCH                                                           
006084     .                                                                    
006085     EJECT                                                                
006086 DB2-SELECT-TP4TRAN     SECTION.                                          
006087     MOVE 'DB2-SELECT-TP4TRAN   ' TO  WS-DB2-SEKTION                      
006088                                                                          
006089     MOVE 000100  TO GOOD-SQLCODECODES                                    
006090                                                                          
006091     EXEC SQL                                                             
006092           SELECT  KDARBTYP                                               
006093                  ,IDDC_SEND                                              
006094                  ,IDDC_REC                                               
006095                  ,IDDISTR                                                
006096                  ,IDKUNDNR                                               
006097                                                                          
006098           INTO   :TP4TRAN-KDARBTYP                                       
006099                 ,:TP4TRAN-IDDC-SEND                                      
006100                 ,:TP4TRAN-IDDC-REC                                       
006101                 ,:TP4TRAN-IDDISTR                                        
006102                 ,:TP4TRAN-IDKUNDNR                                       
006103                                                                          
006104           FROM    TP4TRAN                                                
006105                                                                          
006106           WHERE KDARBTYP  = :WS-KDARBTYP-X3                              
006107           AND   IDDC_SEND = :WS-IDDC-SEND                                
006108           AND   IDDC_REC  = :WS-IDDC-REC                                 
006109     END-EXEC                                                             
006110                                                                          
006111     MOVE SQLCODE TO SQLCODE-WS                                           
006112     PERFORM DB2-STATUS-CHECK                                             
006113     .                                                                    
006114     EJECT                                                                
006115 DB2-SELECT-TP4TRAN-2   SECTION.                                          
006116     MOVE 'DB2-SELECT-TP4TRAN-2 ' TO  WS-DB2-SEKTION                      
006117                                                                          
006118     MOVE 000100  TO GOOD-SQLCODECODES                                    
006119                                                                          
006120     EXEC SQL                                                             
006121           SELECT  KDARBTYP                                               
006122                  ,IDDC_SEND                                              
006123                  ,IDDC_REC                                               
006124                  ,IDDISTR                                                
006125                  ,IDKUNDNR                                               
006126                                                                          
006127           INTO   :TP4TRAN-KDARBTYP                                       
006128                 ,:TP4TRAN-IDDC-SEND                                      
006129                 ,:TP4TRAN-IDDC-REC                                       
006130                 ,:TP4TRAN-IDDISTR                                        
006131                 ,:TP4TRAN-IDKUNDNR                                       
006132                                                                          
006133           FROM    TP4TRAN                                                
006134                                                                          
006135           WHERE KDARBTYP  = :WS-KDARBTYP-X3                              
006136           AND   IDDC_REC  = :REF-IDDC                                    
006137           AND   IDDISTR   = :REF-IDDISTR                                 
006138           AND   IDKUNDNR  = :REF-IDKUNDNR                                
006139     END-EXEC                                                             
006140                                                                          
006141     MOVE SQLCODE TO SQLCODE-WS                                           
006142     PERFORM DB2-STATUS-CHECK                                             
006143     .                                                                    
006144     EJECT                                                                
006145 DB2-DCL-OPN-TP5IDDC-CRS  SECTION.                                        
006146                                                                          
006147     MOVE 000100  TO GOOD-SQLCODECODES                                    
006148                                                                          
006149     EXEC SQL                                                             
006150         DECLARE TP5IDDC-CRS CURSOR FOR                                   
006151                                                                          
006152           SELECT  IDDC, IDLOPNR_DC                                       
006153                                                                          
006154           FROM    TP5IDDC                                                
006155           WHERE   IDLOPNR_DC = (SELECT IDLOPNR_DC                        
006156                                 FROM TP5IDDC                             
006157                                 WHERE IDDC = :W-IDDC-TP5)                
006158           ORDER BY IDDC                                                  
006159     END-EXEC                                                             
006160                                                                          
006161     MOVE 000100  TO GOOD-SQLCODECODES                                    
006162     EXEC SQL OPEN TP5IDDC-CRS END-EXEC                                   
006163                                                                          
006164     .                                                                    
006165     SKIP3                                                                
006166 DB2-FETCH-TP5IDDC-CRS  SECTION.                                          
006167     SKIP2                                                                
006168     MOVE 000100  TO GOOD-SQLCODECODES                                    
006169     EXEC SQL                                                             
006170         FETCH TP5IDDC-CRS INTO :TP5IDDC-IDDC                             
006171                               ,:TP5IDDC-IDLOPNR-DC                       
006172     END-EXEC                                                             
006173                                                                          
006174     MOVE SQLCODE TO SQLCODE-WS                                           
006175     PERFORM DB2-STATUS-CHECK                                             
006176     .                                                                    
006177     SKIP3                                                                
006178 DB2-CLOSE-TP5IDDC-CRS  SECTION.                                          
006179                                                                          
006180     EXEC SQL CLOSE TP5IDDC-CRS END-EXEC                                  
006181     .                                                                    
006182     EJECT                                                                
006183 DB2-STATUS-CHECK  SECTION.                                               
006184                                                                          
006185     SET SQLCODE-IX TO 1                                                  
006186     SEARCH GOOD-SQLCODE                                                  
006187       AT END                                                             
006188*         STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
006189*         DELIMITED BY SIZE INTO ERROR-TEXT                               
006190          CALL ABEND USING RKOD-ABEND-DB2                                 
006191       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
006192     END-SEARCH                                                           
006193     .                                                                    
006194     EJECT                                                                
006195*    -COPY WY2000P1                                                       
