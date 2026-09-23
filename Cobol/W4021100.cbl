000001 ID DIVISION.                                                             
000002     SKIP2                                                                
000003 PROGRAM-ID.     W4021100.                                                
000004 AUTHOR.         LASSI OLGRENER.                                          
000005 DATE-WRITTEN.   MARS -90.                                                
000006                                                                          
000007     REMARKS.                                                             
000008*                                                                         
000009*    FUNKTION.                                                            
000010*        PROGRAMMET HANTERAR UPPLÄGGNING AV ORDERHUVUD I                  
000011*        ORDERKÖN. REGISRERADE VÄRDEN KONTROLLERAS OCH ÖVRIGA             
000012*        HÄMTAS FRÅN KUNDREGISTRET.                                       
000013*        EFTER UPPLÄGGNING AV GODKÄNT ORDERHUVUD SKER UTHOPP TILL         
000014*        REGISTRERING AV ORDERRADER, ELLER OM FLOH2 = J/Y                 
000015*        TILL 4214                                                        
000016*                                                                         
000017*        PROGRAMMET ÄNDRAT MARS 2007 SÅ KONTROLL SKER OM                  
000018*        TVINGANDE TILLÄGG SKALL GÖRAS.                                   
000019*        KONTROLL SKER MED HJÄLP AV W411OHKK OCH W411TVAG                 
000020*        OM TILLÄGG SKALL GÖRAS SKER UTHOPP TILL 4206 FÖR                 
000021*        TILLÄGGSREGISTRERING                                             
000022*                                                                         
000023*        PROGRAMMET ÄR ETT UPPDATERINGS-MPP. FLERA SUBMODULER             
000024*        ANROPAS PÅ VÄGEN.                                                
000025*        PROGRAMMET UPPDATERAR WLORQI (WDQ2)  ORDERHUVUD                  
000026*                   LÄSER      WLUSEA (WDP7)  KUNDREGISTER                
000027*                   LÄSER      WLGMTA (WDB2)  KUNDREGISTER                
000028*                   LÄSER      WLGMTB (WDB3)  KUNDREGISTER                
000029*                   LÄSER      WLGMTC (WDB5)  KUNDREGISTER                
000030*                   UPPDATERAR WLXXKP (WDR1)  ORDERNUMMERREGISTER         
000031*                   LÄSER              WDM2   KAMPANJREGISTER             
000032*                   LÄSER      WLXXKB (WDR1)  TRANSPORTREGISTER           
000033*                                                                         
000034*    INDATA.                                                              
000035*        TRANSAKTION: W4T211                                              
000036*        MID:         W4I21101                                            
000037*                                                                         
000038*    UTDATA.                                                              
000039*        MOD:         W4O21101                                            
000040                                                                          
000041     EJECT                                                                
000042 ENVIRONMENT DIVISION.                                                    
000043                                                                          
000044 DATA DIVISION.                                                           
000045 WORKING-STORAGE SECTION.                                                 
000046                                                                          
000047*    -- CHECKED BY WY2000                                                 
000048 77  IDPGM                       PIC X(8)   VALUE 'W4021100'.             
000049 77  JA                          PIC X(1)   VALUE 'J'.                    
000050 77  NEJ                         PIC X(1)   VALUE 'N'.                    
000051 77  OBEHORIG                    PIC X(1)   VALUE 'F'.                    
000052 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +0    COMP SYNC.        
000053 77  4206-MOD-LAENGD             PIC S9(4)  VALUE +1089 COMP SYNC.        
000054 77  4212-MOD-LAENGD             PIC S9(4)  VALUE +107  COMP SYNC.        
000056 77  WS-IDKUNDNR                 PIC 9(6)   VALUE ZERO.                   
000057 77  HOPP                        PIC X(1)   VALUE 'N'.                    
000058 77  YES                         PIC X(1)   VALUE 'Y'.                    
000059 77  WS-TEDDI                    PIC X(11)  VALUE SPACE.                  
000060                                                                          
000061 77  ALLT-SW                     PIC X       VALUE 'J'.                   
000062     88  ALLT-OK                             VALUE 'J'.                   
000063                                                                          
000064 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
000065     88  EGEN-MID                            VALUE '4211'.                
000066                                                                          
000067 77  IX                          PIC S9(3)  VALUE ZERO COMP-3.            
000068 77  MAX-IX                      PIC S9(3)  VALUE +7   COMP-3.            
000069 77  MAX-4206-IX                 PIC S9(3)  VALUE +14  COMP-3.            
000070                                                                          
000071 77  FELTEXT                     PIC X(80)  VALUE SPACE.                  
000072                                                                          
000073 01  FILLER PIC X(16)   VALUE 'SPARAREA'.                                 
000074 01  SPAR-AREA.                                                           
000075     03  SPAR-IDTRANS            PIC X(4).                                
000076     03  SPAR-IDDC               PIC X(2).                                
000077                                                                          
000078 01  WS-DATUM-TID                PIC 9(10).                               
000079 01  FILLER REDEFINES WS-DATUM-TID.                                       
000080     03  WS-DATUM                PIC 9(6).                                
000081     03  WS-TID                  PIC 9(4).                                
000082                                                                          
000083 01  WS-TIHHMMSS                 PIC 9(6)    VALUE ZERO.                  
000084 01  FILLER REDEFINES WS-TIHHMMSS.                                        
000085     03  WS-TIHHMM               PIC 9(4).                                
000086     03  FILLER                  PIC 9(2).                                
000087                                                                          
000088 01  WS-TIREGDAT-9KOMPL          PIC 9(9)    VALUE ZERO.                  
000089     EJECT                                                                
000090*    --- VALID IDDC CODES                                                 
000091*                                                                         
000092*01  -COPY WWDCKONS                                                       
000093                                                                          
000094 01  TEST-IDDISTR                PIC S9(5)   COMP-3.                      
000095                                                                          
000096 01  FILLER REDEFINES TEST-IDDISTR.                                       
000097*    03 -COPY WWDIST03                                                    
000098     EJECT                                                                
000099 01  FILLER REDEFINES TEST-IDDISTR.                                       
000100*    03 -COPY WWDIST15                                                    
000101     EJECT                                                                
000102 01  FILLER REDEFINES TEST-IDDISTR.                                       
000103*    03 -COPY WWDIST20                                                    
000104 01  FILLER REDEFINES TEST-IDDISTR.                                       
000105*     ----DISTR-DEALER-PRICE----                                          
000106*    03 -COPY WWDIST79                                                    
000107     EJECT                                                                
000108                                                                          
000109*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
000110 01  GENERELLA-SUBPROGRAM.                                                
000111     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
000112     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000113     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000114     03  WSECURIT                PIC X(8)    VALUE 'WSECURIT'.            
000115     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
000116     SKIP3                                                                
000117 01  GEMENSAMMA-SUBPROGRAM.                                               
000118     03  W411OHFK                PIC X(8)    VALUE 'W411OHFK'.            
000119*        FORMELLA KONTROLLER                                              
000120     03  W411KREG                PIC X(8)    VALUE 'W411KREG'.            
000121*        LÄSNING AV KUNDREGISTRET                                         
000122     03  W411OHLK                PIC X(8)    VALUE 'W411OHLK'.            
000123*        LOGISKA KONTROLLER                                               
000124     03  W411ORDN                PIC X(8)    VALUE 'W411ORDN'.            
000125*        KONTROLL OCH UTTAG AV AUTOMATISKT ORDERNUMMER                    
000126     03  W411TRAN                PIC X(8)    VALUE 'W411TRAN'.            
000127*        BESTÄM TRANSPORT                                                 
000128     03  W411OHKK                PIC X(8)    VALUE 'W411OHKK'.            
000129*        KOSOLIDERINGSKONTROLL                                            
000130                                                                          
000131*    --- PARAMETRAR TILL SUBPROGRAM WSECURIT                              
000132*   -COPY WSECAREA                                                        
000133     EJECT                                                                
000134*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
000135*01 -COPY WMSGINIT                                                        
000136     EJECT                                                                
000137*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
000138*   -COPY WMEDAREA                                                        
000139     EJECT                                                                
000140 01  MESSAGE-CODES.                                                       
000141     03  ERR-OBEHORIG            PIC X(3)    VALUE '405'.                 
000142     03  ERR-DISTRICT-WRONG      PIC X(3)    VALUE '747'.                 
000143     03  ERR-ORDER-FINNS         PIC X(3)    VALUE '065'.                 
000144     03  ERR-UPPLYSTA-FEL        PIC X(3)    VALUE '001'.                 
000145     03  ERR-SAKNAS-KREG         PIC X(3)    VALUE '063'.                 
000146     03  ERR-TRANSPORT-FEL       PIC X(3)    VALUE '087'.                 
000147     03  ERR-KAMPANJ-FEL         PIC X(3)    VALUE '157'.                 
000148     03  ERR-KUND-SPAERRAD       PIC X(3)    VALUE '213'.                 
000149     03  ERR-MOMS-REGNR-FEL      PIC X(3)    VALUE '223'.                 
000150     03  ERR-SAKNAS-BET          PIC X(3)    VALUE '145'.                 
000151     EJECT                                                                
000152*    --- PARAMETRAR TILL GEMENSAMMA SUBPROGRAM                            
000153 01 FILLER                       PIC X(8)    VALUE 'W411OHFK'.            
000154*   -COPY W411OHFK                                                        
000155     EJECT                                                                
000156 01 FILLER                       PIC X(8)    VALUE 'W411KREG'.            
000157*   -COPY W411KREG                                                        
000158     EJECT                                                                
000159 01 FILLER                       PIC X(8)    VALUE 'W411OHLK'.            
000160*   -COPY W411OHLK                                                        
000161     EJECT                                                                
000162 01 FILLER                       PIC X(8)    VALUE 'W411ORDN'.            
000163*   -COPY W411ORDN                                                        
000164     EJECT                                                                
000165 01 FILLER                       PIC X(8)    VALUE 'W411TRAN'.            
000166*   -COPY W411TRAN                                                        
000167     EJECT                                                                
000168 01 FILLER                       PIC X(8)    VALUE 'W411OHKK'.            
000169*   -COPY W411OHKK                                                        
000170*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
000171 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
000172     SKIP3                                                                
000173*01  MID -COPY W4I21101                                                   
000174     EJECT                                                                
000175 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
000176     SKIP3                                                                
000177*01  -COPY WMSGAREA                                                       
000178     EJECT                                                                
000179*    03  MOD -COPY W4O21101 -RED MSG-AREA.                                
000180     EJECT                                                                
000181*    03  -COPY W4O20601 -PRE 4206- -RED MSG-AREA.                         
000182     EJECT                                                                
000183*    03  -COPY W4O21201 -PRE 4212- -RED MSG-AREA.                         
000184     EJECT                                                                
000185*    03  -COPY W4O21401 -PRE 4214- -RED MSG-AREA.                         
000186     EJECT                                                                
000187 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
000188     SKIP3                                                                
000189*01  -COPY WMFSAREA                                                       
000190     EJECT                                                                
000191*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
000192 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000193                                                                          
000194 01  NYCKLAR-TILL-DLI.                                                    
000195     03  W-WDQ2CSEQ-X.                                                    
000196         05  W-IDDISTR           PIC S9(5)   VALUE +0 COMP-3.             
000197         05  W-IDKUNDNR          PIC S9(7)   VALUE +0 COMP-3.             
000198         05  W-IDKUNDRF.                                                  
000199           07  W-IDORDNR         PIC 9(7)    VALUE ZERO.                  
000200           07  FILLER            PIC X(3)    VALUE SPACE.                 
000201                                                                          
000202     03  W-IDORDER-X.                                                     
000203         05  W-IDORDER           PIC S9(7)   VALUE +0 COMP-3.             
000204                                                                          
000205     03  W-IDDC-X.                                                        
000206         05  W-IDDC              PIC X(2).                                
000207*                                                                         
000208     03  W-IDGMT-X.                                                       
000209         05  W-IDDISTR-WDB2      PIC S9(5) VALUE ZERO COMP-3.             
000210         05  W-IDKUNDNR-WDB2     PIC S9(7) VALUE ZERO COMP-3.             
000211*                                                                         
000212     03  W-IDGMT-MIN-X.                                                   
000213         05  W-IDDISTR-WDB2-MIN  PIC S9(5) VALUE ZERO COMP-3.             
000214         05  W-IDKUNDNR-WDB2-MIN PIC S9(7) VALUE ZERO COMP-3.             
000215*                                                                         
000216     03  W-IDGMT-MAX-X.                                                   
000217         05  W-IDDISTR-WDB2-MAX  PIC S9(5) VALUE ZERO COMP-3.             
000218         05  W-IDKUNDNR-WDB2-MAX PIC S9(7) VALUE ZERO COMP-3.             
000219     EJECT                                                                
000220*    --- STATUS-KOD FRÅN IMS                                              
000230 01  STATUS-WS                   PIC XX.                                  
000231     88  SEGMENT-FINNS                       VALUE '  '.                  
000232     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
000233     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
000234     SKIP2                                                                
000235 01  GODK-STATUSKODER.                                                    
000236     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000237     SKIP3                                                                
000238 01  SSA1                        PIC X(64).                               
000239 01  SSA2                        PIC X(64).                               
000240     EJECT                                                                
000241*    --- IMS FUNKTIONSKODER                                               
000242*01  -COPY W0003                                                          
000243     EJECT                                                                
000244*    ---  DLI INPUT-OUTPUT AREA                                           
000245 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
000246     SKIP3                                                                
000247 01  FILLER                      PIC X(16)   VALUE 'WDQ201-AREA'.         
000248 01  DLI-IO-AREA-OHUV.                                                    
000249     03  WLORQI01.                                                        
000250*        05  -COPY WDQ201                                                 
000251     EJECT                                                                
000252 01  FILLER                      PIC X(16)   VALUE 'WDQ212-AREA'.         
000253 01  DLI-IO-AREA-ARB.                                                     
000254     03  WLORQI12.                                                        
000255*        05  -COPY WDQ212                                                 
000256     EJECT                                                                
000257 01  FILLER                      PIC X(16)   VALUE 'WDB201-AREA'.         
000258 01  DLI-IO-AREA-WDB201.                                                  
000259     03  WLGMTA01.                                                        
000260*        05  -COPY WDB201                                                 
000261     EJECT                                                                
000262 01  FILLER                  PIC X(16)  VALUE 'P-TO-P-SW 4297'.           
000263     SKIP3                                                                
000264 01  4297-MSG-IO-AREA.                                                    
000265     03  4297-LL               PIC S9(4)  VALUE +0 COMP SYNC.             
000266     03  4297-Z1               PIC X.                                     
000267     03  4297-Z2               PIC X.                                     
000268     03  4297-TRANSKOD         PIC X(8)   VALUE 'W4T297X '.               
000269     03  4297-IDTRANS          PIC X(4)   VALUE '4211'.                   
000270     03  4297-SPRAK            PIC X.                                     
000271*    03 -COPY W4I29701  -PRE 4297-                                        
000272                                                                          
000273 LINKAGE SECTION.                                                         
000274                                                                          
000275*01  -COPY W0009      -PRE MSG-                                           
000276*01  -COPY W0009      -PRE 4297-                                          
000277     EJECT                                                                
000278 01  USEA-PCB                    PIC X.                                   
000279     SKIP2                                                                
000280*01  -COPY W0008      -PRE ORQL-                                          
000281     05  FILLER                  PIC X.                                   
000282     EJECT                                                                
000283*01  -COPY W0008      -PRE ORQI-                                          
000284     05  FILLER                  PIC X.                                   
000285     EJECT                                                                
000286*01  -COPY W0008      -PRE WDB2-                                          
000287     05  FILLER                  PIC X.                                   
000288     EJECT                                                                
000289 01  KREG-GMTA-PCB               PIC X.                                   
000290 01  KREG-GMTB-PCB               PIC X.                                   
000300 01  KREG-GMTC-PCB               PIC X.                                   
000301 01  KREG-BETC-PCB               PIC X.                                   
000302 01  OHLK-WDM2-PCB               PIC X.                                   
000303 01  OHLK-WDB6-PCB               PIC X.                                   
000304 01  ORDN-XXKP-PCB               PIC X.                                   
000305 01  ORDN-ORQL-PCB               PIC X.                                   
000306 01  ORDN-PROC-PCB               PIC X.                                   
000307 01  ORDN-ORQI-PCB               PIC X.                                   
000308 01  TRAN-XXKB-PCB               PIC X.                                   
000309 01  SAP-SAPC-PCB                PIC X.                                   
000310                                                                          
000311 01  OHKK-WDQ2-PCB               PIC X.                                   
000312 01  OHKK-WDQ2-UPD-PCB           PIC X.                                   
000313 01  OHKK-WDQ2C-PCB              PIC X.                                   
000314 01  OHKK-GMTA-PCB               PIC X.                                   
000315 01  OHKK-GMTB-PCB               PIC X.                                   
000316 01  OHKK-GMTC-PCB               PIC X.                                   
000317 01  OHKK-BETC-PCB               PIC X.                                   
000318 01  OHKK-WDB2-PCB               PIC X.                                   
000319 01  OHKK-WDB3-PCB               PIC X.                                   
000320 01  OHKK-WDB5-PCB               PIC X.                                   
000321 01  OHKK-WDP7-PCB               PIC X.                                   
000322 01  OHKK-XXKB-PCB               PIC X.                                   
000323     EJECT                                                                
000324 PROCEDURE DIVISION  USING MSG-PCB 4297-PCB USEA-PCB ORQL-PCB             
000325                           ORQI-PCB WDB2-PCB                              
000326                     KREG-GMTA-PCB KREG-GMTB-PCB KREG-GMTC-PCB            
000327                     KREG-BETC-PCB                                        
000328                     OHLK-WDM2-PCB OHLK-WDB6-PCB                          
000329                     ORDN-XXKP-PCB ORDN-ORQL-PCB ORDN-PROC-PCB            
000330                     ORDN-ORQI-PCB                                        
000331                     TRAN-XXKB-PCB                                        
000332                     SAP-SAPC-PCB                                         
000333                     OHKK-WDQ2-PCB OHKK-WDQ2-UPD-PCB                      
000334                     OHKK-WDQ2C-PCB                                       
000335                     OHKK-GMTA-PCB OHKK-GMTB-PCB                          
000336                     OHKK-GMTC-PCB OHKK-BETC-PCB OHKK-WDB2-PCB            
000337                     OHKK-WDB3-PCB OHKK-WDB5-PCB OHKK-WDP7-PCB            
000338                     OHKK-XXKB-PCB.                                       
000339 MAIN SECTION.                                                            
000340     ENTRY 'DLITCBL' USING MSG-PCB 4297-PCB USEA-PCB ORQL-PCB             
000341                                   ORQI-PCB WDB2-PCB                      
000342                     KREG-GMTA-PCB KREG-GMTB-PCB KREG-GMTC-PCB            
000343                     KREG-BETC-PCB                                        
000344                     OHLK-WDM2-PCB OHLK-WDB6-PCB                          
000345                     ORDN-XXKP-PCB ORDN-ORQL-PCB ORDN-PROC-PCB            
000346                     ORDN-ORQI-PCB                                        
000347                     TRAN-XXKB-PCB                                        
000348                     SAP-SAPC-PCB                                         
000349                     OHKK-WDQ2-PCB OHKK-WDQ2-UPD-PCB                      
000350                     OHKK-WDQ2C-PCB                                       
000351                     OHKK-GMTA-PCB OHKK-GMTB-PCB                          
000352                     OHKK-GMTC-PCB OHKK-BETC-PCB OHKK-WDB2-PCB            
000353                     OHKK-WDB3-PCB OHKK-WDB5-PCB OHKK-WDP7-PCB            
000354                     OHKK-XXKB-PCB.                                       
000355     EJECT                                                                
000356     PERFORM IMS-GET-MSG                                                  
000357     IF SEGMENT-FINNS                                                     
000358        PERFORM A-INIT                                                    
000359        IF ALLT-OK                                                        
000360           PERFORM B-KOLLA-O-KOMPLETTERA-INDATA                           
000361           IF ALLT-OK                                                     
000362              IF OHKK-IDORDER > ZERO                                      
000363                 PERFORM S02-HOPPA-TILL-TILLAGGSREG                       
000364              ELSE                                                        
000365                 IF MID-FLOH2 = JA OR YES                                 
000366                    PERFORM C-BESTAM-ORDERNUMMER                          
000367                    PERFORM D-SKAPA-ORDERHUVUD                            
000368                    IF ALLT-OK                                            
000369                       PERFORM E-HOPPA-TILL-ORDERHUVUD2                   
000370                    END-IF                                                
000371                 ELSE                                                     
000372                    PERFORM C-BESTAM-ORDERNUMMER                          
000373                    PERFORM D-SKAPA-ORDERHUVUD                            
000374                    IF ALLT-OK                                            
000375                      IF MID-KDROPACK = 'L'                               
000376                         PERFORM H-STARTA-BIPACKNINGEN                    
000377                      ELSE                                                
000378                         PERFORM S01-HOPPA-TILL-RADREGISTRERING           
000379                      END-IF                                              
000380                    END-IF                                                
000381                 END-IF                                                   
000382              END-IF                                                      
000383           END-IF                                                         
000384        END-IF                                                            
000385        IF HOPP = NEJ                                                     
000386           PERFORM Z-FINIT                                                
000387           MOVE MAX-MOD-LAENGD TO MSG-KVLL                                
000388           PERFORM IMS-INSERT-MSG                                         
000389        END-IF                                                            
000390     END-IF                                                               
000391                                                                          
000392     MOVE +0 TO RETURN-CODE                                               
000393     GOBACK                                                               
000394     .                                                                    
000395     EJECT                                                                
000396 A-INIT SECTION.                                                          
000397                                                                          
000398     MOVE SPACE                TO MED-IDMFSFEL                            
000399     MOVE JA                   TO ALLT-SW                                 
000400     MOVE NEJ                  TO HOPP                                    
000401                                                                          
000402     IF MSG-DUBBLA-TRANSKODER                                             
000403       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I21101                 
000404       MOVE MSG-IDTRANS-2      TO MFS-IDTRANS                             
000405       MOVE MSG-KDMFSFOR-2     TO MFS-KDMFSFOR                            
000406     ELSE                                                                 
000407       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I21101                  
000408       MOVE MSG-IDTRANS-1      TO MFS-IDTRANS                             
000409       MOVE MSG-KDMFSFOR-1     TO MFS-KDMFSFOR                            
000410     END-IF                                                               
000411                                                                          
000412     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
000413     MOVE MSG-IDPFK            TO MFS-IDPFK                               
000414     MOVE MFS-IDTRANS          TO W-IDTRANS                               
000415                                                                          
000416     MOVE LOW-VALUE            TO MSG-AREA                                
000417     MOVE 'W4O211N1'           TO MFS-IDMOD                               
000418     MOVE '4211'               TO MOD-IDTRANS                             
000419     MOVE MFS-RENSA-FAELT      TO MOD-TEMFSFEL MOD-TEMFSINF               
000420     COMPUTE MAX-MOD-LAENGD = LENGTH OF MOD-W4O21101 + 4                  
000421                                                                          
000422     IF ENGLISH-TEXT                                                      
000423       MOVE 'GB '              TO MED-IDSKYLT                             
000424     ELSE                                                                 
000425       MOVE 'S  '              TO MED-IDSKYLT                             
000426     END-IF                                                               
000427     EJECT                                                                
000428     IF NOT EGEN-MID                                                      
000429        MOVE NEJ               TO ALLT-SW                                 
000430        PERFORM MFS-RENSA-BILD                                            
000431     ELSE                                                                 
000432        PERFORM AA-KOLLA-BEHORIGHET                                       
000433     END-IF                                                               
000434     .                                                                    
000435     EJECT                                                                
000436                                                                          
000437 AA-KOLLA-BEHORIGHET SECTION.                                             
000438                                                                          
000439     IF MID-IDDISTR NUMERIC AND MID-IDDISTR > ZERO                        
000440        MOVE MSG-SIGNON-USERID    TO SEC-IDUSER                           
000441        MOVE '4211'               TO SEC-IDTRANS                          
000442        MOVE MID-IDDISTR          TO SEC-IDKEY                            
000443                                                                          
000444        CALL WSECURIT USING SEC-IDUSER                                    
000445                            SEC-IDTRANS                                   
000446                            SEC-IDKEY                                     
000447                            SEC-KDSVAR                                    
000448                                                                          
000449        IF SEC-KDSVAR = OBEHORIG                                          
000450           MOVE MFS-NUM-FAELT-FEL TO MOD-IDDISTR-ATTR                     
000451           MOVE ERR-OBEHORIG      TO MED-IDMFSFEL                         
000452           MOVE NEJ               TO ALLT-SW                              
000453        END-IF                                                            
000454     ELSE                                                                 
000455        MOVE OBEHORIG             TO SEC-KDSVAR                           
000456     END-IF                                                               
000457     .                                                                    
000458     EJECT                                                                
000459                                                                          
000460 B-KOLLA-O-KOMPLETTERA-INDATA SECTION.                                    
000461                                                                          
000462     PERFORM BA-KONTROLLERA-FORMELLA-FEL                                  
000463     IF ALLT-OK                                                           
000464                                                                          
000465        PERFORM BB-KONTROLLERA-OM-ORDER-FINNS                             
000466        IF ALLT-OK                                                        
000467                                                                          
000468           PERFORM BC-LAS-KUNDREGISTRET                                   
000469           IF ALLT-OK                                                     
000470                                                                          
000471              PERFORM BD-KONTROLLERA-LOGISKA-FEL                          
000472              IF ALLT-OK                                                  
000473                                                                          
000474                 PERFORM BE-BESTAM-TRANSPORT                              
000475                 IF ALLT-OK                                               
000476                                                                          
000477                    PERFORM BF-KONTROLLERA-KOSOLIDERING                   
000478                 END-IF                                                   
000479              END-IF                                                      
000480           END-IF                                                         
000481        END-IF                                                            
000482     END-IF                                                               
000483     .                                                                    
000484     EJECT                                                                
000485                                                                          
000486 BA-KONTROLLERA-FORMELLA-FEL SECTION.                                     
000487                                                                          
000488     MOVE 'IMS '               TO OHFK-IDSYSTEM                           
000489     MOVE MID-IDDISTR          TO OHFK-IDDISTR                            
000490     MOVE MID-IDKUNDNR         TO OHFK-IDKUNDNR                           
000491     MOVE MID-IDORDNR          TO OHFK-IDORDNR                            
000492     MOVE MID-KDORDKL          TO OHFK-KDORDKL                            
000493     MOVE MID-KDFRAKT          TO OHFK-KDFRAKT                            
000494     MOVE MID-FLAUTFAK         TO OHFK-FLAUTFAK                           
000495     IF MID-FLAUTFAK = 'Y'                                                
000496        MOVE 'J'               TO MID-FLAUTFAK                            
000497     END-IF                                                               
000498     MOVE NEJ                  TO OHFK-FLFORBI                            
000499                                  OHFK-FLORDSPE                           
000500                                  OHFK-FLVORKO                            
000501     MOVE JA                   TO OHFK-FLLSBOK                            
000502     MOVE NEJ                  TO OHFK-FLAUTPAC                           
000503     MOVE MID-FLRESTN          TO OHFK-FLRESTN                            
000504     IF MID-FLRESTN = 'Y'                                                 
000505        MOVE 'J'               TO MID-FLRESTN                             
000506     END-IF                                                               
000507     MOVE MID-IDBIPREF         TO OHFK-IDBIPREF                           
000508     MOVE MID-IDFTG            TO OHFK-IDFTG                              
000509     MOVE MID-IDKONTO          TO OHFK-IDKONTO                            
000510     MOVE MID-IDANALYS         TO OHFK-IDANALYS                           
000511     MOVE MID-IDKST            TO OHFK-IDKST                              
000512     MOVE MID-IDKAMPRF         TO OHFK-IDKAMPRF                           
000513     MOVE MID-KDFAKTYP         TO OHFK-KDFAKTYP                           
000514     MOVE MID-KDROPACK         TO OHFK-KDROPACK                           
000515     MOVE MID-KDTPOTYP         TO OHFK-KDTPOTYP                           
000516     MOVE MID-TITPO            TO OHFK-TITPO                              
000517     ACCEPT OHFK-TIREGDAT      FROM DATE                                  
000518     ACCEPT OHFK-TIHHMM        FROM TIME                                  
000519     MOVE MID-TIRFS-DAT        TO OHFK-TIRFSDAT                           
000520     MOVE MID-TIRFS-TID        TO OHFK-TIRFSTID                           
000521                                                                          
000522     MOVE ALL '+'              TO OHFK-IDDC                               
000523                                  OHFK-IDSKYLT                            
000524                                  OHFK-KDTULLVE                           
000525                                  OHFK-KDVRINFO                           
000526                                  OHFK-TIFORDAT                           
000527                                                                          
000528     CALL W411OHFK USING OHFK-W411OHFK                                    
000529     PERFORM BAA-KOLLA-FEL-FK                                             
000530                                                                          
000531     IF MID-FLOH2 NOT = '+'                                               
000532       IF MID-FLOH2 = 'J' OR 'Y'                                          
000533          CONTINUE                                                        
000534       ELSE                                                               
000535          MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                           
000536          MOVE MFS-ALFA-FAELT-FEL  TO MOD-FLOH2-ATTR                      
000537          MOVE NEJ                 TO ALLT-SW                             
000538       END-IF                                                             
000539     END-IF                                                               
000540     .                                                                    
000541     EJECT                                                                
000542 BAA-KOLLA-FEL-FK SECTION.                                                
000543                                                                          
000544     IF OHFK-IDDISTR-OK = NEJ                                             
000545        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
000546        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDDISTR-ATTR                      
000547        MOVE NEJ                 TO ALLT-SW                               
000548     ELSE                                                                 
000549        MOVE OHFK-IDDISTR        TO W-IDDISTR                             
000550                                    TEST-IDDISTR                          
000551     END-IF                                                               
000552                                                                          
000553     IF DIST79-DEALER-PRICE                                               
000554        IF ENGLISH-TEXT                                                   
000555           MOVE 'DEALERPRICE'    TO MOD-TEDDI                             
000556                                    WS-TEDDI                              
000557        ELSE                                                              
000558           MOVE '    ÅF PRIS'    TO MOD-TEDDI                             
000559                                    WS-TEDDI                              
000560        END-IF                                                            
000570     ELSE                                                                 
000571        MOVE SPACES              TO MOD-TEDDI                             
000572                                    WS-TEDDI                              
000573     END-IF                                                               
000574                                                                          
000575     IF OHFK-IDKUNDNR-OK = NEJ                                            
000576        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
000577        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKUNDNR-ATTR                     
000578        MOVE NEJ                 TO ALLT-SW                               
000579     ELSE                                                                 
000580        IF MID-IDKUNDNR = ALL '+'                                         
000581           MOVE ZERO             TO MOD-IDKUNDNR                          
000582                                    W-IDKUNDNR                            
000583        ELSE                                                              
000584           MOVE OHFK-IDKUNDNR    TO W-IDKUNDNR                            
000585        END-IF                                                            
000586     END-IF                                                               
000587                                                                          
000588     IF OHFK-IDORDNR-OK = NEJ                                             
000589        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
000590        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDORDNR-ATTR                      
000591        MOVE NEJ                 TO ALLT-SW                               
000592     ELSE                                                                 
000593        IF MID-IDORDNR = ALL '+'                                          
000594           MOVE ZERO             TO W-IDORDNR                             
000595           MOVE MFS-RENSA-FAELT  TO MOD-IDORDNR                           
000596        ELSE                                                              
000597           MOVE OHFK-IDORDNR     TO W-IDORDNR                             
000598        END-IF                                                            
000599     END-IF                                                               
000600                                                                          
000601     IF OHFK-KDORDKL-OK = NEJ                                             
000602        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
000603        MOVE MFS-NUM-FAELT-FEL   TO MOD-KDORDKL-ATTR                      
000604        MOVE NEJ                 TO ALLT-SW                               
000605     END-IF                                                               
000606                                                                          
000607     IF OHFK-KDFRAKT-OK = NEJ                                             
000608        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
000609        MOVE MFS-NUM-FAELT-FEL   TO MOD-KDFRAKT-ATTR                      
000610        MOVE NEJ                 TO ALLT-SW                               
000611     END-IF                                                               
000612                                                                          
000613     IF OHFK-FLAUTFAK-OK = NEJ                                            
000614        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
000615        MOVE MFS-ALFA-FAELT-FEL  TO MOD-FLAUTFAK-ATTR                     
000616        MOVE NEJ                 TO ALLT-SW                               
000617     END-IF                                                               
000618                                                                          
000619     IF OHFK-FLRESTN-OK = NEJ                                             
000620        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
000621        MOVE MFS-ALFA-FAELT-FEL  TO MOD-FLRESTN-ATTR                      
000622        MOVE NEJ                 TO ALLT-SW                               
000623     END-IF                                                               
000624                                                                          
000625     IF OHFK-IDFTG-OK = NEJ                                               
000626        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
000627        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDFTG-ATTR                        
000628        MOVE NEJ                 TO ALLT-SW                               
000629     END-IF                                                               
000630                                                                          
000631     IF OHFK-IDBIPREF-OK = NEJ                                            
000632        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
000633        MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDBIPREF-ATTR                     
000634        MOVE NEJ                 TO ALLT-SW                               
000635     END-IF                                                               
000636                                                                          
000637     IF OHFK-IDKONTO-OK = NEJ                                             
000638        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
000639        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKONTO-ATTR                      
000640        MOVE NEJ                 TO ALLT-SW                               
000641     END-IF                                                               
000642                                                                          
000643     IF OHFK-IDANALYS-OK = NEJ                                            
000644        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
000645        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDANALYS-ATTR                     
000646        MOVE NEJ                 TO ALLT-SW                               
000647     END-IF                                                               
000648                                                                          
000649     IF OHFK-IDKST-OK = NEJ                                               
000650        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
000651        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKST-ATTR                        
000652        MOVE NEJ                 TO ALLT-SW                               
000653     END-IF                                                               
000654                                                                          
000655     IF OHFK-IDKAMPRF-OK = NEJ                                            
000656        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
000657        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKAMPRF-ATTR                     
000658        MOVE NEJ                 TO ALLT-SW                               
000659     END-IF                                                               
000660                                                                          
000661     IF OHFK-KDFAKTYP-OK = NEJ                                            
000662        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
000663        MOVE MFS-ALFA-FAELT-FEL  TO MOD-KDFAKTYP-ATTR                     
000664        MOVE NEJ                 TO ALLT-SW                               
000665     END-IF                                                               
000666                                                                          
000667     IF OHFK-KDROPACK-OK = NEJ                                            
000668        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
000669        MOVE MFS-ALFA-FAELT-FEL  TO MOD-KDROPACK-ATTR                     
000670        MOVE NEJ                 TO ALLT-SW                               
000671     END-IF                                                               
000672                                                                          
000673     IF OHFK-KDTPOTYP-OK = NEJ                                            
000674        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
000675        MOVE MFS-NUM-FAELT-FEL   TO MOD-KDTPOTYP-ATTR                     
000676        MOVE NEJ                 TO ALLT-SW                               
000677     END-IF                                                               
000678                                                                          
000679     IF OHFK-TITPO-OK = NEJ                                               
000680        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
000681        MOVE MFS-NUM-FAELT-FEL   TO MOD-TITPO-ATTR                        
000682        MOVE NEJ                 TO ALLT-SW                               
000683     END-IF                                                               
000684                                                                          
000685     IF OHFK-TIRFSDAT-OK = NEJ                                            
000686        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
000687        MOVE MFS-NUM-FAELT-FEL   TO MOD-TIRFS-DAT-ATTR                    
000688        MOVE NEJ                 TO ALLT-SW                               
000689     END-IF                                                               
000690                                                                          
000691     IF OHFK-TIRFSTID-OK = NEJ                                            
000692        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
000693        MOVE MFS-NUM-FAELT-FEL   TO MOD-TIRFS-TID-ATTR                    
000694        MOVE NEJ                 TO ALLT-SW                               
000695     END-IF                                                               
000696     .                                                                    
000697     EJECT                                                                
000698 BB-KONTROLLERA-OM-ORDER-FINNS SECTION.                                   
000699                                                                          
000700     IF MID-IDORDNR NOT = ALL '+'                                         
000701                                                                          
000702        PERFORM IMS-GU-ORQL-WDQ201                                        
000703                                                                          
000704        IF SEGMENT-FINNS                                                  
000705          IF MID-KDTPOTYP = '1' AND OHUV-KDTPOTYP = +1                    
000706                                                                          
000707             MOVE NEJ          TO OHUV-FLKLAR                             
000708                                                                          
000709             MOVE OHUV-IDORDER TO W-IDORDER                               
000710             PERFORM IMS-GHU-ORQI-WDQ201                                  
000711             PERFORM IMS-REPL-ORQI-WDQ201                                 
000712                                                                          
000713             MOVE WC-CDC-SE        TO W-IDDC                              
000714             PERFORM IMS-GNP-ORQI-WDQ212                                  
000715                                                                          
000716             PERFORM S01-HOPPA-TILL-RADREGISTRERING                       
000717             MOVE NEJ          TO ALLT-SW                                 
000718          ELSE                                                            
000719             MOVE MFS-NUM-FAELT-FEL TO MOD-IDORDNR-ATTR                   
000720             MOVE ERR-ORDER-FINNS    TO MED-IDMFSFEL                      
000721             MOVE NEJ                TO ALLT-SW                           
000722          END-IF                                                          
000723        END-IF                                                            
000724     END-IF                                                               
000725     .                                                                    
000726     EJECT                                                                
000727 BC-LAS-KUNDREGISTRET SECTION.                                            
000728                                                                          
000729     MOVE W-IDDISTR            TO KREG-IDDISTR                            
000730     MOVE W-IDKUNDNR           TO KREG-IDKUNDNR                           
000731     MOVE 'IMS '               TO KREG-IDSYSTEM                           
000732     IF MID-KDTPOTYP > ZERO OR MID-IDKAMPRF > ZERO                        
000733                            OR MID-KDROPACK = 'L'                         
000734        MOVE WC-CDC-SE         TO KREG-IDDC-TVS                           
000735     ELSE                                                                 
000736        MOVE SPACE             TO KREG-IDDC-TVS                           
000737     END-IF                                                               
000738     IF MID-KDFRAKT = ALL '+'                                             
000739        MOVE +0                TO KREG-KDFRAKT-IN                         
000740     ELSE                                                                 
000741        MOVE MID-KDFRAKT       TO KREG-KDFRAKT-IN                         
000742     END-IF                                                               
000743     MOVE MID-KDORDKL          TO KREG-KDORDKL                            
000744     IF MID-KDFAKTYP = ALL '+'                                            
000745        MOVE SPACE             TO KREG-KDFAKTYP-IN                        
000746     ELSE                                                                 
000747        MOVE MID-KDFAKTYP      TO KREG-KDFAKTYP-IN                        
000748     END-IF                                                               
000749                                                                          
000750     MOVE NEJ                  TO KREG-FLVORKO                            
000751                                  KREG-FLVORFK                            
000752                                                                          
000753     CALL W411KREG USING KREG-W411KREG KREG-GMTA-PCB KREG-GMTB-PCB        
000754                                       KREG-GMTC-PCB KREG-BETC-PCB        
000755                                                                          
000756     IF KREG-KDKREDSP = '1'                                               
000757        MOVE ERR-KUND-SPAERRAD TO MED-IDMFSFEL                            
000758        MOVE NEJ                 TO ALLT-SW                               
000759     ELSE                                                                 
000760        IF KREG-IDVAT-OK = NEJ                                            
000761           MOVE ERR-MOMS-REGNR-FEL TO MED-IDMFSFEL                        
000762           MOVE NEJ                TO ALLT-SW                             
000763        ELSE                                                              
000764          IF KREG-IDPARTNR-OK = NEJ                                       
000765            MOVE ERR-SAKNAS-BET      TO MED-IDMFSFEL                      
000766            MOVE NEJ                 TO ALLT-SW                           
000767          END-IF                                                          
000768        END-IF                                                            
000769     END-IF                                                               
000770                                                                          
000771     PERFORM BCA-KOLLA-FEL-KREG                                           
000772     IF KREG-KDTRPKAT = 'C'                                               
000773        MOVE ZERO             TO KREG-IDTRP                               
000774                                 KREG-IDTRP-ALT                           
000775     END-IF                                                               
000776     .                                                                    
000777     EJECT                                                                
000778 BCA-KOLLA-FEL-KREG SECTION.                                              
000779                                                                          
000780     IF KREG-IDDISTR-OK = NEJ                                             
000781        MOVE ERR-SAKNAS-KREG  TO MED-IDMFSFEL                             
000782        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDDISTR-ATTR                      
000783        MOVE NEJ                 TO ALLT-SW                               
000784     END-IF                                                               
000785                                                                          
000786     IF KREG-IDKUNDNR-OK = NEJ                                            
000787        MOVE ERR-SAKNAS-KREG  TO MED-IDMFSFEL                             
000788        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKUNDNR-ATTR                     
000789        MOVE NEJ                 TO ALLT-SW                               
000790     END-IF                                                               
000791                                                                          
000792     IF KREG-KDFRAKT-OK = NEJ                                             
000793        MOVE ERR-SAKNAS-KREG  TO MED-IDMFSFEL                             
000794        MOVE MFS-NUM-FAELT-FEL   TO MOD-KDFRAKT-ATTR                      
000795        MOVE NEJ                 TO ALLT-SW                               
000796     END-IF                                                               
000797                                                                          
000798     .                                                                    
000799     EJECT                                                                
000800 BD-KONTROLLERA-LOGISKA-FEL SECTION.                                      
000801                                                                          
000802     MOVE 'IMS '               TO OHLK-IDSYSTEM                           
000803     MOVE W-IDDISTR            TO OHLK-IDDISTR                            
000804     MOVE W-IDKUNDNR           TO OHLK-IDKUNDNR                           
000805     MOVE W-IDORDNR            TO OHLK-IDORDNR                            
000806     MOVE MID-KDORDKL          TO OHLK-KDORDKL                            
000807     MOVE KREG-IDDC            TO OHLK-IDDC                               
000808     MOVE SPACE                TO OHLK-IDDC-TVS                           
000809     MOVE SEC-KDSVAR           TO OHLK-SEC-KDSVAR                         
000810     MOVE KREG-FLAUTORD        TO OHLK-FLAUTORD                           
000811     IF MID-IDKONTO = ALL '+'                                             
000812        MOVE +0                TO OHLK-IDKONTO                            
000813     ELSE                                                                 
000814        MOVE MID-IDKONTO       TO OHLK-IDKONTO                            
000815     END-IF                                                               
000816     IF MID-IDANALYS = ALL '+'                                            
000817        MOVE SPACE             TO OHLK-IDANALYS                           
000818     ELSE                                                                 
000819        MOVE MID-IDANALYS      TO OHLK-IDANALYS                           
000820     END-IF                                                               
000821     IF MID-IDKST = ALL '+'                                               
000822        MOVE SPACE             TO OHLK-IDKST                              
000823     ELSE                                                                 
000824        MOVE MID-IDKST         TO OHLK-IDKST                              
000825     END-IF                                                               
000826     IF MID-IDFTG = ALL '+'                                               
000827        MOVE ZERO              TO OHLK-IDFTG                              
000828     ELSE                                                                 
000829        MOVE MID-IDFTG         TO OHLK-IDFTG                              
000830     END-IF                                                               
000831     EJECT                                                                
000832     IF MID-IDKAMPRF = ALL '+'                                            
000833        MOVE +0                TO OHLK-IDKAMPRF                           
000834     ELSE                                                                 
000835        MOVE MID-IDKAMPRF      TO OHLK-IDKAMPRF                           
000836     END-IF                                                               
000837     MOVE KREG-FLOKFAK-G       TO OHLK-FLOKFAK-G                          
000838     MOVE KREG-FLOKFAK-N       TO OHLK-FLOKFAK-N                          
000839     MOVE KREG-FLOKFAK-R       TO OHLK-FLOKFAK-R                          
000840     MOVE KREG-FLOKFAK-K       TO OHLK-FLOKFAK-K                          
000841     MOVE NEJ                  TO OHLK-FLORDSPE                           
000842     MOVE NEJ                  TO OHLK-FLVORKO                            
000843     IF MID-KDFAKTYP = ALL '+'                                            
000844        MOVE KREG-KDGENFAK     TO OHLK-KDFAKTYP                           
000845     ELSE                                                                 
000846        MOVE MID-KDFAKTYP      TO OHLK-KDFAKTYP                           
000847     END-IF                                                               
000848     IF MID-KDTPOTYP = ALL '+'                                            
000849        MOVE +0                TO OHLK-KDTPOTYP                           
000850     ELSE                                                                 
000851        MOVE MID-KDTPOTYP      TO OHLK-KDTPOTYP                           
000852     END-IF                                                               
000853     IF MID-TITPO = ALL '+'                                               
000854        MOVE +0                TO OHLK-TITPO                              
000855     ELSE                                                                 
000856        MOVE MID-TITPO         TO OHLK-TITPO                              
000857     END-IF                                                               
000858                                                                          
000859     CALL W411OHLK USING OHLK-W411OHLK OHLK-WDM2-PCB ORDN-XXKP-PCB        
000860                                       KREG-GMTA-PCB                      
000861                                       SAP-SAPC-PCB                       
000862                                       OHLK-WDB6-PCB                      
000863                                                                          
000864     PERFORM BDA-KOLLA-FEL-LK                                             
000865     .                                                                    
000866     EJECT                                                                
000867 BDA-KOLLA-FEL-LK SECTION.                                                
000868                                                                          
000869     IF OHLK-IDDISTR-OK = NEJ                                             
000870        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
000871        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDDISTR-ATTR                      
000872        MOVE NEJ                 TO ALLT-SW                               
000873     END-IF                                                               
000874                                                                          
000875     IF OHLK-IDORDNR-OK = NEJ                                             
000876        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
000877        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDORDNR-ATTR                      
000878        MOVE NEJ                 TO ALLT-SW                               
000879     END-IF                                                               
000880                                                                          
000881     IF OHLK-KDORDKL-OK = NEJ                                             
000882        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
000883        MOVE MFS-NUM-FAELT-FEL   TO MOD-KDORDKL-ATTR                      
000884        MOVE NEJ                 TO ALLT-SW                               
000885     END-IF                                                               
000886                                                                          
000887     IF OHLK-IDFTG-OK = NEJ                                               
000888        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
000889        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDFTG-ATTR                        
000890        MOVE NEJ                 TO ALLT-SW                               
000891     END-IF                                                               
000892                                                                          
000893     IF OHLK-IDKONTO-OK = NEJ                                             
000894        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
000895        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKONTO-ATTR                      
000896        MOVE NEJ                 TO ALLT-SW                               
000897     END-IF                                                               
000898                                                                          
000899     IF OHLK-IDANALYS-OK = NEJ                                            
000900        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
000901        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDANALYS-ATTR                     
000902        MOVE NEJ                 TO ALLT-SW                               
000903     END-IF                                                               
000904                                                                          
000905     IF OHLK-IDKST-OK = NEJ                                               
000906        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
000907        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKST-ATTR                        
000908        MOVE NEJ                 TO ALLT-SW                               
000909     END-IF                                                               
000910                                                                          
000911     IF OHLK-KDFAKTYP-OK = NEJ                                            
000912        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
000913        MOVE MFS-ALFA-FAELT-FEL  TO MOD-KDFAKTYP-ATTR                     
000914        MOVE NEJ                 TO ALLT-SW                               
000915     END-IF                                                               
000916                                                                          
000917     IF OHLK-KDTPOTYP-OK = NEJ                                            
000918        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
000919        MOVE MFS-NUM-FAELT-FEL   TO MOD-KDTPOTYP-ATTR                     
000920        MOVE NEJ                 TO ALLT-SW                               
000921     END-IF                                                               
000922                                                                          
000923     IF OHLK-TITPO-OK = NEJ                                               
000924        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
000925        MOVE MFS-NUM-FAELT-FEL   TO MOD-TITPO-ATTR                        
000926        MOVE NEJ                 TO ALLT-SW                               
000927     END-IF                                                               
000928                                                                          
000929     IF OHLK-IDKAMPRF-OK = NEJ                                            
000930        MOVE ERR-KAMPANJ-FEL  TO MED-IDMFSFEL                             
000931        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKAMPRF-ATTR                     
000932        MOVE NEJ                 TO ALLT-SW                               
000933     END-IF                                                               
000934     .                                                                    
000935     EJECT                                                                
000936 BE-BESTAM-TRANSPORT SECTION.                                             
000937                                                                          
000938     MOVE 'IMS '               TO TRAN-IDSYSTEM                           
000939     MOVE KREG-IDTRP           TO TRAN-IDTRP                              
000940     MOVE OHLK-IDDC            TO TRAN-IDDC                               
000941     MOVE OHLK-KDORDKL         TO TRAN-KDORDKL                            
000942     MOVE KREG-KDTRPKAT        TO TRAN-KDTRPKAT                           
000943     MOVE KREG-KVLEDTIM-0      TO TRAN-KVLEDTIM-0                         
000944     MOVE KREG-KVLEDTIM-1      TO TRAN-KVLEDTIM-1                         
000945     MOVE KREG-KVLEDTIM-2      TO TRAN-KVLEDTIM-2                         
000946     MOVE KREG-KVLEDTIM-3      TO TRAN-KVLEDTIM-3                         
000947     MOVE KREG-KVLEDTIM-4      TO TRAN-KVLEDTIM-4                         
000948     MOVE NEJ                  TO TRAN-FLORDSPE                           
000949     MOVE NEJ                  TO TRAN-FLOVRLEV                           
000950     PERFORM BEA-FIXA-LOKAL-TID                                           
000951     MOVE MSGI-TILOKDAT        TO TRAN-TIREGDAT                           
000952     MOVE MSGI-TILOKTID        TO TRAN-TIHHMM-REG                         
000953     IF MID-TIRFS-DAT = ALL '+'                                           
000954        MOVE +0                TO TRAN-TIRFS                              
000955     ELSE                                                                 
000956        MOVE OHFK-TIRFSDAT     TO WS-DATUM                                
000957        MOVE OHFK-TIRFSTID     TO WS-TID                                  
000958        MOVE WS-DATUM-TID      TO TRAN-TIRFS                              
000959     END-IF                                                               
000960     IF OHLK-KDTPOTYP = +2                                                
000961        MOVE +0                TO TRAN-KDTPOTYP                           
000962     ELSE                                                                 
000963        MOVE OHLK-KDTPOTYP     TO TRAN-KDTPOTYP                           
000964     END-IF                                                               
000965                                                                          
000966     CALL W411TRAN USING TRAN-W411TRAN TRAN-XXKB-PCB                      
000967                                                                          
000968     IF TRAN-KDSVAR = '1'                                                 
000969        MOVE ERR-TRANSPORT-FEL     TO MED-IDMFSFEL                        
000970        MOVE NEJ                   TO ALLT-SW                             
000971     ELSE                                                                 
000972       IF TRAN-KDSVAR = '2' OR '3' OR '4'                                 
000973          MOVE ERR-UPPLYSTA-FEL    TO MED-IDMFSFEL                        
000974          MOVE MFS-NUM-FAELT-FEL   TO MOD-TIRFS-DAT-ATTR                  
000975                                      MOD-TIRFS-TID-ATTR                  
000976          MOVE NEJ                 TO ALLT-SW                             
000977       END-IF                                                             
000978     END-IF                                                               
000979     .                                                                    
000980     EJECT                                                                
000981 BEA-FIXA-LOKAL-TID SECTION.                                              
000982                                                                          
000983     MOVE ALL '+'            TO MSGI-WMSGINIT                             
000984     MOVE '001'              TO MSGI-KDCALL                               
000985     MOVE 'WIDDC   '         TO MSGI-IDUSER                               
000986     MOVE KREG-IDDC          TO MSGI-IDUSER(6:2)                          
000987     MOVE '4211'             TO MSGI-IDTRANS                              
000988     MOVE MSG-LTERM-NAME     TO MSGI-IDLTERM-USER                         
000989                                                                          
000990     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
000991     .                                                                    
000992     EJECT                                                                
000993 BF-KONTROLLERA-KOSOLIDERING SECTION.                                     
000994                                                                          
000995     IF  MID-ADGMT        =  ALL '+' AND                                  
000996         MID-BEGMT        =  ALL '+'                                      
000997         MOVE 1                TO OHKK-KDCALL                             
000998         MOVE 'IMS'            TO OHKK-IDSYSTEM                           
000999         MOVE W-IDDISTR        TO OHKK-IDDISTR                            
001000                                  TEST-IDDISTR                            
001001         MOVE W-IDKUNDNR       TO OHKK-IDKUNDNR                           
001002         MOVE ZERO             TO OHKK-IDORDNR-IN                         
001003         MOVE OHLK-KDORDKL     TO OHKK-KDORDKL                            
001004                                                                          
001005         MOVE KREG-ADBETRAD-1  TO OHKK-ADBETRAD-1                         
001006         MOVE KREG-ADBETRAD-2  TO OHKK-ADBETRAD-2                         
001007         MOVE KREG-ADGMT       TO OHKK-ADGMT                              
001008         MOVE KREG-BEBETRAD-1  TO OHKK-BEBETRAD-1                         
001009         MOVE KREG-BEBETRAD-2  TO OHKK-BEBETRAD-2                         
001010         MOVE KREG-BEGMRK      TO OHKK-BEGMRK                             
001020         MOVE KREG-BEGMT       TO OHKK-BEGMT                              
001030         IF OHFK-FLAUTFAK = '+'                                           
001040            MOVE NEJ           TO OHKK-FLAUTFAK                           
001050         ELSE                                                             
001060            MOVE OHFK-FLAUTFAK TO OHKK-FLAUTFAK                           
001070         END-IF                                                           
001080         MOVE OHFK-FLAUTPAC    TO OHKK-FLAUTPAC                           
001090         IF DIST20-EMBALLAGE                                              
001091            MOVE JA            TO OHKK-FLEMBORD                           
001092         ELSE                                                             
001093            MOVE NEJ           TO OHKK-FLEMBORD                           
001094         END-IF                                                           
001095         MOVE OHFK-FLFORBI     TO OHKK-FLFORBI                            
001096         MOVE OHFK-FLLSBOK     TO OHKK-FLLSBOK                            
001097         MOVE OHFK-FLORDSPE    TO OHKK-FLORDSPE                           
001098         MOVE NEJ              TO OHKK-FLOVRLEV                           
001099         MOVE KREG-FLPRELRO    TO OHKK-FLPRELRO                           
001100         MOVE KREG-FLPRERS     TO OHKK-FLPRERS                            
001101         MOVE OHFK-FLRESTN     TO OHKK-FLRESTN                            
001102         IF MID-FLRESTN = ALL '+'                                         
001103            IF KREG-FLRESTN = YES                                         
001104               MOVE JA         TO OHKK-FLRESTN                            
001105            ELSE                                                          
001106               MOVE KREG-FLRESTN TO OHKK-FLRESTN                          
001107            END-IF                                                        
001108         ELSE                                                             
001109            MOVE MID-FLRESTN   TO OHKK-FLRESTN                            
001110         END-IF                                                           
001111         MOVE OHFK-FLVORKO     TO OHKK-FLVORKO                            
001112                                                                          
001113         IF OHFK-IDANALYS = ALL '+'                                       
001114            MOVE SPACE         TO OHKK-IDANALYS                           
001115         ELSE                                                             
001116            MOVE OHFK-IDANALYS TO OHKK-IDANALYS                           
001117         END-IF                                                           
001118         IF OHFK-IDBIPREF = ALL '+'                                       
001119            MOVE SPACE         TO OHKK-IDBIPREF                           
001120         ELSE                                                             
001121            MOVE OHFK-IDBIPREF TO OHKK-IDBIPREF                           
001122         END-IF                                                           
001123         MOVE OHLK-IDDC-TVS    TO OHKK-IDDC-TVS                           
001124         MOVE KREG-IDDEPOT     TO OHKK-IDDEPOT                            
001125         MOVE ZERO             TO OHKK-IDDEPT                             
001126         MOVE OHLK-IDFTG       TO OHKK-IDFTG                              
001127         MOVE OHLK-IDKAMPRF    TO OHKK-IDKAMPRF                           
001128         MOVE OHLK-IDKONTO     TO OHKK-IDKONTO                            
001129         MOVE OHLK-IDKST       TO OHKK-IDKST                              
001130         MOVE KREG-IDRFTAB     TO OHKK-IDRFTAB                            
001131         MOVE KREG-IDROUTE     TO OHKK-IDROUTE                            
001132         MOVE OHFK-IDSKYLT     TO OHKK-IDSKYLT                            
001133         MOVE KREG-IDZON       TO OHKK-IDZON                              
001134         MOVE OHLK-KDFAKTYP    TO OHKK-KDFAKTYP                           
001135         IF MID-KDFRAKT = ALL '+'                                         
001136            MOVE KREG-KDFRAKT  TO OHKK-KDFRAKT                            
001137         ELSE                                                             
001138            MOVE MID-KDFRAKT   TO OHKK-KDFRAKT                            
001139         END-IF                                                           
001140         IF KREG-KDORDING = +3                                            
001141            MOVE KREG-KDORDING     TO OHKK-KDORDING                       
001142         ELSE                                                             
001143            IF OHLK-KDTPOTYP > +0 OR OHLK-IDKAMPRF > +0                   
001144               MOVE +2             TO OHKK-KDORDING                       
001145            ELSE                                                          
001146               MOVE KREG-KDORDING  TO OHKK-KDORDING                       
001147            END-IF                                                        
001148         END-IF                                                           
001149         MOVE ZERO             TO OHKK-KDVRINFO                           
001150         MOVE OHLK-KDTPOTYP    TO OHKK-KDTPOTYP                           
001151         MOVE KREG-KDTULLVE    TO OHKK-KDTULLVE                           
001152         MOVE KREG-KVDAGAR-DOW TO OHKK-KVDAGAR-DOW                        
001153         MOVE KREG-RESLATT     TO OHKK-RESLATT                            
001154         MOVE OHLK-TITPO       TO OHKK-TITPO                              
001155         MOVE NEJ              TO OHKK-FLSOFT                             
001156                                                                          
001157         CALL W411OHKK USING OHKK-W411OHKK OHKK-WDQ2-PCB                  
001158                             OHKK-WDQ2-UPD-PCB                            
001159                             OHKK-WDQ2C-PCB                               
001160                             OHKK-GMTA-PCB                                
001161                             OHKK-GMTB-PCB OHKK-GMTC-PCB                  
001162                             OHKK-BETC-PCB OHKK-WDB2-PCB                  
001163                             OHKK-WDB3-PCB OHKK-WDB5-PCB                  
001164                             OHKK-WDP7-PCB OHKK-XXKB-PCB                  
001165                                                                          
001166     ELSE                                                                 
001167        MOVE  ZERO             TO OHKK-IDORDER                            
001168     END-IF                                                               
001169     .                                                                    
001170     EJECT                                                                
001171                                                                          
001172                                                                          
001173                                                                          
001174 C-BESTAM-ORDERNUMMER SECTION.                                            
001175                                                                          
001176     MOVE 'IMS '               TO ORDN-IDSYSTEM                           
001177                                                                          
001178     MOVE W-IDDISTR            TO ORDN-IDDISTR                            
001179     MOVE W-IDKUNDNR           TO ORDN-IDKUNDNR                           
001180                                                                          
001181     IF MID-IDORDNR = ALL '+'                                             
001182        MOVE ZERO              TO ORDN-IDORDNR-IN                         
001183     ELSE                                                                 
001184        MOVE W-IDORDNR         TO ORDN-IDORDNR-IN                         
001185     END-IF                                                               
001186                                                                          
001187     CALL W411ORDN USING ORDN-W411ORDN ORDN-XXKP-PCB ORDN-ORQL-PCB        
001188                                       ORDN-PROC-PCB ORDN-ORQI-PCB        
001189                                                                          
001190     MOVE ORDN-IDORDNR-UT           TO W-IDORDNR                          
001191     .                                                                    
001192     EJECT                                                                
001193 D-SKAPA-ORDERHUVUD SECTION.                                              
001194                                                                          
001195     PERFORM DA-REDIGERA-OHUV                                             
001196     PERFORM IMS-ISRT-ORQI-WDQ201                                         
001197                                                                          
001198     PERFORM DB-REDIGERA-ARBETSTABELL                                     
001199     PERFORM IMS-ISRT-ORQI-WDQ212                                         
001200     .                                                                    
001201     EJECT                                                                
001202 DA-REDIGERA-OHUV SECTION.                                                
001203                                                                          
001204     MOVE W-IDDISTR              TO W-IDDISTR-WDB2                        
001205                                    W-IDDISTR-WDB2-MIN                    
001206                                    W-IDDISTR-WDB2-MAX                    
001207     MOVE W-IDKUNDNR             TO W-IDKUNDNR-WDB2                       
001208     PERFORM IMS-GET-WDB201-UNIK                                          
001209     IF SEGMENT-FINNS                                                     
001210        CONTINUE                                                          
001211     ELSE                                                                 
001212        PERFORM IMS-GU-WDB201                                             
001213     END-IF                                                               
001214                                                                          
001215     MOVE KREG-IDDEPOT             TO OHUV-IDDEPOT                        
001216     MOVE KREG-IDROUTE             TO OHUV-IDROUTE                        
001217     MOVE KREG-IDZON               TO OHUV-IDZON                          
001218                                                                          
001219     MOVE ORDN-IDORDER-UT          TO OHUV-IDORDER                        
001220     MOVE KREG-ADBETRAD-1          TO OHUV-ADBETRAD-1                     
001221     MOVE KREG-ADBETRAD-2          TO OHUV-ADBETRAD-2                     
001222     IF MID-ADGMT = ALL '+'                                               
001223        MOVE KREG-ADGMT            TO OHUV-ADGMT                          
001224     ELSE                                                                 
001225        IF MID-ADGMT-GATA = ALL '+'                                       
001226           MOVE SPACE              TO OHUV-ADGMT-GATA                     
001227        ELSE                                                              
001228           MOVE MID-ADGMT-GATA     TO OHUV-ADGMT-GATA                     
001229        END-IF                                                            
001230        IF MID-ADGMT-PADR = ALL '+'                                       
001231           MOVE SPACE              TO OHUV-ADGMT-PADR                     
001232        ELSE                                                              
001233           MOVE MID-ADGMT-PADR     TO OHUV-ADGMT-PADR                     
001234        END-IF                                                            
001235        IF MID-ADGMT-LAND = ALL '+'                                       
001236           MOVE SPACE              TO OHUV-ADGMT-LAND                     
001237        ELSE                                                              
001238           MOVE MID-ADGMT-LAND     TO OHUV-ADGMT-LAND                     
001239        END-IF                                                            
001240        INSPECT OHUV-ADGMT REPLACING ALL '+' BY SPACE                     
001241     END-IF                                                               
001242     MOVE KREG-BEBETRAD-1          TO OHUV-BEBETRAD-1                     
001243     MOVE KREG-BEBETRAD-2          TO OHUV-BEBETRAD-2                     
001244     IF MID-BEGMT = ALL '+'                                               
001245        MOVE KREG-BEGMT            TO OHUV-BEGMT                          
001246     ELSE                                                                 
001247        MOVE MID-BEGMT             TO OHUV-BEGMT                          
001248        INSPECT OHUV-BEGMT REPLACING ALL '+' BY SPACE                     
001249     END-IF                                                               
001250     IF MID-BEKUNDRF = ALL '+'                                            
001251        MOVE SPACE                 TO OHUV-BEKUNDRF                       
001252     ELSE                                                                 
001253        MOVE MID-BEKUNDRF          TO OHUV-BEKUNDRF                       
001254     END-IF                                                               
001255     MOVE ZERO                     TO OHUV-IDDEPT                         
001256                                                                          
001257     IF MID-BELAGINS-DEL1 = ALL '+'                                       
001258        MOVE SPACE                 TO OHUV-BELAGINS-DEL1                  
001259     ELSE                                                                 
001260        MOVE MID-BELAGINS-DEL1     TO OHUV-BELAGINS-DEL1                  
001261     END-IF                                                               
001262     IF MID-BELAGINS-DEL2 = ALL '+'                                       
001263        MOVE SPACE                 TO OHUV-BELAGINS-DEL2                  
001264     ELSE                                                                 
001265        MOVE MID-BELAGINS-DEL2     TO OHUV-BELAGINS-DEL2                  
001266     END-IF                                                               
001267     IF MID-BEVARREF = ALL '+'                                            
001268        MOVE SPACE                 TO OHUV-BEVARREF                       
001269     ELSE                                                                 
001270        MOVE MID-BEVARREF          TO OHUV-BEVARREF                       
001271     END-IF                                                               
001272     IF MID-FLAUTFAK = '+'                                                
001273        MOVE NEJ                   TO OHUV-FLAUTFAK                       
001274     ELSE                                                                 
001275        MOVE MID-FLAUTFAK          TO OHUV-FLAUTFAK                       
001276     END-IF                                                               
001277     IF DIST03-SVERIGE-EJ-778                                             
001278        AND NOT DIST03-EJ-AUTFAK                                          
001279        AND NOT DIST03-S                                                  
001280        MOVE JA                    TO OHUV-FLAUTFAK                       
001281     END-IF                                                               
001282     MOVE NEJ                      TO OHUV-FLAUTPAC                       
001283     MOVE NEJ                      TO OHUV-FLBORT                         
001284     IF DIST20-EMBALLAGE                                                  
001285        MOVE JA                    TO OHUV-FLEMBORD                       
001286     ELSE                                                                 
001287        MOVE NEJ                   TO OHUV-FLEMBORD                       
001288     END-IF                                                               
001289     MOVE KREG-FLPRELRO            TO OHUV-FLPRELRO                       
001290     MOVE KREG-FLPRERS             TO OHUV-FLPRERS                        
001291     MOVE NEJ                      TO OHUV-FLFORBI                        
001292     MOVE NEJ                      TO OHUV-FLKLAR                         
001293     MOVE JA                       TO OHUV-FLLSBOK                        
001294     MOVE JA                       TO OHUV-FLOBTRAN                       
001295     MOVE NEJ                      TO OHUV-FLORDSPE                       
001296     MOVE NEJ                      TO OHUV-FLOVRLEV                       
001297     IF OHLK-KDORDKL = +0                                                 
001298        MOVE NEJ                   TO OHUV-FLRESTN                        
001299     ELSE                                                                 
001300        IF MID-FLRESTN = ALL '+'                                          
001301           IF KREG-FLRESTN = YES                                          
001302              MOVE JA              TO OHUV-FLRESTN                        
001303           ELSE                                                           
001304              MOVE KREG-FLRESTN    TO OHUV-FLRESTN                        
001305           END-IF                                                         
001306        ELSE                                                              
001307           MOVE MID-FLRESTN        TO OHUV-FLRESTN                        
001308        END-IF                                                            
001309     END-IF                                                               
001310     MOVE NEJ                      TO OHUV-FLVORKO                        
001311     IF OHUV-FLRESTN = NEJ                                                
001312        MOVE SPACE                 TO OHUV-IDBIPREF                       
001313     ELSE                                                                 
001314       IF MID-IDBIPREF = ALL '+'                                          
001315          MOVE SPACE               TO OHUV-IDBIPREF                       
001316       ELSE                                                               
001317          MOVE MID-IDBIPREF        TO OHUV-IDBIPREF                       
001318       END-IF                                                             
001319     END-IF                                                               
001320                                                                          
001321     MOVE KREG-IDDC-TVS            TO OHUV-IDDC-TVS                       
001325     MOVE OHLK-IDFTG               TO OHUV-IDFTG                          
001329     IF DIST79-ECOM-PRICE                                                 
001330        MOVE 'ECOM'                TO OHUV-IDSYSTEM                       
001331        MOVE ERR-DISTRICT-WRONG    TO MED-IDMFSFEL                        
001332        MOVE NEJ                   TO ALLT-SW                             
001333     ELSE                                                                 
001334        MOVE '4211'                TO OHUV-IDSYSTEM                       
001335     END-IF                                                               
001337     MOVE OHLK-IDDISTR             TO OHUV-IDDISTR                        
001338     MOVE OHLK-IDKUNDNR            TO OHUV-IDKUNDNR                       
001339     MOVE W-IDKUNDRF               TO OHUV-IDKUNDRF                       
001340     MOVE OHLK-IDKAMPRF            TO OHUV-IDKAMPRF                       
001341     MOVE KREG-IDRFTAB             TO OHUV-IDRFTAB                        
001342     MOVE OHLK-IDKONTO             TO OHUV-IDKONTO                        
001343     MOVE OHLK-IDANALYS            TO OHUV-IDANALYS                       
001344     MOVE OHLK-IDKST               TO OHUV-IDKST                          
001345     MOVE MSG-SIGNON-USERID        TO OHUV-IDUSER                         
001346     MOVE KREG-IDSKYLT             TO OHUV-IDSKYLT                        
001347     IF MID-KDFAKTYP = ALL '+'                                            
001348        MOVE KREG-KDGENFAK         TO OHUV-KDFAKTYP                       
001349     ELSE                                                                 
001350        MOVE MID-KDFAKTYP          TO OHUV-KDFAKTYP                       
001351     END-IF                                                               
001352     IF KREG-KDORDING = +3                                                
001353        MOVE KREG-KDORDING         TO OHUV-KDORDING                       
001354     ELSE                                                                 
001355        IF OHLK-KDTPOTYP > +0 OR OHLK-IDKAMPRF > +0                       
001356           MOVE +2                 TO OHUV-KDORDING                       
001357        ELSE                                                              
001358           MOVE KREG-KDORDING      TO OHUV-KDORDING                       
001359        END-IF                                                            
001360     END-IF                                                               
001370     MOVE OHLK-KDORDKL             TO OHUV-KDORDKL                        
001374     MOVE OHLK-KDTPOTYP            TO OHUV-KDTPOTYP                       
001375     MOVE KREG-KDTULLVE            TO OHUV-KDTULLVE                       
001376     MOVE +0                       TO OHUV-KDVRINFO                       
001377     MOVE KREG-KVDAGAR-DOW         TO OHUV-KVDAGAR-DOW                    
001378     MOVE KREG-RESLATT             TO OHUV-RESLATT                        
001379     MOVE MSGI-TILOKDAT            TO OHUV-TIREGDAT                       
001380                                      WS-TIREGDAT-9KOMPL                  
001381     MOVE MSGI-TILOKTID            TO WS-TIHHMM                           
001382     MOVE WS-TIHHMMSS              TO OHUV-TIREGTID                       
001383     MOVE +0                       TO OHUV-TIREGDAT-STO                   
001384     MOVE +0                       TO OHUV-TIREGTID-STO                   
001385     IF MID-TITPO = ALL '+'                                               
001386        MOVE +0                    TO OHUV-TITPO                          
001387     ELSE                                                                 
001388        MOVE OHLK-TITPO            TO OHUV-TITPO                          
001389     END-IF                                                               
001390     IF OHUV-KDORDKL = 1 AND                                              
001391        GMT-FLLDCKND = JA                                                 
001392        MOVE 'FW'                  TO OHUV-KDORDTYP-LDC                   
001393        MOVE ZERO                  TO OHUV-TIREPDAT                       
001394     ELSE                                                                 
001395        MOVE SPACE                 TO OHUV-KDORDTYP-LDC                   
001396        MOVE ZERO                  TO OHUV-TIREPDAT                       
001397     END-IF                                                               
001398     COMPUTE OHUV-TIREGDAT-9KOMPL = 9999999 - WS-TIREGDAT-9KOMPL          
001399     MOVE OHKK-FLORDTIL            TO OHUV-FLORDTIL                       
001400     MOVE KREG-IDDC                TO OHUV-IDDC-PRIM                      
001401     MOVE ZERO                     TO OHUV-KVORDTIL                       
001402                                      OHUV-IDGROSS                        
001403     MOVE SPACE                    TO OHUV-IDLEVNR-EJLS                   
001404     MOVE NEJ                      TO OHUV-FLSOFT                         
001405     MOVE NEJ                      TO OHUV-FLVORFK                        
001406     MOVE SPACE                    TO OHUV-IDBILREG                       
001407                                      OHUV-IDVIN                          
001408                                      OHUV-IDCISNR                        
001409     .                                                                    
001410     EJECT                                                                
001411 DB-REDIGERA-ARBETSTABELL SECTION.                                        
001412                                                                          
001413     MOVE KREG-IDDC            TO ARB-IDDC                                
001414     IF MID-BEGMRK = ALL '+'                                              
001415        MOVE KREG-BEGMRK       TO ARB-BEGMRK                              
001416     ELSE                                                                 
001417        MOVE MID-BEGMRK        TO ARB-BEGMRK                              
001418        INSPECT ARB-BEGMRK REPLACING ALL '+' BY SPACE                     
001419     END-IF                                                               
001420     MOVE NEJ                  TO ARB-FLODELUT                            
001421     MOVE +0                   TO ARB-IDRADNR-SISTA                       
001422     MOVE KREG-IDTRP           TO ARB-IDTRP                               
001423     MOVE KREG-IDTRP-ALT       TO ARB-IDTRP-ALT                           
001424     MOVE +0                   TO ARB-IDPLKLST-SISTA                      
001425     MOVE KREG-KDFDKRAV        TO ARB-KDFDKRAV                            
001426     IF MID-KDFRAKT = ALL '+'                                             
001427        MOVE KREG-KDFRAKT      TO ARB-KDFRAKT                             
001428     ELSE                                                                 
001429        MOVE MID-KDFRAKT       TO ARB-KDFRAKT                             
001430     END-IF                                                               
001431     IF OHUV-FLRESTN = NEJ                                                
001432       MOVE +0                 TO ARB-KDROPACK                            
001433     ELSE                                                                 
001434       IF MID-KDROPACK = ALL '+'                                          
001435         IF OHUV-ADGMT         EQUAL GMT-ADGMT       AND                  
001436            OHUV-BEGMT         EQUAL GMT-BEGMT                            
001437           MOVE KREG-KDROPACK  TO ARB-KDROPACK                            
001438         ELSE                                                             
001439           MOVE '3'            TO ARB-KDROPACK                            
001440         END-IF                                                           
001441       ELSE                                                               
001442           MOVE MID-KDROPACK   TO ARB-KDROPACK                            
001443       END-IF                                                             
001444     END-IF                                                               
001445                                                                          
001446     MOVE OHUV-IDDISTR         TO TEST-IDDISTR                            
001447     IF (DIST15-NA      AND (ARB-KDFRAKT > +11 AND < +20)) OR             
001448        (DIST15-ENGLAND AND OHUV-KDORDKL = +1                             
001449                        AND ARB-KDFRAKT = +16) OR                         
001450        (DIST15-JAPAN   AND OHUV-KDORDKL = +1                             
001451                        AND (ARB-KDFRAKT = +15 OR +25)) OR                
001452        (DIST15-AUSTRALIEN AND OHUV-KDORDKL = +1                          
001453                        AND ARB-KDFRAKT = +5)                             
001454       MOVE +0                 TO ARB-KDROPACK                            
001455     END-IF                                                               
001456                                                                          
001457     MOVE KREG-KDTRPKAT        TO ARB-KDTRPKAT                            
001458     MOVE +0                   TO ARB-KVSEMBRA                            
001459     MOVE TRAN-TIRFS           TO ARB-TIRFS                               
001460     MOVE TRAN-TIAAMMDD        TO ARB-DATRPAVD                            
001461     IF TRAN-TIAAMMDD NOT = ZERO                                          
001462       IF TRAN-TIAAMMDD < 500000                                          
001463         MOVE 20               TO ARB-DATRPAVD (1:2)                      
001464       ELSE                                                               
001465         IF TRAN-TIAAMMDD < 999999                                        
001466           MOVE 19             TO ARB-DATRPAVD (1:2)                      
001467         ELSE                                                             
001468           MOVE 99999999       TO ARB-DATRPAVD                            
001469         END-IF                                                           
001470       END-IF                                                             
001471     END-IF                                                               
001472     MOVE TRAN-TIHHMM          TO ARB-TIHHMM                              
001473     MOVE SPACE                TO ARB-KDORDSTA-O                          
001474     MOVE 'E'                  TO ARB-KDORDSTA                            
001487     .                                                                    
001488     EJECT                                                                
001489                                                                          
001490 E-HOPPA-TILL-ORDERHUVUD2 SECTION.                                        
001491                                                                          
001492     MOVE 'W4O214N1'            TO MFS-IDMOD                              
001493                                                                          
001494     MOVE '4214'                TO 4214-MOD-IDTRANS                       
001495     MOVE MFS-RENSA-FAELT       TO 4214-MOD-TEMFSFEL                      
001496                                   4214-MOD-TEMFSINF                      
001497                                   4214-MOD-IDDISTR-IN                    
001498                                   4214-MOD-IDKUNDNR-IN                   
001499                                   4214-MOD-IDORDNR-IN                    
001500                                                                          
001501     MOVE MID-IDDISTR           TO 4214-MOD-IDDISTR-UT                    
001502     INSPECT 4214-MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE          
001503                                                                          
001504     MOVE W-IDKUNDNR            TO WS-IDKUNDNR                            
001505     MOVE WS-IDKUNDNR           TO 4214-MOD-IDKUNDNR-UT                   
001506     INSPECT 4214-MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE         
001507                                                                          
001508     MOVE W-IDORDNR(3:5)        TO 4214-MOD-IDORDNR-UT                    
001509     INSPECT 4214-MOD-IDORDNR-UT REPLACING LEADING ZERO BY SPACE          
001510                                                                          
001511     MOVE ARB-IDDC              TO 4214-MOD-IDDC-UT                       
001512     MOVE WS-TEDDI              TO 4214-MOD-TEDDI                         
001513     MOVE OHUV-BEBETRAD-1       TO 4214-MOD-BEBETRAD-1                    
001514     MOVE OHUV-BEBETRAD-2       TO 4214-MOD-BEBETRAD-2                    
001515     MOVE OHUV-ADBETRAD-1       TO 4214-MOD-ADBETRAD-1                    
001516     MOVE OHUV-ADBETRAD-2       TO 4214-MOD-ADBETRAD-2                    
001517     MOVE OHUV-IDSKYLT          TO 4214-MOD-IDSKYLT                       
001518     IF MID-TIRFS-DAT = ALL '+'                                           
001519        MOVE ZERO               TO 4214-MOD-TIRFS-DAT                     
001520                                   4214-MOD-TIRFS-TID                     
001521     ELSE                                                                 
001522        MOVE MID-TIRFS-DAT      TO 4214-MOD-TIRFS-DAT                     
001523        MOVE MID-TIRFS-TID      TO 4214-MOD-TIRFS-TID                     
001524     END-IF                                                               
001525     MOVE OHUV-TITPO            TO 4214-MOD-TITPO                         
001526     MOVE OHUV-KDTULLVE         TO 4214-MOD-KDTULLVE                      
001527     MOVE OHUV-KDVRINFO         TO 4214-MOD-KDVRINFO                      
001528                                                                          
001529     COMPUTE MSG-KVLL = LENGTH OF 4214-MOD-W4O21401 + 4                   
001530     PERFORM IMS-INSERT-MSG                                               
001531     MOVE JA                     TO HOPP                                  
001532     .                                                                    
001533     EJECT                                                                
001534                                                                          
001535 H-STARTA-BIPACKNINGEN SECTION.                                           
001536                                                                          
001537     COMPUTE 4297-LL = LENGTH OF 4297-MID-W4I29701 + 17                   
001538     MOVE MFS-KDMFSFOR           TO 4297-SPRAK                            
001539                                                                          
001540     MOVE W-IDDISTR              TO 4297-MID-IDDISTR                      
001541     MOVE W-IDKUNDNR             TO 4297-MID-IDKUNDNR                     
001542     MOVE W-IDKUNDRF             TO 4297-MID-IDKUNDRF                     
001543                                                                          
001544     MOVE OHUV-KDTPOTYP          TO 4297-MID-KDTPOTYP                     
001545     MOVE OHUV-KDORDKL           TO 4297-MID-KDORDKL                      
001546     MOVE OHUV-KDFAKTYP          TO 4297-MID-KDFAKTYP                     
001547     MOVE OHUV-IDKAMPRF          TO 4297-MID-IDKAMPRF                     
001548     MOVE OHUV-IDKONTO           TO 4297-MID-IDKONTO                      
001549     MOVE OHUV-IDKST             TO 4297-MID-IDKST                        
001550     MOVE OHUV-IDANALYS          TO 4297-MID-IDANALYS                     
001551     MOVE OHUV-FLFORBI           TO 4297-MID-FLFORBI                      
001552     MOVE OHUV-IDORDER           TO 4297-MID-IDORDER                      
001553     MOVE OHUV-BEKUNDRF          TO 4297-MID-BEKUNDRF                     
001554     MOVE OHUV-IDBIPREF          TO 4297-MID-IDBIPREF                     
001555     MOVE OHUV-TIREGDAT          TO 4297-MID-TIREGDAT                     
001556     MOVE OHUV-IDFTG             TO 4297-MID-IDFTG                        
001557     MOVE MID-BEVARREF           TO 4297-MID-BEVARREF                     
001558     INSPECT 4297-MID-BEVARREF REPLACING ALL '+' BY SPACE                 
001559     MOVE MID-KDROPACK           TO 4297-MID-KDROPACK                     
001560     MOVE ARB-KDFRAKT            TO 4297-MID-KDFRAKT                      
001561     IF OHUV-IDDC-TVS NOT = SPACE                                         
001562       MOVE OHUV-IDDC-TVS        TO 4297-MID-IDDC                         
001563     ELSE                                                                 
001564       MOVE SPACE                TO 4297-MID-IDDC                         
001565     END-IF                                                               
001566                                                                          
001567     PERFORM IMS-INSERT-4297-MSG                                          
001568     MOVE JA                     TO HOPP                                  
001569     .                                                                    
001570     EJECT                                                                
001571 S01-HOPPA-TILL-RADREGISTRERING SECTION.                                  
001572                                                                          
001573     MOVE 'W4O212N1'            TO MFS-IDMOD                              
001574                                                                          
001575     MOVE '4212'                TO 4212-MOD-IDTRANS                       
001576     MOVE MFS-RENSA-FAELT       TO 4212-MOD-TEMFSFEL                      
001577     MOVE MID-IDDISTR           TO 4212-MOD-IDDISTR                       
001578     INSPECT 4212-MOD-IDDISTR REPLACING LEADING ZERO BY SPACE             
001579     MOVE W-IDKUNDNR            TO WS-IDKUNDNR                            
001580     MOVE WS-IDKUNDNR           TO 4212-MOD-IDKUNDNR                      
001581     INSPECT 4212-MOD-IDKUNDNR REPLACING LEADING ZERO BY SPACE            
001582     MOVE W-IDORDNR(3:5)        TO 4212-MOD-IDORDNR5                      
001583     INSPECT 4212-MOD-IDORDNR5 REPLACING LEADING ZERO BY SPACE            
001584     MOVE MID-KDORDKL           TO 4212-MOD-KDORDKL                       
001585     MOVE ARB-KDFRAKT           TO 4212-MOD-KDFRAKT                       
001586     MOVE WS-TEDDI              TO 4212-MOD-TEDDI                         
001587     MOVE OHUV-BEKUNDRF         TO 4212-MOD-BEVOLREF                      
001588     MOVE SPACE                 TO 4212-MOD-KDTRTYP                       
001589                                   4212-MOD-KDVALISO                      
001590     MOVE MFS-ADD-SAETT-CURSOR  TO 4212-MOD-IDARTNR-ATTR(1)               
001591                                                                          
001592     MOVE 4212-MOD-LAENGD      TO MSG-KVLL                                
001593     PERFORM IMS-INSERT-MSG                                               
001594                                                                          
001595     MOVE JA                    TO HOPP                                   
001596     .                                                                    
001597     EJECT                                                                
001598 S02-HOPPA-TILL-TILLAGGSREG     SECTION.                                  
001599                                                                          
001600     MOVE '002'              TO MSGI-KDCALL                               
001601     MOVE MSG-LTERM-NAME     TO MSGI-IDLTERM-USER                         
001602     MOVE MSG-SIGNON-USERID  TO MSGI-IDUSER                               
001603     MOVE '4211'             TO SPAR-IDTRANS                              
001604     MOVE KREG-IDDC          TO SPAR-IDDC                                 
001605     MOVE SPAR-AREA          TO MSGI-SPAR-AREA                            
001606     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
001607                                                                          
001608     MOVE LOW-VALUE             TO MSG-AREA                               
001609     MOVE 'W4O206N1'            TO MFS-IDMOD                              
001610                                                                          
001611     MOVE SPACE                 TO 4206-MOD-KDVALISO                      
001612     PERFORM MFS-TOM-4206-BILD                                            
001613                                                                          
001614     MOVE OHKK-IDORDER TO W-IDORDER                                       
001615     PERFORM IMS-GHU-ORQI-WDQ201                                          
001616                                                                          
001617     MOVE '4206'                TO 4206-MOD-IDTRANS                       
001618     MOVE OHUV-IDDISTR          TO 4206-MOD-IDDISTR                       
001619     INSPECT 4206-MOD-IDDISTR   REPLACING LEADING ZERO BY SPACE           
001620     MOVE OHUV-IDKUNDNR         TO 4206-MOD-IDKUNDNR                      
001621     INSPECT 4206-MOD-IDKUNDNR  REPLACING LEADING ZERO BY SPACE           
001622     MOVE OHUV-IDORDNR7(3:5)    TO 4206-MOD-IDORDNR5                      
001623     MOVE OHUV-KDORDKL          TO 4206-MOD-KDORDKL                       
001624     MOVE OHKK-KDFRAKT          TO 4206-MOD-KDFRAKT                       
001625     MOVE WS-TEDDI              TO 4206-MOD-TEDDI                         
001626     MOVE SPACE                 TO 4206-MOD-KDTRTYP                       
001627     MOVE MFS-ADD-SAETT-CURSOR  TO 4206-MOD-IDARTNR-ATTR(1)               
001628                                                                          
001629     IF MID-IDORDNR NOT = ALL '+'                                         
001630        STRING 'YOUR ORDER NO. HAS BEEN CHANGED TO '                      
001631               OHUV-IDORDNR7(3:5)                                         
001632            DELIMITED BY SIZE INTO 4206-MOD-TEMFSFEL                      
001633        MOVE MID-IDORDNR        TO 4206-MOD-IDORDNR5-REG                  
001634*       PERFORM MFS-SKYDDA-4206-BERADREF                                  
001635     ELSE                                                                 
001636        MOVE SPACE              TO 4206-MOD-IDORDNR5-REG                  
001637     END-IF                                                               
001638                                                                          
001639     MOVE 4206-MOD-LAENGD      TO MSG-KVLL                                
001640     PERFORM IMS-INSERT-MSG                                               
001641                                                                          
001642     MOVE JA                    TO HOPP                                   
001643     .                                                                    
001644     EJECT                                                                
001645 Z-FINIT SECTION.                                                         
001646                                                                          
001647     IF MED-IDMFSFEL NOT = SPACE                                          
001648         CALL WMEDKONV USING MED-WMEDAREA                                 
001649         MOVE MED-MFSFEL    TO MOD-TEMFSFEL                               
001650     END-IF                                                               
001651     PERFORM MFS-ROER-EJ-BILD                                             
001652     .                                                                    
001653     EJECT                                                                
001654 MFS-RENSA-BILD SECTION.                                                  
001655                                                                          
001656     MOVE MFS-RENSA-FAELT  TO   MOD-IDDISTR                               
001657                                MOD-IDKUNDNR                              
001658                                MOD-IDORDNR                               
001659                                MOD-KDORDKL                               
001660                                MOD-KDFRAKT                               
001661                                MOD-FLOH2                                 
001662                                MOD-TIRFS-DAT                             
001663                                MOD-TIRFS-TID                             
001664                                MOD-BEKUNDRF                              
001665                                MOD-KDFAKTYP                              
001666                                MOD-FLAUTFAK                              
001667                                MOD-FLRESTN                               
001668                                MOD-KDTPOTYP                              
001669                                MOD-TITPO                                 
001670                                MOD-IDKAMPRF                              
001671                                MOD-BELAGINS-DEL1                         
001672                                MOD-BELAGINS-DEL2                         
001673                                MOD-BEGMT-RAD1                            
001674                                MOD-BEGMT-RAD2                            
001675                                MOD-ADGMT-GATA                            
001676                                MOD-ADGMT-PADR                            
001677                                MOD-ADGMT-LAND                            
001678                                MOD-BEGMRK-RAD1                           
001679                                MOD-BEGMRK-RAD2                           
001680                                MOD-KDROPACK                              
001681                                MOD-IDBIPREF                              
001682                                MOD-IDFTG                                 
001683                                MOD-IDKONTO                               
001684                                MOD-IDANALYS                              
001685                                MOD-IDKST                                 
001686                                MOD-BEVARREF                              
001687     .                                                                    
001688     EJECT                                                                
001689 MFS-ROER-EJ-BILD SECTION.                                                
001690                                                                          
001691     MOVE MFS-ROER-EJ-FAELT TO  MOD-IDDISTR                               
001692                                MOD-IDKUNDNR                              
001693                                MOD-IDORDNR                               
001694                                MOD-KDORDKL                               
001695                                MOD-KDFRAKT                               
001696                                MOD-FLOH2                                 
001697                                MOD-TIRFS-DAT                             
001698                                MOD-TIRFS-TID                             
001699                                MOD-BEKUNDRF                              
001700                                MOD-KDFAKTYP                              
001701                                MOD-FLAUTFAK                              
001702                                MOD-FLRESTN                               
001703                                MOD-KDTPOTYP                              
001704                                MOD-TITPO                                 
001705                                MOD-IDKAMPRF                              
001706                                MOD-BELAGINS-DEL1                         
001707                                MOD-BELAGINS-DEL2                         
001708                                MOD-BEGMT-RAD1                            
001709                                MOD-BEGMT-RAD2                            
001710                                MOD-ADGMT-GATA                            
001711                                MOD-ADGMT-PADR                            
001712                                MOD-ADGMT-LAND                            
001713                                MOD-BEGMRK-RAD1                           
001714                                MOD-BEGMRK-RAD2                           
001715                                MOD-KDROPACK                              
001716                                MOD-IDBIPREF                              
001717                                MOD-IDFTG                                 
001718                                MOD-IDKONTO                               
001719                                MOD-IDANALYS                              
001720                                MOD-IDKST                                 
001721                                MOD-BEVARREF                              
001722     .                                                                    
001723     EJECT                                                                
001724 MFS-TOM-4206-BILD     SECTION.                                           
001725                                                                          
001726     MOVE 1 TO IX                                                         
001727     PERFORM UNTIL IX > MAX-4206-IX                                       
001728                                                                          
001729        MOVE MFS-RENSA-FAELT TO 4206-MOD-IDARTNR (IX)                     
001730                                4206-MOD-KVBEART (IX)                     
001731                                4206-MOD-PRARTNTO(IX)                     
001732                                4206-MOD-TITPO   (IX)                     
001733                                4206-MOD-FLRESTN (IX)                     
001734                                4206-MOD-FLSLATT (IX)                     
001735                                4206-MOD-KDKVBRYT(IX)                     
001736                                4206-MOD-FLINVEST(IX)                     
001737                                4206-MOD-KDVRINFO(IX)                     
001738                                4206-MOD-BERADREF(IX)                     
001739                                                                          
001740        ADD 1 TO IX                                                       
001741     END-PERFORM                                                          
001742     .                                                                    
001743     EJECT                                                                
001744*MFS-SKYDDA-4206-BERADREF SECTION.                                        
001745*                                                                         
001746*    MOVE 1 TO IX                                                         
001747*    PERFORM UNTIL IX > MAX-4206-IX                                       
001748*       MOVE MFS-CLOSE-FIELD TO 4206-MOD-BERADREF-ATTR(IX)                
001749*       ADD 1 TO IX                                                       
001750*    END-PERFORM                                                          
001751*    .                                                                    
001752*                                                                         
001753     EJECT                                                                
001754* --- IMS SEKTIONER ---                                                   
001755     SKIP2                                                                
001756 IMS-GET-MSG SECTION.                                                     
001757                                                                          
001758     MOVE '  QC' TO GODK-STATUSKODER                                      
001759     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
001760     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
001761     PERFORM IMS-STATUSKONTROLL                                           
001762     .                                                                    
001763     SKIP2                                                                
001764 IMS-INSERT-MSG SECTION.                                                  
001765                                                                          
001766     IF NOT ENGLISH-TEXT                                                  
001767       MOVE '0' TO MFS-KDHUVOMR                                           
001768     END-IF                                                               
001769     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
001770     MOVE SPACE TO GODK-STATUSKODER                                       
001771     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
001772     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
001773     PERFORM IMS-STATUSKONTROLL                                           
001774     .                                                                    
001775     SKIP2                                                                
001776 IMS-INSERT-4297-MSG SECTION.                                             
001777                                                                          
001778     MOVE LOW-VALUE TO 4297-Z1 4297-Z2                                    
001779     MOVE SPACE TO GODK-STATUSKODER                                       
001780     CALL CBLTDLI USING ISRT 4297-PCB 4297-MSG-IO-AREA                    
001781     MOVE 4297-STATUS-CODE TO STATUS-WS                                   
001782     PERFORM IMS-STATUSKONTROLL                                           
001783     .                                                                    
001784     EJECT                                                                
001785 IMS-GU-ORQL-WDQ201 SECTION.                                              
001786                                                                          
001787     STRING 'WLORQI01(WDQ2CSEQ =' W-WDQ2CSEQ-X ')'                        
001788          DELIMITED BY SIZE INTO SSA1                                     
001789     MOVE '  GE'               TO GODK-STATUSKODER                        
001790     CALL CBLTDLI USING GU ORQL-PCB DLI-IO-AREA-OHUV SSA1                 
001791     MOVE ORQL-STATUS-CODE    TO STATUS-WS                                
001792     PERFORM IMS-STATUSKONTROLL                                           
001793     .                                                                    
001794     SKIP3                                                                
001795 IMS-GHU-ORQI-WDQ201 SECTION.                                             
001796                                                                          
001797     STRING 'WLORQI01(IDORDER  =' W-IDORDER-X ')'                         
001798          DELIMITED BY SIZE INTO SSA1                                     
001799     MOVE '  '                 TO GODK-STATUSKODER                        
001800     CALL CBLTDLI USING GHU ORQI-PCB DLI-IO-AREA-OHUV SSA1                
001801     MOVE ORQI-STATUS-CODE    TO STATUS-WS                                
001802     PERFORM IMS-STATUSKONTROLL                                           
001803     .                                                                    
001804     SKIP3                                                                
001805 IMS-GNP-ORQI-WDQ212 SECTION.                                             
001806                                                                          
001807     STRING 'WLORQI12(IDDC     =' W-IDDC-X ')'                            
001808          DELIMITED BY SIZE INTO SSA1                                     
001809     MOVE '    '               TO GODK-STATUSKODER                        
001810     CALL CBLTDLI USING GNP  ORQI-PCB DLI-IO-AREA-ARB SSA1                
001811     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
001812     PERFORM IMS-STATUSKONTROLL                                           
001813     .                                                                    
001814     SKIP3                                                                
001815 IMS-REPL-ORQI-WDQ201 SECTION.                                            
001816                                                                          
001817     MOVE '    '               TO GODK-STATUSKODER                        
001818     CALL CBLTDLI USING REPL ORQI-PCB DLI-IO-AREA-OHUV                    
001819     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
001820     PERFORM IMS-STATUSKONTROLL                                           
001821     .                                                                    
001822     EJECT                                                                
001823 IMS-ISRT-ORQI-WDQ201 SECTION.                                            
001824                                                                          
001825     MOVE 'WLORQI01 '          TO SSA1                                    
001826     MOVE '    '               TO GODK-STATUSKODER                        
001827     CALL CBLTDLI USING ISRT ORQI-PCB DLI-IO-AREA-OHUV SSA1               
001828     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
001829     PERFORM IMS-STATUSKONTROLL                                           
001830     .                                                                    
001831     SKIP3                                                                
001832 IMS-ISRT-ORQI-WDQ212 SECTION.                                            
001833                                                                          
001834     MOVE 'WLORQI12 '          TO SSA1                                    
001835     MOVE '    '               TO GODK-STATUSKODER                        
001836     CALL CBLTDLI USING ISRT ORQI-PCB DLI-IO-AREA-ARB SSA1                
001837     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
001838     PERFORM IMS-STATUSKONTROLL                                           
001839     .                                                                    
001840     SKIP3                                                                
001841 IMS-GET-WDB201-UNIK SECTION.                                             
001842                                                                          
001843     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
001844          DELIMITED BY SIZE INTO SSA1                                     
001845     MOVE '  GE'               TO GODK-STATUSKODER                        
001846     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-AREA-WDB201 SSA1               
001847     MOVE WDB2-STATUS-CODE     TO STATUS-WS                               
001848     PERFORM IMS-STATUSKONTROLL                                           
001849     .                                                                    
001850     SKIP2                                                                
001851 IMS-GU-WDB201 SECTION.                                                   
001852                                                                          
001853     STRING 'WDB201  (IDGMT   >=' W-IDGMT-MIN-X                           
001854                    '&IDGMT   <=' W-IDGMT-MAX-X ')'                       
001855            DELIMITED BY SIZE INTO SSA1                                   
001856     MOVE '  GE'               TO GODK-STATUSKODER                        
001857     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-AREA-WDB201 SSA1               
001858     MOVE WDB2-STATUS-CODE     TO STATUS-WS                               
001859     PERFORM IMS-STATUSKONTROLL                                           
001860     .                                                                    
001861     SKIP2                                                                
001862 IMS-STATUSKONTROLL SECTION.                                              
001863                                                                          
001864     SET STATUS-IX TO 1                                                   
001865     SEARCH GODK-STATUS                                                   
001866       AT END CALL FELLOG                                                 
001867       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
001868     END-SEARCH                                                           
001869     .                                                                    
