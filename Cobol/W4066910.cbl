000001 ID DIVISION.                                                             
000002 PROGRAM-ID.     W4066910.                                                
000003 AUTHOR.         JONNY SANDSTEN.                                          
000004 DATE-WRITTEN.   98/05/06.                                                
000005 DATE-COMPILED.                                                           
000006                                                                          
000007*    FUNKTION:                                                            
000008*        BILL OF LADING. ADRESS, KOLLIKOD OCH VIKT KAN UPPDATERAS         
000009*        M.H.A DETTA PROGRAM                                              
000010*                                                                         
000011*        PROGRAMMET LÄSER/UPPDATERAR WLORQI (WDQ2)                        
000012*        PROGRAMMET LÄSER/UPPDATERAR WL4463 (WDGX)                        
000013*        PROGRAMMET LÄSER            WL1165 (WDGX)                        
000014*        PROGRAMMET LÄSER            WDB6                                 
000015*                                                                         
000016*    INDATA.                                                              
000017*        TRANSAKTION: W4T669 W4T669U                                      
000018*        MID:         W4I66901                                            
000019*                                                                         
000020*    UTDATA.                                                              
000021*        MOD:         W4O66901                                            
000022*                                                                         
000023* CHANGE LOG:                                                             
000024*                                                                         
000025*      14/05/06 - REDDY RAHUL     - NEW PROG WITH BUSINES LOGIC.          
000026*                                   COMMON FOR CLASSIC (W4066900)         
000027*                                   AND WEB (W4W66900) INTERFACE.         
000028*                                   ETRACKER 10228352 - TORONTO.          
000029*                                                                         
000030*                                                                         
000031*                                                                         
000032                                                                          
000033     SKIP3                                                                
000034 ENVIRONMENT DIVISION.                                                    
000035     EJECT                                                                
000036 DATA DIVISION.                                                           
000037 WORKING-STORAGE SECTION.                                                 
000038                                                                          
000039*    -- CHECKED BY WY2000                                                 
000040 77  IDPGM                       PIC X(08)   VALUE 'W4066910'.            
000041                                                                          
000042*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
000043 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
000044                                                                          
000045 77  JA                          PIC X       VALUE 'J'.                   
000046 77  NEJ                         PIC X       VALUE 'N'.                   
000047                                                                          
000048 01  ALL-SPACE.                                                           
000049     03 FILLER                   PIC X(50)     VALUE SPACE.               
000050 01  ALL-PLUS.                                                            
000051     03 FILLER                   PIC X(50)     VALUE ALL '+'.             
000052                                                                          
000053*    --- INDEX FÖR BLÄDDRINGSRADER                                        
000054 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
000055                                                                          
000056 77  PSN-INDX                    PIC S9(4)  VALUE +0    COMP SYNC.        
000057 77  PSN-MAX-INDX                PIC S9(4)  VALUE +10   COMP SYNC.        
000058                                                                          
000059*    --- GENERELLA ARBETSFÄLT                                             
000060 77  WS-VKORDBTO-KOLLI-ORIG    PIC S9(6)V9   VALUE ZERO.                  
000061 77  WS-VKORDBTO-KOLLI-BACK    PIC S9(6)V9   VALUE ZERO.                  
000062                                                                          
000063*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
000064                                                                          
000065 77  ALLT-SW                     PIC X       VALUE 'J'.                   
000066     88  ALLT-OK                             VALUE 'J'.                   
000067                                                                          
000068 77  INDATA-SW                   PIC X       VALUE 'J'.                   
000069     88  INDATA-OK                           VALUE 'J'.                   
000070     88  INDATA-FEL                          VALUE 'N'.                   
000071                                                                          
000072 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
000073     88  NYCKLAR-OK                          VALUE 'J'.                   
000074     88  NYCKLAR-FEL                         VALUE 'N'.                   
000075                                                                          
000076 77  DATA-ENTERED-SW             PIC X       VALUE 'N'.                   
000077     88  DATA-ENTERED-YES                    VALUE 'J'.                   
000078     88  DATA-ENTERED-NO                     VALUE 'N'.                   
000079                                                                          
000080 77  US-MEASURES-SW              PIC X       VALUE 'N'.                   
000081     88  US-MEASURES                         VALUE 'J'.                   
000082     88  SI-MEASURES                         VALUE 'N'.                   
000083                                                                          
000084 01  WS-IDKUNDRF                 PIC X(10)   VALUE SPACE.                 
000085 01  FILLER                      REDEFINES WS-IDKUNDRF.                   
000086     03  WS-IDORDNR7             PIC  9(7).                               
000087     03  FILLER                  PIC  X(3).                               
000088*                                                                         
000089     EJECT                                                                
000090*      --- VALID IDDC CODES                                               
000091*                                                                         
000092*01    -COPY WWDC99                                                       
000093       EJECT                                                              
000094                                                                          
000095*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
000096 01  GENERELLA-SUBPROGRAM.                                                
000097     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000098     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000099     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
000100     EJECT                                                                
000101*    --- PARAMETRAR TILL SUBPROGRAM WDECEDIT                              
000102*01 -COPY WDECAREA                                                        
000103     SKIP3                                                                
000104*    --- PARAMETRAR TILL COPYTEXT   WWOMVAND                              
000105*01 -COPY WWOMVAND                                                        
000106     EJECT                                                                
000107 01  MESSAGE-CODES.                                                       
000108     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '020'.                 
000109     03  INF-PRESS-PF11          PIC X(3)    VALUE '013'.                 
000110     03  INF-WEIGHT-ADJUSTED     PIC X(3)    VALUE '415'.                 
000111     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '014'.                 
000112     03  INF-UPDATE-DONE         PIC X(3)    VALUE '001'.                 
000113     03  INF-FIRST-PAGE          PIC X(3)    VALUE '010'.                 
000114     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '011'.                 
000115     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
000116     03  CUST-MISSING            PIC X(3)    VALUE '025'.                 
000117     03  KOLLI-FINNS             PIC X(3)    VALUE '149'.                 
000118     03  CASE-MISSING            PIC X(3)    VALUE '041'.                 
000119     03  FILL-CMD                PIC X(3)    VALUE '026'.                 
000120     03  FORBIDDEN-UPDATE        PIC X(3)    VALUE '007'.                 
000121     EJECT                                                                
000122*                                                                         
000123 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
000124     SKIP3                                                                
000125*01  -COPY WMFSAREA                                                       
000126     EJECT                                                                
000127*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
000128*                                                                         
000129     EJECT                                                                
000130 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000131     SKIP3                                                                
000132 01  NYCKLAR-TILL-DLI.                                                    
000133*    --- VÄRDE PÅ BLÄDDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN            
000134     03  W-IDGMTREF-X.                                                    
000135         05  W-IDDISTR-WDQ2       PIC S9(5)   VALUE ZERO  COMP-3.         
000136         05  W-IDKUNDNR-WDQ2      PIC S9(7)   VALUE ZERO  COMP-3.         
000137         05  W-IDKUNDRF-WDQ2      PIC X(10).                              
000138         05  W-IDKUNDRF-IDORDNR-FILLER REDEFINES W-IDKUNDRF-WDQ2.         
000139           07  W-IDORDNR7-WDQ2    PIC  9(7).                              
000140           07  FILLER             PIC  X(3).                              
000141                                                                          
000142     03  W-4463-X.                                                        
000143         05  W-IDHTYP            PIC X(4)    VALUE '4463'.                
000144         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
000145         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
000146                                                                          
000147     03  W-DASKEPPN-X.                                                    
000148         05  W-DASKEPPN          PIC  9(8)   VALUE ZERO.                  
000149                                                                          
000150     03  W-KY4466-X.                                                      
000151         05  W-IDTRPTNR          PIC S9(3)   VALUE ZERO   COMP-3.         
000152         05  W-IDLBBET           PIC X(12)   VALUE SPACE.                 
000153                                                                          
000154     03  W-KY4468-X.                                                      
000155         05  W-IDDISTR            PIC S9(5)   VALUE ZERO  COMP-3.         
000156         05  W-IDKUNDNR           PIC S9(7)   VALUE ZERO  COMP-3.         
000157         05  W-IDKUNDRF           PIC X(10)   VALUE SPACE.                
000158         05  W-IDKUNDRF-IDORDNR7-FILLER REDEFINES W-IDKUNDRF.             
000159           07  W-IDORDNR7         PIC  9(7).                              
000160           07  FILLER             PIC  X(3).                              
000161         05  W-IDPRODNR           PIC S9(7)   VALUE ZERO  COMP-3.         
000162         05  W-IDKOLLI            PIC S9(5)   VALUE ZERO  COMP-3.         
000163                                                                          
000164     03  W-KY4468-MIN-X.                                                  
000165         05  W-IDDISTR-MIN        PIC S9(5)   VALUE ZERO  COMP-3.         
000166         05  W-IDKUNDNR-MIN       PIC S9(7)   VALUE ZERO  COMP-3.         
000167         05  W-IDKUNDRF-MIN       PIC X(10)   VALUE SPACE.                
000168         05  W-IDPRODNR-MIN       PIC S9(7)   VALUE ZERO  COMP-3.         
000169         05  W-IDKOLLI-MIN        PIC S9(5)   VALUE ZERO  COMP-3.         
000170                                                                          
000171     03  W-KY4468-MAX-X.                                                  
000172         05  W-IDDISTR-MAX        PIC S9(5)   VALUE ZERO  COMP-3.         
000173         05  W-IDKUNDNR-MAX       PIC S9(7)   VALUE ZERO  COMP-3.         
000174         05  FILLER               PIC X(17).                              
000175                                                                          
000176     03  W-1165-X.                                                        
000177         05  W-IDHTYP            PIC X(4)    VALUE '1165'.                
000178         05  W-IDPSN             PIC 9(3).                                
000179         05  W-LOW-VALUE         PIC X(23)   VALUE LOW-VALUE.             
000180                                                                          
000181     03  W-IDGMT-B2-X.                                                    
000182         05  W-IDDISTR-B2        PIC S9(5)  COMP-3.                       
000183         05  W-IDKUNDNR-B2       PIC S9(7)  COMP-3.                       
000184                                                                          
000185     03  W-IDDC-B6-X.                                                     
000186         05 W-IDDC-B6            PIC X(2).                                
000187                                                                          
000188     SKIP2                                                                
000189*    --- STATUS-KOD FRÅN IMS                                              
000190 01  STATUS-WS                   PIC XX.                                  
000191     88  SEGMENT-FINNS                       VALUE '  '.                  
000192     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
000193     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
000194     SKIP2                                                                
000195 01  GODK-STATUSKODER.                                                    
000196     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000197     SKIP3                                                                
000198 01  SSA1                        PIC X(128).                              
000199 01  SSA2                        PIC X(64).                               
000200 01  SSA3                        PIC X(64).                               
000201 01  SSA4                        PIC X(128).                              
000202     EJECT                                                                
000203*    --- IMS FUNKTIONSKODER                                               
000204*01  -COPY W0003                                                          
000205     EJECT                                                                
000206*    ---  DLI INPUT-OUTPUT AREA                                           
000207                                                                          
000208 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLORQI01'.                    
000209 01  DLI-IO-WLORQI01.                                                     
000210*    03  -COPY WDQ201                                                     
000211 01  FILLER         PIC X(16) VALUE 'DLI-IO-WL446301'.                    
000212 01  DLI-IO-WL446301.                                                     
000213*    03  -COPY WDGX4463                                                   
000214     EJECT                                                                
000215 01  FILLER         PIC X(16) VALUE 'DLI-IO-WL446311'.                    
000216 01  DLI-IO-WL446311.                                                     
000217*    03  -COPY WDGX4464                                                   
000218     EJECT                                                                
000219 01  FILLER         PIC X(16) VALUE 'DLI-IO-WL446321'.                    
000220 01  DLI-IO-WL446321.                                                     
000221*    03  -COPY WDGX4466                                                   
000222     EJECT                                                                
000223 01  FILLER         PIC X(16) VALUE 'DLI-IO-WL446331'.                    
000224 01  DLI-IO-WL446331.                                                     
000225*    03  -COPY WDGX4468                                                   
000226 01  FILLER         PIC X(16) VALUE 'DLI-IO-WL116501'.                    
000227 01  DLI-IO-WL116501.                                                     
000228*    03  -COPY WDGX1165                                                   
000229     EJECT                                                                
000230                                                                          
000231 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
000232 01   DLI-IO-AREA-B601.                                                   
000233*     03  -COPY WDB601                                                    
000234                                                                          
000235     EJECT                                                                
000236 LINKAGE SECTION.                                                         
000237 01  REQU-AREA.                                                           
000238*    03 -COPY WZ01REQU                                                    
000239*    03 -COPY W40669I1                                                    
000240     EJECT                                                                
000241 01  RESP-AREA.                                                           
000242*    03 -COPY WZ01RESP                                                    
000243*    03 -COPY W40669O1                                                    
000244     EJECT                                                                
000245 01  MAX-KVRADER                 PIC S9(4) COMP.                          
000246*01  -COPY W0008  -PRE ORQI-                                              
000247     05  FILLER                  PIC X.                                   
000248                                                                          
000249*01  -COPY W0008  -PRE 4463-                                              
000250     05  FILLER                  PIC X.                                   
000251                                                                          
000252*01  -COPY W0008  -PRE 1165-                                              
000253     05  FILLER                  PIC X.                                   
000254*01  -COPY W0008  -PRE WDB6-                                              
000255     05  FILLER                  PIC X.                                   
000256                                                                          
000257     EJECT                                                                
000258 PROCEDURE DIVISION  USING REQU-AREA RESP-AREA MAX-KVRADER                
000259         ORQI-PCB 4463-PCB 1165-PCB  WDB6-PCB.                            
000260                                                                          
000261 MAIN SECTION.                                                            
000262     ENTRY 'DLITCBL' USING REQU-AREA RESP-AREA MAX-KVRADER                
000263         ORQI-PCB 4463-PCB 1165-PCB  WDB6-PCB.                            
000264                                                                          
000265     PERFORM A-INIT                                                       
000266     PERFORM B-KOLLA-NYCKLAR                                              
000267     IF NYCKLAR-OK                                                        
000268       IF REQU-UPDATE                                                     
000269         PERFORM G-KOLLA-INPUT                                            
000270         IF INDATA-OK                                                     
000271           PERFORM H-UPPDATERA                                            
000272         END-IF                                                           
000273       ELSE                                                               
000274         IF REQU-FIRST                                                    
000275           PERFORM C-FOERSTA-SIDA                                         
000276         ELSE                                                             
000277           IF REQU-NEXT                                                   
000278             PERFORM D-NAESTA-SIDA                                        
000279           ELSE                                                           
000280             PERFORM E-SAMMA-SIDA                                         
000281           END-IF                                                         
000282         END-IF                                                           
000283       END-IF                                                             
000284       IF ALLT-OK                                                         
000285         PERFORM F-LAES-VISA-INFO                                         
000286       END-IF                                                             
000287     END-IF                                                               
000288     GOBACK                                                               
000289     .                                                                    
000290     EJECT                                                                
000291 A-INIT SECTION.                                                          
000292                                                                          
000293     IF REQU-KVRADER NOT NUMERIC                                          
000294        MOVE ZERO TO REQU-KVRADER                                         
000295     END-IF                                                               
000296                                                                          
000297     MOVE ALL '+'                TO RESP-W40669O1                         
000298                                                                          
000299     MOVE 001                    TO RESP-IDMSGVER                         
000300     MOVE SPACE                  TO RESP-IDMSG-ERROR                      
000301                                    RESP-IDMSG-INFO                       
000302                                    RESP-IDELMT-ERROR                     
000303                                                                          
000304     MOVE REQU-KVRADER           TO RESP-KVRADER                          
000305                                                                          
000306     MOVE REQU-IDDISTR-START     TO RESP-IDDISTR-START                    
000307     MOVE REQU-IDKUNDNR-START    TO RESP-IDKUNDNR-START                   
000308     MOVE REQU-IDPRODNR-START    TO RESP-IDPRODNR-START                   
000309     MOVE REQU-IDKOLLI-START     TO RESP-IDKOLLI-START                    
000310     MOVE REQU-IDKUNDRF-START    TO RESP-IDKUNDRF-START                   
000311     MOVE ZERO                   TO RESP-IDDISTR-NEXT                     
000312                                    RESP-IDKUNDNR-NEXT                    
000313                                    RESP-IDPRODNR-NEXT                    
000314                                    RESP-IDKOLLI-NEXT                     
000315     MOVE SPACE                  TO RESP-IDKUNDRF-NEXT                    
000316     PERFORM MFS-RENSA-FAELT-UT                                           
000317     PERFORM MFS-FORM-ATTR                                                
000318                                                                          
000319     .                                                                    
000320     EJECT                                                                
000321 B-KOLLA-NYCKLAR SECTION.                                                 
000322                                                                          
000323     MOVE LOW-VALUE              TO W-KY4468-MIN-X                        
000324     MOVE HIGH-VALUE             TO W-KY4468-MAX-X                        
000325                                                                          
000326     MOVE JA                     TO NYCKLAR-SW                            
000327                                                                          
000328*    -- KONTROLL AV NYCKLAR                                               
000329                                                                          
000330     IF REQU-TISKEPPN-KEY NUMERIC AND REQU-TISKEPPN-KEY > ZERO            
000331       MOVE REQU-TISKEPPN-KEY    TO W-DASKEPPN                            
000332       IF W-DASKEPPN > 700000                                             
000333         MOVE 19                 TO W-DASKEPPN (1:2)                      
000334       ELSE                                                               
000335         MOVE 20                 TO W-DASKEPPN (1:2)                      
000336       END-IF                                                             
000337     ELSE                                                                 
000338       MOVE NEJ                  TO NYCKLAR-SW                            
000339       MOVE 'TISKEPPN'           TO RESP-IDELMT-ERROR                     
000340     END-IF                                                               
000341                                                                          
000342     IF REQU-IDTRPTNR-KEY NUMERIC AND REQU-IDTRPTNR-KEY > ZERO            
000343       MOVE REQU-IDTRPTNR-KEY    TO W-IDTRPTNR                            
000344       MOVE REQU-IDLBBET-KEY     TO W-IDLBBET                             
000345     ELSE                                                                 
000346       MOVE NEJ                  TO NYCKLAR-SW                            
000347       MOVE 'IDTRPTNR'           TO RESP-IDELMT-ERROR                     
000348     END-IF                                                               
000349                                                                          
000350     IF REQU-IDDISTR-KEY NUMERIC AND REQU-IDDISTR-KEY > ZERO              
000351       MOVE REQU-IDDISTR-KEY     TO W-IDDISTR                             
000352                                    W-IDDISTR-WDQ2                        
000353                                    W-IDDISTR-MIN                         
000354                                    W-IDDISTR-MAX                         
000355                                    W-IDDISTR-B2                          
000356     ELSE                                                                 
000357       MOVE NEJ                  TO NYCKLAR-SW                            
000358     END-IF                                                               
000359                                                                          
000360     INSPECT REQU-IDKUNDNR-KEY REPLACING LEADING SPACE BY ZERO            
000361     IF REQU-IDKUNDNR-KEY NUMERIC AND REQU-IDKUNDNR-KEY > ZERO            
000362       MOVE REQU-IDKUNDNR-KEY    TO W-IDKUNDNR                            
000363                                    W-IDKUNDNR-WDQ2                       
000364                                    W-IDKUNDNR-MIN                        
000365                                    W-IDKUNDNR-MAX                        
000366                                    W-IDKUNDNR-B2                         
000367     ELSE                                                                 
000368       MOVE NEJ                  TO NYCKLAR-SW                            
000369       MOVE 'IDKUNDNR'           TO RESP-IDELMT-ERROR                     
000370     END-IF                                                               
000371                                                                          
000372     MOVE REQU-IDDC-KEY          TO W-IDDC                                
000373                                    WS-IDDC                               
000374     IF NDC-US                                                            
000375       MOVE 'Lbs'                TO RESP-BESORT                           
000376       SET US-MEASURES           TO TRUE                                  
000377     ELSE                                                                 
000378       MOVE 'Kg'                 TO RESP-BESORT                           
000379       SET SI-MEASURES           TO TRUE                                  
000380     END-IF                                                               
000381                                                                          
000382     IF NYCKLAR-FEL                                                       
000383*---FÖR ATT INTE FÅ NYCKLAR FEL NÄR MAN KOMMER FRÅN EN ICKE               
000384*---GODKÄND BILD                                                          
000385       MOVE ERR-WRONG-KEY        TO RESP-IDMSG-ERROR                      
000386       MOVE ZERO                 TO RESP-KVRADER                          
000387       PERFORM MFS-RENSA-FAELT-IN                                         
000388       PERFORM MFS-RENSA-FAELT-UT                                         
000389     END-IF                                                               
000390     .                                                                    
000391     EJECT                                                                
000392 C-FOERSTA-SIDA SECTION.                                                  
000393                                                                          
000394     MOVE INF-FIRST-PAGE         TO RESP-IDMSG-INFO                       
000395                                                                          
000396     PERFORM MFS-RENSA-FAELT-IN                                           
000397     .                                                                    
000398     EJECT                                                                
000399 D-NAESTA-SIDA SECTION.                                                   
000400                                                                          
000401     MOVE REQU-IDDISTR-START     TO W-IDDISTR                             
000402                                    W-IDDISTR-MIN                         
000403     MOVE REQU-IDKUNDNR-START    TO W-IDKUNDNR                            
000404                                    W-IDKUNDNR-MIN                        
000405     MOVE REQU-IDKUNDRF-START    TO W-IDKUNDRF                            
000406                                    W-IDKUNDRF-MIN                        
000407     MOVE REQU-IDPRODNR-START    TO W-IDPRODNR                            
000408                                    W-IDPRODNR-MIN                        
000409     MOVE REQU-IDKOLLI-START     TO W-IDKOLLI                             
000410                                    W-IDKOLLI-MIN                         
000411     PERFORM MFS-RENSA-FAELT-IN                                           
000412     .                                                                    
000413     EJECT                                                                
000414 E-SAMMA-SIDA SECTION.                                                    
000415                                                                          
000416     IF REQU-IDDISTR-START NOT = ALL '+' AND                              
000417        REQU-IDDISTR-START IS NUMERIC                                     
000418       MOVE REQU-IDDISTR-START   TO W-IDDISTR                             
000419                                    W-IDDISTR-MIN                         
000420     ELSE                                                                 
000421       MOVE ZERO                 TO W-IDDISTR                             
000422                                    W-IDDISTR-MIN                         
000423     END-IF                                                               
000424     IF REQU-IDKUNDNR-START NOT = ALL '+' AND                             
000425        REQU-IDKUNDNR-START IS NUMERIC                                    
000426       MOVE REQU-IDKUNDNR-START  TO W-IDKUNDNR                            
000427                                    W-IDKUNDNR-MIN                        
000428     ELSE                                                                 
000429       MOVE ZERO                 TO W-IDKUNDNR                            
000430                                    W-IDKUNDNR-MIN                        
000431     END-IF                                                               
000432     IF REQU-IDKUNDRF-START NOT = ALL '+'                                 
000433       MOVE REQU-IDKUNDRF-START  TO W-IDKUNDRF                            
000434                                    W-IDKUNDRF-MIN                        
000435     ELSE                                                                 
000436       MOVE SPACE                TO W-IDKUNDRF                            
000437                                    W-IDKUNDRF-MIN                        
000438     END-IF                                                               
000439     IF REQU-IDPRODNR-START NOT = ALL '+' AND                             
000440        REQU-IDPRODNR-START IS NUMERIC                                    
000441       MOVE REQU-IDPRODNR-START  TO W-IDPRODNR                            
000442                                    W-IDPRODNR-MIN                        
000443     ELSE                                                                 
000444       MOVE ZERO                 TO W-IDPRODNR                            
000445                                    W-IDPRODNR-MIN                        
000446     END-IF                                                               
000447     IF REQU-IDKOLLI-START NOT = ALL '+' AND                              
000448        REQU-IDKOLLI-START IS NUMERIC                                     
000449       MOVE REQU-IDKOLLI-START TO W-IDKOLLI                               
000450                                  W-IDKOLLI-MIN                           
000451     ELSE                                                                 
000452       MOVE ZERO                 TO W-IDKOLLI                             
000453                                    W-IDKOLLI-MIN                         
000454     END-IF                                                               
000455                                                                          
000456     PERFORM EB-CHECK-INPUT-FIELDS                                        
000457     IF DATA-ENTERED-YES                                                  
000458       MOVE INF-PRESS-PF11       TO RESP-IDMSG-ERROR                      
000459*      PERFORM MFS-ROER-EJ-FAELT-IN                                       
000460*      PERFORM MFS-LAES-IN-IGEN                                           
000461       PERFORM EA-MID-INDATA-TILL-MOD                                     
000462     ELSE                                                                 
000463       PERFORM MFS-RENSA-FAELT-IN                                         
000464     END-IF                                                               
000465     .                                                                    
000466     EJECT                                                                
000467 EA-MID-INDATA-TILL-MOD SECTION.                                          
000468                                                                          
000469     IF REQU-BEGMT-RAD1-UPD NOT = ALL '+'                                 
000470       MOVE REQU-BEGMT-RAD1-UPD  TO RESP-BEGMT-RAD1-UPD                   
000471     ELSE                                                                 
000472       MOVE ALL-SPACE            TO RESP-BEGMT-RAD1-UPD                   
000473     END-IF                                                               
000474                                                                          
000475     IF REQU-BEGMT-RAD2-UPD NOT = ALL '+'                                 
000476       MOVE REQU-BEGMT-RAD2-UPD  TO RESP-BEGMT-RAD2-UPD                   
000477     ELSE                                                                 
000478       MOVE ALL-SPACE            TO RESP-BEGMT-RAD2-UPD                   
000479     END-IF                                                               
000480                                                                          
000481     IF REQU-ADGMT-GATA-UPD NOT = ALL '+'                                 
000482       MOVE REQU-ADGMT-GATA-UPD  TO RESP-ADGMT-GATA-UPD                   
000483     ELSE                                                                 
000484       MOVE ALL-SPACE            TO RESP-ADGMT-GATA-UPD                   
000485     END-IF                                                               
000486                                                                          
000487     IF REQU-ADGMT-PADR-UPD NOT = ALL '+'                                 
000488       MOVE REQU-ADGMT-PADR-UPD  TO RESP-ADGMT-PADR-UPD                   
000489     ELSE                                                                 
000490       MOVE ALL-SPACE            TO RESP-ADGMT-PADR-UPD                   
000491     END-IF                                                               
000492                                                                          
000493     IF REQU-KDCMD-UPD NOT = ALL '+'                                      
000494       MOVE REQU-KDCMD-UPD       TO RESP-KDCMD-UPD                        
000495       MOVE MFS-ADD-LAES-IN-FAELT                                         
000496                                 TO RESP-KDCMD-UPD-ATTR                   
000497     ELSE                                                                 
000498       MOVE ALL-SPACE            TO RESP-KDCMD-UPD                        
000499     END-IF                                                               
000500                                                                          
000501     IF REQU-IDORDNR7-UPD NOT = ALL '+'                                   
000502       MOVE REQU-IDORDNR7-UPD    TO RESP-IDORDNR7-UPD                     
000503       MOVE MFS-ADD-LAES-IN-FAELT                                         
000504                                 TO RESP-IDORDNR7-UPD-ATTR                
000505     ELSE                                                                 
000506       MOVE ALL-SPACE            TO RESP-IDORDNR7-UPD                     
000507     END-IF                                                               
000508                                                                          
000509     IF REQU-IDPRODNR-UPD NOT = ALL '+'                                   
000510       MOVE REQU-IDPRODNR-UPD    TO RESP-IDPRODNR-UPD                     
000511       MOVE MFS-ADD-LAES-IN-FAELT                                         
000512                                 TO RESP-IDPRODNR-UPD-ATTR                
000513     ELSE                                                                 
000514       MOVE ALL-SPACE            TO RESP-IDPRODNR-UPD                     
000515     END-IF                                                               
000516                                                                          
000517     IF REQU-IDKOLLI-UPD NOT = ALL '+'                                    
000518       MOVE REQU-IDKOLLI-UPD     TO RESP-IDKOLLI-UPD                      
000519       MOVE MFS-ADD-LAES-IN-FAELT                                         
000520                                 TO RESP-IDKOLLI-UPD-ATTR                 
000521     ELSE                                                                 
000522       MOVE ALL-SPACE            TO RESP-IDKOLLI-UPD                      
000523     END-IF                                                               
000524                                                                          
000525     IF REQU-IDPSN-UPD NOT = ALL '+'                                      
000526       MOVE REQU-IDPSN-UPD       TO RESP-IDPSN-UPD                        
000527       MOVE MFS-ADD-LAES-IN-FAELT                                         
000528                                 TO RESP-IDPSN-UPD-ATTR                   
000529     ELSE                                                                 
000530       MOVE ALL-SPACE            TO RESP-IDPSN-UPD                        
000531     END-IF                                                               
000532                                                                          
000533     IF REQU-KDORDKL-UPD NOT = ALL '+'                                    
000534       MOVE REQU-KDORDKL-UPD     TO RESP-KDORDKL-UPD                      
000535       MOVE MFS-ADD-LAES-IN-FAELT                                         
000536                                 TO RESP-KDORDKL-UPD-ATTR                 
000537     ELSE                                                                 
000538       MOVE ALL-SPACE            TO RESP-KDORDKL-UPD                      
000539     END-IF                                                               
000540                                                                          
000541     IF REQU-KDKOLLI-UPD NOT = ALL '+'                                    
000542       MOVE REQU-KDKOLLI-UPD     TO RESP-KDKOLLI-UPD                      
000543       MOVE MFS-ADD-LAES-IN-FAELT                                         
000544                                 TO RESP-KDKOLLI-UPD-ATTR                 
000545     ELSE                                                                 
000546       MOVE ALL-SPACE            TO RESP-KDKOLLI-UPD                      
000547     END-IF                                                               
000548                                                                          
000549     IF REQU-VKORDBTO-KOLLI-UPD NOT = ALL '+'                             
000550       MOVE REQU-VKORDBTO-KOLLI-UPD                                       
000551                                 TO DEC-IDFRIDATA                         
000552       MOVE 6                    TO DEC-KVHELTAL                          
000553       MOVE 1                    TO DEC-KVDECIMAL                         
000554       CALL WDECEDIT          USING DEC-WDECAREA                          
000555       IF DEC-KDSVAR-OK                                                   
000556         MOVE DEC-IDEDITDATA     TO RESP-VKORDBTO-KOLLI-UPD               
000557         MOVE MFS-ADD-LAES-IN-FAELT                                       
000558                                 TO RESP-VKORDBTO-KOLLI-UPD-ATTR          
000559       ELSE                                                               
000560         MOVE MFS-NUM-FAELT-FEL  TO RESP-VKORDBTO-KOLLI-UPD-ATTR          
000561         MOVE ALL-SPACE          TO RESP-VKORDBTO-KOLLI-UPD               
000562       END-IF                                                             
000563     ELSE                                                                 
000564       MOVE ALL-SPACE            TO RESP-VKORDBTO-KOLLI-UPD               
000565     END-IF                                                               
000566                                                                          
000567     PERFORM                                                              
000568     VARYING INDX FROM +1 BY +1                                           
000569       UNTIL INDX > REQU-KVRADER                                          
000570       IF REQU-CMD-LINE (INDX) NOT = ALL '+'                              
000571         MOVE REQU-CMD-LINE (INDX)                                        
000572                                 TO RESP-CMD-LINE (INDX)                  
000573         MOVE MFS-ADD-LAES-IN-FAELT                                       
000574                                 TO RESP-CMD-LINE-ATTR (INDX)             
000575       ELSE                                                               
000576         MOVE ALL-SPACE          TO RESP-CMD-LINE (INDX)                  
000577       END-IF                                                             
000578                                                                          
000579     END-PERFORM                                                          
000580     .                                                                    
000581     EJECT                                                                
000582 EB-CHECK-INPUT-FIELDS SECTION.                                           
000583                                                                          
000584     IF REQU-BEGMT-RAD1-UPD = ALL '+' AND                                 
000585        REQU-BEGMT-RAD2-UPD = ALL '+' AND                                 
000586        REQU-ADGMT-GATA-UPD = ALL '+' AND                                 
000587        REQU-ADGMT-PADR-UPD = ALL '+' AND                                 
000588        REQU-KDCMD-UPD      = ALL '+' AND                                 
000589        REQU-IDORDNR7-UPD   = ALL '+' AND                                 
000590        REQU-IDPRODNR-UPD   = ALL '+' AND                                 
000591        REQU-IDKOLLI-UPD    = ALL '+' AND                                 
000592        REQU-IDPSN-UPD      = ALL '+' AND                                 
000593        REQU-KDORDKL-UPD    = ALL '+' AND                                 
000594        REQU-KDKOLLI-UPD    = ALL '+' AND                                 
000595        REQU-VKORDBTO-KOLLI-UPD = ALL '+'                                 
000596       SET DATA-ENTERED-NO       TO TRUE                                  
000597     ELSE                                                                 
000598       SET DATA-ENTERED-YES      TO TRUE                                  
000599     END-IF                                                               
000600                                                                          
000601     PERFORM                                                              
000602     VARYING INDX FROM +1 BY +1                                           
000603       UNTIL INDX > REQU-KVRADER OR                                       
000604             DATA-ENTERED-YES                                             
000605       IF REQU-CMD-LINE     (INDX) = ALL '+'                              
000606         CONTINUE                                                         
000607       ELSE                                                               
000608         SET DATA-ENTERED-YES    TO TRUE                                  
000609       END-IF                                                             
000610     END-PERFORM                                                          
000611     .                                                                    
000612     EJECT                                                                
000613 F-LAES-VISA-INFO SECTION.                                                
000614                                                                          
000615     MOVE ZERO                   TO RESP-KVRADER                          
000616                                                                          
000617     IF REQU-UPDATE                                                       
000618       PERFORM IMS-GHU-WL446331-CUST                                      
000619     ELSE                                                                 
000620       PERFORM IMS-GHN-WL446331-CUST                                      
000621     END-IF                                                               
000622                                                                          
000623     IF SEGMENT-SAKNAS                                                    
000624       MOVE CUST-MISSING         TO RESP-IDMSG-ERROR                      
000625       MOVE 'IDDISTR-IDKUNDNR'   TO RESP-IDELMT-ERROR                     
000626       MOVE +1                   TO INDX                                  
000627       PERFORM MFS-RENSA-FAELT-UT                                         
000628     ELSE                                                                 
000629       MOVE +1                   TO INDX                                  
000630       PERFORM UNTIL INDX > MAX-KVRADER                                   
000631         IF SEGMENT-FINNS                                                 
000632           IF INDX = 1                                                    
000633             PERFORM FA-LAES-ADRESS                                       
000634             MOVE 4468-IDDISTR   TO RESP-IDDISTR-START                    
000635             MOVE 4468-IDKUNDNR  TO RESP-IDKUNDNR-START                   
000636             MOVE 4468-IDKUNDRF  TO RESP-IDKUNDRF-START                   
000637             MOVE 4468-IDPRODNR  TO RESP-IDPRODNR-START                   
000638             MOVE 4468-IDKOLLI   TO RESP-IDKOLLI-START                    
000639           END-IF                                                         
000640           PERFORM FB-LAEGG-UT-4468-TILL-MOD                              
000641           ADD +1                TO INDX                                  
000642                                    RESP-KVRADER                          
000643         ELSE                                                             
000644           COMPUTE INDX = MAX-KVRADER + 1                                 
000645         END-IF                                                           
000646         PERFORM IMS-GHN-WL446331-CUST                                    
000647       END-PERFORM                                                        
000648                                                                          
000649       IF SEGMENT-FINNS                                                   
000650         MOVE 4468-IDDISTR       TO RESP-IDDISTR-NEXT                     
000651         MOVE 4468-IDKUNDNR      TO RESP-IDKUNDNR-NEXT                    
000652         MOVE 4468-IDKUNDRF      TO RESP-IDKUNDRF-NEXT                    
000653         MOVE 4468-IDPRODNR      TO RESP-IDPRODNR-NEXT                    
000654         MOVE 4468-IDKOLLI       TO RESP-IDKOLLI-NEXT                     
000655         IF RESP-IDMSG-INFO = SPACE                                       
000656           MOVE INF-MORE-INFO-EXISTS                                      
000657                                 TO RESP-IDMSG-INFO                       
000658         END-IF                                                           
000659       ELSE                                                               
000660         MOVE RESP-IDDISTR-START TO RESP-IDDISTR-NEXT                     
000661         MOVE RESP-IDKUNDNR-START                                         
000662                                 TO RESP-IDKUNDNR-NEXT                    
000663         MOVE RESP-IDKUNDRF-START                                         
000664                                 TO RESP-IDKUNDRF-NEXT                    
000665         MOVE RESP-IDPRODNR-START                                         
000666                                 TO RESP-IDPRODNR-NEXT                    
000667         MOVE RESP-IDKOLLI-START TO RESP-IDKOLLI-NEXT                     
000668       END-IF                                                             
000669                                                                          
000670     END-IF                                                               
000671     .                                                                    
000672     EJECT                                                                
000673 FA-LAES-ADRESS SECTION.                                                  
000674                                                                          
000675     MOVE 4468-IDKUNDRF          TO  W-IDKUNDRF-WDQ2                      
000676                                                                          
000677     PERFORM IMS-GET-OHUV                                                 
000678                                                                          
000679     MOVE OHUV-BEGMT-RAD1        TO RESP-BEGMT-RAD1-UT                    
000680     MOVE OHUV-BEGMT-RAD2        TO RESP-BEGMT-RAD2-UT                    
000690     MOVE OHUV-ADGMT-GATA        TO RESP-ADGMT-GATA-UT                    
000691     MOVE OHUV-ADGMT-PADR        TO RESP-ADGMT-PADR-UT                    
000692     .                                                                    
000693     EJECT                                                                
000694 FB-LAEGG-UT-4468-TILL-MOD SECTION.                                       
000695*---LÄGG UT DATA PÅ SKÄRM---*                                             
000696     MOVE 4468-IDKUNDRF          TO WS-IDKUNDRF                           
000697     MOVE WS-IDORDNR7            TO RESP-IDORDNR7-LINE (INDX)             
000698     INSPECT RESP-IDORDNR7-LINE (INDX)                                    
000699       REPLACING LEADING ZERO BY SPACE                                    
000700                                                                          
000701     MOVE 4468-IDPRODNR          TO RESP-IDPRODNR-LINE (INDX)             
000702     INSPECT RESP-IDPRODNR-LINE (INDX)                                    
000703       REPLACING LEADING ZERO BY SPACE                                    
000704                                                                          
000705     MOVE 4468-IDKOLLI           TO RESP-IDKOLLI-LINE (INDX)              
000706     INSPECT RESP-IDKOLLI-LINE (INDX)                                     
000707       REPLACING LEADING ZERO BY SPACE                                    
000708                                                                          
000709                                                                          
000710     MOVE +1                     TO PSN-INDX                              
000711     PERFORM UNTIL PSN-INDX > PSN-MAX-INDX                                
000712       IF PSN-INDX = 1                                                    
000713         MOVE 4468-IDPSN (PSN-INDX)                                       
000714                                 TO RESP-IDPSN-LINE (INDX)                
000715       END-IF                                                             
000716       ADD +1                    TO PSN-INDX                              
000717     END-PERFORM                                                          
000718     INSPECT RESP-IDPSN-LINE (INDX)                                       
000719       REPLACING LEADING ZERO BY SPACE                                    
000720                                                                          
000721     MOVE 4468-KDORDKL           TO RESP-KDORDKL-LINE (INDX)              
000722     MOVE 4468-KDKOLLI           TO RESP-KDKOLLI-LINE (INDX)              
000723                                                                          
000724*                                                                         
000725*                                                                         
000726     IF SI-MEASURES                                                       
000727*      -- SHOW WEIGHT STORED IN KG                                        
000728       MOVE 4468-VKORDBTO-KOLLI TO RESP-VKORDBTO-KOLLI-LINE (INDX)        
000729     ELSE                                                                 
000730*      -- CONVERT WEIGHT IN KG TO LBS                                     
000731       COMPUTE RESP-VKORDBTO-KOLLI-LINE (INDX)  ROUNDED =                 
000732                              4468-VKORDBTO-KOLLI *                       
000733                              CONV-KG-TO-LB                               
000734     END-IF                                                               
000735                                                                          
000736     .                                                                    
000737     EJECT                                                                
000738 G-KOLLA-INPUT SECTION.                                                   
000739                                                                          
000740     MOVE JA                     TO INDATA-SW                             
000741     IF REQU-BEGMT-RAD1-UPD = ALL '+' AND                                 
000742        REQU-BEGMT-RAD2-UPD = ALL '+' AND                                 
000743        REQU-ADGMT-GATA-UPD = ALL '+' AND                                 
000744        REQU-ADGMT-PADR-UPD = ALL '+'                                     
000745       IF REQU-IDORDNR7-UPD NOT = ALL '+'                                 
000746         PERFORM GA-KOLLA-NY-RAD                                          
000747       ELSE                                                               
000748         PERFORM GB-KOLLA-UPPDATERA-RAD                                   
000749       END-IF                                                             
000750     ELSE                                                                 
000751       PERFORM GC-KOLLA-ADRESS                                            
000752     END-IF                                                               
000753     IF INDATA-FEL                                                        
000754       MOVE REQU-IDDISTR-START   TO RESP-IDDISTR-START                    
000755                                    RESP-IDDISTR-NEXT                     
000756       MOVE REQU-IDKUNDNR-START  TO RESP-IDKUNDNR-START                   
000757                                    RESP-IDKUNDNR-NEXT                    
000758       MOVE REQU-IDKUNDRF-START  TO RESP-IDKUNDRF-START                   
000759                                    RESP-IDKUNDRF-NEXT                    
000760       MOVE REQU-IDPRODNR-START  TO RESP-IDPRODNR-START                   
000761                                    RESP-IDPRODNR-NEXT                    
000762       MOVE REQU-IDKOLLI-START   TO RESP-IDKOLLI-START                    
000763                                    RESP-IDKOLLI-NEXT                     
000764     END-IF                                                               
000765     .                                                                    
000766     EJECT                                                                
000767 GA-KOLLA-NY-RAD SECTION.                                                 
000768                                                                          
000769                                                                          
000770*---ANVÄNDS FÖR ATT SE SÅ ATT MAN INTE FÖRSÖKER LÄGGA TILL                
000771*---EN NY RAD SAMTIDIGT SOM MAN UPPDATERAR GAMLA RADER                    
000772     MOVE +1                     TO INDX                                  
000773     PERFORM UNTIL INDX > REQU-KVRADER                                    
000774       IF REQU-CMD-LINE (INDX) = 'C'                                      
000775         COMPUTE INDX = MAX-KVRADER + 1                                   
000776         MOVE NEJ                TO INDATA-SW                             
000777         MOVE FORBIDDEN-UPDATE   TO RESP-IDMSG-ERROR                      
000778         MOVE MFS-ALFA-FAELT-FEL TO RESP-CMD-LINE-ATTR (INDX)             
000779       ELSE                                                               
000780         MOVE JA                 TO INDATA-SW                             
000781       END-IF                                                             
000782       ADD +1                    TO INDX                                  
000783     END-PERFORM                                                          
000784                                                                          
000785     MOVE REQU-VKORDBTO-KOLLI-UPD                                         
000786                                 TO DEC-IDFRIDATA                         
000787     MOVE 6                      TO DEC-KVHELTAL                          
000788     MOVE 1                      TO DEC-KVDECIMAL                         
000789     CALL WDECEDIT USING DEC-WDECAREA                                     
000790     IF DEC-KDSVAR-OK                                                     
000791       MOVE DEC-IDEDITDATA       TO RESP-VKORDBTO-KOLLI-UPD               
000792       MOVE MFS-NUM-FAELT-RAETT  TO RESP-VKORDBTO-KOLLI-UPD-ATTR          
000793     ELSE                                                                 
000794       MOVE MFS-NUM-FAELT-FEL    TO RESP-VKORDBTO-KOLLI-UPD-ATTR          
000795                                                                          
000796       MOVE NEJ                  TO INDATA-SW                             
000797     END-IF                                                               
000798                                                                          
000799     IF REQU-KDCMD-UPD = 'A' OR 'D'                                       
000800       MOVE MFS-ALFA-FAELT-RAETT TO RESP-KDCMD-UPD-ATTR                   
000801     ELSE                                                                 
000802       MOVE MFS-ALFA-FAELT-FEL   TO RESP-KDCMD-UPD-ATTR                   
000803       MOVE NEJ                  TO INDATA-SW                             
000804     END-IF                                                               
000805                                                                          
000806     IF REQU-IDORDNR7-UPD NOT NUMERIC                                     
000807       MOVE MFS-NUM-FAELT-FEL    TO RESP-IDORDNR7-UPD-ATTR                
000808       MOVE NEJ                  TO INDATA-SW                             
000809     ELSE                                                                 
000810       MOVE MFS-NUM-FAELT-RAETT  TO RESP-IDORDNR7-UPD-ATTR                
000811     END-IF                                                               
000812                                                                          
000813     IF REQU-IDPRODNR-UPD NOT NUMERIC                                     
000814       MOVE MFS-NUM-FAELT-FEL    TO RESP-IDPRODNR-UPD-ATTR                
000815       MOVE NEJ                  TO INDATA-SW                             
000816     ELSE                                                                 
000817       MOVE MFS-NUM-FAELT-RAETT  TO RESP-IDPRODNR-UPD-ATTR                
000818     END-IF                                                               
000819                                                                          
000820     IF REQU-IDKOLLI-UPD NOT NUMERIC                                      
000821       MOVE MFS-NUM-FAELT-FEL TO RESP-IDKOLLI-UPD-ATTR                    
000822       MOVE NEJ               TO INDATA-SW                                
000823     ELSE                                                                 
000824       MOVE MFS-NUM-FAELT-RAETT                                           
000825                              TO RESP-IDKOLLI-UPD-ATTR                    
000826     END-IF                                                               
000827                                                                          
000828     IF REQU-KDORDKL-UPD NOT NUMERIC                                      
000829       MOVE MFS-NUM-FAELT-FEL TO RESP-KDORDKL-UPD-ATTR                    
000830       MOVE NEJ               TO INDATA-SW                                
000831     ELSE                                                                 
000832       MOVE MFS-NUM-FAELT-RAETT                                           
000833                              TO RESP-KDORDKL-UPD-ATTR                    
000834     END-IF                                                               
000835                                                                          
000836     IF REQU-KDKOLLI-UPD = ALL '+'                                        
000837       MOVE MFS-ALFA-FAELT-FEL                                            
000838                              TO RESP-KDKOLLI-UPD-ATTR                    
000839       MOVE NEJ               TO INDATA-SW                                
000840     ELSE                                                                 
000841       MOVE MFS-ALFA-FAELT-RAETT                                          
000842                              TO RESP-KDKOLLI-UPD-ATTR                    
000843     END-IF                                                               
000844                                                                          
000845     IF REQU-KDCMD-UPD = 'A'                                              
000846*----KOLLAR   SÅ ATT DET FINNS PSN-NUMMER PÅ 1165-BASEN                   
000847*----PSN-NUMMER   MÅSTE FINNAS PÅ BAS ANNARS FÄLT-FEL                     
000848       IF REQU-IDPSN-UPD NOT = ALL '+'                                    
000849         MOVE REQU-IDPSN-UPD     TO W-IDPSN                               
000850         PERFORM IMS-GET-1165                                             
000851         IF SEGMENT-FINNS                                                 
000852           MOVE MFS-ALFA-FAELT-RAETT                                      
000853                                 TO RESP-IDPSN-UPD-ATTR                   
000854         ELSE                                                             
000855           MOVE MFS-ALFA-FAELT-FEL                                        
000856                                 TO RESP-IDPSN-UPD-ATTR                   
000857           MOVE NEJ              TO INDATA-SW                             
000858         END-IF                                                           
000859       ELSE                                                               
000860*--------FÖR ATT FÅ NOLL I ALLA INDEX I BASEN PÅ 4468-SEGMENTET           
000861         MOVE ZERO               TO REQU-IDPSN-UPD                        
000862       END-IF                                                             
000863                                                                          
000864     ELSE                                                                 
000865       IF REQU-KDCMD-UPD = 'D'                                            
000866         MOVE REQU-IDORDNR7-UPD  TO W-IDORDNR7                            
000867         MOVE REQU-IDPRODNR-UPD  TO W-IDPRODNR                            
000868         MOVE REQU-IDKOLLI-UPD   TO W-IDKOLLI                             
000869         PERFORM IMS-GHU-WL446331-CUST                                    
000870         IF SEGMENT-FINNS                                                 
000871           MOVE MFS-ALFA-FAELT-RAETT                                      
000872                                 TO RESP-KDCMD-UPD-ATTR                   
000873         ELSE                                                             
000874           MOVE NEJ              TO INDATA-SW                             
000875****       MOVE CASE-MISSING     TO RESP-IDMSG-ERROR                      
000876****       MOVE 'IDKOLLI'        TO RESP-IDELMT-ERROR                     
000877           MOVE MFS-NUM-FAELT-FEL                                         
000878                                 TO RESP-IDKOLLI-UPD-ATTR                 
000879         END-IF                                                           
000880       END-IF                                                             
000881     END-IF                                                               
000882                                                                          
000883     IF INDATA-OK                                                         
000884*---KOLLAR SÅ ATT KUNDNUMMER OCH PRODUKTIONSNUMMER FINNS                  
000885*---PÅ BILDEN, DESSA MÅSTE FINNAS ANNARS FÄLT-FEL                         
000886       MOVE +1                   TO INDX                                  
000887       PERFORM UNTIL INDX > REQU-KVRADER                                  
000888         INSPECT REQU-IDORDNR7-UPD REPLACING LEADING SPACE BY ZERO        
000889         INSPECT REQU-IDPRODNR-UPD REPLACING LEADING SPACE BY ZERO        
000890         INSPECT REQU-IDORDNR7-LINE (INDX)                                
000891                                 REPLACING LEADING SPACE BY ZERO          
000892         INSPECT REQU-IDPRODNR-LINE (INDX)                                
000893                                 REPLACING LEADING SPACE BY ZERO          
000894                                                                          
000895*---KOLLAR ATT KUNDNR OCH ORDERNR ÄR PÅ SAMMA RAD                         
000896         IF REQU-IDORDNR7-LINE (INDX) = REQU-IDORDNR7-UPD AND             
000897            REQU-IDPRODNR-LINE (INDX) = REQU-IDPRODNR-UPD                 
000898           MOVE JA               TO INDATA-SW                             
000899           COMPUTE INDX = MAX-KVRADER + 1                                 
000900         ELSE                                                             
000901           MOVE NEJ              TO INDATA-SW                             
000902           MOVE MFS-NUM-FAELT-FEL                                         
000903                                 TO RESP-IDORDNR7-UPD-ATTR                
000904                                    RESP-IDPRODNR-UPD-ATTR                
000905         END-IF                                                           
000906         ADD +1                  TO INDX                                  
000907       END-PERFORM                                                        
000908     END-IF                                                               
000909                                                                          
000910     IF INDATA-FEL                                                        
000911       MOVE NEJ                  TO ALLT-SW                               
000912       IF RESP-IDMSG-ERROR = SPACE                                        
000913         MOVE ERR-CORR-HILITE-FLDS                                        
000914                                 TO RESP-IDMSG-ERROR                      
000915       END-IF                                                             
000916       PERFORM MFS-ROER-EJ-FAELT-UT                                       
000917       PERFORM MFS-ROER-EJ-FAELT-IN                                       
000918     END-IF                                                               
000919     .                                                                    
000920     EJECT                                                                
000921 GB-KOLLA-UPPDATERA-RAD SECTION.                                          
000922                                                                          
000923*---NEDANSTÅENDE SNURRA ANVÄNDS FÖR ATT KONTROLLERA                       
000924*---ATT ANVÄNDAREN HAR MARKERAT MED C FÖR ATT VISA                        
000925*---ATT HAN VILL GÖRA EN UPPDATERING AV POSTEN                            
000926     MOVE +1                     TO INDX                                  
000927     PERFORM UNTIL INDX > REQU-KVRADER                                    
000928       IF REQU-CMD-LINE (INDX) = 'C'                                      
000929         COMPUTE INDX = MAX-KVRADER + 1                                   
000930         MOVE JA                 TO INDATA-SW                             
000931       ELSE                                                               
000932         MOVE NEJ                TO INDATA-SW                             
000933       END-IF                                                             
000934       ADD +1                    TO INDX                                  
000935     END-PERFORM                                                          
000936                                                                          
000937     IF INDATA-FEL                                                        
000938       MOVE FILL-CMD             TO RESP-IDMSG-ERROR                      
000939       MOVE 'CMD'                TO RESP-IDELMT-ERROR                     
000940     END-IF                                                               
000941     IF INDATA-OK                                                         
000942       MOVE +1                   TO INDX                                  
000943       PERFORM UNTIL INDX > REQU-KVRADER                                  
000944         IF REQU-CMD-LINE (INDX) = 'C'                                    
000945           MOVE REQU-VKORDBTO-KOLLI-LINE (INDX)                           
000946                                 TO DEC-IDFRIDATA                         
000947           MOVE 6                TO DEC-KVHELTAL                          
000948           MOVE 1                TO DEC-KVDECIMAL                         
000949           CALL WDECEDIT USING DEC-WDECAREA                               
000950           IF DEC-KDSVAR-OK                                               
000951             MOVE DEC-IDEDITDATA TO RESP-VKORDBTO-KOLLI-LINE(INDX)        
000952             MOVE MFS-NUM-FAELT-RAETT                                     
000953                                 TO                                       
000954                             RESP-VKORDBTO-KOLLI-LINE-ATTR (INDX)         
000955           ELSE                                                           
000956             MOVE MFS-NUM-FAELT-FEL                                       
000957                                 TO                                       
000958                             RESP-VKORDBTO-KOLLI-LINE-ATTR (INDX)         
000959             MOVE NEJ            TO INDATA-SW                             
000960           END-IF                                                         
000961         END-IF                                                           
000962         ADD +1                  TO INDX                                  
000963       END-PERFORM                                                        
000964     END-IF                                                               
000965                                                                          
000966     IF INDATA-FEL                                                        
000967       MOVE NEJ                  TO ALLT-SW                               
000968       IF RESP-IDMSG-ERROR = SPACE                                        
000969         MOVE ERR-CORR-HILITE-FLDS                                        
000970                                 TO RESP-IDMSG-ERROR                      
000971       END-IF                                                             
000972       PERFORM MFS-ROER-EJ-FAELT-UT                                       
000973       PERFORM MFS-ROER-EJ-FAELT-IN                                       
000974       COMPUTE INDX = MAX-KVRADER + 1                                     
000975     END-IF                                                               
000976     .                                                                    
000977     EJECT                                                                
000978 GC-KOLLA-ADRESS SECTION.                                                 
000979                                                                          
000980*---ANVÄNDS FÖR ATT SE SÅ ATT MAN INTE FÖRSÖKER                           
000981*---UPPDATERA BÅDE ADRESS OCH RADER SAMTIDIGT                             
000982     MOVE +1                     TO INDX                                  
000983     PERFORM UNTIL INDX > REQU-KVRADER                                    
000984       IF REQU-CMD-LINE (INDX) = 'C'                                      
000985         COMPUTE INDX = MAX-KVRADER + 1                                   
000986         MOVE NEJ                TO INDATA-SW                             
000987         MOVE FORBIDDEN-UPDATE   TO RESP-IDMSG-ERROR                      
000988         MOVE MFS-ALFA-FAELT-FEL TO RESP-CMD-LINE-ATTR (INDX)             
000989       ELSE                                                               
000990         MOVE JA                 TO INDATA-SW                             
000991       END-IF                                                             
000992       ADD +1                    TO INDX                                  
000993     END-PERFORM                                                          
000994                                                                          
000995*---ANVÄNDS FÖR ATT SE SÅ ATT MAN INTE FÖRSÖKER                           
000996*---UPPDATERA BÅDE ADRESS OCH LÄGGA IN NY RAD SAMTIDIGT                   
000997     IF REQU-IDORDNR7-UPD NOT = ALL '+'                                   
000998       MOVE NEJ                  TO INDATA-SW                             
000999       MOVE FORBIDDEN-UPDATE     TO RESP-IDMSG-ERROR                      
001000       MOVE MFS-ALFA-FAELT-FEL   TO RESP-KDCMD-UPD-ATTR                   
001001       MOVE MFS-ALFA-FAELT-FEL   TO RESP-IDORDNR7-UPD-ATTR                
001002       MOVE MFS-ALFA-FAELT-FEL   TO RESP-IDPRODNR-UPD-ATTR                
001003       MOVE MFS-ALFA-FAELT-FEL   TO RESP-IDKOLLI-UPD-ATTR                 
001004       MOVE MFS-ALFA-FAELT-FEL   TO RESP-IDPSN-UPD-ATTR                   
001005       MOVE MFS-ALFA-FAELT-FEL   TO RESP-KDORDKL-UPD-ATTR                 
001006       MOVE MFS-ALFA-FAELT-FEL   TO RESP-KDKOLLI-UPD-ATTR                 
001007       MOVE MFS-ALFA-FAELT-FEL   TO RESP-VKORDBTO-KOLLI-UPD-ATTR          
001008     END-IF                                                               
001009                                                                          
001010     IF INDATA-FEL                                                        
001011       MOVE NEJ                  TO ALLT-SW                               
001012       IF RESP-IDMSG-ERROR = SPACE                                        
001013         MOVE ERR-CORR-HILITE-FLDS                                        
001014                                 TO RESP-IDMSG-ERROR                      
001015       END-IF                                                             
001016       PERFORM MFS-ROER-EJ-FAELT-UT                                       
001017       PERFORM MFS-ROER-EJ-FAELT-IN                                       
001018     END-IF                                                               
001019     .                                                                    
001020     EJECT                                                                
001021 H-UPPDATERA SECTION.                                                     
001022                                                                          
001023                                                                          
001024     IF REQU-BEGMT-RAD1-UPD = ALL '+' AND                                 
001025        REQU-BEGMT-RAD2-UPD = ALL '+' AND                                 
001026        REQU-ADGMT-GATA-UPD = ALL '+' AND                                 
001027        REQU-ADGMT-PADR-UPD = ALL '+'                                     
001028       IF REQU-IDORDNR7-UPD NOT = ALL '+'                                 
001029         PERFORM HB-UPPDATERA-NY-RAD                                      
001030       ELSE                                                               
001031         PERFORM HC-UPPDATERA-RAD                                         
001032       END-IF                                                             
001033     ELSE                                                                 
001034       PERFORM HA-UPPDATERA-ADRESS                                        
001035     END-IF                                                               
001036                                                                          
001037*---ANVÄNDS FÖR ATT HITTA RÄTT KUND PÅ WDQ2                               
001038     MOVE REQU-IDDISTR-START     TO W-IDDISTR                             
001039     MOVE REQU-IDKUNDNR-START    TO W-IDKUNDNR                            
001040     MOVE REQU-IDKUNDRF-START    TO W-IDKUNDRF                            
001041     MOVE REQU-IDPRODNR-START    TO W-IDPRODNR                            
001042     MOVE REQU-IDKOLLI-START     TO W-IDKOLLI                             
001043*                                                                         
001044     IF RESP-IDMSG-INFO NOT = INF-WEIGHT-ADJUSTED                         
001045       MOVE INF-UPDATE-DONE      TO RESP-IDMSG-INFO                       
001046     END-IF                                                               
001047     PERFORM MFS-FORM-ATTR                                                
001048     PERFORM MFS-RENSA-FAELT-IN                                           
001049                                                                          
001050     .                                                                    
001051     EJECT                                                                
001052 HA-UPPDATERA-ADRESS SECTION.                                             
001053                                                                          
001054     MOVE REQU-IDKUNDRF-START    TO W-IDKUNDRF-WDQ2                       
001055                                                                          
001056     PERFORM IMS-GET-OHUV                                                 
001057     IF REQU-BEGMT-RAD1-UPD = ALL '+'                                     
001058       MOVE REQU-BEGMT-RAD1-UT   TO OHUV-BEGMT-RAD1                       
001059     ELSE                                                                 
001060       MOVE REQU-BEGMT-RAD1-UPD  TO OHUV-BEGMT-RAD1                       
001061     END-IF                                                               
001062                                                                          
001063     IF REQU-BEGMT-RAD2-UPD = ALL '+'                                     
001064       MOVE REQU-BEGMT-RAD2-UT   TO OHUV-BEGMT-RAD2                       
001065     ELSE                                                                 
001066       MOVE REQU-BEGMT-RAD2-UPD  TO OHUV-BEGMT-RAD2                       
001067     END-IF                                                               
001068                                                                          
001069     IF REQU-ADGMT-GATA-UPD = ALL '+'                                     
001070       MOVE REQU-ADGMT-GATA-UT   TO OHUV-ADGMT-GATA                       
001071     ELSE                                                                 
001072       MOVE REQU-ADGMT-GATA-UPD  TO OHUV-ADGMT-GATA                       
001073     END-IF                                                               
001074                                                                          
001075     IF REQU-ADGMT-PADR-UPD = ALL '+'                                     
001076       MOVE REQU-ADGMT-PADR-UT   TO OHUV-ADGMT-PADR                       
001077     ELSE                                                                 
001078       MOVE REQU-ADGMT-PADR-UPD  TO OHUV-ADGMT-PADR                       
001079     END-IF                                                               
001080                                                                          
001081     PERFORM IMS-REPL-OHUV                                                
001082     .                                                                    
001083     EJECT                                                                
001084 HB-UPPDATERA-NY-RAD SECTION.                                             
001085                                                                          
001086     IF REQU-KDCMD-UPD = 'A'                                              
001087       MOVE W-IDKUNDNR           TO 4468-IDKUNDNR                         
001088       MOVE W-IDDISTR            TO 4468-IDDISTR                          
001089       MOVE REQU-IDORDNR7-UPD    TO 4468-IDKUNDRF                         
001090       MOVE REQU-IDPRODNR-UPD    TO 4468-IDPRODNR                         
001091       MOVE REQU-IDKOLLI-UPD     TO 4468-IDKOLLI                          
001092       MOVE REQU-KDORDKL-UPD     TO 4468-KDORDKL                          
001093       MOVE REQU-KDKOLLI-UPD     TO 4468-KDKOLLI                          
001094       MOVE REQU-VKORDBTO-KOLLI-UPD                                       
001095                                 TO DEC-IDFRIDATA                         
001096       MOVE 6                    TO DEC-KVHELTAL                          
001097       MOVE 1                    TO DEC-KVDECIMAL                         
001098       CALL WDECEDIT USING DEC-WDECAREA                                   
001099       IF DEC-KDSVAR-OK                                                   
001100         IF SI-MEASURES                                                   
001101*          --- STORE DATA ENTERED IN KG                                   
001102           MOVE DEC-IDEDITDATA TO 4468-VKORDBTO-KOLLI                     
001103         ELSE                                                             
001104*          --- CONVERT DATA ENTERED IN LBS TO KG                          
001105           COMPUTE 4468-VKORDBTO-KOLLI   ROUNDED =                        
001106                   DEC-IDEDITDATA * CONV-LB-TO-KG                         
001107*          --- CHECK THAT CONVERSION BACK TO LBS GIVES                    
001108*          --- THE SAME RESULT AS THE ORIGINAL INPUT VALUE.               
001109*          --- OTHERWISE ADJUST THE KG VALUE (MAY NOT HELP)               
001110           MOVE DEC-IDEDITDATA TO WS-VKORDBTO-KOLLI-ORIG                  
001111           COMPUTE WS-VKORDBTO-KOLLI-BACK ROUNDED =                       
001112                   4468-VKORDBTO-KOLLI * CONV-KG-TO-LB                    
001113                                                                          
001114           IF WS-VKORDBTO-KOLLI-ORIG < WS-VKORDBTO-KOLLI-BACK             
001115*            SUBTRACT 0.1  FROM 4468-VKORDBTO-KOLLI                       
001116             MOVE INF-WEIGHT-ADJUSTED TO RESP-IDMSG-INFO                  
001117           END-IF                                                         
001118           IF WS-VKORDBTO-KOLLI-ORIG > WS-VKORDBTO-KOLLI-BACK             
001119*            ADD      0.1  TO   4468-VKORDBTO-KOLLI                       
001120             MOVE INF-WEIGHT-ADJUSTED TO RESP-IDMSG-INFO                  
001121           END-IF                                                         
001122         END-IF                                                           
001123       END-IF                                                             
001124                                                                          
001125       MOVE +1                   TO PSN-INDX                              
001126       PERFORM UNTIL PSN-INDX > PSN-MAX-INDX                              
001127         IF PSN-INDX = 1                                                  
001128           MOVE REQU-IDPSN-UPD   TO 4468-IDPSN (PSN-INDX)                 
001129         ELSE                                                             
001130           MOVE ZERO             TO 4468-IDPSN (PSN-INDX)                 
001131         END-IF                                                           
001132         ADD +1                  TO PSN-INDX                              
001133       END-PERFORM                                                        
001134                                                                          
001135       PERFORM IMS-ISRT-4468                                              
001136                                                                          
001137     ELSE                                                                 
001138       IF REQU-KDCMD-UPD = 'D'                                            
001139         MOVE REQU-IDORDNR7-UPD  TO W-IDKUNDRF                            
001140         MOVE REQU-IDPRODNR-UPD  TO W-IDPRODNR                            
001141         MOVE REQU-IDKOLLI-UPD   TO W-IDKOLLI                             
001142                                                                          
001143         PERFORM IMS-GHU-WL446331-CUST                                    
001144         IF SEGMENT-FINNS                                                 
001145           PERFORM IMS-DLET-4468                                          
001146         END-IF                                                           
001147       END-IF                                                             
001148     END-IF                                                               
001149     .                                                                    
001150     EJECT                                                                
001151 HC-UPPDATERA-RAD SECTION.                                                
001152     MOVE +1                     TO INDX                                  
001153     PERFORM UNTIL INDX > REQU-KVRADER                                    
001154       IF REQU-CMD-LINE (INDX) = 'C'                                      
001155         INSPECT REQU-IDORDNR7-LINE (INDX)                                
001156                            REPLACING LEADING SPACE BY ZERO               
001157         MOVE REQU-IDORDNR7-LINE (INDX)                                   
001158                                 TO W-IDORDNR7                            
001159                                                                          
001160         INSPECT REQU-IDPRODNR-LINE (INDX)                                
001161                            REPLACING LEADING SPACE BY ZERO               
001162         MOVE REQU-IDPRODNR-LINE (INDX)                                   
001163                                 TO W-IDPRODNR                            
001164                                                                          
001165         INSPECT REQU-IDKOLLI-LINE (INDX)                                 
001166                            REPLACING LEADING SPACE BY ZERO               
001167         MOVE REQU-IDKOLLI-LINE (INDX) TO W-IDKOLLI                       
001168                                                                          
001169         PERFORM IMS-GHU-WL446331-CUST                                    
001170         IF SEGMENT-FINNS                                                 
001171           MOVE REQU-KDKOLLI-LINE (INDX)                                  
001172                                 TO 4468-KDKOLLI                          
001173           MOVE REQU-VKORDBTO-KOLLI-LINE (INDX)                           
001174                                 TO DEC-IDFRIDATA                         
001175           MOVE 6                TO DEC-KVHELTAL                          
001176           MOVE 1                TO DEC-KVDECIMAL                         
001177           CALL WDECEDIT USING DEC-WDECAREA                               
001178           IF DEC-KDSVAR-OK                                               
001179             IF US-MEASURES                                               
001180*              --- CONVERT WEIGHT ENTERED IN LBS TO KG                    
001181               COMPUTE 4468-VKORDBTO-KOLLI ROUNDED =                      
001182                       DEC-IDEDITDATA * CONV-LB-TO-KG                     
001183*              --- CHECK THAT CONVERSION BACK TO LBS GIVES                
001184*              --- THE SAME RESULT AS THE ORIGINAL INPUT VALUE.           
001185*              --- OTHERWISE ADJUST THE KG VALUE                          
001186               MOVE DEC-IDEDITDATA TO WS-VKORDBTO-KOLLI-ORIG              
001187               COMPUTE WS-VKORDBTO-KOLLI-BACK ROUNDED =                   
001188                       4468-VKORDBTO-KOLLI * CONV-KG-TO-LB                
001189                                                                          
001190               IF WS-VKORDBTO-KOLLI-ORIG < WS-VKORDBTO-KOLLI-BACK         
001191*                SUBTRACT 0.1 FROM 4468-VKORDBTO-KOLLI                    
001192                 MOVE INF-WEIGHT-ADJUSTED TO RESP-IDMSG-INFO              
001193               END-IF                                                     
001194               IF WS-VKORDBTO-KOLLI-ORIG > WS-VKORDBTO-KOLLI-BACK         
001195*                ADD  0.1  TO   4468-VKORDBTO-KOLLI                       
001196                 MOVE INF-WEIGHT-ADJUSTED TO RESP-IDMSG-INFO              
001197               END-IF                                                     
001198             ELSE                                                         
001199*               --- STORE WEIGHT ENTERED IN KG                            
001200                MOVE DEC-IDEDITDATA TO 4468-VKORDBTO-KOLLI                
001201             END-IF                                                       
001202           END-IF                                                         
001203           PERFORM IMS-REPL-4468                                          
001204         END-IF                                                           
001205       END-IF                                                             
001206       ADD +1                    TO INDX                                  
001207     END-PERFORM                                                          
001208     .                                                                    
001209     EJECT                                                                
001210 MFS-RENSA-FAELT-UT SECTION.                                              
001211                                                                          
001212*    --- ALLA UTDATA-FÄLT                                                 
001213*    --- INKL. BLÄDDRINGSNYCKLAR                                          
001214     MOVE +1                     TO INDX                                  
001215     PERFORM UNTIL INDX > MAX-KVRADER                                     
001216       MOVE ALL-SPACE            TO RESP-CMD-LINE (INDX)                  
001217                                    RESP-IDORDNR7-LINE (INDX)             
001218                                    RESP-IDPRODNR-LINE (INDX)             
001219                                    RESP-IDKOLLI-LINE (INDX)              
001220                                    RESP-IDPSN-LINE (INDX)                
001221                                    RESP-KDORDKL-LINE (INDX)              
001222                                    RESP-KDKOLLI-LINE (INDX)              
001223                                    RESP-VKORDBTO-KOLLI-LINE(INDX)        
001224     ADD +1                      TO INDX                                  
001225     END-PERFORM                                                          
001226     MOVE ALL-SPACE              TO RESP-BEGMT-RAD1-UT                    
001227                                    RESP-BEGMT-RAD2-UT                    
001228                                    RESP-ADGMT-GATA-UT                    
001229                                    RESP-ADGMT-PADR-UT                    
001230                                    RESP-KDCMD-UPD                        
001231                                    RESP-IDORDNR7-UPD                     
001232                                    RESP-IDPRODNR-UPD                     
001233                                    RESP-IDKOLLI-UPD                      
001234                                    RESP-IDPSN-UPD                        
001235                                    RESP-KDORDKL-UPD                      
001236                                    RESP-KDKOLLI-UPD                      
001237                                    RESP-VKORDBTO-KOLLI-UPD               
001238     .                                                                    
001239     SKIP3                                                                
001240 MFS-RENSA-RAD-UT SECTION.                                                
001241                                                                          
001242*    --- ALLA UTDATA-FÄLT                                                 
001243*    --- INKL. BLÄDDRINGSNYCKLAR                                          
001244     MOVE ALL-SPACE              TO RESP-CMD-LINE (INDX)                  
001245                                    RESP-IDORDNR7-LINE (INDX)             
001246                                    RESP-IDPRODNR-LINE (INDX)             
001247                                    RESP-IDKOLLI-LINE (INDX)              
001248                                    RESP-IDPSN-LINE (INDX)                
001249                                    RESP-KDORDKL-LINE (INDX)              
001250                                    RESP-KDKOLLI-LINE (INDX)              
001251                                    RESP-VKORDBTO-KOLLI-LINE(INDX)        
001252     .                                                                    
001253     SKIP3                                                                
001254 MFS-RENSA-FAELT-IN SECTION.                                              
001255                                                                          
001256*    --- ALLA INDATA-FÄLT                                                 
001257     MOVE ALL-SPACE              TO RESP-BEGMT-RAD1-UPD                   
001258                                    RESP-BEGMT-RAD2-UPD                   
001259                                    RESP-ADGMT-GATA-UPD                   
001260                                    RESP-ADGMT-PADR-UPD                   
001261                                    RESP-KDCMD-UPD                        
001262                                    RESP-IDORDNR7-UPD                     
001263                                    RESP-IDPRODNR-UPD                     
001264                                    RESP-IDKOLLI-UPD                      
001265                                    RESP-IDPSN-UPD                        
001266                                    RESP-KDKOLLI-UPD                      
001267                                    RESP-KDORDKL-UPD                      
001268                                    RESP-VKORDBTO-KOLLI-UPD               
001269     PERFORM                                                              
001270     VARYING INDX FROM +1 BY +1                                           
001271       UNTIL INDX > MAX-KVRADER                                           
001272       MOVE ALL-SPACE            TO RESP-CMD-LINE (INDX)                  
001273     END-PERFORM                                                          
001274     .                                                                    
001275     EJECT                                                                
001276 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
001277                                                                          
001278*    --- ALLA UTDATA-FÄLT                                                 
001279*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
001280     MOVE ALL-PLUS               TO RESP-BEGMT-RAD1-UT                    
001281                                    RESP-BEGMT-RAD2-UT                    
001282                                    RESP-ADGMT-GATA-UT                    
001283                                    RESP-ADGMT-PADR-UT                    
001284     MOVE +1                     TO INDX                                  
001285     PERFORM UNTIL INDX > MAX-KVRADER                                     
001286       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
001287       ADD +1                    TO INDX                                  
001288     END-PERFORM                                                          
001289     .                                                                    
001290     SKIP2                                                                
001291 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
001292                                                                          
001293*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
001294     MOVE ALL-PLUS               TO RESP-CMD-LINE (INDX)                  
001295                                    RESP-IDORDNR7-LINE (INDX)             
001296                                    RESP-IDPRODNR-LINE (INDX)             
001297                                    RESP-IDKOLLI-LINE (INDX)              
001298                                    RESP-IDPSN-LINE (INDX)                
001299                                    RESP-KDORDKL-LINE (INDX)              
001300                                    RESP-KDKOLLI-LINE (INDX)              
001301                                    RESP-VKORDBTO-KOLLI-LINE(INDX)        
001302     .                                                                    
001303     SKIP3                                                                
001304 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
001305                                                                          
001306*    --- ALLA INDATA-FÄLT                                                 
001307     MOVE ALL-PLUS               TO RESP-BEGMT-RAD1-UPD                   
001308                                    RESP-BEGMT-RAD2-UPD                   
001309                                    RESP-ADGMT-GATA-UPD                   
001310                                    RESP-ADGMT-PADR-UPD                   
001311                                    RESP-KDCMD-UPD                        
001312                                    RESP-IDORDNR7-UPD                     
001313                                    RESP-IDPRODNR-UPD                     
001314                                    RESP-IDPSN-UPD                        
001315                                    RESP-IDKOLLI-UPD                      
001316                                    RESP-KDORDKL-UPD                      
001317                                    RESP-KDKOLLI-UPD                      
001318                                    RESP-VKORDBTO-KOLLI-UPD               
001319     PERFORM                                                              
001320     VARYING INDX FROM +1 BY +1                                           
001321       UNTIL INDX > MAX-KVRADER                                           
001322       MOVE ALL-PLUS             TO RESP-CMD-LINE (INDX)                  
001323     END-PERFORM                                                          
001324     .                                                                    
001325     EJECT                                                                
001326 MFS-FORM-ATTR SECTION.                                                   
001327                                                                          
001328*    --- ALLA INDATA-FÄLT                                                 
001329     MOVE MFS-FORMATETS-ATTR     TO RESP-KDCMD-UPD-ATTR                   
001330                                    RESP-IDORDNR7-UPD-ATTR                
001331                                    RESP-IDPRODNR-UPD-ATTR                
001332                                    RESP-IDKOLLI-UPD-ATTR                 
001333                                    RESP-KDKOLLI-UPD-ATTR                 
001334                                    RESP-IDPSN-UPD-ATTR                   
001335                                    RESP-KDORDKL-UPD-ATTR                 
001336                                    RESP-VKORDBTO-KOLLI-UPD-ATTR          
001337     MOVE +1                     TO INDX                                  
001338     PERFORM UNTIL INDX > MAX-KVRADER                                     
001339       MOVE MFS-FORMATETS-ATTR   TO RESP-CMD-LINE-ATTR (INDX)             
001340                                    RESP-VKORDBTO-KOLLI-LINE-ATTR         
001341                                      (INDX)                              
001342       ADD +1                    TO INDX                                  
001343     END-PERFORM                                                          
001344     .                                                                    
001345     SKIP2                                                                
001346 MFS-LAES-IN-IGEN SECTION.                                                
001347                                                                          
001348*    --- ALLA INDATA-FÄLT                                                 
001349     MOVE MFS-ADD-LAES-IN-FAELT  TO RESP-KDCMD-UPD-ATTR                   
001350                                    RESP-IDORDNR7-UPD-ATTR                
001351                                    RESP-IDPRODNR-UPD-ATTR                
001352                                    RESP-IDKOLLI-UPD-ATTR                 
001353                                    RESP-KDORDKL-UPD-ATTR                 
001354                                    RESP-IDPSN-UPD-ATTR                   
001355                                    RESP-KDKOLLI-UPD-ATTR                 
001356                                    RESP-VKORDBTO-KOLLI-UPD-ATTR          
001357     MOVE +1                     TO INDX                                  
001358     PERFORM UNTIL INDX > MAX-KVRADER                                     
001359       MOVE MFS-ADD-LAES-IN-FAELT                                         
001360                                 TO RESP-CMD-LINE-ATTR (INDX)             
001361                                    RESP-VKORDBTO-KOLLI-LINE-ATTR         
001362                                        (INDX)                            
001363       ADD +1                    TO INDX                                  
001364     END-PERFORM                                                          
001365     .                                                                    
001366     EJECT                                                                
001367                                                                          
001368* --- IMS SEKTIONER ---                                                   
001369     SKIP3                                                                
001370 IMS-GET-OHUV SECTION.                                                    
001371                                                                          
001372     STRING 'WLORQI01(WDQ2CSEQ =' W-IDGMTREF-X ')'                        
001373          DELIMITED BY SIZE INTO SSA1                                     
001374     MOVE '    ' TO GODK-STATUSKODER                                      
001375     CALL CBLTDLI USING GHU ORQI-PCB DLI-IO-WLORQI01 SSA1                 
001376     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
001377     PERFORM IMS-STATUSKONTROLL                                           
001378     .                                                                    
001379     SKIP3                                                                
001380 IMS-REPL-OHUV SECTION.                                                   
001381                                                                          
001382     MOVE '  ' TO GODK-STATUSKODER                                        
001383     CALL CBLTDLI USING REPL ORQI-PCB DLI-IO-WLORQI01                     
001384     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
001385     PERFORM IMS-STATUSKONTROLL                                           
001386     .                                                                    
001387     SKIP3                                                                
001388 IMS-GHU-WL446331-CUST SECTION.                                           
001389                                                                          
001390     STRING 'WL446301(WDGXKEY  =' W-4463-X ')'                            
001391          DELIMITED BY SIZE INTO SSA1                                     
001392     STRING 'WL446311(DASKEPPN =' W-DASKEPPN-X ')'                        
001393          DELIMITED BY SIZE INTO SSA2                                     
001394     STRING 'WL446321(KY4466   =' W-KY4466-X ')'                          
001395          DELIMITED BY SIZE INTO SSA3                                     
001396     STRING 'WL446331(KY4468   =' W-KY4468-X ')'                          
001397          DELIMITED BY SIZE INTO SSA4                                     
001398     MOVE '  GE' TO GODK-STATUSKODER                                      
001399     CALL CBLTDLI USING GHU 4463-PCB DLI-IO-WL446331                      
001400                        SSA1 SSA2 SSA3 SSA4                               
001401     MOVE 4463-STATUS-CODE TO STATUS-WS                                   
001402     PERFORM IMS-STATUSKONTROLL                                           
001403     .                                                                    
001404     EJECT                                                                
001405 IMS-GHN-WL446331-CUST SECTION.                                           
001406                                                                          
001407     STRING 'WL446301(WDGXKEY  =' W-4463-X ')'                            
001408          DELIMITED BY SIZE INTO SSA1                                     
001409     STRING 'WL446311(DASKEPPN =' W-DASKEPPN-X ')'                        
001410          DELIMITED BY SIZE INTO SSA2                                     
001411     STRING 'WL446321(KY4466   =' W-KY4466-X ')'                          
001412          DELIMITED BY SIZE INTO SSA3                                     
001413     STRING 'WL446331(KY4468  >=' W-KY4468-MIN-X                          
001414                    '&KY4468  <=' W-KY4468-MAX-X ')'                      
001415          DELIMITED BY SIZE INTO SSA4                                     
001416     MOVE '  GE' TO GODK-STATUSKODER                                      
001417     CALL CBLTDLI USING GHN 4463-PCB DLI-IO-WL446331                      
001418                        SSA1 SSA2 SSA3 SSA4                               
001419     MOVE 4463-STATUS-CODE TO STATUS-WS                                   
001420     PERFORM IMS-STATUSKONTROLL                                           
001421     .                                                                    
001422     EJECT                                                                
001423 IMS-REPL-4468 SECTION.                                                   
001424                                                                          
001425     MOVE '  ' TO GODK-STATUSKODER                                        
001426     CALL CBLTDLI USING REPL 4463-PCB DLI-IO-WL446331                     
001427     MOVE 4463-STATUS-CODE TO STATUS-WS                                   
001428     PERFORM IMS-STATUSKONTROLL                                           
001429     .                                                                    
001430     SKIP3                                                                
001431 IMS-ISRT-4468 SECTION.                                                   
001432                                                                          
001433     STRING 'WL446301(WDGXKEY  =' W-4463-X ')'                            
001434          DELIMITED BY SIZE INTO SSA1                                     
001435     STRING 'WL446311(DASKEPPN =' W-DASKEPPN-X ')'                        
001436          DELIMITED BY SIZE INTO SSA2                                     
001437     STRING 'WL446321(KY4466   =' W-KY4466-X ')'                          
001438          DELIMITED BY SIZE INTO SSA3                                     
001439     MOVE 'WL446331 ' TO SSA4                                             
001440     MOVE '  II' TO GODK-STATUSKODER                                      
001441     CALL CBLTDLI USING ISRT 4463-PCB DLI-IO-WL446331                     
001442                        SSA1 SSA2 SSA3 SSA4                               
001443     MOVE 4463-STATUS-CODE TO STATUS-WS                                   
001444     PERFORM IMS-STATUSKONTROLL                                           
001445     .                                                                    
001446     SKIP3                                                                
001447 IMS-DLET-4468 SECTION.                                                   
001448                                                                          
001449     MOVE '  ' TO GODK-STATUSKODER                                        
001450     CALL CBLTDLI USING DLET 4463-PCB DLI-IO-WL446331                     
001451     MOVE 4463-STATUS-CODE TO STATUS-WS                                   
001452     PERFORM IMS-STATUSKONTROLL                                           
001453     .                                                                    
001454     EJECT                                                                
001455 IMS-GET-1165 SECTION.                                                    
001456                                                                          
001457     STRING 'WL116501(WDGXKEY  =' W-1165-X ')'                            
001458          DELIMITED BY SIZE INTO SSA1                                     
001459     MOVE '  GE' TO GODK-STATUSKODER                                      
001460     CALL CBLTDLI USING GU 1165-PCB DLI-IO-WL116501 SSA1                  
001461     MOVE 1165-STATUS-CODE TO STATUS-WS                                   
001462     PERFORM IMS-STATUSKONTROLL                                           
001463     .                                                                    
001464     SKIP3                                                                
001465 IMS-GU-WDB601    SECTION.                                                
001466     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
001467          DELIMITED BY SIZE INTO SSA1                                     
001468     MOVE '  ' TO GODK-STATUSKODER                                        
001469     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
001470     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
001471     PERFORM IMS-STATUSKONTROLL                                           
001472     .                                                                    
001473     EJECT                                                                
001474 IMS-STATUSKONTROLL SECTION.                                              
001475                                                                          
001476     SET STATUS-IX TO 1                                                   
001477     SEARCH GODK-STATUS                                                   
001478       AT END                                                             
001479         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
001480         DELIMITED BY SIZE INTO FELTEXT                                   
001481         CALL FELLOG                                                      
001482       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
001483         CONTINUE                                                         
001484     END-SEARCH                                                           
001485     .                                                                    
