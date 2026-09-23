000001**********************************************************                
000002 ID DIVISION.                                                             
000003 PROGRAM-ID.     W2035600.                                                
000004 AUTHOR.         STEFAN ANDREASSON, FRONTEC.                              
000005 DATE-WRITTEN.   96/05/21.                                                
000006 DATE-COMPILED.                                                           
000007                                                                          
000008*    FUNKTION:                                                            
000009*        FÖRSÄLJNINGSSTATISTIK                                            
000010*                                                                         
000020*        PROGRAMMET UPPDATERAR WLUSEA (WDP7)                              
000021*        PROGRAMMET LÄSER      WDL7 + WDL4                                
000022*                              WLARTS (WDK7)                              
000023*                              WLBENA (WDD3)                              
000024*                              WDB6                                       
000025*                                                                         
000026*    INDATA.                                                              
000027*        TRANSAKTION: W2T356                                              
000028*        MID:         W2I35601                                            
000029*                                                                         
000030*    UTDATA.                                                              
000031*        MOD:         W2O35601                                            
000032*                                                                         
000033*    E'TRACKER: 4823800 DAT. 20071204 NEW LDC                             
000034*                                                                         
000035                                                                          
000036     SKIP3                                                                
000037 ENVIRONMENT DIVISION.                                                    
000038     EJECT                                                                
000039 DATA DIVISION.                                                           
000040 WORKING-STORAGE SECTION.                                                 
000041                                                                          
000042*    -- CHECKED BY WY2000                                                 
000043 77  IDPGM                       PIC X(08)   VALUE 'W2035600'.            
000044                                                                          
000045*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
000046 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
000047                                                                          
000048 77  JA                          PIC X       VALUE 'J'.                   
000049 77  NEJ                         PIC X       VALUE 'N'.                   
000050 77  IX-AR                       PIC 9(9)    VALUE ZERO  COMP-3.          
000051 77  IX-PER                      PIC 9(9)    VALUE ZERO  COMP-3.          
000052 77  IX-FRAN-AR                  PIC 9(9)    VALUE ZERO  COMP-3.          
000053 77  IX-TILL-AR                  PIC 9(9)    VALUE ZERO  COMP-3.          
000054 77  IX-RULL                     PIC 9(9)    VALUE ZERO  COMP-3.          
000055 77  SPRAK-IX                    PIC 9(9)    VALUE ZERO  COMP-3.          
000056 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
000057 77  DAGENS-PER                  PIC  9(4)   VALUE ZERO.                  
000058 77  DAGENS-AAR                  PIC 9(4)    VALUE ZERO.                  
000059 01  WS.                                                                  
000060  05 WS-AAPP                     PIC 9(4)    VALUE ZERO.                  
000061  05 FILLER REDEFINES            WS-AAPP.                                 
000062   10 WS-AA                      PIC 9(2).                                
000063   10 WS-PP                      PIC 9(2).                                
000064  05 WS-AARTAL                   PIC 9(4)    VALUE ZERO.                  
000065  05 WS-PER                      PIC 9(2)    VALUE ZERO.                  
000066  05 WS-KVOI-RED                 PIC 9(7)    VALUE ZERO.                  
000067  05 WS-TOTAL-KVOI               PIC 9(7)    VALUE ZERO.                  
000068  05 WS-DC-IX                    PIC S9(3)   VALUE ZERO.                  
000069  05 WS-DC-IX-MAX                PIC S9(3)   VALUE 4.                     
000070                                                                          
000071  05 WS-KVOI-PER         OCCURS 6 TIMES.                                  
000072   10 WS-KVOI-TOT-NDC            PIC 9(7)        VALUE ZERO.              
000073   10 WS-KVOI-NDC        OCCURS 12 TIMES                                  
000074                                 PIC 9(7)        VALUE ZERO.              
000075  05 WS-VV                       PIC  9(2)   VALUE ZERO.                  
000076  05  WS-TABELL    OCCURS 12.                                             
000077   10 WS-FORSTA-V                PIC  9(2)   VALUE ZERO.                  
000078   10 WS-SISTA-V                 PIC  9(2)   VALUE ZERO.                  
000079   10 WS-KVVIPER                 PIC 9       VALUE ZERO.                  
000080   10 WS-KVOI                    PIC S9(7)   VALUE ZERO COMP-3.           
000081  05 WS-TESTFAELT.                                                        
000082   10 WS-FORSTA-TF               PIC  9(2)   VALUE ZERO.                  
000083   10 FILLER                     PIC  X      VALUE SPACE.                 
000084   10 WS-SISTA-TF                PIC  9(2)   VALUE ZERO.                  
000085   10 FILLER                     PIC  X      VALUE SPACE.                 
000086   10 WS-KVOI-TF                 PIC  9(7)   VALUE ZERO.                  
000087                                                                          
000088                                                                          
000089                                                                          
000090*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
000091                                                                          
000092 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
000093 77  IDDC-WS                     PIC X(2)    VALUE SPACE.                 
000094                                                                          
000095                                                                          
000096 77  INDATA-SW                   PIC X       VALUE 'J'.                   
000097     88  INDATA-OK                           VALUE 'J'.                   
000098     88  INDATA-FEL                          VALUE 'N'.                   
000099                                                                          
000100 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
000101     88  NYCKLAR-OK                          VALUE 'J'.                   
000102     88  NYCKLAR-FEL                         VALUE 'N'.                   
000103                                                                          
000104 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
000105     88  EGEN-MID                            VALUE '2356'.                
000106     88  GODK-MID                            VALUE '2351' '2352'          
000107                                                   '2353' '2354'          
000108                                                   '2355' '2356'          
000109                                                   '2357' '2358'          
000110                                                   '2359'.                
000111     88  HELP-MID                            VALUE '0551'.                
000112     EJECT                                                                
000113*      --- VALID IDDC CODES                                               
000114*                                                                         
000115*01    -COPY WWDC99                                                       
000116*01    -COPY WWDCKONS                                                     
000117       EJECT                                                              
000118*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
000119 01  GENERELLA-SUBPROGRAM.                                                
000120     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
000121     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
000122     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000123     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000124     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
000125     EJECT                                                                
000126*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
000127*01  -COPY WDATAREA                                                       
000128     EJECT                                                                
000129*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
000130*01 -COPY WMEDAREA                                                        
000131     EJECT                                                                
000132 01  FILLER                      PIC X(16)   VALUE 'DISTRIKT-TAB'.        
000133     SKIP3                                                                
000134 01  MESSAGE-CODES.                                                       
000135     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
000136     03  INF-NOT-REFILL          PIC X(3)    VALUE '957'.                 
000137     EJECT                                                                
000138*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
000139*                                                                         
000140 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
000141     SKIP3                                                                
000142*01 -COPY WMSGINIT                                                        
000143     SKIP3                                                                
000144*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
000145*                                                                         
000146 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
000147     SKIP3                                                                
000148*01  MID -COPY W2I35601                                                   
000149     EJECT                                                                
000150 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
000151     SKIP3                                                                
000152*01  -COPY WMSGAREA                                                       
000153     EJECT                                                                
000154     03  MOD REDEFINES MSG-AREA.                                          
000155*      05  -COPY W2O35601                                                 
000156     EJECT                                                                
000157 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
000158     SKIP3                                                                
000159*01  -COPY WMFSAREA                                                       
000160     EJECT                                                                
000161*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
000162*                                                                         
000163     EJECT                                                                
000164 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000165     SKIP3                                                                
000166 01  NYCKLAR-TILL-DLI.                                                    
000167     03  W-IDARTNR-X.                                                     
000168         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
000169     03  W-IDDC-X.                                                        
000170         05  W-IDDC              PIC X(02)    VALUE SPACE.                
000171     03  W-IDUSER-X.                                                      
000172         05  W-IDUSER            PIC X(8)    VALUE SPACE.                 
000173     03  W-IDSKYLT-X.                                                     
000174         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
000175     SKIP2                                                                
000176*    --- STATUS-KOD FRÅN IMS                                              
000177 01  STATUS-WS                   PIC XX.                                  
000178     88  SEGMENT-FINNS                       VALUE '  '.                  
000179     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
000180     88  SEGMENT-SAKNAS                      VALUE 'GE'                   
000181                                                   'GB'.                  
000182     SKIP2                                                                
000183 01  GODK-STATUSKODER.                                                    
000184     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000185     SKIP3                                                                
000186 01  SSA1                        PIC X(64).                               
000187 01  SSA2                        PIC X(64).                               
000188 01  SSA3                        PIC X(64).                               
000189     EJECT                                                                
000190*    --- IMS FUNKTIONSKODER                                               
000191*01  -COPY W0003                                                          
000192     EJECT                                                                
000193*    ---  DLI INPUT-OUTPUT AREA                                           
000194 01  FILLER                    PIC X(16) VALUE 'DLI-IO-AREA-L711'.        
000195     SKIP3                                                                
000196 01  DLI-IO-AREA-L711.                                                    
000197*    03  -COPY WDL711                                                     
000198     SKIP3                                                                
000199 01  FILLER                    PIC X(16) VALUE 'DLI-IO-AREA-L411'.        
000200     SKIP3                                                                
000201 01  DLI-IO-AREA-L411.                                                    
000202*    03  -COPY WDL411                                                     
000203     SKIP3                                                                
000204 01  FILLER                     PIC X(16) VALUE 'DLI-IO-AREA-D3'.         
000205     SKIP3                                                                
000206 01  DLI-IO-AREA-D3.                                                      
000207     03  IO-AREA-D3              PIC X(150)  VALUE SPACE.                 
000208     SKIP3                                                                
000209     03  WLBENA11 REDEFINES IO-AREA-D3.                                   
000210*        05  -COPY WDD311  -PRE BENA-                                     
000211                                                                          
000212 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
000213 01   DLI-IO-AREA-B601.                                                   
000214*     03  -COPY WDB601                                                    
000215     SKIP3                                                                
000216                                                                          
000217 01  FILLER               PIC X(16)   VALUE 'WDK711 AREA'.                
000218 01   DLI-IO-AREA-K711.                                                   
000219*     03  -COPY WDK711                                                    
000220                                                                          
000221 01  FILLER               PIC X(16)   VALUE 'WDK629 AREA'.                
000222 01   DLI-IO-AREA-K629.                                                   
000223*     03  -COPY WDK629                                                    
000224                                                                          
000225     EJECT                                                                
000226 LINKAGE SECTION.                                                         
000227                                                                          
000228*01  -COPY W0009   -PRE MSG-                                              
000229     EJECT                                                                
000230*01  -COPY W0008  -PRE  USEA-                                             
000231     05  FILLER                  PIC X.                                   
000232     EJECT                                                                
000233*01  -COPY W0008  -PRE  WDL7-                                             
000234     05  FILLER                  PIC X.                                   
000235     EJECT                                                                
000236*01  -COPY W0008  -PRE  WDL4-                                             
000237     05  FILLER                  PIC X.                                   
000238     EJECT                                                                
000239*01  -COPY W0008  -PRE  BENA-                                             
000240     05  FILLER                  PIC X.                                   
000241     EJECT                                                                
000242*01  -COPY W0008  -PRE  WDB6-                                             
000243     05  FILLER                  PIC X.                                   
000244     EJECT                                                                
000245*01  -COPY W0008  -PRE  WDK7-                                             
000246     05  FILLER                  PIC X.                                   
000247     EJECT                                                                
000248*01  -COPY W0008  -PRE  WDK6-                                             
000249     05  FILLER                  PIC X.                                   
000250     EJECT                                                                
000251 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB WDL7-PCB WDL4-PCB             
000252     BENA-PCB WDB6-PCB WDK7-PCB WDK6-PCB.                                 
000253 MAIN SECTION.                                                            
000254     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB WDL7-PCB WDL4-PCB             
000255     BENA-PCB WDB6-PCB WDK7-PCB WDK6-PCB.                                 
000256                                                                          
000257     PERFORM IMS-GET-MSG                                                  
000258     IF SEGMENT-FINNS                                                     
000259       PERFORM A-INIT                                                     
000260       PERFORM B-KOLLA-NYCKLAR                                            
000261       IF NYCKLAR-OK                                                      
000262         IF NDC-CN OR NDC-US                                              
000263           PERFORM IMS-GU-WDK711                                          
000264           IF SEGMENT-FINNS                                               
000265             IF SLAG-IDDC-REF = SPACE                                     
000266               PERFORM MFS-RENSA-FAELT-UT                                 
000267               MOVE INF-NOT-REFILL TO MED-IDMFSINF                        
000268               CALL WMEDKONV USING MED-WMEDAREA                           
000269               MOVE MED-MFSINF TO MOD-TEMFSINF                            
000270             ELSE                                                         
000271               PERFORM F-LAES-VISA-INFO                                   
000272             END-IF                                                       
000273           END-IF                                                         
000274         ELSE                                                             
000275           IF CDC-SE                                                      
000276             PERFORM IMS-GU-WDK629                                        
000277             IF SEGMENT-FINNS                                             
000278               PERFORM MFS-RENSA-FAELT-UT                                 
000279               MOVE 'USE SCREEN 2107' TO MOD-TEMFSINF                     
000280             ELSE                                                         
000281               PERFORM MFS-RENSA-FAELT-UT                                 
000282               MOVE INF-NOT-REFILL TO MED-IDMFSINF                        
000283               CALL WMEDKONV USING MED-WMEDAREA                           
000284               MOVE MED-MFSINF TO MOD-TEMFSINF                            
000285             END-IF                                                       
000286           ELSE                                                           
000287             PERFORM F-LAES-VISA-INFO                                     
000288           END-IF                                                         
000289         END-IF                                                           
000290       END-IF                                                             
000291       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O35601 + 4                      
000292       PERFORM IMS-INSERT-MSG                                             
000293     END-IF                                                               
000294                                                                          
000295     MOVE ZERO TO RETURN-CODE                                             
000296     GOBACK                                                               
000297     .                                                                    
000298     EJECT                                                                
000299 A-INIT SECTION.                                                          
000300                                                                          
000301     IF MSG-DUBBLA-TRANSKODER                                             
000302       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I35601                 
000303       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
000304       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
000305     ELSE                                                                 
000306       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I35601                  
000307       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
000308       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
000309     END-IF                                                               
000310                                                                          
000311     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
000312     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
000313     MOVE MFS-IDTRANS TO W-IDTRANS                                        
000314                                                                          
000315     MOVE LOW-VALUE TO MSG-AREA                                           
000316     MOVE 'W2O356N1' TO MFS-IDMOD                                         
000317     MOVE '2356' TO MOD-IDTRANS                                           
000318     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
000319                                                                          
000320     IF EGEN-MID OR HELP-MID                                              
000321       CONTINUE                                                           
000322     ELSE                                                                 
000323       MOVE SPACE TO MFS-KDTRTYP                                          
000324       MOVE '7' TO MFS-IDPFK                                              
000325     END-IF                                                               
000326                                                                          
000327     ACCEPT DAGENS-DATUM FROM DATE                                        
000328                                                                          
000329     MOVE 'AAMMDD'           TO DAT-KDDATFORM                             
000330     MOVE DAGENS-DATUM       TO DAT-I-TIDATUM                             
000331                                                                          
000332     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
000333                     DAT-O-TIDATUM DAT-KDSVAR                             
000334                                                                          
000335     IF DAT-KDSVAR-OK                                                     
000336****             HÄMTA SEKELSIFFROR                                       
000337                                                                          
000338       MOVE DAT-TISEKEL      TO DAGENS-AAR(1:2)                           
000339       MOVE DAT-TIAARP       TO DAGENS-PER                                
000340       MOVE DAGENS-PER(3:2)  TO WS-PER                                    
000341                                                                          
000342     ELSE                                                                 
000343         STRING ' FEL FRÅN DATUMRUTIN WDATKONV ' STATUS-WS                
000344         DELIMITED BY SIZE INTO FELTEXT                                   
000345         CALL FELLOG                                                      
000346     END-IF                                                               
000347                                                                          
000348     MOVE DAGENS-DATUM(1:2)   TO DAGENS-AAR(3:2)                          
000349                                                                          
000350     MOVE +2      TO SPRAK-IX                                             
000351     MOVE 'GB ' TO MED-IDSKYLT                                            
000352     .                                                                    
000353     EJECT                                                                
000354 B-KOLLA-NYCKLAR SECTION.                                                 
000355                                                                          
000356     MOVE ALL '+'           TO MSGI-WMSGINIT                              
000357     MOVE '001'             TO MSGI-KDCALL                                
000358     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
000359     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
000360     MOVE '2356'            TO MSGI-IDTRANS                               
000361     IF EGEN-MID                                                          
000362       MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                               
000363       MOVE MID-IDDC-IN     TO MSGI-IDDC-KEY                              
000364     ELSE                                                                 
000365       IF  MID-IDARTNR-IN NUMERIC                                         
000366       AND MID-IDARTNR-IN > ZERO                                          
000367         MOVE MID-IDARTNR-IN                                              
000368                            TO MSGI-IDARTNR                               
000369       END-IF                                                             
000370     END-IF                                                               
000371     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
000372                                                                          
000373     MOVE JA TO NYCKLAR-SW                                                
000374                                                                          
000375*    -- KONTROLL AV IDARTNR                                               
000376     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
000377                                                                          
000378     IF MID-IDARTNR-IN NOT = ALL '+'                                      
000379       MOVE '7'         TO MFS-IDPFK                                      
000380       MOVE SPACE       TO MFS-KDTRTYP                                    
000381     END-IF                                                               
000382     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
000383     IF MSGI-IDARTNR NUMERIC                                              
000384       MOVE MSGI-IDARTNR TO WS-IDARTNR                                    
000385     ELSE                                                                 
000386       MOVE NEJ TO NYCKLAR-SW                                             
000387     END-IF                                                               
000388                                                                          
000389*    -- KONTROLL AV IDDC                                                  
000390     MOVE MFS-RENSA-FAELT    TO MOD-IDDC-IN                               
000391                                                                          
000392     IF MID-IDDC-IN NOT = ALL '+'                                         
000393       MOVE '7'              TO MFS-IDPFK                                 
000394       MOVE SPACE            TO MFS-KDTRTYP                               
000395     END-IF                                                               
000396                                                                          
000397     MOVE MSGI-IDDC-KEY    TO WS-IDDC                                     
000398                              IDDC-WS                                     
000399                                                                          
000400****  SDC : 20 FÖREKOMMER BARA SOM INMATNING I BILDEN                     
000401****           FÖR ATT MAN VILL SE TOTALEN FÖR SDC                        
000402****           DVS NDC 21, 23, 24, 25 OCH 26                              
000403****  NDC : 40 FÖREKOMMER BARA SOM INMATNING I BILDEN                     
000404****           FÖR ATT MAN VILL SE TOTALEN FÖR USA                        
000405****           DVS NDC 41, 42, 43, 44 OCH 45                              
000406****  NDC : 50 FÖREKOMMER BARA SOM INMATNING I BILDEN                     
000407****           FÖR ATT MAN VILL SE TOTALEN FÖR BRAZIL - 52                
000408****                                           MEXICO - 53                
000409****  NDC : 60 FÖREKOMMER BARA SOM INMATNING I BILDEN                     
000410****           FÖR ATT MAN VILL SE TOTALEN FÖR JAPAN OCH                  
000411****                                           AUSTRALIEN                 
000412****           DVS NDC 61, 6A OCH 62                                      
000413****  NDC : 70 FÖREKOMMER BARA SOM INMATNING I BILDEN                     
000414****           FÖR ATT MAN VILL SE TOTALEN FÖR KINA                       
000415****           DVS DC 7*                                                  
000416     IF IDDC-WS = '20'                                                    
000417     OR IDDC-WS = '40'                                                    
000418     OR IDDC-WS = '50'                                                    
000419     OR IDDC-WS = '60'                                                    
000420     OR IDDC-WS = '70'                                                    
000421     OR IDDC-WS = '80'                                                    
000422     OR GOOD-DC                                                           
000423         CONTINUE                                                         
000424     ELSE                                                                 
000425       MOVE NEJ TO NYCKLAR-SW                                             
000426     END-IF                                                               
000427                                                                          
000428     MOVE WS-IDARTNR       TO MOD-IDARTNR-UT                              
000429                              W-IDARTNR                                   
000430     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
000431     MOVE IDDC-WS          TO MOD-IDDC-UT                                 
000432                              W-IDDC                                      
000433                                                                          
000434     IF NYCKLAR-FEL                                                       
000435       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
000436       CALL WMEDKONV USING MED-WMEDAREA                                   
000437       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
000438       PERFORM MFS-RENSA-FAELT-UT                                         
000439     END-IF                                                               
000440     .                                                                    
000441     EJECT                                                                
000442 F-LAES-VISA-INFO SECTION.                                                
000443                                                                          
000444     PERFORM FA-HAEMTA-BENAEMNING                                         
000445                                                                          
000446     PERFORM FB-HAMTA-VV-I-PER                                            
000447                                                                          
000448     MOVE DAGENS-AAR         TO MOD-AARTAL (1)                            
000449                                WS-AARTAL                                 
000450     SUBTRACT 1 FROM WS-AARTAL                                            
000451     MOVE WS-AARTAL          TO MOD-AARTAL (2)                            
000452     SUBTRACT 1 FROM WS-AARTAL                                            
000453     MOVE WS-AARTAL          TO MOD-AARTAL (3)                            
000454     SUBTRACT 1 FROM WS-AARTAL                                            
000455     MOVE WS-AARTAL          TO MOD-AARTAL (4)                            
000456     SUBTRACT 1 FROM WS-AARTAL                                            
000457     MOVE WS-AARTAL          TO MOD-AARTAL (5)                            
000458     SUBTRACT 1 FROM WS-AARTAL                                            
000459     MOVE WS-AARTAL          TO MOD-AARTAL (6)                            
000460                                                                          
000461     IF IDDC-WS = 20                                                      
000462     OR IDDC-WS = 40                                                      
000463     OR IDDC-WS = 50                                                      
000464     OR IDDC-WS = 60                                                      
000465     OR IDDC-WS = 70                                                      
000466     OR IDDC-WS = 80                                                      
000467                                                                          
000468       PERFORM IMS-GN-WDB601                                              
000469       PERFORM UNTIL SEGMENT-SAKNAS                                       
000470                                                                          
000471          IF DCS-DDC OR                                                   
000472             DCS-IDDC = WC-LDC-GB-3A                                      
000473                                                                          
000474             CONTINUE                                                     
000475                                                                          
000476          ELSE                                                            
000477             MOVE DCS-IDDC   TO WS-IDDC                                   
000478             IF (IDDC-WS = 20 AND SDC)                                    
000479             OR (IDDC-WS = 40 AND NDC-US)                                 
000480             OR (IDDC-WS = 50 AND NDC-NS)                                 
000481             OR (IDDC-WS = 60 AND NDC-PACIFIC)                            
000482             OR (IDDC-WS = 70 AND NDC-CN)                                 
000483             OR (IDDC-WS = 80 AND NDC-NX)                                 
000485                                                                          
000486                MOVE DCS-IDDC TO W-IDDC                                   
000487                PERFORM IMS-GU-WDK711                                     
000488                IF SEGMENT-FINNS                                          
000489                  IF NDC-CN OR NDC-US                                     
000490                    IF SLAG-IDDC-REF NOT = SPACE                          
000491                      PERFORM FC-HAEMTA-HISTORIK                          
000492                    END-IF                                                
000493                  ELSE                                                    
000494                    PERFORM FC-HAEMTA-HISTORIK                            
000495                  END-IF                                                  
000496                END-IF                                                    
000497             END-IF                                                       
000498          END-IF                                                          
000499                                                                          
000500          PERFORM IMS-GN-WDB601                                           
000501       END-PERFORM                                                        
000502                                                                          
000503       IF IDDC-WS = 20                                                    
000504         MOVE 'DC 20 : SUMMARY OF SDC'                                    
000505                             TO MOD-TEMFSINF                              
000506       END-IF                                                             
000507       IF IDDC-WS = 40                                                    
000508         MOVE 'DC 40 : SUMMARY OF NDC US'                                 
000509                             TO MOD-TEMFSINF                              
000510       END-IF                                                             
000511       IF IDDC-WS = 50                                                    
000512         MOVE 'DC 50 : SUMMARY OF NDC-NS'                                 
000513                             TO MOD-TEMFSINF                              
000514       END-IF                                                             
000515       IF IDDC-WS = 60                                                    
000516         MOVE 'DC 60 : SUMMARY OF 61, 6A OCH 62'                          
000517                             TO MOD-TEMFSINF                              
000518       END-IF                                                             
000519       IF IDDC-WS = 70                                                    
000520         MOVE 'DC 70 : SUMMARY OF LDC-CN AND NDC-CN'                      
000521                             TO MOD-TEMFSINF                              
000522       END-IF                                                             
000523       IF IDDC-WS = 80                                                    
000524         MOVE 'DC 80 : SUMMARY OF NDC-NX'                                 
000525                             TO MOD-TEMFSINF                              
000526       END-IF                                                             
000528                                                                          
000529     ELSE                                                                 
000530                                                                          
000531       PERFORM FC-HAEMTA-HISTORIK                                         
000532     END-IF                                                               
000533                                                                          
000534     MOVE +1                 TO IX-AR                                     
000535                                IX-PER                                    
000536     PERFORM UNTIL IX-AR > +6                                             
000537       PERFORM UNTIL IX-PER > +12                                         
000538         MOVE WS-KVOI-NDC (IX-AR, IX-PER)                                 
000539                             TO MOD-KVOI (IX-AR, IX-PER)                  
000540         ADD +1              TO IX-PER                                    
000541       END-PERFORM                                                        
000542                                                                          
000543       MOVE WS-KVOI-TOT-NDC (IX-AR)                                       
000544                             TO MOD-KVOI-TOT (IX-AR)                      
000545       ADD +1                TO IX-AR                                     
000546       MOVE +1               TO IX-PER                                    
000547     END-PERFORM                                                          
000548     .                                                                    
000549     EJECT                                                                
000550 FA-HAEMTA-BENAEMNING SECTION.                                            
000551                                                                          
000552     PERFORM IMS-GU-BENA01-BSEQ                                           
000553     IF SEGMENT-FINNS                                                     
000554       MOVE 'USA'            TO W-IDSKYLT                                 
000555       PERFORM IMS-GNP-BENA11                                             
000556       IF SEGMENT-FINNS                                                   
000557         MOVE BENA-TEXT-BEART TO MOD-BEART-ENG                            
000558       ELSE                                                               
000559         MOVE MFS-RENSA-FAELT   TO MOD-BEART-ENG                          
000560       END-IF                                                             
000561     END-IF                                                               
000562     .                                                                    
000563     EJECT                                                                
000564 FB-HAMTA-VV-I-PER SECTION.                                               
000565                                                                          
000566*    --- FYLL I VECKONR FÖR PERIODERNA                                    
000567     MOVE DAGENS-DATUM(1:2)  TO WS-AAPP(1:2)                              
000568     MOVE 01                 TO WS-AAPP(3:2)                              
000569     MOVE WS-AAPP            TO DAT-I-TIDATUM                             
000570     MOVE 'AARP'             TO DAT-KDDATFORM                             
000571     MOVE +1                 TO WS-PP                                     
000572                                                                          
000573     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
000574                     DAT-O-TIDATUM DAT-KDSVAR                             
000575                                                                          
000576     IF DAT-KDSVAR-OK                                                     
000577                                                                          
000578       MOVE DAT-KVVIPER      TO WS-KVVIPER(WS-PP)                         
000579       MOVE 1                TO WS-FORSTA-V(WS-PP)                        
000580                                                                          
000581     ELSE                                                                 
000582         STRING ' FEL FRÅN DATUMRUTIN WDATKONV ' STATUS-WS                
000583         DELIMITED BY SIZE INTO FELTEXT                                   
000584         CALL FELLOG                                                      
000585     END-IF                                                               
000586                                                                          
000587     PERFORM UNTIL WS-PP      >  WS-PER                                   
000588       ADD +1                 TO WS-PP                                    
000589       IF WS-PP = +13                                                     
000590         MOVE 53             TO WS-SISTA-V(12)                            
000591       ELSE                                                               
000592                                                                          
000593         MOVE WS-AAPP         TO DAT-I-TIDATUM                            
000594         CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                  
000595                             DAT-O-TIDATUM DAT-KDSVAR                     
000596         IF DAT-KDSVAR-OK                                                 
000597             COMPUTE WS-SISTA-V(WS-PP - 1) = DAT-TIVV - 1                 
000598             IF WS-PP > +1                                                
000599               MOVE DAT-TIVV  TO WS-FORSTA-V(WS-PP)                       
000600               MOVE DAT-KVVIPER TO WS-KVVIPER(WS-PP)                      
000601             END-IF                                                       
000602         ELSE                                                             
000603           MOVE 'FELAKTIGT DATUM - DATKONV3' TO FELTEXT                   
000604           CALL FELLOG                                                    
000605         END-IF                                                           
000606       END-IF                                                             
000607     END-PERFORM                                                          
000608     .                                                                    
000609     EJECT                                                                
000610 FC-HAEMTA-HISTORIK SECTION.                                              
000611                                                                          
000612     PERFORM IMS-GU-WDL711                                                
000613     IF SEGMENT-FINNS                                                     
000614*   LÄGG UT HISTORIK I BILDEN                                             
000615       PERFORM IMS-GU-WDL411                                              
000616       IF SEGMENT-SAKNAS                                                  
000617******** IF PART/DC FROM WDL7 IS MISSING IN WDL4 THEN L411-AREA           
000618******** MUST BE ZEROED BCZ SOME ITEMS ARE USED IN COMPUTES BELOW         
000619         INITIALIZE OIHD-WDL411                                           
000620       END-IF                                                             
000621                                                                          
000622       PERFORM FCA-BEHANDLA-ORDERINGGANG                                  
000623                                                                          
000624     ELSE                                                                 
000625       PERFORM MFS-RENSA-FAELT-UT                                         
000626     END-IF                                                               
000627     .                                                                    
000628     EJECT                                                                
000629 FCA-BEHANDLA-ORDERINGGANG SECTION.                                       
000630                                                                          
000631     MOVE +1                   TO IX-PER                                  
000632     PERFORM UNTIL IX-PER      > 12                                       
000633       MOVE ZERO               TO WS-KVOI(IX-PER)                         
000634       ADD +1                  TO IX-PER                                  
000635     END-PERFORM                                                          
000636                                                                          
000637     MOVE +1                   TO IX-PER                                  
000638                                                                          
000639     PERFORM UNTIL IX-PER      =  WS-PER                                  
000640       MOVE WS-FORSTA-V(IX-PER)                                           
000641                               TO WS-VV                                   
000642       PERFORM UNTIL WS-VV     >  WS-SISTA-V(IX-PER)                      
000643         ADD DC-KVOI-RULL(WS-VV)                                          
000644                               TO WS-KVOI(IX-PER)                         
000645         ADD +1                TO WS-VV                                   
000646       END-PERFORM                                                        
000647       ADD +1                  TO IX-PER                                  
000648     END-PERFORM                                                          
000649                                                                          
000650     MOVE WS-FORSTA-V(10)      TO WS-FORSTA-TF                            
000651     MOVE WS-SISTA-V(10)       TO WS-SISTA-TF                             
000652     MOVE WS-KVOI(10)          TO WS-KVOI-TF                              
000653                                                                          
000654*                                                                         
000655     MOVE +1                   TO IX-AR                                   
000656                                  IX-PER                                  
000657     MOVE ZERO                 TO WS-TOTAL-KVOI                           
000658                                                                          
000659     PERFORM UNTIL IX-PER = WS-PER                                        
000660*  VISA HISTORIK TOM FÖREGÅENDE MÅNAD                                     
000661                                                                          
000662       IF WS-KVOI (IX-PER) > ZERO                                         
000663         COMPUTE WS-KVOI-RED       ROUNDED =                              
000664               WS-KVOI (IX-PER)                                           
000665                            / WS-KVVIPER (IX-PER) * +4.33                 
000666       ELSE                                                               
000667         MOVE ZERO             TO WS-KVOI-RED                             
000668       END-IF                                                             
000669                                                                          
000670       ADD WS-KVOI-RED         TO WS-KVOI-NDC (IX-AR, IX-PER)             
000671                                  WS-TOTAL-KVOI                           
000672       ADD +1                  TO IX-PER                                  
000673     END-PERFORM                                                          
000674                                                                          
000675     ADD WS-TOTAL-KVOI         TO WS-KVOI-TOT-NDC (IX-AR)                 
000676                                                                          
000677     MOVE 2                    TO IX-TILL-AR                              
000678     MOVE 1                    TO IX-FRAN-AR                              
000679                                  IX-PER                                  
000680     PERFORM UNTIL IX-TILL-AR > 6                                         
000681*  LÄGG UT HISTORIK FÖR FÖREGÅENDE ÅR OCH TIDIGARE                        
000682       MOVE ZERO               TO WS-TOTAL-KVOI                           
000683       PERFORM UNTIL IX-PER > 12                                          
000684                                                                          
000685         IF OIHD-KVOI (IX-FRAN-AR, IX-PER) > ZERO                         
000686           COMPUTE WS-KVOI-RED ROUNDED =                                  
000687              OIHD-KVOI (IX-FRAN-AR, IX-PER)                              
000688                 / OIHD-KVVIPER (IX-FRAN-AR, IX-PER) * +4.33              
000689         ELSE                                                             
000690           MOVE ZERO           TO WS-KVOI-RED                             
000691         END-IF                                                           
000692                                                                          
000693         ADD WS-KVOI-RED      TO WS-KVOI-NDC (IX-TILL-AR, IX-PER)         
000694                                 WS-TOTAL-KVOI                            
000695         ADD +1                TO IX-PER                                  
000696       END-PERFORM                                                        
000697                                                                          
000698       ADD WS-TOTAL-KVOI       TO WS-KVOI-TOT-NDC (IX-TILL-AR)            
000699                                                                          
000700       ADD +1                  TO IX-FRAN-AR                              
000701                                  IX-TILL-AR                              
000702       MOVE +1                 TO IX-PER                                  
000703     END-PERFORM                                                          
000704     .                                                                    
000705     EJECT                                                                
000706 MFS-RENSA-FAELT-UT SECTION.                                              
000707                                                                          
000708*    --- ALLA UTDATA-FÄLT                                                 
000709     MOVE MFS-RENSA-FAELT TO MOD-BEART-ENG                                
000710                             MOD-TEMFSINF                                 
000711                                                                          
000712     MOVE +1                 TO IX-AR                                     
000713                                IX-PER                                    
000714     PERFORM UNTIL IX-AR > +6                                             
000715*      MOVE MFS-RENSA-FAELT  TO MOD-AARTAL (IX-AR)                        
000716       MOVE MFS-RENSA-FAELT  TO MOD-KVOI-TOT (IX-AR)                      
000717       PERFORM UNTIL IX-PER > +12                                         
000718         MOVE MFS-RENSA-FAELT                                             
000719                             TO MOD-KVOI (IX-AR, IX-PER)                  
000720         ADD +1              TO IX-PER                                    
000721       END-PERFORM                                                        
000722       ADD +1                TO IX-AR                                     
000723       MOVE +1               TO IX-PER                                    
000724     END-PERFORM                                                          
000725     .                                                                    
000726     EJECT                                                                
000727* --- IMS SEKTIONER ---                                                   
000728     SKIP3                                                                
000729 IMS-GET-MSG SECTION.                                                     
000730                                                                          
000731     MOVE '  QC' TO GODK-STATUSKODER                                      
000732     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
000733     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
000734     PERFORM IMS-STATUSKONTROLL                                           
000735     .                                                                    
000736     SKIP3                                                                
000737 IMS-INSERT-MSG SECTION.                                                  
000738                                                                          
000739     IF ENGLISH-TEXT                                                      
000740       MOVE 'N' TO MFS-KDHUVOMR                                           
000741     END-IF                                                               
000742     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
000743     MOVE SPACE TO GODK-STATUSKODER                                       
000744     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
000745     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
000746     PERFORM IMS-STATUSKONTROLL                                           
000747     .                                                                    
000748     EJECT                                                                
000749 IMS-GU-WDL711 SECTION.                                                   
000750                                                                          
000751     STRING 'WDL701  (IDARTNR  =' W-IDARTNR-X ')'                         
000752          DELIMITED BY SIZE INTO SSA1                                     
000753     STRING 'WDL711  (IDDC     =' W-IDDC-X ')'                            
000754          DELIMITED BY SIZE INTO SSA2                                     
000755     MOVE '  GE' TO GODK-STATUSKODER                                      
000756     CALL CBLTDLI USING GU WDL7-PCB DLI-IO-AREA-L711 SSA1 SSA2            
000757     MOVE WDL7-STATUS-CODE TO STATUS-WS                                   
000758     PERFORM IMS-STATUSKONTROLL                                           
000759     .                                                                    
000760     SKIP3                                                                
000761 IMS-GU-WDL411 SECTION.                                                   
000762                                                                          
000763     STRING 'WDL401  (IDARTNR  =' W-IDARTNR-X ')'                         
000764          DELIMITED BY SIZE INTO SSA1                                     
000765     STRING 'WDL411  (IDDC     =' W-IDDC-X ')'                            
000766          DELIMITED BY SIZE INTO SSA2                                     
000767     MOVE '  GE' TO GODK-STATUSKODER                                      
000768     CALL CBLTDLI USING GU WDL4-PCB DLI-IO-AREA-L411 SSA1 SSA2            
000769     MOVE WDL4-STATUS-CODE TO STATUS-WS                                   
000770     PERFORM IMS-STATUSKONTROLL                                           
000771     .                                                                    
000772     SKIP3                                                                
000773 IMS-GU-BENA01-BSEQ SECTION.                                              
000774                                                                          
000775     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
000776          DELIMITED BY SIZE INTO SSA1                                     
000777     MOVE '  GE' TO GODK-STATUSKODER                                      
000778     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA-D3 SSA1                   
000779     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
000780     PERFORM IMS-STATUSKONTROLL                                           
000781     .                                                                    
000782     SKIP3                                                                
000783 IMS-GNP-BENA11 SECTION.                                                  
000784                                                                          
000785     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
000786          DELIMITED BY SIZE INTO SSA1                                     
000787     MOVE '  GE' TO GODK-STATUSKODER                                      
000788     CALL CBLTDLI USING GNP BENA-PCB DLI-IO-AREA-D3 SSA1                  
000789     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
000790     PERFORM IMS-STATUSKONTROLL                                           
000791     .                                                                    
000792     EJECT                                                                
000793 IMS-GN-WDB601    SECTION.                                                
000794     MOVE 'WDB601  ' TO SSA1                                              
000795     MOVE '  GB' TO GODK-STATUSKODER                                      
000796     CALL CBLTDLI USING GN WDB6-PCB DLI-IO-AREA-B601 SSA1                 
000797     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
000798     PERFORM IMS-STATUSKONTROLL                                           
000799     .                                                                    
000800 IMS-GU-WDK711    SECTION.                                                
000801     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
000802          DELIMITED BY SIZE INTO SSA1                                     
000803     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
000804          DELIMITED BY SIZE INTO SSA2                                     
000805     MOVE '  GE' TO GODK-STATUSKODER                                      
000806     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-K711 SSA1 SSA2            
000807     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
000808     PERFORM IMS-STATUSKONTROLL                                           
000809     .                                                                    
000810     EJECT                                                                
000811 IMS-GU-WDK629      SECTION.                                              
000812                                                                          
000813     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
000814          DELIMITED BY SIZE INTO SSA1                                     
000815     MOVE 'WDK611  '       TO SSA2                                        
000816     MOVE 'WDK629  '       TO SSA3                                        
000817     MOVE '  GE' TO GODK-STATUSKODER                                      
000818     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-K629                      
000819                                    SSA1 SSA2 SSA3                        
000820     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
000821     PERFORM IMS-STATUSKONTROLL                                           
000822     .                                                                    
000823     SKIP3                                                                
000824 IMS-STATUSKONTROLL SECTION.                                              
000825                                                                          
000826     SET STATUS-IX TO 1                                                   
000827     SEARCH GODK-STATUS                                                   
000828       AT END                                                             
000829         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
000830         DELIMITED BY SIZE INTO FELTEXT                                   
000831         CALL FELLOG                                                      
000832       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
000833         CONTINUE                                                         
000834     END-SEARCH                                                           
000840     .                                                                    
