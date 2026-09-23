000001*COMPOPT STDSUB=YES                                                       
000002 ID DIVISION.                                                             
000003 PROGRAM-ID.     W335PRQU.                                                
000004 AUTHOR.         ÖSTRÖM ELEONOR.                                          
000005 DATE-WRITTEN.   02/03/12.                                                
000006 DATE-COMPILED.                                                           
000007                                                                          
000008*    FUNKTION:                                                            
000009*        PROGRAMMET UPPDATERAR WDC7 MED FRÅGOR TILL GETPRICE              
000010*        FÖR MARKNADER(WWDIST79) SOM SKA HA DEALERINFORMATION.            
000011*        FÖR DE DEALERKUNDER SOM EJ HAR LOKALT PRIS PÅ RADEN              
000012*        RÄKNAS ETT PRELIMINÄRT PRIS UT MHA SUB-PROGRAM                   
000013*        W411SJKO OCH VARS PRIS SEDAN RÄKNAS OM TILL LOKAL VALUTA         
000014*        GENOM SUB-PROGRAM W411EXCH.                                      
000015*        PROGRAMMET KAN OCKSÅ TA BORT PRISFRÅGOR, ANTINGEN FÖR HEL        
000016*        ORDER ELLER ENSTAKA RADER. PROGRAMMET KAN ÄVEN UPPDATERA         
000017*        ANTAL (KVANTAL-AVBOK).                                           
000018*                                                                         
000019*        PROGRAMMET UPPDATERAR WDC7                                       
000020*                   LÄSER      WDG2                                       
000021*                                                                         
000022*        LÄNKAREA: W335PRQU                                               
000023*                                                                         
000024*    E'TRACKER: 5276159 DATED 2007-08-15                                  
000025*                                                                         
000026                                                                          
000027     SKIP3                                                                
000028 ENVIRONMENT DIVISION.                                                    
000029     SKIP2                                                                
000030 DATA DIVISION.                                                           
000031     SKIP2                                                                
000032 WORKING-STORAGE SECTION.                                                 
000033                                                                          
000034 77  IDPGM                       PIC X(8)    VALUE 'W335PRQU'.            
000035 77  JA                          PIC X       VALUE 'J'.                   
000036 77  NEJ                         PIC X       VALUE 'N'.                   
000037 77  WS-PRARTSJK-SEK             PIC S9(7)V9(2) VALUE +0  COMP-3.         
000038 77  WS-KVROS                    PIC S9(7)      VALUE +0  COMP-3.         
000039 77  W-DATE-AAMM                 PIC 9(4)       VALUE ZERO.               
000040 77  WS-KDVALISO-HUV             PIC X(3)       VALUE 'SEK'.              
000041     EJECT                                                                
000042 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
000043 01  FILLER REDEFINES DAGENS-DATUM.                                       
000044     03  DAGENS-DATUM-AAR        PIC 9(2).                                
000045     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
000046     03  DAGENS-DATUM-DAG        PIC 9(2).                                
000047     EJECT                                                                
000048 01  TEST-IDDISTR                PIC  9(5)   COMP-3.                      
000049*01  FILLER   -COPY WWDIST03    -RED TEST-IDDISTR.                        
000050     EJECT                                                                
000051 01  GENERELLA-SUBPROGRAM.                                                
000052*                                                                         
000053     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
000054     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000055     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000056     03  W411EXCH                PIC X(8)    VALUE 'W411EXCH'.            
000057     03  W411SJKO                PIC X(8)    VALUE 'W411SJKO'.            
000058     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
000059     SKIP2                                                                
000060*    --- PARAMETRAR TILL ABEND                                            
000061                                                                          
000062 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
000063 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
000064 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
000065     SKIP2                                                                
000066 01  FELTEXT.                                                             
000067     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
000068     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
000069     EJECT                                                                
000070*    --- PARAMETRAR TILL SUBPROGRAM                                       
000071                                                                          
000072*01  -COPY W411EXCH                                                       
000073     EJECT                                                                
000074*01  -COPY W411SJKO                                                       
000075     EJECT                                                                
000076*01  -COPY W510CURR                                                       
000077     EJECT                                                                
000078*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
000079*                                                                         
000080     EJECT                                                                
000081 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000082     SKIP3                                                                
000083 01  NYCKLAR-TILL-DLI.                                                    
000084* TILL DDI PRISFRÅGA                                                      
000085   03  W-WDC701KY-X.                                                      
000086     05  W-PRQ-IDDISTR           PIC 9(4)  VALUE ZERO.                    
000087     05  W-PRQ-IDKUNDNR          PIC 9(7)  VALUE ZERO.                    
000088     05  W-PRQ-IDBUNDLE          PIC X(15) VALUE SPACE.                   
000089     05  W-PRQ-IDORDER-FILLER REDEFINES W-PRQ-IDBUNDLE.                   
000090       07  W-PRQ-IDORDNR7       PIC 9(7).                                 
000091       07  FILLER                PIC X(8).                                
000092                                                                          
000093   03  W-IDPRQUES-X.                                                      
000094     05  W-LPRQ-IDPRQUES         PIC 9(7)  VALUE ZERO.                    
000100     SKIP2                                                                
000101                                                                          
000102*    --- STATUS-KOD FRÅN IMS                                              
000103 01  STATUS-WS                   PIC XX.                                  
000104     88  SEGMENT-FINNS                       VALUE '  '.                  
000105     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
000106     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
000107     SKIP2                                                                
000108 01  GODK-STATUSKODER.                                                    
000109     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000110     SKIP3                                                                
000111 01  SSA1                        PIC X(64).                               
000112 01  SSA2                        PIC X(64).                               
000113     EJECT                                                                
000114*    --- IMS FUNKTIONSKODER                                               
000115*01  -COPY W0003                                                          
000116     EJECT                                                                
000117*    ---  DLI INPUT-OUTPUT AREA                                           
000118 01  FILLER                   PIC X(16) VALUE 'DLI-IO-WDC701'.            
000119 01  DLI-IO-WDC701.                                                       
000120*    03  -COPY WDC701                                                     
000121     EJECT                                                                
000122 01  FILLER                   PIC X(16) VALUE 'DLI-IO-WDC711'.            
000123 01  DLI-IO-WDC711.                                                       
000124*    03  -COPY WDC711                                                     
000125     EJECT                                                                
000131 LINKAGE SECTION.                                                         
000132                                                                          
000133*01  -COPY W335PRQU                                                       
000134     EJECT                                                                
000135*01  -COPY W0008  -PRE WDG2-                                              
000136     05  FILLER                  PIC X.                                   
000137*01  -COPY W0008  -PRE WDC7-                                              
000138     05  FILLER                  PIC X.                                   
000139     EJECT                                                                
000140 01  SJKO-WDK6-PCB               PIC X.                                   
000141                                                                          
000142 PROCEDURE DIVISION  USING PRQU-W335PRQU WDG2-PCB                         
000143                                         WDC7-PCB                         
000144                                    SJKO-WDK6-PCB.                        
000145                                                                          
000146 STYR SECTION.                                                            
000147                                                                          
000148     PERFORM A-INIT                                                       
000149                                                                          
000150     EVALUATE PRQU-KDCALL                                                 
000151        WHEN 1                                                            
000152          PERFORM B-SKAPA-WDC701                                          
000153          IF PRQU-PRARTNTO-LOC    = +0                                    
000154            PERFORM CA-HAEMTA-SJAELVKOST-W411SJKO                         
000155            PERFORM D-HAEMTA-LOKAL-VALUTA                                 
000156          END-IF                                                          
000157          PERFORM E-SKAPA-WDC711                                          
000158        WHEN 2                                                            
000159          PERFORM IMS-GET-WDC701-KDCALL-2                                 
000160          IF PRQU-PRARTNTO-LOC    = +0                                    
000161            PERFORM CA-HAEMTA-SJAELVKOST-W411SJKO                         
000162            PERFORM D-HAEMTA-LOKAL-VALUTA                                 
000163          END-IF                                                          
000164          PERFORM E-SKAPA-WDC711                                          
000165        WHEN 3                                                            
000166          PERFORM F-TA-BORT-FRAGOR-FOR-ORDER                              
000167        WHEN 4                                                            
000168          PERFORM G-TA-BORT-ENSTAKA-FRAGA                                 
000169        WHEN 5                                                            
000170          PERFORM H-ANDRA-ANTAL-I-FRAGA                                   
000171        WHEN 6                                                            
000172          PERFORM I-ANDRA-ORDERKLASS                                      
000173     END-EVALUATE                                                         
000174                                                                          
000175     MOVE ZERO TO RETURN-CODE                                             
000176     GOBACK                                                               
000177     .                                                                    
000178     EJECT                                                                
000179 A-INIT SECTION.                                                          
000180     ACCEPT DAGENS-DATUM  FROM DATE                                       
000181     MOVE DAGENS-DATUM-AAR      TO W-DATE-AAMM(1:2)                       
000182     MOVE DAGENS-DATUM-MAANAD   TO W-DATE-AAMM(3:2)                       
000183     MOVE W-DATE-AAMM           TO CURR-TIAAMM                            
000184     MOVE WS-KDVALISO-HUV       TO CURR-KDVALISO-HUV                      
000185     MOVE 'M'                   TO CURR-KDVALTYP                          
000186                                                                          
000187     MOVE PRQU-IDDISTR       TO W-PRQ-IDDISTR                             
000188     MOVE PRQU-IDKUNDNR      TO W-PRQ-IDKUNDNR                            
000189     IF PRQU-IDSYSTEM    = 'PROF'                                         
000190        MOVE PRQU-IDORDNR7      TO W-PRQ-IDORDNR7                         
000191     ELSE                                                                 
000192        MOVE PRQU-IDORDNR7      TO W-PRQ-IDORDNR7                         
000193     END-IF                                                               
000194     .                                                                    
000195     EJECT                                                                
000196                                                                          
000197 B-SKAPA-WDC701 SECTION.                                                  
000198                                                                          
000199     MOVE SPACE              TO PRQ-IDBUNDLE                              
000200     MOVE PRQU-IDDISTR       TO PRQ-IDDISTR                               
000201     MOVE PRQU-IDKUNDNR      TO PRQ-IDKUNDNR                              
000202     IF PRQU-IDSYSTEM    = 'PROF'                                         
000203        MOVE PRQU-IDKUNDRF      TO PRQ-IDBUNDLE                           
000204     ELSE                                                                 
000205        MOVE PRQU-IDORDNR7      TO PRQ-IDORDNR7                           
000206     END-IF                                                               
000207     PERFORM IMS-GET-WDC701-KDCALL-1                                      
000208     IF SEGMENT-FINNS                                                     
000209       CONTINUE                                                           
000210     ELSE                                                                 
000211       PERFORM IMS-ISRT-WDC701                                            
000212     END-IF                                                               
000213     .                                                                    
000214                                                                          
000215 CA-HAEMTA-SJAELVKOST-W411SJKO SECTION.                                   
000216                                                                          
000217     MOVE 1                 TO SJKO-KDCALL                                
000218     MOVE PRQU-IDARTNR      TO SJKO-IDARTNR                               
000219                                                                          
000220     CALL W411SJKO USING SJKO-W411SJKO SJKO-WDK6-PCB                      
000221                                                                          
000222     MOVE SJKO-PRARTSJK     TO WS-PRARTSJK-SEK                            
000223     .                                                                    
000224                                                                          
000225 CB-HAMTA-ROSALDO-W411SJKO SECTION.                                       
000226                                                                          
000227     MOVE 2                 TO SJKO-KDCALL                                
000228     MOVE PRQU-IDARTNR      TO SJKO-IDARTNR                               
000229                                                                          
000230     CALL W411SJKO USING SJKO-W411SJKO SJKO-WDK6-PCB                      
000231                                                                          
000232     MOVE SJKO-KVROS        TO WS-KVROS                                   
000233     .                                                                    
000234                                                                          
000235 D-HAEMTA-LOKAL-VALUTA SECTION.                                           
000236                                                                          
000237     MOVE PRQU-KDVALISO      TO CURR-KDVALISO-ROW                         
000239                                                                          
000240     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
000241     IF CURR-KDSVAR = ' '                                                 
000242        MOVE CURR-PRKURS-NEW TO EXCH-PRKURS                               
000243     ELSE                                                                 
000244        MOVE 1               TO EXCH-PRKURS                               
000245     END-IF                                                               
000247     MOVE +2                 TO EXCH-KDCALL                               
000248     MOVE +0                 TO EXCH-SUORDV-IN                            
000249     MOVE WS-PRARTSJK-SEK    TO EXCH-PRARTNTO-IN                          
000250                                                                          
000251     CALL W411EXCH USING EXCH-W411EXCH                                    
000252                                                                          
000253     MOVE EXCH-PRARTNTO-UT   TO PRQU-PRARTNTO-LOCPREL                     
000254     .                                                                    
000255                                                                          
000256 E-SKAPA-WDC711 SECTION.                                                  
000257                                                                          
000258                                                                          
000259                                                                          
000260     MOVE PRQU-IDPRQUES          TO LPRQ-IDPRQUES                         
000261     IF PRQU-IDSYSTEM    = 'PROF'                                         
000262        MOVE 'CARPARTS.PULS.PROFORMA'   TO LPRQ-ADDISPABS                 
000263     ELSE                                                                 
000264        MOVE SPACE          TO LPRQ-ADDISPABS                             
000265     END-IF                                                               
000266     MOVE PRQU-KDPRSTA      TO LPRQ-KDPRSTA                               
000267     MOVE PRQU-KDORDKL      TO LPRQ-KDORDKL                               
000268     PERFORM EA-FIXA-KDORDKL-DAG-SE                                       
000269     MOVE PRQU-IDARTNR      TO LPRQ-IDARTNR                               
000270     MOVE PRQU-KVBEART-Q    TO LPRQ-KVBEART                               
000271     MOVE NEJ               TO LPRQ-FLARTSTD                              
000272     MOVE SPACE             TO LPRQ-KDORDTYP                              
000273     IF PRQU-KDPRSTA = 'Q'                                                
000274       MOVE 'N'             TO LPRQ-KDPRSTA                               
000275       MOVE 'M'             TO LPRQ-KDORDTYP                              
000276     END-IF                                                               
000277     MOVE ZERO              TO LPRQ-PRARTBTO-LOC                          
000278     MOVE ZERO              TO LPRQ-PRARTNTO-LOC                          
000279     MOVE SPACE             TO LPRQ-KDRAB                                 
000280     MOVE SPACE             TO LPRQ-KDVALISO                              
000281     MOVE SPACE             TO LPRQ-KDVAT                                 
000282     MOVE ZERO              TO LPRQ-REARTRAB                              
000283     MOVE SPACE             TO LPRQ-BEART-VIPS                            
000284     MOVE NEJ               TO LPRQ-FLALL                                 
000285     MOVE ZERO              TO LPRQ-KVANTAL-AVBOK                         
000286                                                                          
000287     MOVE FUNCTION CURRENT-DATE (1:14) TO LPRQ-DADATTID-REG               
000288                                                                          
000289     MOVE ZERO              TO LPRQ-DADATTID-SEND                         
000290     MOVE ZERO              TO LPRQ-DADATTID-SVAR                         
000291     MOVE ZERO              TO LPRQ-DADATTID-OK                           
000292     MOVE ZERO              TO LPRQ-KDFEL                                 
000293     MOVE ZERO              TO LPRQ-KDSKEPP                               
000294     MOVE SPACE             TO LPRQ-FILLER1                               
000295     MOVE SPACE             TO LPRQ-KDSEGKEY                              
000296     MOVE SPACE             TO LPRQ-FILLER                                
000297                                                                          
000298     PERFORM IMS-ISRT-WDC711                                              
000299     .                                                                    
000300                                                                          
000301 EA-FIXA-KDORDKL-DAG-SE SECTION.                                          
000302                                                                          
000303     MOVE PRQU-IDDISTR         TO TEST-IDDISTR                            
000304     MOVE NEJ                  TO PRQU-FLPRTILL                           
000305     IF DIST03-SVERIGE                                                    
000306        AND PRQU-KDORDKL < 3                                              
000307       PERFORM CB-HAMTA-ROSALDO-W411SJKO                                  
000308*      IF WS-KVROS > ZERO                                                 
000309**       MOVE 4                TO LPRQ-KDORDKL                            
000310*        MOVE NEJ              TO PRQU-FLPRTILL                           
000311*      ELSE                                                               
000312         MOVE JA               TO PRQU-FLPRTILL                           
000313*      END-IF                                                             
000314     END-IF                                                               
000315     .                                                                    
000316     EJECT                                                                
000317                                                                          
000318 F-TA-BORT-FRAGOR-FOR-ORDER SECTION.                                      
000319                                                                          
000320     MOVE SPACE              TO W-PRQ-IDBUNDLE                            
000321     MOVE PRQU-IDDISTR       TO W-PRQ-IDDISTR                             
000322     MOVE PRQU-IDKUNDNR      TO W-PRQ-IDKUNDNR                            
000323     MOVE PRQU-IDKUNDRF      TO W-PRQ-IDBUNDLE                            
000324     PERFORM IMS-GHU-WDC701                                               
000325     IF SEGMENT-FINNS                                                     
000326       PERFORM IMS-DLET-WDC701                                            
000327     END-IF                                                               
000328     .                                                                    
000329     EJECT                                                                
000330                                                                          
000331 G-TA-BORT-ENSTAKA-FRAGA SECTION.                                         
000332                                                                          
000333     MOVE SPACE              TO W-PRQ-IDBUNDLE                            
000334     MOVE PRQU-IDDISTR       TO W-PRQ-IDDISTR                             
000335     MOVE PRQU-IDKUNDNR      TO W-PRQ-IDKUNDNR                            
000336     MOVE PRQU-IDKUNDRF      TO W-PRQ-IDBUNDLE                            
000337     MOVE PRQU-IDPRQUES      TO W-LPRQ-IDPRQUES                           
000338     PERFORM IMS-GHU-WDC711                                               
000339     IF SEGMENT-FINNS                                                     
000340       PERFORM IMS-DLET-WDC711                                            
000341     END-IF                                                               
000342*    PERFORM IMS-GU-WDC711-OKVAL                                          
000343*    IF SEGMENT-SAKNAS                                                    
000344*       PERFORM IMS-GHU-WDC701                                            
000345*       IF SEGMENT-FINNS                                                  
000346*         PERFORM IMS-DLET-WDC701                                         
000347*       END-IF                                                            
000348*    END-IF                                                               
000349     .                                                                    
000350     EJECT                                                                
000351                                                                          
000352 H-ANDRA-ANTAL-I-FRAGA SECTION.                                           
000353                                                                          
000354*    MANUELL ÄNDRING AV ANTAL PÅ ORDERRAD                                 
000355*    PRISFRÅGANS URSPRUNGLIGA ANTAL BEHÅLLS VID ÄNDRING                   
000356*    AV ANTAL PÅ ORDERN. ISTÄLLET ÖKAS AVBOKAT ANTAL                      
000357*    MED FÖRÄNDRINGEN I BESTÄLLT ANTAL.                                   
000358     MOVE SPACE              TO W-PRQ-IDBUNDLE                            
000359     MOVE PRQU-IDDISTR       TO W-PRQ-IDDISTR                             
000360     MOVE PRQU-IDKUNDNR      TO W-PRQ-IDKUNDNR                            
000361     MOVE PRQU-IDKUNDRF      TO W-PRQ-IDBUNDLE                            
000362     MOVE PRQU-IDPRQUES      TO W-LPRQ-IDPRQUES                           
000363     PERFORM IMS-GHU-WDC711                                               
000364     IF SEGMENT-FINNS                                                     
000365        COMPUTE LPRQ-KVANTAL-AVBOK =                                      
000366                LPRQ-KVANTAL-AVBOK +                                      
000367                     (LPRQ-KVBEART - PRQU-KVBEART-Q)                      
000368       IF LPRQ-KVANTAL-AVBOK >= LPRQ-KVBEART                              
000369*TEST FIX                                                                 
000370*         MOVE JA TO LPRQ-FLALL                                           
000371*         PERFORM IMS-REPL-WDC711                                         
000372*TEST FIX SLUT                                                            
000373          PERFORM IMS-DLET-WDC711                                         
000374       ELSE                                                               
000375          PERFORM IMS-REPL-WDC711                                         
000376       END-IF                                                             
000377     END-IF                                                               
000378     .                                                                    
000379     EJECT                                                                
000380 I-ANDRA-ORDERKLASS    SECTION.                                           
000381                                                                          
000382*    FÖR SVERIGE KAN RO GE ORDERKLASS 4 I PRISFRÅGA                       
000383     MOVE SPACE              TO W-PRQ-IDBUNDLE                            
000384     MOVE PRQU-IDDISTR       TO W-PRQ-IDDISTR                             
000385     MOVE PRQU-IDKUNDNR      TO W-PRQ-IDKUNDNR                            
000386     MOVE PRQU-IDKUNDRF      TO W-PRQ-IDBUNDLE                            
000387     MOVE PRQU-IDPRQUES      TO W-LPRQ-IDPRQUES                           
000388     PERFORM IMS-GHU-WDC711                                               
000389     IF SEGMENT-FINNS                                                     
000390       MOVE 4                TO LPRQ-KDORDKL                              
000391       MOVE ZERO             TO LPRQ-PRARTBTO-LOC                         
000392       MOVE ZERO             TO LPRQ-PRARTNTO-LOC                         
000393       MOVE SPACE            TO LPRQ-KDRAB                                
000394       MOVE ZERO             TO LPRQ-REARTRAB                             
000395       MOVE NEJ              TO LPRQ-FLALL                                
000396       MOVE ZERO             TO LPRQ-KVANTAL-AVBOK                        
000397       MOVE 'M'              TO LPRQ-KDORDTYP                             
000398                                                                          
000399       MOVE FUNCTION CURRENT-DATE (1:14) TO LPRQ-DADATTID-REG             
000400                                                                          
000401       MOVE ZERO             TO LPRQ-DADATTID-SEND                        
000402       MOVE ZERO             TO LPRQ-DADATTID-SVAR                        
000403       MOVE ZERO             TO LPRQ-DADATTID-OK                          
000404       MOVE ZERO             TO LPRQ-KDFEL                                
000405       MOVE ZERO             TO LPRQ-KDSKEPP                              
000406       MOVE SPACE            TO LPRQ-FILLER                               
000407       PERFORM IMS-REPL-WDC711                                            
000408     ELSE                                                                 
000409       MOVE -1               TO PRQU-KDCALL                               
000410     END-IF                                                               
000411     .                                                                    
000412     EJECT                                                                
000413                                                                          
000414* --- IMS SEKTIONER ---                                                   
000415                                                                          
000416     EJECT                                                                
000417 IMS-GET-WDC701-KDCALL-1 SECTION.                                         
000418                                                                          
000419     STRING 'WDC701  (WDC701KY =' W-WDC701KY-X ')'                        
000420          DELIMITED BY SIZE INTO SSA1                                     
000421     MOVE 'GE  ' TO GODK-STATUSKODER                                      
000422     CALL CBLTDLI USING GU WDC7-PCB DLI-IO-WDC701 SSA1                    
000423     MOVE WDC7-STATUS-CODE TO STATUS-WS                                   
000424     PERFORM IMS-STATUSKONTROLL                                           
000425     .                                                                    
000426     SKIP3                                                                
000427 IMS-GET-WDC701-KDCALL-2 SECTION.                                         
000428                                                                          
000429     STRING 'WDC701  (WDC701KY =' W-WDC701KY-X ')'                        
000430          DELIMITED BY SIZE INTO SSA1                                     
000431     MOVE '  ' TO GODK-STATUSKODER                                        
000432     CALL CBLTDLI USING GU WDC7-PCB DLI-IO-WDC701 SSA1                    
000433     MOVE WDC7-STATUS-CODE TO STATUS-WS                                   
000434     PERFORM IMS-STATUSKONTROLL                                           
000435     .                                                                    
000436     SKIP3                                                                
000437 IMS-GHU-WDC701 SECTION.                                                  
000438                                                                          
000439     STRING 'WDC701  (WDC701KY =' W-WDC701KY-X ')'                        
000440          DELIMITED BY SIZE INTO SSA1                                     
000441     MOVE 'GE  ' TO GODK-STATUSKODER                                      
000442     CALL CBLTDLI USING GHU WDC7-PCB DLI-IO-WDC701 SSA1                   
000443     MOVE WDC7-STATUS-CODE TO STATUS-WS                                   
000444     PERFORM IMS-STATUSKONTROLL                                           
000445     .                                                                    
000446     SKIP3                                                                
000447 IMS-GHU-WDC711 SECTION.                                                  
000448                                                                          
000449     STRING 'WDC701  (WDC701KY =' W-WDC701KY-X ')'                        
000450          DELIMITED BY SIZE INTO SSA1                                     
000451     STRING 'WDC711  (IDPRQUES =' W-IDPRQUES-X ')'                        
000452          DELIMITED BY SIZE INTO SSA2                                     
000453     MOVE 'GE  ' TO GODK-STATUSKODER                                      
000454     CALL CBLTDLI USING GHU WDC7-PCB DLI-IO-WDC711 SSA1 SSA2              
000455     MOVE WDC7-STATUS-CODE TO STATUS-WS                                   
000456     PERFORM IMS-STATUSKONTROLL                                           
000457     .                                                                    
000458     EJECT                                                                
000459 IMS-GU-WDC711-OKVAL SECTION.                                             
000460                                                                          
000461     STRING 'WDC701  (WDC701KY =' W-WDC701KY-X ')'                        
000462          DELIMITED BY SIZE INTO SSA1                                     
000463     MOVE 'WDC711  ' TO SSA2                                              
000464     MOVE '  GE' TO GODK-STATUSKODER                                      
000465     CALL CBLTDLI USING GU WDC7-PCB DLI-IO-WDC711 SSA1 SSA2               
000466     MOVE WDC7-STATUS-CODE TO STATUS-WS                                   
000467     PERFORM IMS-STATUSKONTROLL                                           
000468     .                                                                    
000469     EJECT                                                                
000470                                                                          
000471                                                                          
000472 IMS-ISRT-WDC701 SECTION.                                                 
000473                                                                          
000474     MOVE 'WDC701 ' TO SSA1                                               
000475     MOVE '    ' TO GODK-STATUSKODER                                      
000476     CALL CBLTDLI USING ISRT WDC7-PCB DLI-IO-WDC701 SSA1                  
000477     MOVE WDC7-STATUS-CODE TO STATUS-WS                                   
000478     PERFORM IMS-STATUSKONTROLL                                           
000479     .                                                                    
000480     EJECT                                                                
000481 IMS-ISRT-WDC711 SECTION.                                                 
000482                                                                          
000483     MOVE 'WDC711  ' TO SSA1                                              
000484     MOVE '    ' TO GODK-STATUSKODER                                      
000485     CALL CBLTDLI USING ISRT WDC7-PCB DLI-IO-WDC711 SSA1                  
000486     MOVE WDC7-STATUS-CODE TO STATUS-WS                                   
000487     PERFORM IMS-STATUSKONTROLL                                           
000488     .                                                                    
000489                                                                          
000490 IMS-REPL-WDC711 SECTION.                                                 
000491                                                                          
000492     MOVE '  ' TO GODK-STATUSKODER                                        
000493     CALL CBLTDLI USING REPL WDC7-PCB DLI-IO-WDC711                       
000494     MOVE WDC7-STATUS-CODE TO STATUS-WS                                   
000495     PERFORM IMS-STATUSKONTROLL                                           
000496     .                                                                    
000497     EJECT                                                                
000498                                                                          
000499 IMS-DLET-WDC701 SECTION.                                                 
000500                                                                          
000501     MOVE '  ' TO GODK-STATUSKODER                                        
000502     CALL CBLTDLI USING DLET WDC7-PCB DLI-IO-WDC701                       
000503     MOVE WDC7-STATUS-CODE TO STATUS-WS                                   
000504     PERFORM IMS-STATUSKONTROLL                                           
000505     .                                                                    
000506     EJECT                                                                
000507                                                                          
000508 IMS-DLET-WDC711 SECTION.                                                 
000509                                                                          
000510     MOVE '  ' TO GODK-STATUSKODER                                        
000511     CALL CBLTDLI USING DLET WDC7-PCB DLI-IO-WDC711                       
000512     MOVE WDC7-STATUS-CODE TO STATUS-WS                                   
000513     PERFORM IMS-STATUSKONTROLL                                           
000514     .                                                                    
000515     EJECT                                                                
000520                                                                          
000521 IMS-STATUSKONTROLL SECTION.                                              
000522                                                                          
000523     SET STATUS-IX TO 1                                                   
000524     SEARCH GODK-STATUS                                                   
000525       AT END                                                             
000526         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
000527           DELIMITED BY SIZE INTO FELTEXT                                 
000528         DISPLAY FELTEXT                                                  
000529         CALL FELLOG                                                      
000530       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
000531         CONTINUE                                                         
000532     END-SEARCH                                                           
000533     .                                                                    
