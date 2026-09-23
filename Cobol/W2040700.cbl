000001**********************************************************                
000002 ID DIVISION.                                                             
000003 PROGRAM-ID.     W2040700.                                                
000004 AUTHOR.         RAHUL JAIN.                                              
000005 DATE-WRITTEN.   12/09/10.                                                
000006 DATE-COMPILED.                                                           
000007                                                                          
000008*    FUNKTION:                                                            
000009*        FÖRSÄLJNINGSSTATISTIK                                            
000010*                                                                         
000020*        PROGRAMMET UPPDATERAR WDP7 (WLUSEA)                              
000021*        PROGRAMMET LÄSER      WDL7 + WDL4                                
000022*                              WDK7 (WLARTS)                              
000023*                              WDD3 (WLBENA)                              
000024*                              WDB6                                       
000025*                              WDK6                                       
000026*                                                                         
000027*    INDATA.                                                              
000028*        TRANSAKTION: W2T407                                              
000029*        MID:         W2I40701                                            
000030*                                                                         
000031*    UTDATA.                                                              
000032*        MOD:         W2O40701                                            
000033*                                                                         
000034                                                                          
000035     SKIP3                                                                
000036 ENVIRONMENT DIVISION.                                                    
000037     EJECT                                                                
000038 DATA DIVISION.                                                           
000039 WORKING-STORAGE SECTION.                                                 
000040                                                                          
000041*    -- CHECKED BY WY2000                                                 
000042 77  IDPGM                       PIC X(08)   VALUE 'W2040700'.            
000043                                                                          
000044*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
000045 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
000046                                                                          
000047 77  JA                          PIC X       VALUE 'J'.                   
000048 77  NEJ                         PIC X       VALUE 'N'.                   
000049 77  IX-AR                       PIC 9(9)    VALUE ZERO  COMP-3.          
000050 77  IX-PER                      PIC 9(9)    VALUE ZERO  COMP-3.          
000051 77  IX-FRAN-AR                  PIC 9(9)    VALUE ZERO  COMP-3.          
000052 77  IX-TILL-AR                  PIC 9(9)    VALUE ZERO  COMP-3.          
000053 77  IX-RULL                     PIC 9(9)    VALUE ZERO  COMP-3.          
000054 77  WS-IDLEVNR-8                PIC X(8)    VALUE SPACE.                 
000055 77  IDLEVNR-WS                  PIC X(5)    VALUE SPACE.                 
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
000087  05 WS-TYPE                     PIC  X      VALUE SPACE.                 
000088                                                                          
000089                                                                          
000090                                                                          
000091*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
000092                                                                          
000093 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
000094 77  IDDC-WS                     PIC X(2)    VALUE SPACE.                 
000095                                                                          
000096                                                                          
000097 77  SECURITY-SW                 PIC X       VALUE 'N'.                   
000098     88  PASSED-SECURITY-CHECK               VALUE 'J'.                   
000099     88  BLOCKED-SECURITY-CHECK              VALUE 'N'.                   
000100                                                                          
000101 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
000102     88  NYCKLAR-OK                          VALUE 'J'.                   
000103     88  NYCKLAR-FEL                         VALUE 'N'.                   
000104                                                                          
000105 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
000106     88  EGEN-MID                            VALUE '2407'.                
000107     88  GODK-MID                            VALUE '2401' '2402'          
000108                                                   '2403' '2404'          
000109                                                   '2405' '2406'          
000110                                                   '2407' '2408'          
000111                                                   '2409'.                
000112     88  HELP-MID                            VALUE '0551'.                
000113     EJECT                                                                
000114*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
000115 01  GENERELLA-SUBPROGRAM.                                                
000116     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
000117     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
000118     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000119     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000120     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
000121     EJECT                                                                
000122*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
000123*01  -COPY WDATAREA                                                       
000124     EJECT                                                                
000125*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
000126*01 -COPY WMEDAREA                                                        
000127     EJECT                                                                
000128 01  FILLER                      PIC X(16)   VALUE 'DISTRIKT-TAB'.        
000129     SKIP3                                                                
000130 01  MESSAGE-CODES.                                                       
000131     03  ERR-PART-MISSING        PIC X(3)    VALUE '017'.                 
000132     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
000133     03  ERR-NOT-AUTHORIZED      PIC X(3)    VALUE '405'.                 
000134     EJECT                                                                
000135                                                                          
000136 01  MEDDELANDE.                                                          
000137     03  MED-1                  PIC X(30)                                 
000138         VALUE 'TYPE : T,F,S OR R             '.                          
000139     EJECT                                                                
000140*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
000141*                                                                         
000142 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
000143     SKIP3                                                                
000144*01 -COPY WMSGINIT                                                        
000145     SKIP3                                                                
000146*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
000147*                                                                         
000148 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
000149     SKIP3                                                                
000150*01  MID -COPY W2I40701                                                   
000151     EJECT                                                                
000152 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
000153     SKIP3                                                                
000154*01  -COPY WMSGAREA                                                       
000155     EJECT                                                                
000156     03  MOD REDEFINES MSG-AREA.                                          
000157*      05  -COPY W2O40701                                                 
000158     EJECT                                                                
000159 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
000160     SKIP3                                                                
000161*01  -COPY WMFSAREA                                                       
000162     EJECT                                                                
000163*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
000164*                                                                         
000165     EJECT                                                                
000166 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000167     SKIP3                                                                
000168 01  NYCKLAR-TILL-DLI.                                                    
000169     03  W-IDARTNR-X.                                                     
000170         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
000171     03  W-IDDC-X.                                                        
000172         05  W-IDDC              PIC X(02)   VALUE SPACE.                 
000173     03  W-IDDC-K7-X.                                                     
000174         05  W-IDDC-K7           PIC X(02)   VALUE SPACE.                 
000175     03  W-IDUSER-X.                                                      
000176         05  W-IDUSER            PIC X(8)    VALUE SPACE.                 
000177     03  W-IDSKYLT-X.                                                     
000178         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
000179     SKIP2                                                                
000180*    --- STATUS-KOD FRÅN IMS                                              
000181 01  STATUS-WS                   PIC XX.                                  
000182     88  SEGMENT-FINNS                       VALUE '  '.                  
000183     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
000184     88  SEGMENT-SAKNAS                      VALUE 'GE'                   
000185                                                   'GB'.                  
000186     SKIP2                                                                
000187 01  GODK-STATUSKODER.                                                    
000188     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000189     SKIP3                                                                
000190 01  SSA1                        PIC X(64).                               
000191 01  SSA2                        PIC X(64).                               
000192     EJECT                                                                
000193*    --- IMS FUNKTIONSKODER                                               
000194*01  -COPY W0003                                                          
000195     EJECT                                                                
000196*    ---  DLI INPUT-OUTPUT AREA                                           
000197 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
000198 01   DLI-IO-AREA-B601.                                                   
000199*     03  -COPY WDB601                                                    
000200                                                                          
000201 01  FILLER               PIC X(16)   VALUE 'WDK601 AREA'.                
000202 01   DLI-IO-AREA-K601.                                                   
000203*     03  -COPY WDK601                                                    
000204                                                                          
000205 01  FILLER               PIC X(16)   VALUE 'WDL711 AREA'.                
000206 01   DLI-IO-AREA-L711.                                                   
000207*     03  -COPY WDL711                                                    
000208                                                                          
000209 01  FILLER               PIC X(16)   VALUE 'WDL411 AREA'.                
000210 01   DLI-IO-AREA-L411.                                                   
000211*     03  -COPY WDL411                                                    
000212                                                                          
000213 01  FILLER               PIC X(16)   VALUE 'WDD301 AREA'.                
000214 01   DLI-IO-AREA-D301.                                                   
000215*     03  -COPY WDD301                                                    
000216                                                                          
000217 01  FILLER               PIC X(16)   VALUE 'WDD311 AREA'.                
000218 01   DLI-IO-AREA-D311.                                                   
000219*     03  -COPY WDD311                                                    
000220                                                                          
000221 01  FILLER               PIC X(16)   VALUE 'WDK701 AREA'.                
000222 01   DLI-IO-AREA-K701.                                                   
000223*     03  -COPY WDK701                                                    
000224                                                                          
000225 01  FILLER               PIC X(16)   VALUE 'WDK711 AREA'.                
000226 01   DLI-IO-AREA-K711.                                                   
000227*     03  -COPY WDK711                                                    
000228                                                                          
000229     EJECT                                                                
000230 LINKAGE SECTION.                                                         
000231                                                                          
000232*01  -COPY W0009   -PRE MSG-                                              
000233     EJECT                                                                
000234*01  -COPY W0008  -PRE  USEA-                                             
000235     05  FILLER                  PIC X.                                   
000236     EJECT                                                                
000237*01  -COPY W0008  -PRE  WDL7-                                             
000238     05  FILLER                  PIC X.                                   
000239     EJECT                                                                
000240*01  -COPY W0008  -PRE  WDL4-                                             
000241     05  FILLER                  PIC X.                                   
000242     EJECT                                                                
000243*01  -COPY W0008  -PRE  WDD3-                                             
000244     05  FILLER                  PIC X.                                   
000245     EJECT                                                                
000246*01  -COPY W0008  -PRE  WDB6-                                             
000247     05  FILLER                  PIC X.                                   
000248     EJECT                                                                
000249*01  -COPY W0008  -PRE  WDK6-                                             
000250     05  FILLER                  PIC X.                                   
000251     EJECT                                                                
000252*01  -COPY W0008  -PRE  WDK7-                                             
000253     05  FILLER                  PIC X.                                   
000254     EJECT                                                                
000255 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB WDL7-PCB WDL4-PCB             
000256     WDD3-PCB WDB6-PCB WDK7-PCB WDK6-PCB.                                 
000257 MAIN SECTION.                                                            
000258     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB WDL7-PCB WDL4-PCB             
000259     WDD3-PCB WDB6-PCB WDK7-PCB WDK6-PCB.                                 
000260                                                                          
000261     PERFORM IMS-GET-MSG                                                  
000262     IF SEGMENT-FINNS                                                     
000263       PERFORM A-INIT                                                     
000264       PERFORM B-KOLLA-NYCKLAR                                            
000265       IF NYCKLAR-OK                                                      
000266         PERFORM SEC-URITY                                                
000267         IF PASSED-SECURITY-CHECK                                         
000268           PERFORM F-LAES-VISA-INFO                                       
000269*---                                                                      
000270*---     SAVE MFG SUPPLIER USING INIT-IO-AREA                             
000271           IF IDLEVNR-WS > SPACES                                         
000272              MOVE ALL '+'           TO MSGI-WMSGINIT                     
000273              MOVE '001'             TO MSGI-KDCALL                       
000274              MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                 
000275              MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                       
000276              MOVE '2407'            TO MSGI-IDTRANS                      
000277              MOVE IDLEVNR-WS        TO MSGI-IDLEVNR                      
000278                                                                          
000279              CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                  
000280           END-IF                                                         
000281         END-IF                                                           
000282       END-IF                                                             
000283       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O40701 + 4                      
000284       PERFORM IMS-INSERT-MSG                                             
000285     END-IF                                                               
000286                                                                          
000287     MOVE ZERO TO RETURN-CODE                                             
000288     GOBACK                                                               
000289     .                                                                    
000290     EJECT                                                                
000291 A-INIT SECTION.                                                          
000292                                                                          
000293     IF MSG-DUBBLA-TRANSKODER                                             
000294       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I40701                 
000295       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
000296       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
000297     ELSE                                                                 
000298       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I40701                  
000299       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
000300       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
000301     END-IF                                                               
000302                                                                          
000303     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
000304     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
000305     MOVE MFS-IDTRANS TO W-IDTRANS                                        
000306                                                                          
000307     MOVE LOW-VALUE TO MSG-AREA                                           
000308     MOVE 'W2O407N1' TO MFS-IDMOD                                         
000309     MOVE '2407' TO MOD-IDTRANS                                           
000310     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
000311                                                                          
000312     IF EGEN-MID OR HELP-MID                                              
000313       CONTINUE                                                           
000314     ELSE                                                                 
000315       MOVE SPACE TO MFS-KDTRTYP                                          
000316       MOVE '7' TO MFS-IDPFK                                              
000317     END-IF                                                               
000318                                                                          
000319     ACCEPT DAGENS-DATUM FROM DATE                                        
000320                                                                          
000321     MOVE 'AAMMDD'           TO DAT-KDDATFORM                             
000322     MOVE DAGENS-DATUM       TO DAT-I-TIDATUM                             
000323                                                                          
000324     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
000325                     DAT-O-TIDATUM DAT-KDSVAR                             
000326                                                                          
000327     IF DAT-KDSVAR-OK                                                     
000328****             HÄMTA SEKELSIFFROR                                       
000329                                                                          
000330       MOVE DAT-TISEKEL      TO DAGENS-AAR(1:2)                           
000331       MOVE DAT-TIAARP       TO DAGENS-PER                                
000332       MOVE DAGENS-PER(3:2)  TO WS-PER                                    
000333                                                                          
000334     ELSE                                                                 
000335         STRING ' FEL FRÅN DATUMRUTIN WDATKONV ' STATUS-WS                
000336         DELIMITED BY SIZE INTO FELTEXT                                   
000337         CALL FELLOG                                                      
000338     END-IF                                                               
000339                                                                          
000340     MOVE DAGENS-DATUM(1:2)   TO DAGENS-AAR(3:2)                          
000341                                                                          
000342     MOVE 'GB ' TO MED-IDSKYLT                                            
000343     .                                                                    
000344     EJECT                                                                
000345 B-KOLLA-NYCKLAR SECTION.                                                 
000346                                                                          
000347     MOVE ALL '+'           TO MSGI-WMSGINIT                              
000348     MOVE '001'             TO MSGI-KDCALL                                
000349     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
000350     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
000351     MOVE '2407'            TO MSGI-IDTRANS                               
000352     IF EGEN-MID                                                          
000353       MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                               
000354       MOVE MID-IDDC-IN     TO MSGI-IDDC-KEY                              
000355     ELSE                                                                 
000356       IF  MID-IDARTNR-IN NUMERIC                                         
000357       AND MID-IDARTNR-IN > ZERO                                          
000358         MOVE MID-IDARTNR-IN                                              
000359                            TO MSGI-IDARTNR                               
000360       END-IF                                                             
000361     END-IF                                                               
000362     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
000363                                                                          
000364     MOVE JA TO NYCKLAR-SW                                                
000365                                                                          
000366*    -- KONTROLL AV IDARTNR                                               
000367     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
000368                                                                          
000369     IF MID-IDARTNR-IN NOT = ALL '+'                                      
000370       MOVE '7'         TO MFS-IDPFK                                      
000371       MOVE SPACE       TO MFS-KDTRTYP                                    
000372     END-IF                                                               
000373     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
000374     IF MSGI-IDARTNR NUMERIC                                              
000375       MOVE MSGI-IDARTNR TO WS-IDARTNR                                    
000376     ELSE                                                                 
000377       MOVE NEJ TO NYCKLAR-SW                                             
000378     END-IF                                                               
000379                                                                          
000380*    -- KONTROLL AV IDDC                                                  
000381     MOVE MFS-RENSA-FAELT    TO MOD-IDDC-IN                               
000382                                                                          
000383     IF MID-IDDC-IN NOT = ALL '+'                                         
000384       MOVE '7'              TO MFS-IDPFK                                 
000385       MOVE SPACE            TO MFS-KDTRTYP                               
000386     END-IF                                                               
000387                                                                          
000388     MOVE MSGI-IDDC-KEY    TO IDDC-WS                                     
000389                              W-IDDC                                      
000390                              W-IDDC-K7                                   
000391                                                                          
000392     PERFORM IMS-GU-WDB601                                                
000393                                                                          
000394     IF DCS-NDC-CN                                                        
000395     OR (DCS-NDC-NA AND DCS-USA)                                          
000396         CONTINUE                                                         
000397     ELSE                                                                 
000398       MOVE NEJ TO NYCKLAR-SW                                             
000399     END-IF                                                               
000400                                                                          
000401     MOVE IDDC-WS          TO MOD-IDDC-UT                                 
000402                                                                          
000403     MOVE WS-IDARTNR       TO MOD-IDARTNR-UT                              
000404                              W-IDARTNR                                   
000405     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
000406     PERFORM IMS-GU-WDK601                                                
000407     IF SEGMENT-FINNS                                                     
000408        MOVE '-'           TO MOD-HYPHEN-UT                               
000409        MOVE ART-REKSIFFR  TO MOD-REKSIFFR-UT                             
000410     ELSE                                                                 
000411        MOVE SPACE         TO MOD-HYPHEN-UT                               
000412                              MOD-REKSIFFR-UT                             
000413     END-IF                                                               
000414                                                                          
000415     IF EGEN-MID                                                          
000416       IF MID-TYPE-IN NOT = 'T' AND 'F' AND 'S'                           
000417                           AND 'R' AND ALL '+'                            
000418         MOVE NEJ          TO NYCKLAR-SW                                  
000419         MOVE MED-1        TO MOD-TEMFSINF                                
000420         MOVE MID-TYPE-IN  TO MOD-TYPE-UT                                 
000421       ELSE                                                               
000422         IF MID-TYPE-IN = ALL '+'                                         
000423            EVALUATE MID-TYPE-UT                                          
000424              WHEN 'TOTAL      '                                          
000425                MOVE 'T'   TO WS-TYPE                                     
000426              WHEN 'FORECAST   '                                          
000427                MOVE 'F'   TO WS-TYPE                                     
000428              WHEN 'SUM OF DC''S'                                         
000429                MOVE 'S'   TO WS-TYPE                                     
000430              WHEN 'REFILL     '                                          
000431                MOVE 'R'   TO WS-TYPE                                     
000432              WHEN OTHER                                                  
000433                MOVE 'T'   TO WS-TYPE                                     
000434            END-EVALUATE                                                  
000435         ELSE                                                             
000436            MOVE MID-TYPE-IN TO WS-TYPE                                   
000437         END-IF                                                           
000438       END-IF                                                             
000439     ELSE                                                                 
000440         MOVE 'T'          TO WS-TYPE                                     
000441     END-IF                                                               
000442                                                                          
000443     IF NYCKLAR-FEL                                                       
000444       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
000445       CALL WMEDKONV USING MED-WMEDAREA                                   
000446       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
000447       PERFORM MFS-RENSA-FAELT-UT                                         
000448     END-IF                                                               
000449     .                                                                    
000450     EJECT                                                                
000451 F-LAES-VISA-INFO SECTION.                                                
000452                                                                          
000453     PERFORM FA-HAEMTA-BENAEMNING                                         
000454                                                                          
000455     PERFORM FB-HAMTA-VV-I-PER                                            
000456                                                                          
000457     MOVE DAGENS-AAR         TO MOD-AARTAL (1)                            
000458                                WS-AARTAL                                 
000459     SUBTRACT 1 FROM WS-AARTAL                                            
000460     MOVE WS-AARTAL          TO MOD-AARTAL (2)                            
000461     SUBTRACT 1 FROM WS-AARTAL                                            
000462     MOVE WS-AARTAL          TO MOD-AARTAL (3)                            
000463     SUBTRACT 1 FROM WS-AARTAL                                            
000464     MOVE WS-AARTAL          TO MOD-AARTAL (4)                            
000465     SUBTRACT 1 FROM WS-AARTAL                                            
000466     MOVE WS-AARTAL          TO MOD-AARTAL (5)                            
000467     SUBTRACT 1 FROM WS-AARTAL                                            
000468     MOVE WS-AARTAL          TO MOD-AARTAL (6)                            
000469                                                                          
000470     PERFORM FC-HAEMTA-HISTORIK                                           
000471                                                                          
000472     MOVE +1                 TO IX-AR                                     
000473                                IX-PER                                    
000474     PERFORM UNTIL IX-AR > +6                                             
000475       PERFORM UNTIL IX-PER > +12                                         
000476         MOVE WS-KVOI-NDC (IX-AR, IX-PER)                                 
000477                             TO MOD-KVOI (IX-AR, IX-PER)                  
000478         ADD +1              TO IX-PER                                    
000479       END-PERFORM                                                        
000480                                                                          
000481       MOVE WS-KVOI-TOT-NDC (IX-AR)                                       
000482                             TO MOD-KVOI-TOT (IX-AR)                      
000483       ADD +1                TO IX-AR                                     
000484       MOVE +1               TO IX-PER                                    
000485     END-PERFORM                                                          
000486     .                                                                    
000487     EJECT                                                                
000488 FA-HAEMTA-BENAEMNING SECTION.                                            
000489                                                                          
000490     PERFORM IMS-GU-WDD301-BSEQ                                           
000491     IF SEGMENT-FINNS                                                     
000492       MOVE 'GB'                TO W-IDSKYLT                              
000493       PERFORM IMS-GNP-WDD311                                             
000494       IF SEGMENT-FINNS                                                   
000495         MOVE TEXT-BEART        TO MOD-BEART-ENG                          
000496       ELSE                                                               
000497         MOVE MFS-RENSA-FAELT   TO MOD-BEART-ENG                          
000498       END-IF                                                             
000499     END-IF                                                               
000500     .                                                                    
000501     EJECT                                                                
000502 FB-HAMTA-VV-I-PER SECTION.                                               
000503                                                                          
000504*    --- FYLL I VECKONR FÖR PERIODERNA                                    
000505     MOVE DAGENS-DATUM(1:2)  TO WS-AAPP(1:2)                              
000506     MOVE 01                 TO WS-AAPP(3:2)                              
000507     MOVE WS-AAPP            TO DAT-I-TIDATUM                             
000508     MOVE 'AARP'             TO DAT-KDDATFORM                             
000509     MOVE +1                 TO WS-PP                                     
000510                                                                          
000511     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
000512                     DAT-O-TIDATUM DAT-KDSVAR                             
000513                                                                          
000514     IF DAT-KDSVAR-OK                                                     
000515                                                                          
000516       MOVE DAT-KVVIPER      TO WS-KVVIPER(WS-PP)                         
000517       MOVE 1                TO WS-FORSTA-V(WS-PP)                        
000518                                                                          
000519     ELSE                                                                 
000520         STRING ' FEL FRÅN DATUMRUTIN WDATKONV ' STATUS-WS                
000521         DELIMITED BY SIZE INTO FELTEXT                                   
000522         CALL FELLOG                                                      
000523     END-IF                                                               
000524                                                                          
000525     PERFORM UNTIL WS-PP      >  WS-PER                                   
000526       ADD +1                 TO WS-PP                                    
000527       IF WS-PP = +13                                                     
000528         MOVE 53             TO WS-SISTA-V(12)                            
000529       ELSE                                                               
000530                                                                          
000531         MOVE WS-AAPP         TO DAT-I-TIDATUM                            
000532         CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                  
000533                             DAT-O-TIDATUM DAT-KDSVAR                     
000534         IF DAT-KDSVAR-OK                                                 
000535             COMPUTE WS-SISTA-V(WS-PP - 1) = DAT-TIVV - 1                 
000536             IF WS-PP > +1                                                
000537               MOVE DAT-TIVV  TO WS-FORSTA-V(WS-PP)                       
000538               MOVE DAT-KVVIPER TO WS-KVVIPER(WS-PP)                      
000539             END-IF                                                       
000540         ELSE                                                             
000541           MOVE 'FELAKTIGT DATUM - DATKONV3' TO FELTEXT                   
000542           CALL FELLOG                                                    
000543         END-IF                                                           
000544       END-IF                                                             
000545     END-PERFORM                                                          
000546     .                                                                    
000547     EJECT                                                                
000548 FC-HAEMTA-HISTORIK SECTION.                                              
000549                                                                          
000550     MOVE +1                   TO IX-PER                                  
000551     PERFORM UNTIL IX-PER      > 12                                       
000552       MOVE ZERO               TO WS-KVOI(IX-PER)                         
000553       ADD +1                  TO IX-PER                                  
000554     END-PERFORM                                                          
000555                                                                          
000556     MOVE +1                   TO IX-PER                                  
000557                                                                          
000558     EVALUATE WS-TYPE                                                     
000559       WHEN 'T'                                                           
000560         MOVE 'TOTAL'          TO MOD-TYPE-UT                             
000561         PERFORM FCA-PROCESS-TOTAL                                        
000562       WHEN 'F'                                                           
000563         MOVE 'FORECAST'       TO MOD-TYPE-UT                             
000564         PERFORM FCB-PROCESS-FORECAST                                     
000565       WHEN 'S'                                                           
000566         MOVE 'SUM OF DC''S'   TO MOD-TYPE-UT                             
000567         PERFORM FCC-PROCESS-SUM-DCS                                      
000568       WHEN 'R'                                                           
000569         MOVE 'REFILL'         TO MOD-TYPE-UT                             
000570         PERFORM FCD-PROCESS-REFILL                                       
000571     END-EVALUATE                                                         
000572     .                                                                    
000573     EJECT                                                                
000574 FCA-PROCESS-TOTAL    SECTION.                                            
000575                                                                          
000576     PERFORM IMS-GU-WDL711                                                
000577     IF SEGMENT-SAKNAS                                                    
000578       PERFORM MFS-RENSA-FAELT-UT                                         
000579     ELSE                                                                 
000580       PERFORM IMS-GU-WDL411                                              
000581       IF SEGMENT-SAKNAS                                                  
000582******** IF PART/DC FROM WDL7 IS MISSING IN WDL4 THEN L411-AREA           
000583******** MUST BE ZEROED BCZ SOME ITEMS ARE USED IN COMPUTES BELOW         
000584         INITIALIZE OIHD-WDL411                                           
000585       END-IF                                                             
000586                                                                          
000587       MOVE +1                 TO IX-PER                                  
000588                                                                          
000589       PERFORM UNTIL IX-PER    =  WS-PER                                  
000590         MOVE WS-FORSTA-V (IX-PER)                                        
000591                               TO WS-VV                                   
000592         PERFORM UNTIL WS-VV   >  WS-SISTA-V (IX-PER)                     
000593           ADD DC-KVOI-RULL (WS-VV)                                       
000594                               TO WS-KVOI (IX-PER)                        
000595           ADD DC-KVOI-REF-RULL (WS-VV)                                   
000596                               TO WS-KVOI (IX-PER)                        
000597           ADD +1              TO WS-VV                                   
000598         END-PERFORM                                                      
000599         ADD +1                TO IX-PER                                  
000600       END-PERFORM                                                        
000601                                                                          
000602       MOVE +1                 TO IX-AR                                   
000603                                  IX-PER                                  
000604       MOVE ZERO               TO WS-TOTAL-KVOI                           
000605                                                                          
000606       PERFORM UNTIL IX-PER = WS-PER                                      
000607*    VISA HISTORIK TOM FÖREGÅENDE MÅNAD                                   
000608                                                                          
000609         IF WS-KVOI (IX-PER) > ZERO                                       
000610           COMPUTE WS-KVOI-RED     ROUNDED =                              
000611                 WS-KVOI (IX-PER)                                         
000612                              / WS-KVVIPER (IX-PER) * +4.33               
000613         ELSE                                                             
000614           MOVE ZERO           TO WS-KVOI-RED                             
000615         END-IF                                                           
000616                                                                          
000617         ADD WS-KVOI-RED       TO WS-KVOI-NDC (IX-AR, IX-PER)             
000618                                  WS-TOTAL-KVOI                           
000619         ADD +1                TO IX-PER                                  
000620       END-PERFORM                                                        
000621                                                                          
000622       ADD WS-TOTAL-KVOI       TO WS-KVOI-TOT-NDC (IX-AR)                 
000623                                                                          
000624       MOVE 2                  TO IX-TILL-AR                              
000625       MOVE 1                  TO IX-FRAN-AR                              
000626                                  IX-PER                                  
000627       PERFORM UNTIL IX-TILL-AR > 6                                       
000628*    LÄGG UT HISTORIK FÖR FÖREGÅENDE ÅR OCH TIDIGARE                      
000629         MOVE ZERO             TO WS-TOTAL-KVOI                           
000630         PERFORM UNTIL IX-PER > 12                                        
000631                                                                          
000632           IF OIHD-KVOI (IX-FRAN-AR, IX-PER) > ZERO                       
000633             COMPUTE WS-KVOI-RED ROUNDED =                                
000634                OIHD-KVOI (IX-FRAN-AR, IX-PER)                            
000635                   / OIHD-KVVIPER (IX-FRAN-AR, IX-PER) * +4.33            
000636           ELSE                                                           
000637             MOVE ZERO         TO WS-KVOI-RED                             
000638           END-IF                                                         
000639                                                                          
000640           IF OIHD-KVOI-REFILL (IX-FRAN-AR, IX-PER) > ZERO                
000641             COMPUTE WS-KVOI-RED ROUNDED =                                
000642                WS-KVOI-RED +                                             
000643                (OIHD-KVOI-REFILL (IX-FRAN-AR, IX-PER)                    
000644                   / OIHD-KVVIPER (IX-FRAN-AR, IX-PER) * +4.33)           
000645           END-IF                                                         
000646                                                                          
000647           ADD WS-KVOI-RED     TO WS-KVOI-NDC (IX-TILL-AR, IX-PER)        
000648                                  WS-TOTAL-KVOI                           
000649           ADD +1              TO IX-PER                                  
000650         END-PERFORM                                                      
000651                                                                          
000652         ADD WS-TOTAL-KVOI     TO WS-KVOI-TOT-NDC (IX-TILL-AR)            
000653                                                                          
000654         ADD +1                TO IX-FRAN-AR                              
000655                                  IX-TILL-AR                              
000656         MOVE +1               TO IX-PER                                  
000657       END-PERFORM                                                        
000658     END-IF                                                               
000659                                                                          
000660     MOVE +1                   TO IX-PER                                  
000661     PERFORM UNTIL IX-PER      > 12                                       
000662       MOVE ZERO               TO WS-KVOI(IX-PER)                         
000663       ADD +1                  TO IX-PER                                  
000664     END-PERFORM                                                          
000665                                                                          
000666     MOVE +1                   TO IX-PER                                  
000667                                                                          
000668     PERFORM FCC-PROCESS-SUM-DCS                                          
000669     .                                                                    
000670     EJECT                                                                
000671 FCB-PROCESS-FORECAST SECTION.                                            
000672                                                                          
000673     PERFORM IMS-GU-WDL711                                                
000674     IF SEGMENT-SAKNAS                                                    
000675       PERFORM MFS-RENSA-FAELT-UT                                         
000676     ELSE                                                                 
000677       PERFORM IMS-GU-WDL411                                              
000678       IF SEGMENT-SAKNAS                                                  
000679******** IF PART/DC FROM WDL7 IS MISSING IN WDL4 THEN L411-AREA           
000680******** MUST BE ZEROED BCZ SOME ITEMS ARE USED IN COMPUTES BELOW         
000681         INITIALIZE OIHD-WDL411                                           
000682       END-IF                                                             
000683                                                                          
000684       MOVE +1                 TO IX-PER                                  
000685                                                                          
000686       PERFORM UNTIL IX-PER    =  WS-PER                                  
000687         MOVE WS-FORSTA-V (IX-PER)                                        
000688                               TO WS-VV                                   
000689         PERFORM UNTIL WS-VV   >  WS-SISTA-V (IX-PER)                     
000690           ADD DC-KVOI-RULL (WS-VV)                                       
000691                               TO WS-KVOI (IX-PER)                        
000692           ADD +1              TO WS-VV                                   
000693         END-PERFORM                                                      
000694         ADD +1                TO IX-PER                                  
000695       END-PERFORM                                                        
000696                                                                          
000697       MOVE +1                 TO IX-AR                                   
000698                                  IX-PER                                  
000699       MOVE ZERO               TO WS-TOTAL-KVOI                           
000700                                                                          
000701       PERFORM UNTIL IX-PER = WS-PER                                      
000702*    VISA HISTORIK TOM FÖREGÅENDE MÅNAD                                   
000703                                                                          
000704         IF WS-KVOI (IX-PER) > ZERO                                       
000705           COMPUTE WS-KVOI-RED     ROUNDED =                              
000706                 WS-KVOI (IX-PER)                                         
000707                              / WS-KVVIPER (IX-PER) * +4.33               
000708         ELSE                                                             
000709           MOVE ZERO           TO WS-KVOI-RED                             
000710         END-IF                                                           
000711                                                                          
000712         ADD WS-KVOI-RED       TO WS-KVOI-NDC (IX-AR, IX-PER)             
000713                                  WS-TOTAL-KVOI                           
000714         ADD +1                TO IX-PER                                  
000715       END-PERFORM                                                        
000716                                                                          
000717       ADD WS-TOTAL-KVOI       TO WS-KVOI-TOT-NDC (IX-AR)                 
000718                                                                          
000719       MOVE 2                  TO IX-TILL-AR                              
000720       MOVE 1                  TO IX-FRAN-AR                              
000721                                  IX-PER                                  
000722       PERFORM UNTIL IX-TILL-AR > 6                                       
000723*    LÄGG UT HISTORIK FÖR FÖREGÅENDE ÅR OCH TIDIGARE                      
000724         MOVE ZERO             TO WS-TOTAL-KVOI                           
000725         PERFORM UNTIL IX-PER > 12                                        
000726                                                                          
000727           IF OIHD-KVOI (IX-FRAN-AR, IX-PER) > ZERO                       
000728             COMPUTE WS-KVOI-RED ROUNDED =                                
000729                OIHD-KVOI (IX-FRAN-AR, IX-PER)                            
000730                   / OIHD-KVVIPER (IX-FRAN-AR, IX-PER) * +4.33            
000731           ELSE                                                           
000732             MOVE ZERO         TO WS-KVOI-RED                             
000733           END-IF                                                         
000734                                                                          
000735           ADD WS-KVOI-RED     TO WS-KVOI-NDC (IX-TILL-AR, IX-PER)        
000736                                  WS-TOTAL-KVOI                           
000737           ADD +1              TO IX-PER                                  
000738         END-PERFORM                                                      
000739                                                                          
000740         ADD WS-TOTAL-KVOI     TO WS-KVOI-TOT-NDC (IX-TILL-AR)            
000741                                                                          
000742         ADD +1                TO IX-FRAN-AR                              
000743                                  IX-TILL-AR                              
000744         MOVE +1               TO IX-PER                                  
000745       END-PERFORM                                                        
000746     END-IF                                                               
000747     .                                                                    
000748     EJECT                                                                
000749 FCC-PROCESS-SUM-DCS  SECTION.                                            
000750                                                                          
000751     PERFORM IMS-GN-WDK711-FIRST                                          
000752     PERFORM UNTIL SEGMENT-SAKNAS                                         
000753       IF SLAG-IDDC-REF = IDDC-WS                                         
000754         MOVE SLAG-IDDC        TO W-IDDC                                  
000755         PERFORM IMS-GU-WDL711                                            
000756         IF SEGMENT-FINNS                                                 
000757           PERFORM IMS-GU-WDL411                                          
000758           IF SEGMENT-SAKNAS                                              
000759******** IF PART/DC FROM WDL7 IS MISSING IN WDL4 THEN L411-AREA           
000760******** MUST BE ZEROED BCZ SOME ITEMS ARE USED IN COMPUTES BELOW         
000761             INITIALIZE OIHD-WDL411                                       
000762           END-IF                                                         
000763           PERFORM FCCA-PROCESS-DC                                        
000764         END-IF                                                           
000765       END-IF                                                             
000766       PERFORM IMS-GN-WDK711                                              
000767     END-PERFORM                                                          
000768     .                                                                    
000769     EJECT                                                                
000770 FCCA-PROCESS-DC      SECTION.                                            
000771                                                                          
000772     MOVE +1                   TO IX-PER                                  
000773     PERFORM UNTIL IX-PER      > 12                                       
000774       MOVE ZERO               TO WS-KVOI(IX-PER)                         
000775       ADD +1                  TO IX-PER                                  
000776     END-PERFORM                                                          
000777                                                                          
000778     MOVE +1                   TO IX-PER                                  
000779                                                                          
000780     PERFORM UNTIL IX-PER      =  WS-PER                                  
000781       MOVE WS-FORSTA-V (IX-PER)                                          
000782                               TO WS-VV                                   
000783       PERFORM UNTIL WS-VV     >  WS-SISTA-V (IX-PER)                     
000784         ADD DC-KVOI-RULL (WS-VV)                                         
000785                               TO WS-KVOI (IX-PER)                        
000786         ADD DC-KVOI-REF-RULL (WS-VV)                                     
000787                               TO WS-KVOI (IX-PER)                        
000788         ADD +1                TO WS-VV                                   
000789       END-PERFORM                                                        
000790       ADD +1                  TO IX-PER                                  
000791     END-PERFORM                                                          
000792                                                                          
000793     MOVE +1                   TO IX-AR                                   
000794                                  IX-PER                                  
000795     MOVE ZERO                 TO WS-TOTAL-KVOI                           
000796                                                                          
000797     PERFORM UNTIL IX-PER = WS-PER                                        
000798*  VISA HISTORIK TOM FÖREGÅENDE MÅNAD                                     
000799                                                                          
000800       IF WS-KVOI (IX-PER) > ZERO                                         
000801         COMPUTE WS-KVOI-RED       ROUNDED =                              
000802               WS-KVOI (IX-PER)                                           
000803                            / WS-KVVIPER (IX-PER) * +4.33                 
000804       ELSE                                                               
000805         MOVE ZERO             TO WS-KVOI-RED                             
000806       END-IF                                                             
000807                                                                          
000808       ADD WS-KVOI-RED         TO WS-KVOI-NDC (IX-AR, IX-PER)             
000809                                  WS-TOTAL-KVOI                           
000810       ADD +1                  TO IX-PER                                  
000811     END-PERFORM                                                          
000812                                                                          
000813     ADD WS-TOTAL-KVOI         TO WS-KVOI-TOT-NDC (IX-AR)                 
000814                                                                          
000815     MOVE 2                    TO IX-TILL-AR                              
000816     MOVE 1                    TO IX-FRAN-AR                              
000817                                IX-PER                                    
000818     PERFORM UNTIL IX-TILL-AR > 6                                         
000819*  LÄGG UT HISTORIK FÖR FÖREGÅENDE ÅR OCH TIDIGARE                        
000820       MOVE ZERO               TO WS-TOTAL-KVOI                           
000821       PERFORM UNTIL IX-PER > 12                                          
000822                                                                          
000823         IF (OIHD-KVOI (IX-FRAN-AR, IX-PER) +                             
000824             OIHD-KVOI-REFILL (IX-FRAN-AR, IX-PER)) > ZERO                
000825           COMPUTE WS-KVOI-RED ROUNDED =                                  
000826              (OIHD-KVOI (IX-FRAN-AR, IX-PER) +                           
000827               OIHD-KVOI-REFILL (IX-FRAN-AR, IX-PER))                     
000828                 / OIHD-KVVIPER (IX-FRAN-AR, IX-PER) * +4.33              
000829         ELSE                                                             
000830           MOVE ZERO           TO WS-KVOI-RED                             
000831         END-IF                                                           
000832                                                                          
000833         ADD WS-KVOI-RED       TO WS-KVOI-NDC (IX-TILL-AR, IX-PER)        
000834                                  WS-TOTAL-KVOI                           
000835         ADD +1                TO IX-PER                                  
000836       END-PERFORM                                                        
000837                                                                          
000838       ADD WS-TOTAL-KVOI       TO WS-KVOI-TOT-NDC (IX-TILL-AR)            
000839                                                                          
000840       ADD +1                  TO IX-FRAN-AR                              
000841                                  IX-TILL-AR                              
000842       MOVE +1                 TO IX-PER                                  
000843     END-PERFORM                                                          
000844     .                                                                    
000845     EJECT                                                                
000846 FCD-PROCESS-REFILL   SECTION.                                            
000847                                                                          
000848     PERFORM IMS-GU-WDL711                                                
000849     IF SEGMENT-SAKNAS                                                    
000850       PERFORM MFS-RENSA-FAELT-UT                                         
000851     ELSE                                                                 
000852       PERFORM IMS-GU-WDL411                                              
000853       IF SEGMENT-SAKNAS                                                  
000854******** IF PART/DC FROM WDL7 IS MISSING IN WDL4 THEN L411-AREA           
000855******** MUST BE ZEROED BCZ SOME ITEMS ARE USED IN COMPUTES BELOW         
000856         INITIALIZE OIHD-WDL411                                           
000857       END-IF                                                             
000858                                                                          
000859       MOVE +1                 TO IX-PER                                  
000860                                                                          
000861       PERFORM UNTIL IX-PER    =  WS-PER                                  
000862         MOVE WS-FORSTA-V(IX-PER)                                         
000863                               TO WS-VV                                   
000864         PERFORM UNTIL WS-VV   >  WS-SISTA-V(IX-PER)                      
000865           ADD DC-KVOI-REF-RULL(WS-VV)                                    
000866                               TO WS-KVOI(IX-PER)                         
000867           ADD +1              TO WS-VV                                   
000868         END-PERFORM                                                      
000869         ADD +1                TO IX-PER                                  
000870       END-PERFORM                                                        
000871                                                                          
000872       MOVE +1                 TO IX-AR                                   
000873                                  IX-PER                                  
000874       MOVE ZERO               TO WS-TOTAL-KVOI                           
000875                                                                          
000876       PERFORM UNTIL IX-PER = WS-PER                                      
000877*    VISA HISTORIK TOM FÖREGÅENDE MÅNAD                                   
000878                                                                          
000879         IF WS-KVOI (IX-PER) > ZERO                                       
000880           COMPUTE WS-KVOI-RED     ROUNDED =                              
000881                 WS-KVOI (IX-PER)                                         
000882                              / WS-KVVIPER (IX-PER) * +4.33               
000883         ELSE                                                             
000884           MOVE ZERO           TO WS-KVOI-RED                             
000885         END-IF                                                           
000886                                                                          
000887         ADD WS-KVOI-RED       TO WS-KVOI-NDC (IX-AR, IX-PER)             
000888                                  WS-TOTAL-KVOI                           
000889         ADD +1                TO IX-PER                                  
000890       END-PERFORM                                                        
000891                                                                          
000892       ADD WS-TOTAL-KVOI       TO WS-KVOI-TOT-NDC (IX-AR)                 
000893                                                                          
000894       MOVE 2                  TO IX-TILL-AR                              
000895       MOVE 1                  TO IX-FRAN-AR                              
000896                                  IX-PER                                  
000897       PERFORM UNTIL IX-TILL-AR > 6                                       
000898*    LÄGG UT HISTORIK FÖR FÖREGÅENDE ÅR OCH TIDIGARE                      
000899         MOVE ZERO             TO WS-TOTAL-KVOI                           
000900         PERFORM UNTIL IX-PER > 12                                        
000901                                                                          
000902           IF OIHD-KVOI-REFILL (IX-FRAN-AR, IX-PER) > ZERO                
000903             COMPUTE WS-KVOI-RED ROUNDED =                                
000904                OIHD-KVOI-REFILL (IX-FRAN-AR, IX-PER)                     
000905                   / OIHD-KVVIPER (IX-FRAN-AR, IX-PER) * +4.33            
000906           ELSE                                                           
000907             MOVE ZERO         TO WS-KVOI-RED                             
000908           END-IF                                                         
000909                                                                          
000910           ADD WS-KVOI-RED     TO WS-KVOI-NDC (IX-TILL-AR, IX-PER)        
000911                                  WS-TOTAL-KVOI                           
000912           ADD +1              TO IX-PER                                  
000913         END-PERFORM                                                      
000914                                                                          
000915         ADD WS-TOTAL-KVOI     TO WS-KVOI-TOT-NDC (IX-TILL-AR)            
000916                                                                          
000917         ADD +1                TO IX-FRAN-AR                              
000918                                  IX-TILL-AR                              
000919         MOVE +1               TO IX-PER                                  
000920       END-PERFORM                                                        
000921     END-IF                                                               
000922     .                                                                    
000923     EJECT                                                                
000924 SEC-URITY SECTION.                                                       
000925*    --- CHECK IF USER IS GRANTED TO SEE PART-INFO                        
000926     PERFORM IMS-GU-WDK711                                                
000927     IF SEGMENT-FINNS                                                     
000928       MOVE SLAG-IDLEVNR        TO WS-IDLEVNR-8                           
000929                                   IDLEVNR-WS                             
000930       IF MSGI-KDARBTYP-SEC-IDLEV = WS-IDLEVNR-8                          
000931       OR SPACE OR LOW-VALUE                                              
000932*         --- BEHÖRIG USER                                                
000933         SET PASSED-SECURITY-CHECK TO TRUE                                
000934       ELSE                                                               
000935*         --- OBEHÖRIG USER / USER NOT AUTHORIZED                         
000936         MOVE ERR-NOT-AUTHORIZED TO MED-IDMFSFEL                          
000937         CALL WMEDKONV USING MED-WMEDAREA                                 
000938         MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                
000939       END-IF                                                             
000940     ELSE                                                                 
000941       MOVE NEJ TO NYCKLAR-SW                                             
000942       MOVE ERR-PART-MISSING TO MED-IDMFSFEL                              
000943       CALL WMEDKONV USING MED-WMEDAREA                                   
000944       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
000945       PERFORM MFS-RENSA-FAELT-UT                                         
000946     END-IF                                                               
000947     .                                                                    
000948     EJECT                                                                
000949 MFS-RENSA-FAELT-UT SECTION.                                              
000950                                                                          
000951*    --- ALLA UTDATA-FÄLT                                                 
000952     MOVE MFS-RENSA-FAELT TO MOD-BEART-ENG                                
000953*                            MOD-TEMFSINF                                 
000954                                                                          
000955     MOVE +1                 TO IX-AR                                     
000956                                IX-PER                                    
000957     PERFORM UNTIL IX-AR > +6                                             
000958       MOVE MFS-RENSA-FAELT  TO MOD-AARTAL (IX-AR)                        
000959                                MOD-KVOI-TOT (IX-AR)                      
000960       PERFORM UNTIL IX-PER > +12                                         
000961         MOVE MFS-RENSA-FAELT                                             
000962                             TO MOD-KVOI (IX-AR, IX-PER)                  
000963         ADD +1              TO IX-PER                                    
000964       END-PERFORM                                                        
000965       ADD +1                TO IX-AR                                     
000966       MOVE +1               TO IX-PER                                    
000967     END-PERFORM                                                          
000968     .                                                                    
000969     EJECT                                                                
000970* --- IMS SEKTIONER ---                                                   
000971     SKIP3                                                                
000972 IMS-GET-MSG SECTION.                                                     
000973                                                                          
000974     MOVE '  QC' TO GODK-STATUSKODER                                      
000975     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
000976     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
000977     PERFORM IMS-STATUSKONTROLL                                           
000978     .                                                                    
000979     SKIP3                                                                
000980 IMS-INSERT-MSG SECTION.                                                  
000981                                                                          
000982     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
000983     MOVE SPACE TO GODK-STATUSKODER                                       
000984     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
000985     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
000986     PERFORM IMS-STATUSKONTROLL                                           
000987     .                                                                    
000988     EJECT                                                                
000989 IMS-GU-WDB601 SECTION.                                                   
000990     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
000991          DELIMITED BY SIZE INTO SSA1                                     
000992     MOVE '  GE' TO GODK-STATUSKODER                                      
000993     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
000994     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
000995     PERFORM IMS-STATUSKONTROLL                                           
000996     .                                                                    
000997     EJECT                                                                
000998 IMS-GU-WDL711 SECTION.                                                   
000999                                                                          
001000     STRING 'WDL701  (IDARTNR  =' W-IDARTNR-X ')'                         
001001          DELIMITED BY SIZE INTO SSA1                                     
001002     STRING 'WDL711  (IDDC     =' W-IDDC-X ')'                            
001003          DELIMITED BY SIZE INTO SSA2                                     
001004     MOVE '  GE' TO GODK-STATUSKODER                                      
001005     CALL CBLTDLI USING GU WDL7-PCB DLI-IO-AREA-L711 SSA1 SSA2            
001006     MOVE WDL7-STATUS-CODE TO STATUS-WS                                   
001007     PERFORM IMS-STATUSKONTROLL                                           
001008     .                                                                    
001009     SKIP3                                                                
001010 IMS-GU-WDL411 SECTION.                                                   
001011                                                                          
001012     STRING 'WDL401  (IDARTNR  =' W-IDARTNR-X ')'                         
001013          DELIMITED BY SIZE INTO SSA1                                     
001014     STRING 'WDL411  (IDDC     =' W-IDDC-X ')'                            
001015          DELIMITED BY SIZE INTO SSA2                                     
001016     MOVE '  GE' TO GODK-STATUSKODER                                      
001017     CALL CBLTDLI USING GU WDL4-PCB DLI-IO-AREA-L411 SSA1 SSA2            
001018     MOVE WDL4-STATUS-CODE TO STATUS-WS                                   
001019     PERFORM IMS-STATUSKONTROLL                                           
001020     .                                                                    
001021     SKIP3                                                                
001022 IMS-GU-WDD301-BSEQ SECTION.                                              
001023                                                                          
001024     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
001025          DELIMITED BY SIZE INTO SSA1                                     
001026     MOVE '  GE' TO GODK-STATUSKODER                                      
001027     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-AREA-D301 SSA1                 
001028     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
001029     PERFORM IMS-STATUSKONTROLL                                           
001030     .                                                                    
001031     SKIP3                                                                
001032 IMS-GNP-WDD311 SECTION.                                                  
001033                                                                          
001034     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
001035          DELIMITED BY SIZE INTO SSA1                                     
001036     MOVE '  GE' TO GODK-STATUSKODER                                      
001037     CALL CBLTDLI USING GNP WDD3-PCB DLI-IO-AREA-D311 SSA1                
001038     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
001039     PERFORM IMS-STATUSKONTROLL                                           
001040     .                                                                    
001041     EJECT                                                                
001042 IMS-GU-WDK601    SECTION.                                                
001043     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
001044            DELIMITED BY SIZE INTO SSA1                                   
001045     MOVE '  GEGB' TO GODK-STATUSKODER                                    
001046     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-K601 SSA1                 
001047     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
001048     PERFORM IMS-STATUSKONTROLL                                           
001049     .                                                                    
001050     EJECT                                                                
001051 IMS-GU-WDK711 SECTION.                                                   
001052                                                                          
001053     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
001054          DELIMITED BY SIZE INTO SSA1                                     
001055     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
001056          DELIMITED BY SIZE INTO SSA2                                     
001057     MOVE '  GE' TO GODK-STATUSKODER                                      
001058     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-K711 SSA1  SSA2           
001059     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
001060     PERFORM IMS-STATUSKONTROLL                                           
001061     .                                                                    
001062     EJECT                                                                
001063 IMS-GN-WDK711-FIRST SECTION.                                             
001064                                                                          
001065     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
001066          DELIMITED BY SIZE INTO SSA1                                     
001067     MOVE   'WDK711  *F ' TO SSA2                                         
001068     MOVE '  GEGB' TO GODK-STATUSKODER                                    
001069     CALL CBLTDLI USING GN WDK7-PCB DLI-IO-AREA-K711 SSA1  SSA2           
001070     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
001071     PERFORM IMS-STATUSKONTROLL                                           
001072     .                                                                    
001073     EJECT                                                                
001074 IMS-GN-WDK711 SECTION.                                                   
001075                                                                          
001076     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
001077          DELIMITED BY SIZE INTO SSA1                                     
001078     MOVE   'WDK711   ' TO SSA2                                           
001079     MOVE '  GEGB' TO GODK-STATUSKODER                                    
001080     CALL CBLTDLI USING GN WDK7-PCB DLI-IO-AREA-K711 SSA1  SSA2           
001081     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
001082     PERFORM IMS-STATUSKONTROLL                                           
001083     .                                                                    
001084     EJECT                                                                
001085 IMS-STATUSKONTROLL SECTION.                                              
001086                                                                          
001087     SET STATUS-IX TO 1                                                   
001088     SEARCH GODK-STATUS                                                   
001089       AT END                                                             
001090         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
001091         DELIMITED BY SIZE INTO FELTEXT                                   
001092         CALL FELLOG                                                      
001093       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
001094         CONTINUE                                                         
001095     END-SEARCH                                                           
001096     .                                                                    
001100     EJECT                                                                
