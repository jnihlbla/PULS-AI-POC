000001 ID DIVISION.                                                             
000002 PROGRAM-ID.     W6018200.                                                
000003 AUTHOR.         PER-ANDERS HELGEGREN / ARCHANA BHAT.                     
000004 DATE-WRITTEN.   99/08/24 / JULY 2012.                                    
000005 DATE-COMPILED.                                                           
000006                                                                          
000007*    FUNKTION:                                                            
000008*        REGISTRERING AV FÖRPACKNINGS EMBALLAGE                           
000009*                                                                         
000010*        PROGRAMMET UPPDATERAR WLARTC (WDK6)                              
000011*                                                                         
000012*    INDATA.                                                              
000013*        TRANSAKTION: W6T182                                              
000014*        MID:         W6I18201                                            
000015*                                                                         
000016*    UTDATA.                                                              
000017*        MOD:         W6O18201                                            
000018                                                                          
000019     SKIP3                                                                
000020 ENVIRONMENT DIVISION.                                                    
000021     EJECT                                                                
000022 DATA DIVISION.                                                           
000023 WORKING-STORAGE SECTION.                                                 
000024                                                                          
000025*    -- CHECKED BY WY2000                                                 
000026 77  IDPGM                       PIC X(08)   VALUE 'W6018200'.            
000027                                                                          
000028*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
000029 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
000030                                                                          
000031 77  JA                          PIC X       VALUE 'J'.                   
000032 77  NEJ                         PIC X       VALUE 'N'.                   
000033                                                                          
000034*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
000035                                                                          
000036 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
000037     88  EGEN-MID                            VALUE '6182'.                
000038     88  GODK-MID                            VALUE '6181' '6182'          
000039                                                   '6183' '6184'          
000040                                                   '6185' '6186'          
000041                                                   '6187' '6188'          
000042                                                   '6189'.                
000043     88  HELP-MID                            VALUE '0551'.                
000044     EJECT                                                                
000045 01  IX-X                        PIC S9(3) COMP-3 VALUE ZERO.             
000046 01  MAX-IX-X                    PIC S9(3) COMP-3 VALUE  +10.             
000047*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
000048 01  GENERELLA-SUBPROGRAM.                                                
000049     03  W6018210                PIC X(8)    VALUE 'W6018210'.            
000050     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
000051     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000052     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000053     03  WL01MCNV                PIC X(8)    VALUE 'WL01MCNV'.            
000054     EJECT                                                                
000055                                                                          
000056 01  W-IDMSG-ERROR               PIC X(3).                                
000057     88   WRONG-KEY                         VALUE '022'.                  
000058                                                                          
000059 01  FILLER                      PIC X(16)  VALUE 'REQU-AREA'.            
000060 01  REQU-AREA.                                                           
000061*    03 -COPY WZ01REQU                                                    
000062*    03 -COPY W60182I1                                                    
000063 77  MAX-KVRADER                 PIC S9(4)  COMP VALUE +10.               
000064     EJECT                                                                
000065*                                                                         
000066 01  FILLER                      PIC X(16)  VALUE 'RESP-AREA'.            
000067 01  RESP-AREA.                                                           
000068*    03 -COPY WZ01RESP                                                    
000069*    03 -COPY W60182O1                                                    
000070*                                                                         
000071     EJECT                                                                
000072*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
000073*                                                                         
000074 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
000075     SKIP3                                                                
000076*01 -COPY WMSGINIT                                                        
000077     EJECT                                                                
000078*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
000079*                                                                         
000080 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
000081     SKIP3                                                                
000082*01  MID -COPY W6I18201                                                   
000083     EJECT                                                                
000084 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
000085     SKIP3                                                                
000086*01  -COPY WMSGAREA                                                       
000087     EJECT                                                                
000088     03  MOD REDEFINES MSG-AREA.                                          
000089*      05  -COPY W6O18201                                                 
000090     EJECT                                                                
000091 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
000092     SKIP3                                                                
000093*01  -COPY WMFSAREA                                                       
000094     EJECT                                                                
000095 01  FILLER                      PIC X(16)   VALUE 'WL01MCNV'.            
000096     SKIP3                                                                
000097*01  -COPY WL01MCNV                                                       
000098     EJECT                                                                
000099*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
000100*                                                                         
000101     EJECT                                                                
000102 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000103     SKIP3                                                                
000104     SKIP2                                                                
000105*    --- STATUS-KOD FRÅN IMS                                              
000106 01  STATUS-WS                   PIC XX.                                  
000107     88  SEGMENT-FINNS                       VALUE '  '.                  
000108     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
000109     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
000110     SKIP2                                                                
000111 01  GODK-STATUSKODER.                                                    
000112     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000113     SKIP3                                                                
000114 01  SSA1                        PIC X(64).                               
000115 01  SSA2                        PIC X(64).                               
000116     EJECT                                                                
000117*    --- IMS FUNKTIONSKODER                                               
000118*01  -COPY W0003                                                          
000119     EJECT                                                                
000120 LINKAGE SECTION.                                                         
000121*01  -COPY W0009   -PRE MSG-                                              
000122     EJECT                                                                
000123*01  -COPY W0008   -PRE USEA-                                             
000124     05  FILLER                  PIC X.                                   
000125                                                                          
000126     EJECT                                                                
000127*01  -COPY W0008  -PRE WDK6-                                              
000128     05  FILLER                  PIC X.                                   
000129     EJECT                                                                
000130*01  -COPY W0008  -PRE KWDK6-                                             
000131     05  FILLER                  PIC X.                                   
000132     EJECT                                                                
000133*01  -COPY W0008  -PRE WDT3-                                              
000134     05  FILLER                  PIC X.                                   
000135     EJECT                                                                
000136*01  -COPY W0008  -PRE WDK7-                                              
000137     05  FILLER                  PIC X.                                   
000138     EJECT                                                                
000139 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB WDK6-PCB KWDK6-PCB            
000140                           WDT3-PCB WDK7-PCB.                             
000141 MAIN SECTION.                                                            
000142     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB WDK6-PCB KWDK6-PCB            
000143                           WDT3-PCB WDK7-PCB.                             
000144                                                                          
000145     PERFORM IMS-GET-MSG                                                  
000146     IF SEGMENT-FINNS                                                     
000147       PERFORM A-INIT                                                     
000148       PERFORM B-INIT-KEYS                                                
000149       IF MFS-UPDATE                                                      
000150         SET REQU-UPDATE  TO TRUE                                         
000151       ELSE                                                               
000152         IF MFS-UPD-V                                                     
000153           SET REQU-UPD-V TO TRUE                                         
000154         ELSE                                                             
000155           IF MFS-FIRST                                                   
000156              SET REQU-FIRST TO TRUE                                      
000157              PERFORM MFS-RENSA-FAELT-IN                                  
000158           ELSE                                                           
000159              SET REQU-QUERY TO TRUE                                      
000160              PERFORM E-SAMMA-SIDA                                        
000161           END-IF                                                         
000162         END-IF                                                           
000163       END-IF                                                             
000164       PERFORM F-CALL-BIZ-LOGIC-W6018210                                  
000165                                                                          
000166       COMPUTE MSG-KVLL = LENGTH OF MOD-W6O18201 + 4                      
000167       PERFORM IMS-INSERT-MSG                                             
000168     END-IF                                                               
000169                                                                          
000170     MOVE ZERO TO RETURN-CODE                                             
000171     GOBACK                                                               
000172     .                                                                    
000173     EJECT                                                                
000174 A-INIT SECTION.                                                          
000175                                                                          
000176     IF MSG-DUBBLA-TRANSKODER                                             
000177       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I18201                 
000178       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
000179       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
000180     ELSE                                                                 
000181       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I18201                  
000182       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
000183       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
000184     END-IF                                                               
000185                                                                          
000186     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
000187     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
000188     MOVE MFS-IDTRANS TO W-IDTRANS                                        
000189                                                                          
000190     MOVE LOW-VALUE TO MSG-AREA                                           
000191     MOVE 'W6O182N1' TO MFS-IDMOD                                         
000192     MOVE '6182' TO MOD-IDTRANS                                           
000193     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
000194                                                                          
000195     IF MSGI-IDLAND-SPR = 'SE'                                            
000196        MOVE '0' TO MFS-KDHUVOMR                                          
000197     END-IF                                                               
000198                                                                          
000199     IF EGEN-MID OR HELP-MID                                              
000200       CONTINUE                                                           
000201     ELSE                                                                 
000202       MOVE SPACE TO MFS-KDTRTYP                                          
000203       MOVE '7' TO MFS-IDPFK                                              
000204     END-IF                                                               
000205                                                                          
000206     .                                                                    
000207     EJECT                                                                
000208 B-INIT-KEYS SECTION.                                                     
000209                                                                          
000210     MOVE ALL '+'                TO MSGI-WMSGINIT                         
000211     MOVE '001'                  TO MSGI-KDCALL                           
000212     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
000213     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
000214     MOVE '6182'                 TO MSGI-IDTRANS                          
000215     IF GODK-MID                                                          
000216         MOVE MID-IDARTNR-IN     TO MSGI-IDARTNR                          
000217     END-IF                                                               
000218     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
000219                                                                          
000220*    -- KONTROLL AV IDARTNR                                               
000221     MOVE MFS-RENSA-FAELT        TO MOD-IDARTNR-IN                        
000222                                                                          
000223     IF MID-IDARTNR-IN NOT = ALL '+'                                      
000224       MOVE '7'                  TO MFS-IDPFK                             
000225       MOVE SPACE                TO MFS-KDTRTYP                           
000226     END-IF                                                               
000227     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
000228     MOVE MSGI-IDARTNR           TO REQU-IDARTNR-KEY                      
000229                                                                          
000230     MOVE MSGI-IDARTNR           TO MOD-IDARTNR-UT                        
000231     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
000232                                                                          
000233     MOVE MSGI-IDDC              TO REQU-IDDC                             
000234     IF MID-IDDC-KEY-IN NUMERIC                                           
000235        MOVE MID-IDDC-KEY-IN TO REQU-IDDC-KEY                             
000236     ELSE                                                                 
000237       IF MID-IDDC-KEY-UT NUMERIC                                         
000238          MOVE MID-IDDC-KEY-UT TO REQU-IDDC-KEY                           
000239       ELSE                                                               
000240          MOVE MSGI-IDDC       TO REQU-IDDC-KEY                           
000241       END-IF                                                             
000242     END-IF                                                               
000252     .                                                                    
000253     EJECT                                                                
000254 E-SAMMA-SIDA SECTION.                                                    
000255                                                                          
000256     IF EGEN-MID OR HELP-MID                                              
000257       CONTINUE                                                           
000258     ELSE                                                                 
000259       PERFORM MFS-RENSA-FAELT-IN                                         
000260     END-IF                                                               
000261     .                                                                    
000262     EJECT                                                                
000263 F-CALL-BIZ-LOGIC-W6018210 SECTION.                                       
000264                                                                          
000265     PERFORM FA-INIT-REQU                                                 
000266     CALL W6018210 USING REQU-AREA RESP-AREA MAX-KVRADER                  
000267                         WDK6-PCB KWDK6-PCB WDT3-PCB WDK7-PCB             
000268                                                                          
000269     PERFORM FB-SET-MSG-AND-HILIGHT                                       
000270     IF NOT WRONG-KEY                                                     
000271        PERFORM FC-MOVE-RESP-TO-MOD                                       
000272     END-IF                                                               
000273     .                                                                    
000274     EJECT                                                                
000275 FA-INIT-REQU SECTION.                                                    
000276                                                                          
000277     MOVE '101'                     TO REQU-IDMSGVER                      
000278     MOVE MSGI-IDUSER               TO REQU-IDUSER                        
000279                                                                          
000280     MOVE MID-IDARTNR-KOPI          TO REQU-IDARTNR-KOPI                  
000281     MOVE MID-IDARTNR-EMBQ0-IN      TO REQU-IDARTNR-EMBQ0                 
000282     MOVE MID-KVQPACK-EMBQ0-IN      TO REQU-KVQPACK-EMBQ0                 
000283     MOVE MID-KDEMBKOD-EMBQ0-IN     TO REQU-KDEMBKOD-EMBQ0                
000284     MOVE MID-IDARTNR-EMBQ1-IN      TO REQU-IDARTNR-EMBQ1                 
000285     MOVE MID-KVQPACK-EMBQ1-IN      TO REQU-KVQPACK-EMBQ1                 
000286     MOVE MID-KDEMBKOD-EMBQ1-IN     TO REQU-KDEMBKOD-EMBQ1                
000287     MOVE MID-IDARTNR-EMBQ2-IN      TO REQU-IDARTNR-EMBQ2                 
000288     MOVE MID-KVQPACK-EMBQ2-IN      TO REQU-KVQPACK-EMBQ2                 
000289     MOVE MID-KDEMBKOD-EMBQ2-IN     TO REQU-KDEMBKOD-EMBQ2                
000290     MOVE MID-KVQPACK-EMBQ3-IN      TO REQU-KVQPACK-EMBQ3                 
000291     MOVE MID-KVQPACK-EMBQ4-IN      TO REQU-KVQPACK-EMBQ4                 
000292                                                                          
000293     MOVE +1                        TO IX-X                               
000294     PERFORM UNTIL IX-X > MAX-IX-X                                        
000295        MOVE MID-IDARTNR-EMBX-IN(IX-X)                                    
000296                                    TO REQU-IDARTNR-EMBX(IX-X)            
000297        MOVE MID-KVQPACK-EMBX-IN(IX-X)                                    
000298                                    TO REQU-KVQPACK-EMBX(IX-X)            
000299        ADD  +1                     TO IX-X                               
000300     END-PERFORM                                                          
000301     .                                                                    
000302                                                                          
000303 FB-SET-MSG-AND-HILIGHT SECTION.                                          
000304                                                                          
000305     MOVE RESP-IDMSG-ERROR          TO MCNV-IDMSG-ERROR                   
000306                                       W-IDMSG-ERROR                      
000307     MOVE RESP-IDMSG-INFO           TO MCNV-IDMSG-INFO                    
000308     MOVE RESP-IDELMT-ERROR         TO MCNV-IDELMT-ERROR                  
000309     MOVE MSGI-IDSPRAK              TO MCNV-IDSPRAK                       
000310                                                                          
000311     CALL WL01MCNV USING MCNV-AREA                                        
000312                                                                          
000313     MOVE MCNV-MFSINF               TO MOD-TEMFSINF                       
000314     MOVE MCNV-MFSFEL               TO MOD-TEMFSFEL                       
000315     IF WRONG-KEY                                                         
000316        PERFORM MFS-RENSA-FAELT-UT                                        
000317        PERFORM MFS-RENSA-FAELT-IN                                        
000318     END-IF                                                               
000319     .                                                                    
000320     EJECT                                                                
000321 FC-MOVE-RESP-TO-MOD SECTION.                                             
000322                                                                          
000323     IF RESP-IDDC-KEY = SPACE                                             
000324        MOVE MFS-ERASE-FIELD            TO MOD-IDDC-KEY-UT                
000325     ELSE                                                                 
000326        MOVE RESP-IDDC-KEY              TO MOD-IDDC-KEY-UT                
000327     END-IF                                                               
000328                                                                          
000329     MOVE RESP-IDARTNR-KOPI-ATTR          TO MOD-IDARTNR-KOPI-ATTR        
000330                                                                          
000331     IF RESP-IDARTNR-KOPI = SPACE                                         
000332        MOVE MFS-ERASE-FIELD              TO MOD-IDARTNR-KOPI             
000333     ELSE                                                                 
000334       IF RESP-IDARTNR-KOPI = ALL '+'                                     
000335          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-IDARTNR-KOPI             
000336       ELSE                                                               
000337          MOVE RESP-IDARTNR-KOPI          TO MOD-IDARTNR-KOPI             
000338       END-IF                                                             
000339     END-IF                                                               
000340                                                                          
000341     IF RESP-BEFT = SPACE                                                 
000342        MOVE MFS-ERASE-FIELD              TO MOD-BEFT-UT                  
000343     ELSE                                                                 
000344       IF RESP-BEFT = ALL '+'                                             
000345          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-BEFT-UT                  
000346       ELSE                                                               
000347          MOVE RESP-BEFT                  TO MOD-BEFT-UT                  
000348       END-IF                                                             
000349     END-IF                                                               
000350                                                                          
000351     IF RESP-KDFORP = SPACE                                               
000352        MOVE MFS-ERASE-FIELD              TO MOD-KDFORP-UT                
000353     ELSE                                                                 
000354       IF RESP-KDFORP  = ALL '+'                                          
000355          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-KDFORP-UT                
000356       ELSE                                                               
000357          MOVE RESP-KDFORP                TO MOD-KDFORP-UT                
000358       END-IF                                                             
000359     END-IF                                                               
000360                                                                          
000361     IF RESP-FLFPINST-UPP = SPACE                                         
000362        MOVE MFS-ERASE-FIELD              TO MOD-FLFPINST-UPP             
000363     ELSE                                                                 
000364       IF RESP-FLFPINST-UPP = ALL '+'                                     
000365          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-FLFPINST-UPP             
000366       ELSE                                                               
000367          MOVE RESP-FLFPINST-UPP          TO MOD-FLFPINST-UPP             
000368       END-IF                                                             
000369     END-IF                                                               
000370                                                                          
000371     IF RESP-TIUPPDAT = SPACE                                             
000372        MOVE MFS-ERASE-FIELD              TO MOD-TIUPPDAT-UT              
000373     ELSE                                                                 
000374       IF RESP-TIUPPDAT = ALL '+'                                         
000375          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-TIUPPDAT-UT              
000376       ELSE                                                               
000377          MOVE RESP-TIUPPDAT              TO MOD-TIUPPDAT-UT              
000378       END-IF                                                             
000379     END-IF                                                               
000380                                                                          
000381     IF RESP-IDUSER = SPACE                                               
000382        MOVE MFS-ERASE-FIELD              TO MOD-IDUSER-UT                
000383     ELSE                                                                 
000384       IF RESP-IDUSER    =  ALL '+'                                       
000385          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-IDUSER-UT                
000386       ELSE                                                               
000387          MOVE RESP-IDUSER                TO MOD-IDUSER-UT                
000388       END-IF                                                             
000389     END-IF                                                               
000390                                                                          
000391     IF RESP-IDARTNR-EMBQ0-UT = SPACE                                     
000392        MOVE MFS-ERASE-FIELD              TO MOD-IDARTNR-EMBQ0-UT         
000393     ELSE                                                                 
000394       IF RESP-IDARTNR-EMBQ0-UT = ALL '+'                                 
000395          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-IDARTNR-EMBQ0-UT         
000396       ELSE                                                               
000397          MOVE RESP-IDARTNR-EMBQ0-UT      TO MOD-IDARTNR-EMBQ0-UT         
000398       END-IF                                                             
000399     END-IF                                                               
000400                                                                          
000401     MOVE RESP-IDARTNR-EMBQ0-ATTR TO MOD-IDARTNR-EMBQ0-IN-ATTR            
000402                                                                          
000403     IF RESP-IDARTNR-EMBQ0 = SPACE                                        
000404        MOVE MFS-ERASE-FIELD              TO MOD-IDARTNR-EMBQ0-IN         
000405     ELSE                                                                 
000406       IF RESP-IDARTNR-EMBQ0    = ALL '+'                                 
000407          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-IDARTNR-EMBQ0-IN         
000408       ELSE                                                               
000409          MOVE RESP-IDARTNR-EMBQ0         TO MOD-IDARTNR-EMBQ0-IN         
000410       END-IF                                                             
000411     END-IF                                                               
000412                                                                          
000413     IF RESP-KVQPACK-EMBQ0-UT = SPACE                                     
000414        MOVE MFS-ERASE-FIELD              TO MOD-KVQPACK-EMBQ0-UT         
000415     ELSE                                                                 
000416       IF RESP-KVQPACK-EMBQ0-UT = ALL '+'                                 
000417          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-KVQPACK-EMBQ0-UT         
000418       ELSE                                                               
000419          MOVE RESP-KVQPACK-EMBQ0-UT      TO MOD-KVQPACK-EMBQ0-UT         
000420       END-IF                                                             
000421     END-IF                                                               
000422                                                                          
000423     MOVE RESP-KVQPACK-EMBQ0-ATTR TO MOD-KVQPACK-EMBQ0-IN-ATTR            
000424                                                                          
000425     IF RESP-KVQPACK-EMBQ0 = SPACE                                        
000426        MOVE MFS-ERASE-FIELD              TO MOD-KVQPACK-EMBQ0-IN         
000427     ELSE                                                                 
000428       IF RESP-KVQPACK-EMBQ0    = ALL '+'                                 
000429          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-KVQPACK-EMBQ0-IN         
000430       ELSE                                                               
000431          MOVE RESP-KVQPACK-EMBQ0         TO MOD-KVQPACK-EMBQ0-IN         
000432       END-IF                                                             
000433     END-IF                                                               
000434                                                                          
000435     IF RESP-KDEMBKOD-EMBQ0-UT = SPACE                                    
000436        MOVE MFS-ERASE-FIELD              TO MOD-KDEMBKOD-EMBQ0-UT        
000437     ELSE                                                                 
000438       IF RESP-KDEMBKOD-EMBQ0-UT = ALL '+'                                
000439          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-KDEMBKOD-EMBQ0-UT        
000440       ELSE                                                               
000441          MOVE RESP-KDEMBKOD-EMBQ0-UT     TO MOD-KDEMBKOD-EMBQ0-UT        
000442       END-IF                                                             
000443     END-IF                                                               
000444                                                                          
000445     MOVE RESP-KDEMBKOD-EMBQ0-ATTR TO                                     
000446                                      MOD-KDEMBKOD-EMBQ0-IN-ATTR          
000447                                                                          
000448     IF RESP-KDEMBKOD-EMBQ0 = SPACE                                       
000449        MOVE MFS-ERASE-FIELD              TO MOD-KDEMBKOD-EMBQ0-IN        
000450     ELSE                                                                 
000451       IF RESP-KDEMBKOD-EMBQ0    = ALL '+'                                
000452          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-KDEMBKOD-EMBQ0-IN        
000453       ELSE                                                               
000454          MOVE RESP-KDEMBKOD-EMBQ0        TO MOD-KDEMBKOD-EMBQ0-IN        
000455       END-IF                                                             
000456     END-IF                                                               
000457                                                                          
000458     IF RESP-IDARTNR-EMBQ1-UT = SPACE                                     
000459        MOVE MFS-ERASE-FIELD              TO MOD-IDARTNR-EMBQ1-UT         
000460     ELSE                                                                 
000461       IF RESP-IDARTNR-EMBQ1-UT = ALL '+'                                 
000462          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-IDARTNR-EMBQ1-UT         
000463       ELSE                                                               
000464          MOVE RESP-IDARTNR-EMBQ1-UT      TO MOD-IDARTNR-EMBQ1-UT         
000465       END-IF                                                             
000466     END-IF                                                               
000467                                                                          
000468     MOVE RESP-IDARTNR-EMBQ1-ATTR TO MOD-IDARTNR-EMBQ1-IN-ATTR            
000469                                                                          
000470     IF RESP-IDARTNR-EMBQ1 = SPACE                                        
000471        MOVE MFS-ERASE-FIELD              TO MOD-IDARTNR-EMBQ1-IN         
000472     ELSE                                                                 
000473       IF RESP-IDARTNR-EMBQ1    = ALL '+'                                 
000474          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-IDARTNR-EMBQ1-IN         
000475       ELSE                                                               
000476          MOVE RESP-IDARTNR-EMBQ1         TO MOD-IDARTNR-EMBQ1-IN         
000477       END-IF                                                             
000478     END-IF                                                               
000479                                                                          
000480     IF RESP-KVQPACK-EMBQ1-UT = SPACE                                     
000481        MOVE MFS-ERASE-FIELD              TO MOD-KVQPACK-EMBQ1-UT         
000482     ELSE                                                                 
000483       IF RESP-KVQPACK-EMBQ1-UT = ALL '+'                                 
000484          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-KVQPACK-EMBQ1-UT         
000485       ELSE                                                               
000486          MOVE RESP-KVQPACK-EMBQ1-UT      TO MOD-KVQPACK-EMBQ1-UT         
000487       END-IF                                                             
000488     END-IF                                                               
000489                                                                          
000490     MOVE RESP-KVQPACK-EMBQ1-ATTR TO MOD-KVQPACK-EMBQ1-IN-ATTR            
000491                                                                          
000492     IF RESP-KVQPACK-EMBQ1 = SPACE                                        
000493        MOVE MFS-ERASE-FIELD              TO MOD-KVQPACK-EMBQ1-IN         
000494     ELSE                                                                 
000495       IF RESP-KVQPACK-EMBQ1    = ALL '+'                                 
000496          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-KVQPACK-EMBQ1-IN         
000497       ELSE                                                               
000498          MOVE RESP-KVQPACK-EMBQ1         TO MOD-KVQPACK-EMBQ1-IN         
000499       END-IF                                                             
000500     END-IF                                                               
000501                                                                          
000502     IF RESP-KDEMBKOD-EMBQ1-UT = SPACE                                    
000503        MOVE MFS-ERASE-FIELD              TO MOD-KDEMBKOD-EMBQ1-UT        
000504     ELSE                                                                 
000505       IF RESP-KDEMBKOD-EMBQ1-UT = ALL '+'                                
000506          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-KDEMBKOD-EMBQ1-UT        
000507       ELSE                                                               
000508          MOVE RESP-KDEMBKOD-EMBQ1-UT     TO MOD-KDEMBKOD-EMBQ1-UT        
000509       END-IF                                                             
000510     END-IF                                                               
000511                                                                          
000512     MOVE RESP-KDEMBKOD-EMBQ1-ATTR TO                                     
000513                                      MOD-KDEMBKOD-EMBQ1-IN-ATTR          
000514                                                                          
000515     IF RESP-KDEMBKOD-EMBQ1 = SPACE                                       
000516        MOVE MFS-ERASE-FIELD              TO MOD-KDEMBKOD-EMBQ1-IN        
000517     ELSE                                                                 
000518       IF RESP-KDEMBKOD-EMBQ1 = ALL '+'                                   
000519          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-KDEMBKOD-EMBQ1-IN        
000520       ELSE                                                               
000521          MOVE RESP-KDEMBKOD-EMBQ1        TO MOD-KDEMBKOD-EMBQ1-IN        
000522       END-IF                                                             
000523     END-IF                                                               
000524                                                                          
000525     IF RESP-IDARTNR-EMBQ2-UT = SPACE                                     
000526        MOVE MFS-ERASE-FIELD              TO MOD-IDARTNR-EMBQ2-UT         
000527     ELSE                                                                 
000528       IF RESP-IDARTNR-EMBQ2-UT = ALL '+'                                 
000529          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-IDARTNR-EMBQ2-UT         
000530       ELSE                                                               
000531          MOVE RESP-IDARTNR-EMBQ2-UT      TO MOD-IDARTNR-EMBQ2-UT         
000532       END-IF                                                             
000533     END-IF                                                               
000534                                                                          
000535     MOVE RESP-IDARTNR-EMBQ2-ATTR TO MOD-IDARTNR-EMBQ2-IN-ATTR            
000536                                                                          
000537     IF RESP-IDARTNR-EMBQ2 = SPACE                                        
000538        MOVE MFS-ERASE-FIELD              TO MOD-IDARTNR-EMBQ2-IN         
000539     ELSE                                                                 
000540       IF RESP-IDARTNR-EMBQ2    = ALL '+'                                 
000541          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-IDARTNR-EMBQ2-IN         
000542       ELSE                                                               
000543          MOVE RESP-IDARTNR-EMBQ2         TO MOD-IDARTNR-EMBQ2-IN         
000544       END-IF                                                             
000545     END-IF                                                               
000546                                                                          
000547     IF RESP-KVQPACK-EMBQ2-UT = SPACE                                     
000548        MOVE MFS-ERASE-FIELD              TO MOD-KVQPACK-EMBQ2-UT         
000549     ELSE                                                                 
000550       IF RESP-KVQPACK-EMBQ2-UT = ALL '+'                                 
000551          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-KVQPACK-EMBQ2-UT         
000552       ELSE                                                               
000553          MOVE RESP-KVQPACK-EMBQ2-UT      TO MOD-KVQPACK-EMBQ2-UT         
000554       END-IF                                                             
000555     END-IF                                                               
000556                                                                          
000557     MOVE RESP-KVQPACK-EMBQ2-ATTR TO MOD-KVQPACK-EMBQ2-IN-ATTR            
000558                                                                          
000559     IF RESP-KVQPACK-EMBQ2 = SPACE                                        
000560        MOVE MFS-ERASE-FIELD              TO MOD-KVQPACK-EMBQ2-IN         
000561     ELSE                                                                 
000562       IF RESP-KVQPACK-EMBQ2    = ALL '+'                                 
000563          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-KVQPACK-EMBQ2-IN         
000564       ELSE                                                               
000565          MOVE RESP-KVQPACK-EMBQ2         TO MOD-KVQPACK-EMBQ2-IN         
000566       END-IF                                                             
000567     END-IF                                                               
000568                                                                          
000569     IF RESP-KDEMBKOD-EMBQ2-UT = SPACE                                    
000570        MOVE MFS-ERASE-FIELD              TO MOD-KDEMBKOD-EMBQ2-UT        
000571     ELSE                                                                 
000572       IF RESP-KDEMBKOD-EMBQ2-UT = ALL '+'                                
000573          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-KDEMBKOD-EMBQ2-UT        
000574       ELSE                                                               
000575          MOVE RESP-KDEMBKOD-EMBQ2-UT     TO MOD-KDEMBKOD-EMBQ2-UT        
000576       END-IF                                                             
000577     END-IF                                                               
000578                                                                          
000579     MOVE RESP-KDEMBKOD-EMBQ2-ATTR TO                                     
000580                                      MOD-KDEMBKOD-EMBQ2-IN-ATTR          
000581                                                                          
000582     IF RESP-KDEMBKOD-EMBQ2 = SPACE                                       
000583        MOVE MFS-ERASE-FIELD              TO MOD-KDEMBKOD-EMBQ2-IN        
000584     ELSE                                                                 
000585       IF RESP-KDEMBKOD-EMBQ2 = ALL '+'                                   
000586          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-KDEMBKOD-EMBQ2-IN        
000587       ELSE                                                               
000588          MOVE RESP-KDEMBKOD-EMBQ2        TO MOD-KDEMBKOD-EMBQ2-IN        
000589       END-IF                                                             
000590     END-IF                                                               
000591                                                                          
000592     IF RESP-KVQPACK-EMBQ3-UT = SPACE                                     
000593        MOVE MFS-ERASE-FIELD              TO MOD-KVQPACK-EMBQ3-UT         
000594     ELSE                                                                 
000595       IF RESP-KVQPACK-EMBQ3-UT = ALL '+'                                 
000596          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-KVQPACK-EMBQ3-UT         
000597       ELSE                                                               
000598          MOVE RESP-KVQPACK-EMBQ3-UT      TO MOD-KVQPACK-EMBQ3-UT         
000599       END-IF                                                             
000600     END-IF                                                               
000601                                                                          
000602     MOVE RESP-KVQPACK-EMBQ3-ATTR TO MOD-KVQPACK-EMBQ3-IN-ATTR            
000603                                                                          
000604     IF RESP-KVQPACK-EMBQ3 = SPACE                                        
000605        MOVE MFS-ERASE-FIELD              TO MOD-KVQPACK-EMBQ3-IN         
000606     ELSE                                                                 
000607       IF RESP-KVQPACK-EMBQ3    = ALL '+'                                 
000608          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-KVQPACK-EMBQ3-IN         
000609       ELSE                                                               
000610          MOVE RESP-KVQPACK-EMBQ3         TO MOD-KVQPACK-EMBQ3-IN         
000611       END-IF                                                             
000612     END-IF                                                               
000613                                                                          
000614     IF RESP-KVQPACK-EMBQ4-UT = SPACE                                     
000615        MOVE MFS-ERASE-FIELD              TO MOD-KVQPACK-EMBQ4-UT         
000616     ELSE                                                                 
000617       IF RESP-KVQPACK-EMBQ4-UT = ALL '+'                                 
000618          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-KVQPACK-EMBQ4-UT         
000619       ELSE                                                               
000620          MOVE RESP-KVQPACK-EMBQ4-UT      TO MOD-KVQPACK-EMBQ4-UT         
000621       END-IF                                                             
000622     END-IF                                                               
000623                                                                          
000624     MOVE RESP-KVQPACK-EMBQ4-ATTR TO MOD-KVQPACK-EMBQ4-IN-ATTR            
000625                                                                          
000626     IF RESP-KVQPACK-EMBQ4 = SPACE                                        
000627        MOVE MFS-ERASE-FIELD              TO MOD-KVQPACK-EMBQ4-IN         
000628     ELSE                                                                 
000629       IF RESP-KVQPACK-EMBQ4    = ALL '+'                                 
000630          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-KVQPACK-EMBQ4-IN         
000631       ELSE                                                               
000632          MOVE RESP-KVQPACK-EMBQ4         TO MOD-KVQPACK-EMBQ4-IN         
000633       END-IF                                                             
000634     END-IF                                                               
000635                                                                          
000636     MOVE +1  TO IX-X                                                     
000637     PERFORM UNTIL IX-X > MAX-KVRADER                                     
000638                                                                          
000639       IF RESP-IDARTNR-EMBX-UT(IX-X) = SPACE                              
000640          MOVE MFS-ERASE-FIELD           TO                               
000641                                      MOD-IDARTNR-EMBX-UT(IX-X)           
000642         ELSE                                                             
000643         IF RESP-IDARTNR-EMBX-UT(IX-X) = ALL '+'                          
000644            MOVE MFS-DO-NOT-TOUCH-FIELD  TO                               
000645                                      MOD-IDARTNR-EMBX-UT(IX-X)           
000646         ELSE                                                             
000647            MOVE RESP-IDARTNR-EMBX-UT(IX-X) TO                            
000648                                      MOD-IDARTNR-EMBX-UT(IX-X)           
000649         END-IF                                                           
000650       END-IF                                                             
000651                                                                          
000652       MOVE RESP-IDARTNR-EMBX-ATTR(IX-X) TO                               
000653                                   MOD-IDARTNR-EMBX-IN-ATTR(IX-X)         
000654                                                                          
000655       IF RESP-IDARTNR-EMBX(IX-X) = SPACE                                 
000656          MOVE MFS-ERASE-FIELD            TO                              
000657                                   MOD-IDARTNR-EMBX-IN(IX-X)              
000658       ELSE                                                               
000659         IF RESP-IDARTNR-EMBX(IX-X) = ALL "+"                             
000660            MOVE MFS-DO-NOT-TOUCH-FIELD   TO                              
000661                                   MOD-IDARTNR-EMBX-IN(IX-X)              
000662         ELSE                                                             
000663            MOVE RESP-IDARTNR-EMBX(IX-X) TO                               
000664                                   MOD-IDARTNR-EMBX-IN(IX-X)              
000665         END-IF                                                           
000666       END-IF                                                             
000667                                                                          
000668       IF RESP-KVQPACK-EMBX-UT(IX-X) = SPACE                              
000669          MOVE MFS-ERASE-FIELD            TO                              
000670                                   MOD-KVQPACK-EMBX-UT(IX-X)              
000671       ELSE                                                               
000672         IF RESP-KVQPACK-EMBX-UT(IX-X) = ALL '+'                          
000673            MOVE MFS-DO-NOT-TOUCH-FIELD   TO                              
000674                                   MOD-KVQPACK-EMBX-UT(IX-X)              
000675         ELSE                                                             
000676            MOVE RESP-KVQPACK-EMBX-UT(IX-X) TO                            
000677                                   MOD-KVQPACK-EMBX-UT(IX-X)              
000678         END-IF                                                           
000679       END-IF                                                             
000680                                                                          
000681       MOVE RESP-KVQPACK-EMBX-ATTR(IX-X) TO                               
000682                                   MOD-KVQPACK-EMBX-IN-ATTR(IX-X)         
000683                                                                          
000684       IF RESP-KVQPACK-EMBX(IX-X) = SPACE                                 
000685          MOVE MFS-ERASE-FIELD            TO                              
000686                                       MOD-KVQPACK-EMBX-IN(IX-X)          
000687       ELSE                                                               
000688         IF RESP-KVQPACK-EMBX(IX-X) = ALL '+'                             
000689            MOVE MFS-DO-NOT-TOUCH-FIELD   TO                              
000690                                       MOD-KVQPACK-EMBX-IN(IX-X)          
000691         ELSE                                                             
000692            MOVE RESP-KVQPACK-EMBX(IX-X) TO                               
000693                                       MOD-KVQPACK-EMBX-IN(IX-X)          
000694         END-IF                                                           
000695       END-IF                                                             
000696                                                                          
000697                                                                          
000698       ADD +1 TO IX-X                                                     
000699     END-PERFORM                                                          
000700                                                                          
000701     PERFORM UNTIL IX-X > MAX-KVRADER                                     
000702       MOVE MFS-ERASE-FIELD TO MOD-IDARTNR-EMBX-UT(IX-X)                  
000703                               MOD-IDARTNR-EMBX-IN(IX-X)                  
000704                               MOD-KVQPACK-EMBX-UT(IX-X)                  
000705                               MOD-KVQPACK-EMBX-IN(IX-X)                  
000706                                                                          
000707       ADD +1 TO IX-X                                                     
000708     END-PERFORM                                                          
000709     .                                                                    
000710     EJECT                                                                
000711 MFS-RENSA-FAELT-UT SECTION.                                              
000712                                                                          
000713*    --- ALLA UTDATA-FÄLT                                                 
000714     MOVE MFS-RENSA-FAELT TO MOD-BEFT-UT                                  
000715                             MOD-KDFORP-UT                                
000716                             MOD-FLFPINST-UPP                             
000717                             MOD-TIUPPDAT-UT                              
000718                             MOD-IDUSER-UT                                
000719                             MOD-IDARTNR-EMBQ0-UT                         
000720                             MOD-IDARTNR-EMBQ1-UT                         
000721                             MOD-IDARTNR-EMBQ2-UT                         
000722                             MOD-KVQPACK-EMBQ0-UT                         
000723                             MOD-KVQPACK-EMBQ1-UT                         
000724                             MOD-KVQPACK-EMBQ2-UT                         
000725                             MOD-KVQPACK-EMBQ3-UT                         
000726                             MOD-KVQPACK-EMBQ4-UT                         
000727                             MOD-KDEMBKOD-EMBQ0-UT                        
000728                             MOD-KDEMBKOD-EMBQ1-UT                        
000729                             MOD-KDEMBKOD-EMBQ2-UT                        
000730     MOVE +1 TO IX-X                                                      
000731     PERFORM UNTIL IX-X > 10                                              
000732        MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-EMBX-UT (IX-X)                
000733                                MOD-KVQPACK-EMBX-UT (IX-X)                
000734        ADD +1 TO IX-X                                                    
000735     END-PERFORM                                                          
000736                                                                          
000737     .                                                                    
000738     EJECT                                                                
000739 MFS-RENSA-FAELT-IN SECTION.                                              
000740                                                                          
000741*    --- ALLA INDATA-FÄLT                                                 
000742     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-KOPI                             
000743     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-EMBQ0-IN                         
000744                             MOD-IDARTNR-EMBQ1-IN                         
000745                             MOD-IDARTNR-EMBQ2-IN                         
000746                             MOD-KVQPACK-EMBQ0-IN                         
000747                             MOD-KVQPACK-EMBQ1-IN                         
000748                             MOD-KVQPACK-EMBQ2-IN                         
000749                             MOD-KVQPACK-EMBQ3-IN                         
000750                             MOD-KVQPACK-EMBQ4-IN                         
000751                             MOD-KDEMBKOD-EMBQ0-IN                        
000752                             MOD-KDEMBKOD-EMBQ1-IN                        
000753                             MOD-KDEMBKOD-EMBQ2-IN                        
000754     MOVE +1 TO IX-X                                                      
000755     PERFORM UNTIL IX-X > 10                                              
000756        MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-EMBX-IN (IX-X)                
000757                                MOD-KVQPACK-EMBX-IN (IX-X)                
000758        ADD +1 TO IX-X                                                    
000759     END-PERFORM                                                          
000760     .                                                                    
000761     EJECT                                                                
000762* --- IMS SEKTIONER ---                                                   
000763     SKIP3                                                                
000764 IMS-GET-MSG SECTION.                                                     
000765                                                                          
000766     MOVE '  QC' TO GODK-STATUSKODER                                      
000767     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
000768     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
000769     PERFORM IMS-STATUSKONTROLL                                           
000770     .                                                                    
000771     SKIP3                                                                
000772 IMS-INSERT-MSG SECTION.                                                  
000773                                                                          
000774     IF MSGI-IDLAND-SPR = 'SE'                                            
000775       MOVE '0' TO MFS-KDHUVOMR                                           
000776     END-IF                                                               
000777     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
000778     MOVE SPACE TO GODK-STATUSKODER                                       
000779     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
000780     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
000781     PERFORM IMS-STATUSKONTROLL                                           
000782     .                                                                    
000783     EJECT                                                                
000784 IMS-STATUSKONTROLL SECTION.                                              
000785                                                                          
000786     SET STATUS-IX TO 1                                                   
000787     SEARCH GODK-STATUS                                                   
000788       AT END                                                             
000789         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
000790         DELIMITED BY SIZE INTO FELTEXT                                   
000791         CALL FELLOG                                                      
000792       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
000793         CONTINUE                                                         
000794     END-SEARCH                                                           
000800     .                                                                    
