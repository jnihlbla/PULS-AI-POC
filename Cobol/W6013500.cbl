000001 ID DIVISION.                                                             
000002     SKIP2                                                                
000003 PROGRAM-ID.     W6013500.                                                
000004*AUTHOR.         BERT ANDERSSON > RAHUL JAIN.                             
000005*DATE-WRITTEN.   92/06/12 > MAR 2012.                                     
000006                                                                          
000007*    REMARKS.                                                             
000008*                                                                         
000009*    FUNKTION:                                                            
000010*        PROGRAMMET ÄR EN MPP SOM ANVÄNDS FÖR ATT TA BORT                 
000011*        KOLLIN, DVS. KOLLINUMMER FRÅN INLEVERANSREGISTRET.               
000012*        ARTIKLARNA FRÅN DET BORTTAGNA KOLLIT LÄGGS I EN                  
000013*        SK. "POOL" PÅ W6D121 DÄR ENDAST ETT RADNR FINNS.                 
000014*        NYCKEL PÅ 21-SEGMENTET (W6D121) BLIR IDRADNR = 1                 
000015*        EFTER UPPDATERING.                                               
000016*                                                                         
000017*        PROGRAMMET UPPDATERAR W6INLA (W6D1)                              
000018*                                                                         
000019*    INDATA.                                                              
000020*        TRANSAKTION: W6T135                                              
000021*        MID:         W6I13501                                            
000022*                                                                         
000023*    UTDATA.                                                              
000024*        MOD:         W6O13501                                            
000025                                                                          
000026     SKIP3                                                                
000027 ENVIRONMENT DIVISION.                                                    
000028     EJECT                                                                
000029 DATA DIVISION.                                                           
000030 WORKING-STORAGE SECTION.                                                 
000031                                                                          
000032*    -- CHECKED BY WY2000                                                 
000033 77  IDPGM                       PIC X(08)   VALUE 'W6013500'.            
000034                                                                          
000035*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
000036 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
000037                                                                          
000038 77  JA                          PIC X       VALUE 'J'.                   
000039 77  YES                         PIC X       VALUE 'Y'.                   
000040 77  NEJ                         PIC X       VALUE 'N'.                   
000041                                                                          
000042*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
000043 77  WS-IDLEVNR-KOLLI            PIC  X(5).                               
000044 77  WS-IDOKOLLI                 PIC  X(9)     VALUE SPACE.               
000045 77  WS-IDLOPNRM                 PIC  X(9)     VALUE SPACE.               
000046*                                                                         
000047 77  MAX-KOLUMN                  PIC S9(2) VALUE +02  COMP SYNC.          
000048 77  KOL-INDX                    PIC S9(2) VALUE ZERO COMP SYNC.          
000049 77  RAD-INDX                    PIC S9(2) VALUE ZERO COMP SYNC.          
000050*                                                                         
000051 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
000052     88  EGEN-MID                            VALUE '6135'.                
000053     88  GODK-MID                            VALUE '6131' '6132'          
000054                                                   '6133' '6134'          
000055                                                   '6135' '6136'          
000056                                                   '6137' '6138'          
000057                                                   '6139'.                
000058     88  HELP-MID                            VALUE '0551'.                
000059     EJECT                                                                
000060 01  WS-IDMSG-ERROR              PIC X(3).                                
000061     88  WRONG-KEY                           VALUE '022'.                 
000062     EJECT                                                                
000063*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
000064 01  GENERELLA-SUBPROGRAM.                                                
000065     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000066     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000067     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
000068     03  W6013510                PIC X(8)    VALUE 'W6013510'.            
000069     03  WL01MCNV                PIC X(8)    VALUE 'WL01MCNV'.            
000070     EJECT                                                                
000071*01  -COPY WMSGINIT                                                       
000072     EJECT                                                                
000073*                                                                         
000074 01  FILLER                      PIC X(16)  VALUE 'REQU-AREA'.            
000075 01  REQU-AREA.                                                           
000076*    03 -COPY WZ01REQU                                                    
000077*    03 -COPY W60135I1                                                    
000078 77  MAX-KVRADER                 PIC S9(4)  COMP VALUE +14.               
000079     EJECT                                                                
000080*                                                                         
000081 01  FILLER                      PIC X(16)  VALUE 'RESP-AREA'.            
000082 01  RESP-AREA.                                                           
000083*    03 -COPY WZ01RESP                                                    
000084*    03 -COPY W60135O1                                                    
000085     EJECT                                                                
000086*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
000087*                                                                         
000088 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
000089     SKIP3                                                                
000090*01  MID -COPY W6I13501                                                   
000091     EJECT                                                                
000092 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
000093     SKIP3                                                                
000094*01  -COPY WMSGAREA                                                       
000095     EJECT                                                                
000096     03  MOD REDEFINES MSG-AREA.                                          
000097*      05  -COPY W6O13501                                                 
000098     EJECT                                                                
000099 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
000100     SKIP3                                                                
000101*01  -COPY WMFSAREA                                                       
000102     EJECT                                                                
000103 01  FILLER                      PIC X(16)   VALUE 'WL01MCNV'.            
000104     SKIP3                                                                
000105*01  -COPY WL01MCNV                                                       
000106     EJECT                                                                
000107 01      FILLER                  PIC X(24)   VALUE                        
000108                                 'MOD6191-MID-W6I19101'.                  
000109     SKIP2                                                                
000110     -COPY W6I19101 -PRE MOD6191-                                         
000111     EJECT                                                                
000112*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
000113*                                                                         
000114     EJECT                                                                
000115 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000116     SKIP3                                                                
000117*    --- STATUS-KOD FRÅN IMS                                              
000118 01  STATUS-WS                   PIC XX.                                  
000119     88  SEGMENT-FINNS                       VALUE '  '.                  
000120     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
000121     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
000122     88  END-OF-DATA                         VALUE 'GB'.                  
000123     SKIP2                                                                
000124 01  GODK-STATUSKODER.                                                    
000125     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000126     SKIP3                                                                
000127 01  SSA1                        PIC X(128).                              
000128 01  SSA2                        PIC X(128).                              
000129     EJECT                                                                
000130*    --- IMS FUNKTIONSKODER                                               
000131*01  -COPY W0003                                                          
000132     EJECT                                                                
000133 LINKAGE SECTION.                                                         
000134                                                                          
000135*01  -COPY W0009   -PRE MSG-                                              
000136     SKIP2                                                                
000137*01  -COPY W0009   -PRE 6191-                                             
000138     SKIP2                                                                
000139*01  -COPY W0008   -PRE USEA-                                             
000140     05  FILLER                  PIC X.                                   
000141     SKIP2                                                                
000142*01  -COPY W0008   -PRE INLA-                                             
000143     05  FILLER                  PIC X.                                   
000144     SKIP2                                                                
000145*01  -COPY W0008   -PRE INLA-CSEQ-                                        
000146     05  FILLER                  PIC X.                                   
000147     SKIP2                                                                
000148*01  -COPY W0008   -PRE INLD-                                             
000149     05  FILLER                  PIC X.                                   
000150     SKIP2                                                                
000151*01  -COPY W0008   -PRE WDB6-                                             
000152     05  FILLER                  PIC X.                                   
000153     SKIP2                                                                
000154**  PCB'ER FÖR SUBPGM                                                     
000155 01  PMRK-INLB-PCB               PIC X.                                   
000156                                                                          
000157 01  PMRK-INLC-PCB               PIC X.                                   
000158                                                                          
000159 01  PMRK-PLAA-PCB               PIC X.                                   
000160     EJECT                                                                
000161*                                                                         
000162 PROCEDURE DIVISION  USING MSG-PCB 6191-PCB USEA-PCB INLA-PCB             
000163                                   INLA-CSEQ-PCB INLD-PCB WDB6-PCB        
000164                                   PMRK-INLB-PCB PMRK-INLC-PCB            
000165                                   PMRK-PLAA-PCB.                         
000166 W60135 SECTION.                                                          
000167     ENTRY 'DLITCBL' USING MSG-PCB 6191-PCB USEA-PCB INLA-PCB             
000168                                   INLA-CSEQ-PCB INLD-PCB WDB6-PCB        
000169                                   PMRK-INLB-PCB PMRK-INLC-PCB            
000170                                   PMRK-PLAA-PCB.                         
000171                                                                          
000172     PERFORM IMS-GET-MSG                                                  
000173     IF SEGMENT-FINNS                                                     
000174       PERFORM A-INIT                                                     
000175       PERFORM B-INIT-KEYS                                                
000176       PERFORM C-INIT-REQU                                                
000177       PERFORM E-SAMMA-SIDA                                               
000178       IF MFS-UPDATE                                                      
000179         SET REQU-UPDATE TO TRUE                                          
000180       ELSE                                                               
000181         SET REQU-QUERY  TO TRUE                                          
000182       END-IF                                                             
000183       PERFORM F-CALL-BIZ-LOGIC-W6013510                                  
000184       COMPUTE MSG-KVLL = LENGTH OF MOD-W6O13501 + 4                      
000185       PERFORM IMS-INSERT-MSG                                             
000186     END-IF                                                               
000187                                                                          
000188     MOVE ZERO TO RETURN-CODE                                             
000189     GOBACK                                                               
000190     .                                                                    
000191     EJECT                                                                
000192 A-INIT SECTION.                                                          
000193                                                                          
000194     IF MSG-DUBBLA-TRANSKODER                                             
000195       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I13501                 
000196       MOVE MSG-IDTRANS-2           TO MFS-IDTRANS                        
000197       MOVE MSG-KDMFSFOR-2          TO MFS-KDMFSFOR                       
000198     ELSE                                                                 
000199       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I13501                  
000200       MOVE MSG-IDTRANS-1           TO MFS-IDTRANS                        
000201       MOVE MSG-KDMFSFOR-1          TO MFS-KDMFSFOR                       
000202     END-IF                                                               
000203                                                                          
000204     MOVE MSG-KDTRTYP               TO MFS-KDTRTYP                        
000205     MOVE MSG-IDPFK                 TO MFS-IDPFK                          
000206     MOVE MFS-IDTRANS               TO W-IDTRANS                          
000207                                                                          
000208     MOVE LOW-VALUE                 TO MSG-AREA                           
000209     MOVE 'W6O135N1'                TO MFS-IDMOD                          
000210     MOVE '6135'                    TO MOD-IDTRANS                        
000211     MOVE MFS-RENSA-FAELT           TO MOD-TEMFSFEL MOD-TEMFSINF          
000212                                                                          
000213     IF EGEN-MID OR HELP-MID                                              
000214       CONTINUE                                                           
000215     ELSE                                                                 
000216       MOVE SPACE                   TO MFS-KDTRTYP                        
000217       MOVE '7'                     TO MFS-IDPFK                          
000218     END-IF                                                               
000219                                                                          
000220     PERFORM AA-INIT-NYCKLAR                                              
000221                                                                          
000222     IF MSGI-IDLAND-SPR = 'GB'                                            
000223       MOVE 'GB'                    TO MCNV-IDSPRAK                       
000224     ELSE                                                                 
000225       MOVE 'SV'                    TO MCNV-IDSPRAK                       
000226     END-IF                                                               
000227                                                                          
000228     MOVE '101'                     TO REQU-IDMSGVER                      
000229     MOVE MSGI-IDUSER               TO REQU-IDUSER                        
000230     MOVE MSGI-IDSPRAK              TO REQU-IDSPRAK                       
000231     .                                                                    
000232     EJECT                                                                
000233 AA-INIT-NYCKLAR SECTION.                                                 
000234                                                                          
000235     MOVE ALL '+' TO MSGI-WMSGINIT                                        
000236     MOVE '001'                  TO MSGI-KDCALL                           
000237     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
000238     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
000239     MOVE '6135'                 TO MSGI-IDTRANS                          
000240     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
000241     .                                                                    
000242     EJECT                                                                
000243 B-INIT-KEYS SECTION.                                                     
000244                                                                          
000245     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
000246                                                                          
000247     IF MID-IDDC-IN = ALL '+'                                             
000248        MOVE MSGI-IDDC    TO REQU-IDDC-KEY                                
000249     ELSE                                                                 
000250        MOVE MID-IDDC-IN  TO REQU-IDDC-KEY                                
000251     END-IF                                                               
000252                                                                          
000253     IF GODK-MID OR HELP-MID                                              
000254       PERFORM BA-FLYTTA-OEVRIGA-NYCKLAR                                  
000255     ELSE                                                                 
000256       MOVE MFS-RENSA-FAELT         TO MOD-ADINLOMR-PRT                   
000257                                       MOD-IDINLVGN-UT                    
000258                                       MOD-ADINLOMR-UT                    
000259                                       MOD-ADINLOMR-NXT-UT                
000260                                       MOD-KDINLQ-UT                      
000261                                       MOD-BEFT-FOM-UT                    
000262                                       MOD-BEFT-TOM-UT                    
000263                                       MOD-FLINLFB-UT                     
000264                                       MOD-IDLEVNR-KOLLI-UT               
000265                                       MOD-IDOKOLLI-UT                    
000266                                       MOD-IDLOPNRM-UT                    
000267                                       MOD-IDDC-UT                        
000268     END-IF                                                               
000269     .                                                                    
000270     EJECT                                                                
000271 BA-FLYTTA-OEVRIGA-NYCKLAR SECTION.                                       
000272                                                                          
000273     MOVE MID-FLGODK TO REQU-FLGODK                                       
000274                                                                          
000275     MOVE MFS-RENSA-FAELT           TO MOD-ADINLOMR-PRT                   
000276     IF MID-ADINLOMR-PRT = ALL '+'                                        
000277       CONTINUE                                                           
000278     ELSE                                                                 
000279       INSPECT MID-ADINLOMR-PRT REPLACING LEADING '+' BY ZERO             
000280       MOVE MID-ADINLOMR-PRT        TO MOD-ADINLOMR-PRT                   
000281     END-IF                                                               
000282                                                                          
000283     MOVE MFS-RENSA-FAELT           TO MOD-IDINLVGN-IN                    
000284     IF MID-IDINLVGN-IN = ALL '+'                                         
000285       MOVE MID-IDINLVGN-UT         TO MOD-IDINLVGN-UT                    
000286     ELSE                                                                 
000287       MOVE MID-IDINLVGN-IN         TO MOD-IDINLVGN-UT                    
000288     END-IF                                                               
000289                                                                          
000290     MOVE MFS-RENSA-FAELT           TO MOD-ADINLOMR-IN                    
000291     IF MID-ADINLOMR-IN = ALL '+'                                         
000292       MOVE MID-ADINLOMR-UT         TO MOD-ADINLOMR-UT                    
000293     ELSE                                                                 
000294       MOVE MID-ADINLOMR-IN         TO MOD-ADINLOMR-UT                    
000295     END-IF                                                               
000296                                                                          
000297     MOVE MFS-RENSA-FAELT           TO MOD-ADINLOMR-NXT-IN                
000298     IF MID-ADINLOMR-NXT-IN = ALL '+'                                     
000299       MOVE MID-ADINLOMR-NXT-UT     TO MOD-ADINLOMR-NXT-UT                
000300     ELSE                                                                 
000301       MOVE MID-ADINLOMR-NXT-IN     TO MOD-ADINLOMR-NXT-UT                
000302     END-IF                                                               
000303                                                                          
000304     MOVE MFS-RENSA-FAELT           TO MOD-KDINLQ-IN                      
000305     IF MID-KDINLQ-IN = ALL '+'                                           
000306       MOVE MID-KDINLQ-UT           TO MOD-KDINLQ-UT                      
000307     ELSE                                                                 
000308       MOVE MID-KDINLQ-IN           TO MOD-KDINLQ-UT                      
000309     END-IF                                                               
000310                                                                          
000311     MOVE MFS-RENSA-FAELT           TO MOD-BEFT-FOM-IN                    
000312     IF MID-BEFT-FOM-IN = ALL '+'                                         
000313       MOVE MID-BEFT-FOM-UT         TO MOD-BEFT-FOM-UT                    
000314     ELSE                                                                 
000315       MOVE MID-BEFT-FOM-IN         TO MOD-BEFT-FOM-UT                    
000316     END-IF                                                               
000317                                                                          
000318     MOVE MFS-RENSA-FAELT           TO MOD-BEFT-TOM-IN                    
000319     IF MID-BEFT-TOM-IN = ALL '+'                                         
000320       MOVE MID-BEFT-TOM-UT         TO MOD-BEFT-TOM-UT                    
000321     ELSE                                                                 
000322       MOVE MID-BEFT-TOM-IN         TO MOD-BEFT-TOM-UT                    
000323     END-IF                                                               
000324                                                                          
000325     MOVE MFS-RENSA-FAELT           TO MOD-FLINLFB-IN                     
000326     IF MID-FLINLFB-IN = ALL '+'                                          
000327       MOVE MID-FLINLFB-UT          TO MOD-FLINLFB-UT                     
000328     ELSE                                                                 
000329       MOVE MID-FLINLFB-IN          TO MOD-FLINLFB-UT                     
000330     END-IF                                                               
000331                                                                          
000332     MOVE MFS-RENSA-FAELT           TO MOD-IDLEVNR-KOLLI-IN               
000333     IF MID-IDLEVNR-KOLLI-IN = ALL '+'                                    
000334       MOVE MID-IDLEVNR-KOLLI-UT    TO WS-IDLEVNR-KOLLI                   
000335     ELSE                                                                 
000336       MOVE MID-IDLEVNR-KOLLI-IN    TO WS-IDLEVNR-KOLLI                   
000337     END-IF                                                               
000338     MOVE WS-IDLEVNR-KOLLI          TO MOD-IDLEVNR-KOLLI-UT               
000339*                                      REQU-IDLEVNR-KOLLI-KEY             
000340                                                                          
000341     MOVE MFS-RENSA-FAELT           TO MOD-IDOKOLLI-IN                    
000342     IF MID-IDOKOLLI-IN = ALL '+'                                         
000343       MOVE MID-IDOKOLLI-UT         TO WS-IDOKOLLI                        
000344     ELSE                                                                 
000345       MOVE MID-IDOKOLLI-IN         TO WS-IDOKOLLI                        
000346     END-IF                                                               
000347     MOVE WS-IDOKOLLI               TO MOD-IDOKOLLI-UT                    
000348*                                      REQU-IDOKOLLI-KEY                  
000349                                                                          
000350     MOVE MFS-RENSA-FAELT           TO MOD-IDLOPNRM-IN                    
000351     IF MID-IDLOPNRM-IN = ALL '+'                                         
000352       MOVE MID-IDLOPNRM-UT         TO WS-IDLOPNRM                        
000353     ELSE                                                                 
000354       MOVE MID-IDLOPNRM-IN         TO WS-IDLOPNRM                        
000355     END-IF                                                               
000356     MOVE WS-IDLOPNRM               TO MOD-IDLOPNRM-UT                    
000357*                                      REQU-IDLOPNRM-KEY                  
000358     MOVE ZERO                      TO WS-IDLOPNRM                        
000359     .                                                                    
000360     EJECT                                                                
000361 C-INIT-REQU SECTION.                                                     
000362                                                                          
000364                                                                          
000365     MOVE +1                  TO RAD-INDX  KOL-INDX                       
000366     PERFORM UNTIL RAD-INDX > MAX-KVRADER                                 
000367       PERFORM UNTIL KOL-INDX > MAX-KOLUMN                                
000368         IF MID-IDLEVNR-KOLLI(RAD-INDX, KOL-INDX) NOT = SPACES            
000369         AND MID-IDOKOLLI(RAD-INDX, KOL-INDX) NOT = SPACES                
000370           MOVE MID-IDLEVNR-KOLLI(RAD-INDX, KOL-INDX) TO                  
000371                REQU-IDLEVNR-KOLLI(RAD-INDX, KOL-INDX)                    
000372           MOVE MID-IDOKOLLI(RAD-INDX, KOL-INDX) TO                       
000373                REQU-IDOKOLLI(RAD-INDX, KOL-INDX)                         
000374         ELSE                                                             
000375           MOVE ALL '+' TO REQU-IDLEVNR-KOLLI(RAD-INDX, KOL-INDX)         
000376                           REQU-IDOKOLLI(RAD-INDX, KOL-INDX)              
000377         END-IF                                                           
000378         ADD +1                   TO KOL-INDX                             
000379       END-PERFORM                                                        
000380       ADD +1                     TO RAD-INDX                             
000381       MOVE +1                    TO KOL-INDX                             
000382     END-PERFORM                                                          
000383                                                                          
000384     .                                                                    
000385     EJECT                                                                
000386 E-SAMMA-SIDA SECTION.                                                    
000387                                                                          
000388     IF EGEN-MID OR HELP-MID                                              
000389       CONTINUE                                                           
000390     ELSE                                                                 
000391       PERFORM MFS-RENSA-FAELT-IN                                         
000392     END-IF                                                               
000393     .                                                                    
000394     EJECT                                                                
000395 F-CALL-BIZ-LOGIC-W6013510 SECTION.                                       
000396                                                                          
000397     CALL W6013510 USING REQU-AREA RESP-AREA MAX-KVRADER                  
000398                         6191-PCB INLA-PCB                                
000399                         INLA-CSEQ-PCB INLD-PCB WDB6-PCB                  
000400                         PMRK-INLB-PCB PMRK-INLC-PCB                      
000401                         PMRK-PLAA-PCB.                                   
000402                                                                          
000403     PERFORM FA-SET-MSG-AND-HILIGHT                                       
000404     IF NOT WRONG-KEY                                                     
000405       PERFORM FB-MOVE-RESP-TO-MOD                                        
000406     END-IF                                                               
000407     .                                                                    
000408     EJECT                                                                
000409 FA-SET-MSG-AND-HILIGHT SECTION.                                          
000410                                                                          
000411     MOVE RESP-IDMSG-ERROR TO WS-IDMSG-ERROR                              
000412                                                                          
000413     IF WRONG-KEY                                                         
000414       PERFORM MFS-RENSA-FAELT-IN                                         
000415       PERFORM MFS-RENSA-FAELT-UT                                         
000416     END-IF                                                               
000417                                                                          
000418     MOVE RESP-IDMSG-ERROR          TO MCNV-IDMSG-ERROR                   
000419     MOVE RESP-IDMSG-INFO           TO MCNV-IDMSG-INFO                    
000420     MOVE RESP-IDELMT-ERROR         TO MCNV-IDELMT-ERROR                  
000421                                                                          
000422     CALL WL01MCNV USING MCNV-AREA                                        
000423                                                                          
000424     MOVE MCNV-MFSINF               TO MOD-TEMFSINF                       
000425     MOVE MCNV-MFSFEL               TO MOD-TEMFSFEL                       
000426     .                                                                    
000427     EJECT                                                                
000428 FB-MOVE-RESP-TO-MOD SECTION.                                             
000429                                                                          
000430     MOVE RESP-IDDC                 TO MOD-IDDC-UT                        
000431     IF EGEN-MID                                                          
000432       IF RESP-IDLOPNRM = ALL '+'                                         
000433         MOVE MFS-RENSA-FAELT        TO MOD-IDLOPNRM-UT                   
000434       ELSE                                                               
000435         MOVE RESP-IDLOPNRM          TO MOD-IDLOPNRM-UT                   
000436       END-IF                                                             
000437                                                                          
000438       MOVE +1                        TO RAD-INDX KOL-INDX                
000439       PERFORM UNTIL RAD-INDX        >  MAX-KVRADER                       
000440         PERFORM UNTIL KOL-INDX      >  MAX-KOLUMN                        
000441           IF RESP-IDLEVNR-KOLLI-ATTR(RAD-INDX, KOL-INDX)                 
000442                                                        = ALL '+'         
000443             MOVE MFS-FORMATETS-ATTR                                      
000444                     TO MOD-IDLEVNR-KOLLI-ATTR(RAD-INDX, KOL-INDX)        
000445           ELSE                                                           
000446             MOVE RESP-IDLEVNR-KOLLI-ATTR(RAD-INDX, KOL-INDX)             
000447                     TO MOD-IDLEVNR-KOLLI-ATTR(RAD-INDX, KOL-INDX)        
000448           END-IF                                                         
000449                                                                          
000450           IF RESP-IDLEVNR-KOLLI(RAD-INDX, KOL-INDX) = SPACE              
000451             MOVE MFS-RENSA-FAELT                                         
000452                          TO MOD-IDLEVNR-KOLLI(RAD-INDX, KOL-INDX)        
000453           ELSE                                                           
000454             IF RESP-IDLEVNR-KOLLI(RAD-INDX, KOL-INDX) = ALL '+'          
000455               MOVE MFS-ROER-EJ-FAELT                                     
000456                          TO MOD-IDLEVNR-KOLLI(RAD-INDX, KOL-INDX)        
000457             ELSE                                                         
000458               MOVE RESP-IDLEVNR-KOLLI(RAD-INDX, KOL-INDX)                
000459                          TO MOD-IDLEVNR-KOLLI(RAD-INDX, KOL-INDX)        
000460             END-IF                                                       
000461           END-IF                                                         
000462                                                                          
000463           IF RESP-IDOKOLLI-ATTR(RAD-INDX, KOL-INDX) = ALL '+'            
000464             MOVE MFS-FORMATETS-ATTR                                      
000465                          TO MOD-IDOKOLLI-ATTR(RAD-INDX, KOL-INDX)        
000466           ELSE                                                           
000467             MOVE RESP-IDOKOLLI-ATTR(RAD-INDX, KOL-INDX)                  
000468                          TO MOD-IDOKOLLI-ATTR(RAD-INDX, KOL-INDX)        
000469           END-IF                                                         
000470                                                                          
000471           IF RESP-IDOKOLLI(RAD-INDX, KOL-INDX) = SPACE                   
000472             MOVE MFS-RENSA-FAELT TO                                      
000473                                 MOD-IDOKOLLI(RAD-INDX, KOL-INDX)         
000474           ELSE                                                           
000475             IF RESP-IDOKOLLI(RAD-INDX, KOL-INDX) = ALL '+'               
000476               MOVE MFS-ROER-EJ-FAELT                                     
000477                              TO MOD-IDOKOLLI(RAD-INDX, KOL-INDX)         
000478             ELSE                                                         
000479               MOVE RESP-IDOKOLLI(RAD-INDX, KOL-INDX)                     
000480                              TO MOD-IDOKOLLI(RAD-INDX, KOL-INDX)         
000481             END-IF                                                       
000482           END-IF                                                         
000483           ADD +1                   TO KOL-INDX                           
000484                                                                          
000485         END-PERFORM                                                      
000486         ADD +1                     TO RAD-INDX                           
000487         MOVE +1                    TO KOL-INDX                           
000488       END-PERFORM                                                        
000489     ELSE                                                                 
000490       PERFORM MFS-RENSA-FAELT-IN                                         
000491     END-IF                                                               
000492                                                                          
000493     IF RESP-FLGODK = SPACE                                               
000494        MOVE MFS-RENSA-FAELT        TO MOD-FLGODK                         
000495     ELSE                                                                 
000496        IF RESP-FLGODK = '+'                                              
000497           MOVE MFS-ROER-EJ-FAELT   TO MOD-FLGODK                         
000498        ELSE                                                              
000499           MOVE RESP-FLGODK         TO MOD-FLGODK                         
000500        END-IF                                                            
000501        MOVE RESP-FLGODK-ATTR    TO MOD-FLGODK-ATTR                       
000502     END-IF                                                               
000503     .                                                                    
000504     EJECT                                                                
000505 MFS-RENSA-FAELT-UT SECTION.                                              
000506                                                                          
000507*    --- ALLA UTDATA-FÄLT                                                 
000508     MOVE MFS-RENSA-FAELT           TO MOD-FLGODK                         
000509                                       MOD-ADINLOMR-UT                    
000510                                                                          
000511     MOVE +1                        TO RAD-INDX  KOL-INDX                 
000512     PERFORM UNTIL RAD-INDX > MAX-KVRADER                                 
000513       PERFORM UNTIL KOL-INDX > MAX-KOLUMN                                
000514         MOVE MFS-RENSA-FAELT       TO                                    
000515                             MOD-IDLEVNR-KOLLI(RAD-INDX, KOL-INDX)        
000516                             MOD-IDOKOLLI(RAD-INDX, KOL-INDX)             
000517         ADD +1                     TO KOL-INDX                           
000518       END-PERFORM                                                        
000519       ADD +1                       TO RAD-INDX                           
000520       MOVE +1                      TO KOL-INDX                           
000521     END-PERFORM                                                          
000522     .                                                                    
000523     EJECT                                                                
000524 MFS-RENSA-FAELT-IN SECTION.                                              
000525                                                                          
000526*    --- ALLA INDATA-FÄLT                                                 
000527     MOVE MFS-RENSA-FAELT           TO MOD-FLGODK                         
000528                                       MOD-ADINLOMR-UT                    
000529                                                                          
000530     MOVE +1                        TO RAD-INDX  KOL-INDX                 
000531     PERFORM UNTIL RAD-INDX > MAX-KVRADER                                 
000532       PERFORM UNTIL KOL-INDX > MAX-KOLUMN                                
000533         MOVE MFS-RENSA-FAELT       TO                                    
000534                             MOD-IDLEVNR-KOLLI(RAD-INDX, KOL-INDX)        
000535                             MOD-IDOKOLLI(RAD-INDX, KOL-INDX)             
000536         ADD +1                     TO KOL-INDX                           
000537       END-PERFORM                                                        
000538       ADD +1                       TO RAD-INDX                           
000539       MOVE +1                      TO KOL-INDX                           
000540     END-PERFORM                                                          
000541     .                                                                    
000542     EJECT                                                                
000543* --- IMS SEKTIONER ---                                                   
000544     SKIP3                                                                
000545 IMS-GET-MSG SECTION.                                                     
000546                                                                          
000547     MOVE '  QC' TO GODK-STATUSKODER                                      
000548     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
000549     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
000550     PERFORM IMS-STATUSKONTROLL                                           
000551     .                                                                    
000552     SKIP3                                                                
000553 IMS-INSERT-MSG SECTION.                                                  
000554                                                                          
000555     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
000556       MOVE '0' TO MFS-KDHUVOMR                                           
000557     END-IF                                                               
000558     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
000559     MOVE SPACE TO GODK-STATUSKODER                                       
000560     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
000561     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
000562     PERFORM IMS-STATUSKONTROLL                                           
000563     .                                                                    
000564     EJECT                                                                
000565 IMS-STATUSKONTROLL SECTION.                                              
000566                                                                          
000567     SET STATUS-IX TO 1                                                   
000568     SEARCH GODK-STATUS                                                   
000569       AT END                                                             
000570         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
000571         DELIMITED BY SIZE INTO FELTEXT                                   
000572         CALL FELLOG                                                      
000573       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
000574         CONTINUE                                                         
000575     END-SEARCH                                                           
000576     .                                                                    
