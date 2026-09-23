000001 ID DIVISION.                                                             
000002 PROGRAM-ID. W3010100.                                                    
000003*AUTHOR. S.OHLSSON - M-A EVERBÄCK.                                        
000004*DATE-WRITTEN. APRIL 79.                                                  
000005*    FUNKTION.                                                            
000006*        TP-PROGRAM FÖR MARKNADSFÖRING (PRISBILD).                        
000007*        PROGRAMMET LÄSER DATABASERNA WLBENA-WDD3 OCH WLARTC-WDK6.        
000008*        KDMORE LÄGGS UT OM MER ÄN EN TILLKOMMANDE                        
000009*        ARTIKEL FINNS.                                                   
000010*        ENDAST DE 2 FÖRSTA IDAO LÄGGS UT.                                
000011*    INDATA.                                                              
000012*        TRANSAKTION:  W3T101                                             
000013*        MID:  W3I10101                                                   
000014*    UTDATA.                                                              
000015*        MOD:  W3O10101                                                   
000016     EJECT                                                                
000017 ENVIRONMENT DIVISION.                                                    
000018     SKIP3                                                                
000019 DATA DIVISION.                                                           
000020     EJECT                                                                
000021 WORKING-STORAGE SECTION.                                                 
000022                                                                          
000023*    -- CHECKED BY WY2000                                                 
000024 77  IDPGM               PIC X(8)    VALUE 'W3010100'.                    
000025     SKIP2                                                                
000026*****  IDARTNR ÄR NYCKELFÄLT ****                                         
000027 77      IDARTNR-WS      PIC X(9).                                        
000028 77      INDX            PIC S9(9)   VALUE ZERO  COMP SYNC.               
000029 77      IDLEVNR         PIC  X(5)   VALUE SPACE.                         
000030 77      MAX-MOD-LENGD   PIC S9(4)   VALUE +289  COMP SYNC.               
000031 77      JA              PIC X       VALUE 'J'.                           
000032 77      NEJ             PIC X       VALUE 'N'.                           
000033 77      ARTIKEL-RETT    PIC X.                                           
000034 77      ARTIKEL-FINNS   PIC X.                                           
000035 77      WS-PRARTBES     PIC X       VALUE 'N'.                           
000036 77      DEFINITIV       PIC S9      VALUE +1 COMP-3.                     
000037 77      DAGENS-AAAAMMDD PIC 9(8)    VALUE ZERO.                          
000038     SKIP3                                                                
000039                                                                          
000040 01  DYNAMISKA-SUBPROGRAM.                                                
000041     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000042     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000043     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
000044                                                                          
000045 01      NYCKLAR-TILL-DLI.                                                
000046   03    W-IDARTNR-X.                                                     
000047     05  W-IDARTNR       PIC S9(9)   COMP-3.                              
000048   03    W-IDSKYLT-X.                                                     
000049     05  W-IDSKYLT       PIC X(3)    VALUE 'S  '.                         
000050                                                                          
000051   03  W-DAPRLIST-X.                                                      
000052     05  W-DAPRLIST      PIC   9(8)  VALUE ZERO.                          
000053     SKIP3                                                                
000054 01  FELMEDD.                                                             
000055   03    FEL-1           PIC X(26)   VALUE 'PART NUMBER IS NOT NUM        
000056-                                          'ERIC'.                        
000057   03    FEL-2           PIC X(35)   VALUE 'THIS ARTICLE IS NOT IN        
000058-                                          ' THE DATABASE'.               
000059   03    FEL-3           PIC X(29)   VALUE 'THIS ARTICLE HAS BEEN         
000060-                                          'DELETED'.                     
000061     SKIP3                                                                
000062*****  JUSTSTERING AV BENÄMNINGARNA                                       
000063 01  BENAMNING.                                                           
000064   03    SKRAP           PIC X(5).                                        
000065   03    BEN20           PIC X(20).                                       
000066     SKIP2                                                                
000067 01  REDIGERAD-BEN.                                                       
000068   03    RED-BEN20       PIC X(20).                                       
000069   03    RED-SKRAP       PIC X(5).                                        
000070     EJECT                                                                
000071*                    **** PARAMETRAR TILL W005INIT                        
000072*01  -COPY WMSGINIT                                                       
000073     EJECT                                                                
000074*****  TP-AREOR                                                           
000075*                                                                         
000076*****  MIDEN                                                              
000077*01      MID -COPY W3I10101 -PRE MID-.                                    
000078     SKIP3                                                                
000079*01  -COPY WMSGAREA.                                                      
000080     SKIP3                                                                
000081*****  MODEN                                                              
000082*    03  MOD -COPY W3O10101 -PRE MOD- -RED MSG-AREA.                      
000083     SKIP3                                                                
000084*01  -COPY WMFSAREA.                                                      
000085     EJECT                                                                
000086*****  ARBETSAREOR FÖR IMS-SEKTIONERNA                                    
000087*                                                                         
000088 01  IMS-WS.                                                              
000089   03    FILLER          PIC X(8)    VALUE 'IMS-WS  '.                    
000090     SKIP2                                                                
000091*****  STATUSKOD FRÅN IMS                                                 
000092   03    STATUS-WS       PIC XX.                                          
000093     88  SEGMENT-FINNS               VALUE '  '.                          
000094     88  SEGMENT-SAKNAS              VALUE 'GE'.                          
000095     SKIP2                                                                
000096   03    GODK-STATUSKODER.                                                
000097     05  GODK-STATUS  OCCURS 5  INDEXED BY STATUS-IX  PIC XX.             
000098     SKIP3                                                                
000099*****  IMS-FUNKTIONSKODER                                                 
000100*  03    -COPY W0003                                                      
000101     SKIP3                                                                
000102     03  SSA1            PIC X(40).                                       
000103     03  SSA2            PIC X(40).                                       
000104     EJECT                                                                
000105 01  DLI-IO-AREA         PIC X(900)  VALUE SPACE.                         
000106     SKIP3                                                                
000107*01  WLARTC01 -COPY WDK601 -PRE ARTC01- -RED DLI-IO-AREA.                 
000108     EJECT                                                                
000109*01  WLARTC11 -COPY WDK611 -PRE ARTC11- -RED DLI-IO-AREA.                 
000110     EJECT                                                                
000111*01  WLBENA11 -COPY WDD311 -PRE BEN-  -RED DLI-IO-AREA.                   
000112                                                                          
000113 01  DLI-IO-AREA-WDK621.                                                  
000114*    03  WLARTC11 -COPY WDK621                                            
000115     EJECT                                                                
000116     EJECT                                                                
000117 LINKAGE SECTION.                                                         
000118*01  -COPY  W0009  -PRE MSG-                                              
000119     EJECT                                                                
000120*01  -COPY  W0008  -PRE USEA-                                             
000121       05  FILLER          PIC X.                                         
000122     EJECT                                                                
000123*01  -COPY W0008  -PRE ARTC-                                              
000124       05  FILLER          PIC X.                                         
000125     EJECT                                                                
000126*01  -COPY W0008  -PRE BEN-                                               
000127       05  FILLER         PIC X.                                          
000128     EJECT                                                                
000129 PROCEDURE DIVISION USING MSG-PCB USEA-PCB ARTC-PCB BEN-PCB.              
000130     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB ARTC-PCB BEN-PCB              
000131     SKIP2                                                                
000132     PERFORM IMS-GET-MSG                                                  
000133     IF SEGMENT-FINNS                                                     
000134       PERFORM A-KOLLA-NYCKLAR                                            
000135       IF ARTIKEL-RETT = JA                                               
000136         PERFORM B-ARTIKEL-FINNS                                          
000137         IF ARTIKEL-FINNS = JA                                            
000138           PERFORM C-INFORMATIONSBILD                                     
000139         END-IF                                                           
000140       END-IF                                                             
000141       PERFORM IMS-ISRT-MSG                                               
000142     END-IF                                                               
000143     MOVE ZERO TO RETURN-CODE                                             
000144     GOBACK                                                               
000145     .                                                                    
000146     EJECT                                                                
000147 A-KOLLA-NYCKLAR SECTION.                                                 
000148     SKIP2                                                                
000149     IF MSG-DUBBLA-TRANSKODER                                             
000150       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W3I10101                 
000151       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
000152       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
000153     ELSE                                                                 
000154       MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W3I10101                   
000155       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
000156       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
000157     END-IF                                                               
000158     SKIP2                                                                
000159                                                                          
000160     MOVE ALL '+' TO MSGI-WMSGINIT                                        
000161     MOVE '001'             TO MSGI-KDCALL                                
000162     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
000163     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
000164     MOVE '3101'            TO MSGI-IDTRANS                               
000165     IF MFS-IDTRANS = '3101'                                              
000166     OR (MID-IDARTNR-IN NUMERIC                                           
000167     AND MID-IDARTNR-IN > ZERO)                                           
000168         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
000169     END-IF                                                               
000170     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
000171     MOVE MSGI-IDARTNR TO IDARTNR-WS                                      
000172     INSPECT IDARTNR-WS REPLACING ALL SPACE BY ZERO                       
000173                                                                          
000174     IF IDARTNR-WS NUMERIC                                                
000175       MOVE JA TO ARTIKEL-RETT                                            
000176     ELSE                                                                 
000177       MOVE NEJ TO ARTIKEL-RETT                                           
000178       MOVE FEL-1 TO MOD-MESSAGE                                          
000179     END-IF                                                               
000180     SKIP2                                                                
000181     MOVE LOW-VALUE TO MOD-W3O10101                                       
000182     MOVE 'W3O101N1' TO MFS-IDMOD                                         
000183     MOVE '3101' TO MOD-TRANS-NUMMER                                      
000184     SKIP1                                                                
000185     MOVE IDARTNR-WS TO MOD-IDARTNR-UT                                    
000186     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
000187     SKIP1                                                                
000188     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
000189     SKIP1                                                                
000190     MOVE MAX-MOD-LENGD TO MSG-KVLL                                       
000191     .                                                                    
000192     EJECT                                                                
000193*****************************************************************         
000194*    KONTROLL ATT ARTIKELN FINNS PÅ WLARTC-WDK6 ARTIKELREG                
000195*                                                                         
000196 B-ARTIKEL-FINNS SECTION.                                                 
000197     SKIP1                                                                
000198     MOVE NEJ TO ARTIKEL-FINNS                                            
000199     MOVE IDARTNR-WS TO W-IDARTNR                                         
000200     SKIP2                                                                
000201     PERFORM IMS-GET-ARTIKEL-WDK601                                       
000202     IF SEGMENT-FINNS                                                     
000203       MOVE JA TO ARTIKEL-FINNS                                           
000204       MOVE '-' TO MOD-BINDESTRECK                                        
000205       MOVE ARTC01-ART-TIFINLV TO MOD-TIFINLV                             
000206       MOVE ARTC01-ART-TIERSDAT TO MOD-TIERSDAT                           
000207       MOVE ARTC01-ART-KDERS-UTG TO MOD-KDERS                             
000208       MOVE ARTC01-ART-IDAO(1) TO MOD-IDAO1                               
000209       MOVE ARTC01-ART-IDAO(2) TO MOD-IDAO2                               
000210       MOVE ARTC01-ART-IDLEVNR TO MOD-IDLEVNR                             
000211       MOVE ARTC01-ART-REKSIFFR TO MOD-REKSIFFR                           
000212       MOVE ARTC01-ART-KDPRODSL TO MOD-KDPRODSL                           
000213       MOVE ARTC01-ART-KDSORT TO MOD-KDSORT                               
000214       MOVE ARTC01-ART-IDFKNGRP TO MOD-IDFKNGRP                           
000215       IF ARTC01-ART-KDERS-UTG > 0                                        
000216         MOVE FEL-3 TO MOD-MESSAGE                                        
000217       END-IF                                                             
000218       IF ARTC01-ART-FLIART = 'J'                                         
000219         MOVE 'Y' TO MOD-KDIART                                           
000220       ELSE                                                               
000221         MOVE 'N' TO MOD-KDIART                                           
000222       END-IF                                                             
000223     ELSE                                                                 
000224       MOVE FEL-2 TO MOD-MESSAGE                                          
000225     END-IF                                                               
000226     .                                                                    
000227     EJECT                                                                
000228****************************************************************          
000229*    INFORMATIONSVÄRDEN PÅ ÖNSKAT ARTIKELNR LÄGGS UT                      
000230 C-INFORMATIONSBILD SECTION.                                              
000231*                                                                         
000232     MOVE ZERO              TO MOD-KDPRTILL                               
000233     MOVE 1                 TO MOD-FLSPECPR                               
000234     MOVE 'GB ' TO W-IDSKYLT                                              
000235     PERFORM IMS-GU-BEN                                                   
000236     IF SEGMENT-FINNS                                                     
000237       MOVE BEN-TEXT-BEART TO BENAMNING                                   
000238       IF SKRAP = SPACE                                                   
000239         MOVE SPACE TO RED-SKRAP                                          
000240         MOVE BEN20 TO RED-BEN20                                          
000241         MOVE REDIGERAD-BEN TO MOD-BEART-ENG                              
000242       ELSE                                                               
000243         MOVE BENAMNING TO MOD-BEART-ENG                                  
000244       END-IF                                                             
000245     ELSE                                                                 
000246       MOVE SPACE TO MOD-BEART-ENG                                        
000247     END-IF                                                               
000248     MOVE 'S  ' TO W-IDSKYLT                                              
000249     PERFORM IMS-GU-BEN                                                   
000250     IF SEGMENT-FINNS                                                     
000251       MOVE BEN-TEXT-BEART TO BENAMNING                                   
000252       IF SKRAP = SPACE                                                   
000253         MOVE SPACE TO RED-SKRAP                                          
000254         MOVE BEN20 TO RED-BEN20                                          
000255         MOVE REDIGERAD-BEN TO MOD-BEART-SVE                              
000256       ELSE                                                               
000257         MOVE BENAMNING TO MOD-BEART-SVE                                  
000258       END-IF                                                             
000259     ELSE                                                                 
000260       MOVE SPACE TO MOD-BEART-SVE                                        
000261     END-IF                                                               
000262*                                                                         
000263     PERFORM IMS-GET-SEGMENT-WDK611                                       
000264     IF SEGMENT-FINNS                                                     
000265       MOVE ARTC11-CLAG-KDVVKL TO MOD-KDVVKL                              
000266       MOVE ARTC11-CLAG-KDHF TO MOD-KDHF                                  
000267       MOVE ARTC11-CLAG-IDANSK TO MOD-IDANSK                              
000268       MOVE ARTC11-CLAG-KVPB-SEP  TO MOD-KVPB-SEP                         
000269       MOVE ARTC11-CLAG-TIAVIDAT-SEN TO MOD-TIAVIDAT-SEN                  
000270       MOVE ARTC11-CLAG-PRHEMTAG  TO MOD-PRHEMTAG                         
000271       MOVE ARTC11-CLAG-PROVRPAL  TO MOD-PROVRPAL                         
000272       MOVE ARTC11-CLAG-PRARTSTD  TO MOD-PRARTSTD                         
000273       MOVE ARTC11-CLAG-PRARTSJK  TO MOD-PRARTSJK                         
000274       MOVE ARTC11-CLAG-KDVTH     TO MOD-KDVTH                            
000275       MOVE ARTC11-CLAG-KDTIPPR   TO MOD-KDTIPPR                          
000276       MOVE ARTC11-CLAG-PRDIRLON  TO MOD-PRDIRLON                         
000277       MOVE ARTC11-CLAG-KVQPACK-1 TO MOD-KVQPACK-1                        
000278       MOVE ARTC11-CLAG-KDLTK     TO MOD-KDLTK                            
000279       MOVE ARTC11-CLAG-VKART     TO MOD-VKART                            
000280       MOVE ARTC11-CLAG-KDSRA     TO MOD-KDSRA                            
000281       MOVE ARTC11-CLAG-PRDMTRL   TO MOD-PRDMTRL                          
000282       MOVE ARTC11-CLAG-KDERS     TO MOD-KDERS                            
000283       MOVE ARTC11-CLAG-KDPSLLOC  TO MOD-KDPSLLOC                         
000284       PERFORM CA-SPARRKOD                                                
000285       PERFORM CB-BASPRIS                                                 
000286     END-IF                                                               
000287     .                                                                    
000288 CA-SPARRKOD SECTION.                                                     
000289     SKIP1                                                                
000290     IF ARTC11-CLAG-KDLEVSP = +20 OR +21                                  
000291       MOVE 2 TO MOD-KDSPARR                                              
000292     ELSE                                                                 
000293       IF ARTC11-CLAG-KDLEVSP = +22                                       
000294         MOVE 0 TO MOD-KDSPARR                                            
000295       ELSE                                                               
000296         IF ARTC11-CLAG-FLLSRDEL = 'N'                                    
000297           MOVE 3 TO MOD-KDSPARR                                          
000298         ELSE                                                             
000299           IF ARTC11-CLAG-KDUART = 'S'                                    
000300             MOVE 4 TO MOD-KDSPARR                                        
000301           ELSE                                                           
000302             IF ARTC11-CLAG-KDUART = 'M'                                  
000303               MOVE 6 TO MOD-KDSPARR                                      
000304             ELSE                                                         
000305               MOVE 0 TO MOD-KDSPARR                                      
000306             END-IF                                                       
000307           END-IF                                                         
000308         END-IF                                                           
000309       END-IF                                                             
000310     END-IF                                                               
000311     .                                                                    
000312 CB-BASPRIS  SECTION.                                                     
000313     SKIP1                                                                
000314     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-AAAAMMDD                   
000315     COMPUTE W-DAPRLIST = 99999999 - DAGENS-AAAAMMDD                      
000316     PERFORM IMS-GNP-WDK621                                               
000317     IF SEGMENT-SAKNAS                                                    
000318        MOVE ARTC11-CLAG-PRARTSTD  TO MOD-PRARTBES                        
000319     ELSE                                                                 
000320       MOVE NEJ                 TO WS-PRARTBES                            
000321       PERFORM UNTIL  SEGMENT-SAKNAS                                      
000322         IF PRL-SUINLEV-PR > ZERO                                         
000323           MOVE PRL-PRARTBES-PR  TO MOD-PRARTBES                          
000324           SET SEGMENT-SAKNAS TO TRUE                                     
000325         ELSE                                                             
000326           IF WS-PRARTBES = NEJ                                           
000327             MOVE PRL-PRARTBES-PR TO MOD-PRARTBES                         
000328             MOVE JA              TO WS-PRARTBES                          
000329           END-IF                                                         
000330           PERFORM IMS-GNP-WDK621                                         
000331         END-IF                                                           
000332       END-PERFORM                                                        
000333     END-IF                                                               
000334     .                                                                    
000335     EJECT                                                                
000336*****  IMS SEKTIONER                                                      
000337*                                                                         
000338*****  CALL MOT MSG                                                       
000339 IMS-GET-MSG SECTION.                                                     
000340     MOVE '  QC' TO GODK-STATUSKODER                                      
000341     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
000342     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
000343     PERFORM IMS-STATUS-KONTROLL                                          
000344     .                                                                    
000345     SKIP3                                                                
000346 IMS-ISRT-MSG SECTION.                                                    
000347     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
000348       MOVE '0' TO MFS-KDHUVOMR                                           
000349     END-IF                                                               
000350     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
000351     MOVE SPACE TO GODK-STATUSKODER                                       
000352     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
000353     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
000354     PERFORM IMS-STATUS-KONTROLL                                          
000355     .                                                                    
000356     EJECT                                                                
000357 IMS-GET-ARTIKEL-WDK601 SECTION.                                          
000358     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
000359     DELIMITED BY SIZE INTO SSA1                                          
000360     MOVE '  GE' TO GODK-STATUSKODER                                      
000361     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
000362     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
000363     PERFORM IMS-STATUS-KONTROLL                                          
000364     .                                                                    
000365     EJECT                                                                
000366 IMS-GET-SEGMENT-WDK611 SECTION.                                          
000367     MOVE 'WLARTC11 ' TO SSA1                                             
000368     MOVE '  GE' TO GODK-STATUSKODER                                      
000369     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1                     
000370     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
000371     PERFORM IMS-STATUS-KONTROLL                                          
000372     .                                                                    
000373     EJECT                                                                
000374 IMS-GNP-WDK621 SECTION.                                                  
000375                                                                          
000376     STRING 'WLARTC21(DAPRLIST=>' W-DAPRLIST-X ')'                        
000377       DELIMITED BY SIZE INTO SSA1                                        
000378     MOVE '  GE' TO GODK-STATUSKODER                                      
000379     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA-WDK621 SSA1              
000380     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
000381     PERFORM IMS-STATUS-KONTROLL                                          
000382     .                                                                    
000383     EJECT                                                                
000384 IMS-GU-BEN  SECTION.                                                     
000385                                                                          
000386     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X  ')'                        
000387     DELIMITED BY SIZE INTO SSA1                                          
000388     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X  ')'                        
000389     DELIMITED BY SIZE INTO SSA2                                          
000390     MOVE '  GE' TO GODK-STATUSKODER                                      
000391     CALL CBLTDLI USING GU BEN-PCB DLI-IO-AREA SSA1 SSA2                  
000392     MOVE BEN-STATUS-CODE TO STATUS-WS                                    
000393     PERFORM IMS-STATUS-KONTROLL                                          
000394     .                                                                    
000395     SKIP3                                                                
000396 IMS-STATUS-KONTROLL SECTION.                                             
000397     SET STATUS-IX TO 1                                                   
000398     SEARCH GODK-STATUS                                                   
000399       AT END                                                             
000400         CALL FELLOG                                                      
000401     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                    
000402     END-SEARCH                                                           
000403     .                                                                    
