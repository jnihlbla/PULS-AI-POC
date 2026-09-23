000001 PROCESS DYNAM                                                            
000002 ID DIVISION.                                                             
000003     SKIP2                                                                
000004 PROGRAM-ID.     W488PRMA.                                                
000005 AUTHOR.         HÅKAN BOHLIN.                                            
000006 DATE-WRITTEN.   MAJ 2023.                                                
000007 DATE-COMPILED.                                                           
000008                                                                          
000009*    FUNCTION.                                                            
000010*                                                                         
000011*        W488PRMA SENDS EVENT TRANSACTION TO SYNQ VIA AN API.             
000012*                 PRODUCT MASTER INFO ARE SENT.                           
000013*                 KDCALL:                                                 
000014*                 001=CREATE NEW PART IN SYNQ.                            
000015*                     ALWAYS 001 WHEN INITIAL LOAD.                       
000016*                 002=CREATE NEW PART IN SYNQ FROM                        
000017                      6169 AND WL0104.                                    
000018*                 003=UPDATE EXISTING PART IN SYNQ FROM                   
000019                      6169 AND WL0104.                                    
000020*                                                                         
000021*    LÄNKAREA :       W488PRMA                                            
000022*                                                                         
000023*                                                                         
000024 DATA DIVISION.                                                           
000025                                                                          
000026 WORKING-STORAGE SECTION.                                                 
000027     SKIP3                                                                
000028 77    IDPGM                     PIC X(8)   VALUE 'W488PRMA'.             
000029 77    W-ADDISPABS-ATAB          PIC X(50)                                
000030                           VALUE 'APIOUT.SYNQ.PRODUCTMASTER'.             
000031 77    W-ADDISPABS-CRE           PIC X(50)                                
000032                           VALUE 'APIOUT.SYNQ.PRODUCTMASTCRE'.            
000033 77    W-ADDISPABS-UPD           PIC X(50)                                
000034                           VALUE 'APIOUT.SYNQ.PRODUCTMASTUPD'.            
000035 77    ERRORTEXT                 PIC X(64)  VALUE SPACE.                  
000036 77    CURRENT-SECTION           PIC X(30)  VALUE SPACE.                  
000037 77    IMS-SECTION               PIC X(30)  VALUE SPACE.                  
000038 77    YES                       PIC X      VALUE 'J'.                    
000039 77    NEJ                       PIC X      VALUE 'N'.                    
000040 77    KDRC-DISPLAY              PIC Z(5).                                
000041 77    WS-BEART                  PIC X(25).                               
000042 77    WS-KDSORT                 PIC X(2).                                
000043 77    WS-VLARTNTO               PIC 9(8)V9(1).                           
000044 77    WS-SEND-IDCOM             PIC S9(9)   COMP VALUE +0.               
000045 77    WS-TRUE                   PIC X       VALUE X'01'.                 
000046*                                                                         
000047 01  API-RESPONSE                PIC X(9999).                             
000048                                                                          
000049 01  WS-IDARTNR                  PIC Z(8)9.                               
000050                                                                          
000051 01  INCL-PART-SW                PIC X(1).                                
000052     88 INCL-PART-OK                        VALUE 'J'.                    
000053                                                                          
000054 01  DYNAMIC-SUBPROGRAM.                                                  
000055     03 CBLTDLI                  PIC X(8)   VALUE 'CBLTDLI '.             
000056     03 FELLOG                   PIC X(8)   VALUE 'FELLOG  '.             
000057     03 WZ01SEND                 PIC X(8)   VALUE 'WZ01SEND'.             
000058     03 WZ01CALL                 PIC X(8)   VALUE 'WZ01CALL'.             
000059     EJECT                                                                
000060*                                                                         
000061*    --- PARAMETERS TO WZ01SEND FOR SYNQ                                  
000062 01  FILLER                     PIC X(16) VALUE 'WZ01SEND-SYNQ'.          
000063*01  -COPY WZ01SEND                                                       
000064*                                                                         
000065*    --- PARAMETERS TO WZ01CALL FOR SYNQ                                  
000066 01  FILLER                     PIC X(16) VALUE 'WZ01CALL-SYNQ'.          
000067*01  -COPY WZ01CALL                                                       
000068                                                                          
000069*API INFO FILE                                                            
000070*01  -COPY WAPIINFO                                                       
000071                                                                          
000072 01  FILLER                     PIC X(16) VALUE 'SYNQ-POST'.              
000073 01  SY03-AREA.                                                           
000074*    03  -COPY WSY03Q01                                                   
000075                                                                          
000076 01  FILLER                     PIC X(16) VALUE 'SYNQ-PUT'.               
000077 01  SY04-AREA.                                                           
000078*    03  -COPY WSY04Q01                                                   
000079*                                                                         
000080     EJECT                                                                
000081***************************************************************           
000082 01    NYCKLAR-TILL-DLI.                                                  
000083*                                                                         
000084     03 W-WDGXKEY-0103-X.                                                 
000085        05  W-IDHTYP-0103       PIC X(4)    VALUE '0103'.                 
000086        05  FILLER              PIC X(26)   VALUE LOW-VALUE.              
000087     03 W-KY0104-X.                                                       
000088        05  W-ADDISPABS         PIC X(50).                                
000089*                                                                         
000090     03  W-WDD3BSEQ-X.                                                    
000091         05  W-D3BSEQ-IDARTNR    PIC  S9(9) COMP-3.                       
000092*                                                                         
000093     03  W-IDSKYLT-X             PIC X(3).                                
000094*                                                                         
000095     03  W-IDARTNR-X.                                                     
000096         05  W-IDARTNR           PIC S9(9) VALUE ZERO COMP-3.             
000097*                                                                         
000098     03  W-KDSEGKEY-X.                                                    
000099         05  W-KDSEGKEY          PIC X(1)  VALUE '1'.                     
000100*                                                                         
000101*    WS-AREAS TO IMS-SECTIONS                                             
000102*                                                                         
000103 01  IMS-WS.                                                              
000104   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
000105   03    STATUS-WS               PIC XX.                                  
000106     88    SEGMENT-FOUND                     VALUE '  '.                  
000107     88    SEGMENT-MISSING                   VALUE 'GE'.                  
000108     88    SEGMENT-END                       VALUE 'GB'.                  
000109     SKIP3                                                                
000110   03    GODK-STATUSKODER.                                                
000111     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
000112     SKIP3                                                                
000113 01    SSA1                      PIC X(128).                              
000114 01    SSA2                      PIC X(128).                              
000115     EJECT                                                                
000116*                            IMS FUNKTIONSKODER                           
000117*01  -COPY W0003                                                          
000118     EJECT                                                                
000119*    ---  DLI INPUT-OUTPUT AREA                                           
000120 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR501'.                      
000121 01  DLI-IO-WDR501.                                                       
000122*    03   -COPY WDGX01                                                    
000123 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX0104'.                    
000124 01  DLI-IO-WDGX0104.                                                     
000125*    03   -COPY WDGX0104                                                  
000126                                                                          
000127 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD301'.                      
000128 01  DLI-IO-WDD301.                                                       
000129*    03  -COPY WDD301                                                     
000130 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD311'.                      
000131 01  DLI-IO-WDD311.                                                       
000132*    03  -COPY WDD311                                                     
000133                                                                          
000134 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
000135 01  DLI-IO-WDK601.                                                       
000136*    03  -COPY WDK601                                                     
000137 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
000138 01  DLI-IO-WDK611.                                                       
000139*    03  -COPY WDK611                                                     
000140     EJECT                                                                
000141                                                                          
000142 LINKAGE SECTION.                                                         
000143*                                                                         
000144*    -COPY W488PRMA                                                       
000145     EJECT                                                                
000146*01    -COPY W0008     -PRE SYNQ-ATAB-                                    
000147     05  FILLER                  PIC X.                                   
000148     EJECT                                                                
000149*01    -COPY W0008     -PRE SYNQ-WDK6-                                    
000150     05  FILLER                  PIC X.                                   
000151     EJECT                                                                
000152*01    -COPY W0008     -PRE SYNQ-WDD3-                                    
000153     05  FILLER                  PIC X.                                   
000154     EJECT                                                                
000155                                                                          
000156 PROCEDURE DIVISION USING SYNQ-W488PRMA SYNQ-ATAB-PCB                     
000157                          SYNQ-WDK6-PCB SYNQ-WDD3-PCB.                    
000158 MAIN SECTION.                                                            
000159                                                                          
000160     PERFORM A-INIT                                                       
000161                                                                          
000162     PERFORM IMS-GU-WDK601                                                
000163     IF SEGMENT-FOUND                                                     
000164       PERFORM IMS-GNP-WDK611                                             
000165       IF SEGMENT-FOUND                                                   
000166         PERFORM B-CHECK-EXCL-PARTS                                       
000167         IF INCL-PART-OK                                                  
000168           PERFORM C-FILL-SENDAREA                                        
000169           IF SYNQ-KDCALL = 001                                           
000170             PERFORM E-SEND-SYNQ                                          
000171           ELSE                                                           
000172             PERFORM D-SEND-SYNQ                                          
000173           END-IF                                                         
000174         END-IF                                                           
000175       END-IF                                                             
000176     END-IF                                                               
000177     MOVE ZERO TO RETURN-CODE                                             
000178     GOBACK                                                               
000179     .                                                                    
000180     EJECT                                                                
000181 A-INIT SECTION.                                                          
000182                                                                          
000183     MOVE SYNQ-IDARTNR TO W-IDARTNR                                       
000184                          W-D3BSEQ-IDARTNR                                
000185     MOVE 'GB'         TO W-IDSKYLT-X.                                    
000186     .                                                                    
000187     SKIP2                                                                
000188 B-CHECK-EXCL-PARTS SECTION.                                              
000189     MOVE 'B-CHECK-EXCL-PARTS'    TO CURRENT-SECTION                      
000190     MOVE YES TO INCL-PART-SW                                             
000191     IF ART-KDERS-UTG  > ZERO OR                                          
000192        CLAG-KDERS     = 52   OR                                          
000193        ART-IDFKNGRP  = ZERO OR                                           
000194        ART-KDPRODSL  = ZERO OR                                           
000195        CLAG-PRARTSTD  = ZERO OR                                          
000196        CLAG-VLARTNTO  = ZERO OR                                          
000197       (CLAG-KDFARLIG = 2 OR 4) OR                                        
000198       ((CLAG-KDFARLIG NOT = 6) AND                                       
000199        (CLAG-IDPSN > ZERO)) OR                                           
000200       ((CLAG-KDFARLIG = 6) AND                                           
000201        (CLAG-IDPSN = 900 OR 910))                                        
000202       MOVE NEJ TO INCL-PART-SW                                           
000203     END-IF                                                               
000204     .                                                                    
000205     EJECT                                                                
000206                                                                          
000207 C-FILL-SENDAREA SECTION.                                                 
000208     MOVE 'C-FILL-SENDAREA'       TO CURRENT-SECTION                      
000209                                                                          
000210     MOVE ART-IDARTNR             TO WS-IDARTNR                           
000211     MOVE ART-KDSORT              TO WS-KDSORT                            
000212     MOVE CLAG-VLARTNTO           TO WS-VLARTNTO                          
000213     PERFORM IMS-GU-WDD311                                                
000214     IF SEGMENT-FOUND                                                     
000215       MOVE TEXT-BEART            TO WS-BEART                             
000216     ELSE                                                                 
000217       MOVE SPACE                 TO WS-BEART                             
000218     END-IF                                                               
000219                                                                          
000220     IF SYNQ-KDCALL = 001 OR 002                                          
000221       PERFORM CA-FILL-CRE-SENDAREA                                       
000222     ELSE                                                                 
000223       PERFORM CB-FILL-UPD-SENDAREA                                       
000224     END-IF                                                               
000225                                                                          
000226     .                                                                    
000227     EJECT                                                                
000228 CA-FILL-CRE-SENDAREA SECTION.                                            
000229     MOVE 'CA-FILL-CRE-SENDAREA'   TO CURRENT-SECTION                     
000230                                                                          
000231     MOVE FUNCTION TRIM (WS-IDARTNR) TO productId of SY03-AREA            
000232     COMPUTE productId-length of SY03-AREA =                              
000233             FUNCTION BYTE-LENGTH (                                       
000234             FUNCTION TRIM (productId of SY03-AREA))                      
000235                                                                          
000236     MOVE 'Volvo'    TO Xowner of SY03-AREA                               
000237     MOVE 5          TO Xowner-length of SY03-AREA                        
000238                                                                          
000239                                                                          
000240     MOVE 1          TO description-num of SY03-AREA                      
000241     MOVE FUNCTION TRIM (WS-BEART) TO description2 of SY03-AREA           
000242     COMPUTE description2-length of SY03-AREA =                           
000243             FUNCTION BYTE-LENGTH (                                       
000244             FUNCTION TRIM (description2 of SY03-AREA))                   
000245                                                                          
000246     MOVE 1          TO productUom2-num of SY03-AREA                      
000247                                                                          
000248     MOVE FUNCTION TRIM (WS-KDSORT) TO uomId of SY03-AREA(1)              
000249     COMPUTE uomId-length of SY03-AREA(1) =                               
000250             FUNCTION BYTE-LENGTH (                                       
000251             FUNCTION TRIM (uomId of SY03-AREA(1)))                       
000252                                                                          
000253     MOVE ZERO               TO imagePath2-length of SY03-AREA(1)         
000254                                imagePath-num of SY03-AREA(1)             
000255     MOVE SPACES             TO imagePath2 of SY03-AREA(1)                
000256                                                                          
000257     MOVE 1                  TO ratio of SY03-AREA(1)                     
000258                                                                          
000259     MOVE WS-TRUE            TO baseUomFlag of SY03-AREA(1)               
000260     MOVE WS-TRUE            TO pickUomFlag of SY03-AREA(1)               
000261     MOVE WS-TRUE            TO putawayUomFlag of SY03-AREA(1)            
000262                                                                          
000263     MOVE 1                  TO volume-num of SY03-AREA(1)                
000264     MOVE WS-VLARTNTO        TO volume of SY03-AREA(1)                    
000265     .                                                                    
000266     EJECT                                                                
000267 CB-FILL-UPD-SENDAREA SECTION.                                            
000268     MOVE 'CB-FILL-UPD-SENDAREA'   TO CURRENT-SECTION                     
000269                                                                          
000270     MOVE FUNCTION TRIM (WS-IDARTNR) TO productId of SY04-AREA            
000271     COMPUTE productId-length of SY04-AREA =                              
000272             FUNCTION BYTE-LENGTH (                                       
000273             FUNCTION TRIM (productId of SY04-AREA))                      
000274                                                                          
000275     MOVE 'Volvo'    TO Xowner of SY04-AREA                               
000276     MOVE 5          TO Xowner-length of SY04-AREA                        
000277                                                                          
000278     MOVE FUNCTION TRIM (WS-IDARTNR) TO productId2                        
000279     COMPUTE productId2-length  =                                         
000280             FUNCTION BYTE-LENGTH (                                       
000281             FUNCTION TRIM (productId2))                                  
000282                                                                          
000283     MOVE 'Volvo'    TO Xowner2                                           
000284     MOVE 5          TO Xowner2-length                                    
000285                                                                          
000286                                                                          
000287     MOVE 1          TO description-num of SY04-AREA                      
000288     MOVE FUNCTION TRIM (WS-BEART) TO description2 of SY04-AREA           
000289     COMPUTE description2-length of SY04-AREA =                           
000290             FUNCTION BYTE-LENGTH (                                       
000291             FUNCTION TRIM (description2 of SY04-AREA))                   
000292                                                                          
000293     MOVE 1          TO productUom2-num of SY04-AREA                      
000294                                                                          
000295     MOVE FUNCTION TRIM (WS-KDSORT) TO uomId of SY04-AREA(1)              
000296     COMPUTE uomId-length of SY04-AREA(1) =                               
000297             FUNCTION BYTE-LENGTH (                                       
000298             FUNCTION TRIM (uomId of SY04-AREA(1)))                       
000299                                                                          
000300     MOVE ZERO               TO imagePath2-length of SY04-AREA(1)         
000301                                imagePath-num of SY04-AREA(1)             
000302     MOVE SPACES             TO imagePath2 of SY04-AREA(1)                
000303                                                                          
000304     MOVE 1                  TO ratio of SY04-AREA(1)                     
000305                                                                          
000306     MOVE WS-TRUE            TO baseUomFlag of SY04-AREA(1)               
000307     MOVE WS-TRUE            TO pickUomFlag of SY04-AREA(1)               
000308     MOVE WS-TRUE            TO putawayUomFlag of SY04-AREA(1)            
000309                                                                          
000310     MOVE 1                  TO volume-num of SY04-AREA(1)                
000311     MOVE WS-VLARTNTO        TO volume of SY04-AREA(1)                    
000312                                                                          
000313     .                                                                    
000314     EJECT                                                                
000315 D-SEND-SYNQ SECTION.                                                     
000316     MOVE 'D-SEND-SYNQ'           TO CURRENT-SECTION                      
000317                                                                          
000318     PERFORM DA-SEND-OPEN                                                 
000319     PERFORM DB-SEND-PUT-HEADER                                           
000320     PERFORM DC-SEND-PUT-LINE                                             
000321     PERFORM DD-SEND-CLOSE                                                
000322                                                                          
000323     .                                                                    
000324     EJECT                                                                
000325                                                                          
000326 DA-SEND-OPEN SECTION.                                                    
000327     MOVE 'DA-SEND-OPEN'          TO CURRENT-SECTION                      
000328                                                                          
000329                                                                          
000330     MOVE W-ADDISPABS-ATAB        TO SEND-ADDISPABS                       
000331     MOVE 'OPEN'                  TO SEND-KDFUNC                          
000332     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
000333                                     SEND-OPEN-AREA                       
000334     IF SEND-KDRC > ZERO                                                  
000335       MOVE SEND-KDRC             TO KDRC-DISPLAY                         
000336       STRING 'WZ01SEND OPEN ERROR RC= ' KDRC-DISPLAY                     
000337       DELIMITED BY SIZE INTO ERRORTEXT                                   
000338       CALL FELLOG                                                        
000339     ELSE                                                                 
000340       MOVE SEND-IDCOM            TO WS-SEND-IDCOM                        
000341     END-IF                                                               
000342                                                                          
000343     .                                                                    
000344     SKIP2                                                                
000345 DB-SEND-PUT-HEADER SECTION.                                              
000346     MOVE 'DB-SEND-PUT-HEADER'    TO CURRENT-SECTION                      
000347                                                                          
000348     IF SYNQ-KDCALL = 002                                                 
000349       MOVE W-ADDISPABS-CRE       TO W-ADDISPABS                          
000350     ELSE                                                                 
000351       MOVE W-ADDISPABS-UPD       TO W-ADDISPABS                          
000352     END-IF                                                               
000353     PERFORM IMS-GU-WDGX0104                                              
000354     IF SEGMENT-FOUND                                                     
000355       MOVE 0104-IDAPI             TO IDAPI                               
000356       COMPUTE IDAPI-LEN       = FUNCTION BYTE-LENGTH (                   
000357                                 FUNCTION TRIM (IDAPI))                   
000358       MOVE 0104-IDPATH-API        TO IDPATH-API                          
000359       COMPUTE IDPATH-API-LEN  = FUNCTION BYTE-LENGTH (                   
000360                                 FUNCTION TRIM (IDPATH-API))              
000361       MOVE 0104-IDPTYP-API        TO IDPTYP-API                          
000362       COMPUTE IDPTYP-API-LEN  = FUNCTION BYTE-LENGTH (                   
000363                                 FUNCTION TRIM (IDPTYP-API))              
000364                                                                          
000365       IF SYNQ-KDCALL = 002                                               
000366         MOVE 0104-IDAPPKEY        TO USER-KEY OF SY03-AREA               
000367         COMPUTE USER-KEY-LENGTH OF SY03-AREA                             
000368                                 = FUNCTION BYTE-LENGTH (                 
000369                                   FUNCTION TRIM (USER-KEY                
000370                                              OF SY03-AREA))              
000371       ELSE                                                               
000372         MOVE 0104-IDAPPKEY        TO USER-KEY OF SY04-AREA               
000373         COMPUTE USER-KEY-LENGTH OF SY04-AREA                             
000374                                 = FUNCTION BYTE-LENGTH (                 
000375                                   FUNCTION TRIM (USER-KEY                
000376                                              OF SY04-AREA))              
000377       END-IF                                                             
000378     ELSE                                                                 
000379       CALL FELLOG                                                        
000380     END-IF                                                               
000381                                                                          
000382     MOVE 'PUT'                   TO SEND-KDFUNC                          
000383     MOVE WS-SEND-IDCOM           TO SEND-IDCOM                           
000384     MOVE LENGTH OF WAPIINFO      TO SEND-KVDLEN                          
000385     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
000386                                     SEND-KVDLEN                          
000387                                     WAPIINFO                             
000388     IF SEND-KDRC > ZERO                                                  
000389       MOVE SEND-KDRC             TO KDRC-DISPLAY                         
000390       STRING 'WZ01SEND PUT HEADER ERROR RC= ' KDRC-DISPLAY               
000391              DELIMITED BY SIZE INTO ERRORTEXT                            
000392       CALL FELLOG                                                        
000393     END-IF                                                               
000394     .                                                                    
000395                                                                          
000396 DC-SEND-PUT-LINE SECTION.                                                
000397     MOVE 'DC-SEND-PUT-LINE'        TO CURRENT-SECTION                    
000398                                                                          
000399     MOVE 'PUT'                     TO SEND-KDFUNC                        
000400     MOVE WS-SEND-IDCOM             TO SEND-IDCOM                         
000401     IF SYNQ-KDCALL = 002                                                 
000402       MOVE LENGTH OF SY03-AREA      TO SEND-KVDLEN                       
000403       CALL WZ01SEND             USING SEND-CONTROL-AREA                  
000404                                       SEND-KVDLEN                        
000405                                       SY03-AREA                          
000406     ELSE                                                                 
000407       MOVE LENGTH OF SY04-AREA TO SEND-KVDLEN                            
000408       CALL WZ01SEND             USING SEND-CONTROL-AREA                  
000409                                       SEND-KVDLEN                        
000410                                       SY04-AREA                          
000411     END-IF                                                               
000412                                                                          
000413     IF SEND-KDRC > ZERO                                                  
000414       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
000415       STRING 'WZ01SEND PUT LINE ERROR RC= ' KDRC-DISPLAY                 
000416       DELIMITED BY SIZE       INTO ERRORTEXT                             
000417       CALL FELLOG                                                        
000418     END-IF                                                               
000419     .                                                                    
000420     SKIP2                                                                
000421                                                                          
000422 DD-SEND-CLOSE SECTION.                                                   
000423     MOVE 'DD-SEND-CLOSE'        TO CURRENT-SECTION                       
000424                                                                          
000425     MOVE 'CLOSE'                TO SEND-KDFUNC                           
000426     MOVE WS-SEND-IDCOM          TO SEND-IDCOM                            
000427     CALL WZ01SEND USING SEND-CONTROL-AREA                                
000428                                                                          
000429     IF SEND-KDRC > 0                                                     
000430       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
000431       STRING 'WZ01SEND CLOSE ERROR RC= ' KDRC-DISPLAY                    
000432       DELIMITED BY SIZE INTO ERRORTEXT                                   
000433       CALL FELLOG                                                        
000434     END-IF                                                               
000435     .                                                                    
000436     EJECT                                                                
000437 E-SEND-SYNQ SECTION.                                                     
000438     MOVE 'E-SEND-SYNQ'           TO CURRENT-SECTION                      
000439                                                                          
000440     MOVE W-ADDISPABS-CRE        TO W-ADDISPABS                           
000441     PERFORM IMS-GU-WDGX0104                                              
000442     IF SEGMENT-FOUND                                                     
000443       MOVE 0104-IDAPI           TO CALL-IDAPI                            
000444       COMPUTE CALL-IDAPI-LEN  = FUNCTION BYTE-LENGTH (                   
000445                                 FUNCTION TRIM (CALL-IDAPI))              
000446       MOVE 0104-IDPATH-API      TO CALL-IDPATH-API                       
000447       COMPUTE CALL-IDPATH-API-LEN = FUNCTION BYTE-LENGTH (               
000448                                 FUNCTION TRIM (CALL-IDPATH-API))         
000449       MOVE 0104-IDPTYP-API      TO CALL-IDPTYP-API                       
000450       COMPUTE CALL-IDPTYP-API-LEN = FUNCTION BYTE-LENGTH (               
000451                                 FUNCTION TRIM (CALL-IDPTYP-API))         
000452                                                                          
000453       MOVE 0104-IDAPPKEY        TO USER-KEY OF SY03-AREA                 
000454       COMPUTE USER-KEY-LENGTH OF SY03-AREA                               
000455                               = FUNCTION BYTE-LENGTH (                   
000456                                 FUNCTION TRIM (USER-KEY                  
000457                                            OF SY03-AREA))                
000458     ELSE                                                                 
000459       CALL FELLOG                                                        
000460     END-IF                                                               
000461                                                                          
000462     MOVE W-ADDISPABS-CRE        TO CALL-ADDISPABS                        
000463     MOVE LENGTH OF SY03-AREA    TO CALL-KVDLEN-IN                        
000464                                                                          
000465     CALL WZ01CALL            USING CALL-CONTROL-AREA                     
000466                                    CALL-KVDLEN-IN                        
000467                                    SY03-AREA                             
000468                                    CALL-KVDLEN-OUT                       
000469                                    API-RESPONSE                          
000470                                                                          
000471     IF CALL-KDRC = ZERO                                                  
000472        CONTINUE                                                          
000473     ELSE                                                                 
000474        DISPLAY 'IDARTNR:' SYNQ-IDARTNR                                   
000475     END-IF                                                               
000476     .                                                                    
000477     EJECT                                                                
000478*                                                                         
000479***  IMS SECTIONS                                                         
000480*                                                                         
000481 IMS-GU-WDGX0104  SECTION.                                                
000482     MOVE 'IMS-GU-WDGX0104    '   TO IMS-SECTION                          
000483                                                                          
000484     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-0103-X ')'                    
000485             DELIMITED BY SIZE INTO SSA1                                  
000486     STRING 'WDGX0104(ADDISPAB =' W-KY0104-X ')'                          
000487             DELIMITED BY SIZE INTO SSA2                                  
000488     MOVE '  GE'                 TO GODK-STATUSKODER                      
000489     CALL CBLTDLI USING GU SYNQ-ATAB-PCB DLI-IO-WDGX0104 SSA1 SSA2        
000490     MOVE SYNQ-ATAB-STATUS-CODE       TO STATUS-WS                        
000491     PERFORM IMS-STATUSCHECK                                              
000492     .                                                                    
000493     SKIP3                                                                
000494 IMS-GU-WDD311     SECTION.                                               
000495     MOVE 'IMS-GU-WDD311      '   TO IMS-SECTION                          
000496                                                                          
000497     STRING 'WDD301  (WDD3BSEQ =' W-WDD3BSEQ-X ')'                        
000498          DELIMITED BY SIZE INTO SSA1                                     
000499     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
000500          DELIMITED BY SIZE INTO SSA2                                     
000501     MOVE '  GE' TO GODK-STATUSKODER                                      
000502     CALL CBLTDLI USING GU SYNQ-WDD3-PCB DLI-IO-WDD311 SSA1 SSA2          
000503     MOVE SYNQ-WDD3-STATUS-CODE TO STATUS-WS                              
000504     PERFORM IMS-STATUSCHECK                                              
000505     .                                                                    
000506     SKIP3                                                                
000507 IMS-GU-WDK601  SECTION.                                                  
000508     MOVE 'IMS-GU-WDK601     '   TO IMS-SECTION                           
000509                                                                          
000510     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
000511          DELIMITED BY SIZE INTO SSA1                                     
000512     MOVE '  GE' TO GODK-STATUSKODER                                      
000513     CALL CBLTDLI USING GU SYNQ-WDK6-PCB DLI-IO-WDK601 SSA1               
000514     MOVE SYNQ-WDK6-STATUS-CODE TO STATUS-WS                              
000515     PERFORM IMS-STATUSCHECK                                              
000516     .                                                                    
000517     SKIP3                                                                
000518 IMS-GNP-WDK611  SECTION.                                                 
000519     MOVE 'IMS-GNP-WDK611    '   TO IMS-SECTION                           
000520                                                                          
000521     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
000522          DELIMITED BY SIZE INTO SSA1                                     
000523     MOVE '  GE' TO GODK-STATUSKODER                                      
000524     CALL CBLTDLI USING GNP SYNQ-WDK6-PCB DLI-IO-WDK611 SSA1              
000525     MOVE SYNQ-WDK6-STATUS-CODE TO STATUS-WS                              
000526     PERFORM IMS-STATUSCHECK                                              
000527     .                                                                    
000528     EJECT                                                                
000529 IMS-STATUSCHECK SECTION.                                                 
000530     SET STATUS-IX TO 1                                                   
000531     SEARCH GODK-STATUS AT END CALL FELLOG                                
000532       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
000533     END-SEARCH                                                           
000534     CONTINUE                                                             
000535     .                                                                    
