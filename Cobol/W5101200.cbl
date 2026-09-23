000001 ID DIVISION.                                                             
000002 PROGRAM-ID.     W5101200.                                                
000003 AUTHOR.         MARKUS ASPFJÄLL.                                         
000004 DATE-WRITTEN.   02/02/13.                                                
000005 DATE-COMPILED.                                                           
000006                                                                          
000007*    FUNKTION:                                                            
000008*        LÄSER KONTO O KUND INFO IFRÅN BILLIT O SKAPAR FILER:             
000009*              - W51012 MED KONTO O ANALYSNR MM                           
000010*              - W51016 MED KUNDREGISTERINFORMATION.                      
000011*              - W51017 MED VAT INFO                                      
000012*                                                                         
000013*    INDATA.                                                              
000014*        TRANSAKTION: W51012T                                             
000015*        REQUEST:     W51012I1                                            
000016*                                                                         
000017*    UTDATA.                                                              
000018*        RESPONSE:    W51012O1                                            
000019*                                                                         
000020*    E-TRACKER 10143271 - CHINA WAREHOUSE PROJECT-1                       
000021*                                                                         
000022                                                                          
000023     SKIP3                                                                
000024 ENVIRONMENT DIVISION.                                                    
000025     SKIP2                                                                
000026 INPUT-OUTPUT SECTION.                                                    
000027                                                                          
000028 FILE-CONTROL.                                                            
000029     SKIP2                                                                
000030*          --- UTFIL MED KONTO O ANALYSNR                                 
000031     SELECT W51014                     ASSIGN TO W51012D1.                
000032     SKIP2                                                                
000033*          --- UTFIL MED KUND INFO                                        
000034     SELECT W51016                     ASSIGN TO W51012D2.                
000035*          --- UTFIL MED VAT INFO                                         
000036     SELECT W51017                     ASSIGN TO W51012D3.                
000037     EJECT                                                                
000038 DATA DIVISION.                                                           
000039     SKIP3                                                                
000040 FILE SECTION.                                                            
000041     SKIP3                                                                
000042 FD  W51014                                                               
000043     RECORDING       F                                                    
000044     BLOCK CONTAINS  0.                                                   
000045 01  UT1-POST     PIC X(44).                                              
000046                                                                          
000047     SKIP3                                                                
000048 FD  W51016                                                               
000049     RECORDING       F                                                    
000050     BLOCK CONTAINS  0.                                                   
000051 01  UT2-POST.                                                            
000052*    03  -COPY WDB101       -L.                                           
000053     EJECT                                                                
000054 FD  W51017                                                               
000055     RECORDING       F                                                    
000056     BLOCK CONTAINS  0.                                                   
000057 01  UT3-POST.                                                            
000058*    03  -COPY WF10M17      -L.                                           
000059     EJECT                                                                
000060 WORKING-STORAGE SECTION.                                                 
000061                                                                          
000062 77  IDPGM                       PIC X(08)   VALUE 'W5101200'.            
000063                                                                          
000064 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
000065 77  KDRC-DISPLAY                PIC Z(5).                                
000066 77  JA                          PIC X       VALUE 'J'.                   
000067 77  NEJ                         PIC X       VALUE 'N'.                   
000068 77  W-IDLEGSEL                  PIC X(4)    VALUE SPACES.                
000069 77  W-KDTRADP                   PIC X(4)    VALUE SPACES.                
000070 77  WS-SAVE-KDTRADP             PIC X(4)    VALUE SPACES.                
000071                                                                          
000072 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
000073     88  NYCKLAR-OK                          VALUE 'J'.                   
000074     88  NYCKLAR-FEL                         VALUE 'N'.                   
000075     EJECT                                                                
000076 77  WS-KDTRADP-FND-SW           PIC X       VALUE 'N'.                   
000077     88  WS-KDTRADP-FND                      VALUE 'J'.                   
000078     EJECT                                                                
000079*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
000080 01  GENERELLA-SUBPROGRAM.                                                
000081     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000082     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
000083     03  WZ01RECV                PIC X(8)    VALUE 'WZ01RECV'.            
000084     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
000085     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000086     SKIP3                                                                
000087*    --- PARAMETRAR TILL ABEND                                            
000088                                                                          
000089 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
000090 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
000091 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
000092     SKIP3                                                                
000093 01  MESSAGE-CODES.                                                       
000094     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
000095     EJECT                                                                
000096*    --- PARAMETRAR TILL POSTSUM                                          
000097*                                                                         
000098*01  -COPY W0005   -PRE  POSTSUM-                                         
000099     EJECT                                                                
000100 01  IN-AREA-START              PIC X(24)   VALUE                         
000101                                             'IN-AREA-START'.             
000102     SKIP2                                                                
000103 01  IN-AREA.                                                             
000104     03  IN-RECTYP                PIC X(3).                               
000105     03  IN-KONTO                 PIC X(1047).                            
000106                                                                          
000107 01  FILLER                  PIC X(16) VALUE 'IN2-AREA    '.              
000108 01  IN2-AREA.                                                            
000109     03  FILLER              PIC X(3).                                    
000110*    03   -COPY WF10CUS2 -PRE IN2-                                        
000111     EJECT                                                                
000112 01  FILLER                  PIC X(16) VALUE 'IN3-AREA    '.              
000113 01  IN3-AREA.                                                            
000114     03  FILLER              PIC X(3).                                    
000115*    03   -COPY WF10M17  -PRE IN3-                                        
000116     EJECT                                                                
000117                                                                          
000118 01  FILLER                  PIC X(16) VALUE 'IN-M10-AREA'.               
000119 01  IN-M10-AREA.                                                         
000120     03  IN-M10-RECTYP       PIC X(3).                                    
000121     03 -COPY WF10M10 -PRE IN-M10-                                        
000122     EJECT                                                                
000123                                                                          
000124 01  FILLER                  PIC X(16) VALUE 'IN-M11-AREA'.               
000125 01  IN-M11-AREA.                                                         
000126     03  IN-M11-RECTYP       PIC X(3).                                    
000127     03 -COPY WF10M11 -PRE IN-M11-                                        
000128     EJECT                                                                
000129                                                                          
000130 01  FILLER                  PIC X(16) VALUE 'IN-M13-AREA'.               
000131 01  IN-M13-AREA.                                                         
000132     03  IN-M13-RECTYP       PIC X(3).                                    
000133     03 -COPY WF10M13  -PRE IN-M13-                                       
000134     EJECT                                                                
000135                                                                          
000136 01  FILLER                  PIC X(16) VALUE 'IN-M17-AREA'.               
000137 01  IN-M17-AREA.                                                         
000138     03  IN-M17-RECTYP       PIC X(3).                                    
000139     03 -COPY WF10M17  -PRE IN-M17-                                       
000140     EJECT                                                                
000141                                                                          
000142 01  FILLER                  PIC X(16) VALUE 'UT1-AREA    '.              
000143 01  UT1-AREA                PIC X(44).                                   
000144 01  UT-M10-AREA REDEFINES UT1-AREA.                                      
000145       05  UT-M10-RECTYP       PIC X(3).                                  
000146       05  UT-M10-KDSEGKEY     PIC X(1).                                  
000147       05  UT-M10-IDGL         PIC X(4).                                  
000148       05  UT-M10-IDKONTO      PIC X(10).                                 
000149       05  UT-M10-FLKST        PIC X(1).                                  
000150       05  UT-M10-FLANALYS     PIC X(1).                                  
000151       05  UT-M10-DAREGDAT     PIC X(8).                                  
000152       05  UT-M10-DAUPPDAT     PIC X(8).                                  
000153       05  UT-M10-DADELDAT     PIC X(8).                                  
000154 01 UT-M11-AREA REDEFINES UT1-AREA.                                       
000155       05  UT-M11-RECTYP          PIC X(3).                               
000156       05  UT-M11-KDSEGKEY        PIC X(1).                               
000157       05  UT-M11-IDGL            PIC X(4).                               
000158       05  UT-M11-IDKST           PIC X(10).                              
000159       05  UT-M11-DAREGDAT        PIC X(8).                               
000160       05  UT-M11-DADELDAT        PIC X(8).                               
000161 01 UT-M13-AREA REDEFINES UT1-AREA.                                       
000162       05  UT-M13-RECTYP          PIC X(3).                               
000163       05  UT-M13-KDSEGKEY        PIC X(1).                               
000164       05  UT-M13-IDGL            PIC X(4).                               
000165       05  UT-M13-IDANALYS        PIC X(12).                              
000166       05  UT-M13-DAREGDAT        PIC X(8).                               
000167       05  UT-M13-DADELDAT        PIC X(8).                               
000168                                                                          
000169 01  FILLER                  PIC X(16) VALUE 'UT2-AREA    '.              
000170*01  AREA -COPY WDB101   -PRE UT2-                                        
000171     EJECT                                                                
000172 01  FILLER                  PIC X(16) VALUE 'UT3-AREA    '.              
000173*01  AREA -COPY WF10M17  -PRE UT3-                                        
000174     EJECT                                                                
000175 01  FILLER                      PIC X(16)   VALUE 'RECV-CONTROL'.        
000176     SKIP3                                                                
000177 01  -COPY WZ01RECV                                                       
000178     EJECT                                                                
000179 01  FILLER                      PIC X(16)   VALUE 'RECV-AREA'.           
000180     SKIP3                                                                
000181 01  RECV-AREA.                                                           
000182     03  RECV-INFO                PIC X(1050) VALUE SPACE.                
000183     EJECT                                                                
000184                                                                          
000185*       ARBETS-AREOR TILL IMS-SEKTIONERNA                                 
000186                                                                          
000187 01  FILLER                    PIC X(16)   VALUE 'IMS-WS'.                
000188 01  NYCKLAR-TILL-DLI.                                                    
000189                                                                          
000190     03  W-KY4138-X.                                                      
000191       05  W-IDLANDX3          PIC X(3)    VALUE SPACE.                   
000192     03  W-KDSEGKEY-X.                                                    
000193       05  W-KDSEGKEY          PIC X(1)    VALUE SPACE.                   
000194     03  W-WDGXKEY-4137-X.                                                
000195       05  W-IDHTYP-4137       PIC X(4)    VALUE '4137'.                  
000196       05  FILLER              PIC X(26)   VALUE LOW-VALUE.               
000197*                                                                         
000198*              STATUS-KOD FRÅN IMS                                        
000199 01  STATUS-WS                   PIC XX.                                  
000200     88  SEGMENT-FINNS                       VALUE '  '.                  
000201     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
000202     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
000203     88  SEGMENT-SLUT                        VALUE 'GB'.                  
000204     88  IMS-EJ-OK                           VALUE 'XD'.                  
000205     SKIP2                                                                
000206 01  GODK-STATUSKODER.                                                    
000207     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000208     SKIP3                                                                
000209 01  SSA1                        PIC X(64).                               
000210 01  SSA2                        PIC X(64).                               
000211     EJECT                                                                
000212*            -- IMS FUNKTIONSKODER                                        
000213*01  -COPY W0003                                                          
000214     EJECT                                                                
000215*            --  DLI INPUT-OUTPUT AREA                                    
000216 01  FILLER             PIC X(16)  VALUE 'DLI-IO-WDGX4138'.               
000217 01  DLI-IO-WDGX4138.                                                     
000218*    03   -COPY WDGX4138                                                  
000219                                                                          
000220 01  FILLER             PIC X(16)  VALUE 'DLI-IO-WDR501'.                 
000221 01  DLI-IO-WDR501.                                                       
000222*    03   -COPY WDGX01                                                    
000223 01  FILLER             PIC X(16)  VALUE 'DLI-IO-WDB601'.                 
000224 01  DLI-IO-WDB601.                                                       
000225*    03   -COPY WDB601                                                    
000226                                                                          
000227 LINKAGE SECTION.                                                         
000228*01  -COPY W0009   -PRE MSG-                                              
000229                                                                          
000230*01  -COPY W0008  -PRE 4138-                                              
000231     05  FILLER                  PIC X.                                   
000232*01  -COPY W0008  -PRE WDB6-                                              
000233     05  FILLER                  PIC X.                                   
000234     EJECT                                                                
000235                                                                          
000236 PROCEDURE DIVISION USING MSG-PCB                                         
000237                          4138-PCB WDB6-PCB.                              
000238 MAIN SECTION.                                                            
000239     ENTRY 'DLITCBL' USING MSG-PCB                                        
000240                           4138-PCB WDB6-PCB.                             
000241                                                                          
000242     PERFORM A-INIT                                                       
000243                                                                          
000244     PERFORM S03-LAES-OPEN                                                
000245     IF RECV-KDRC = 0                                                     
000246                                                                          
000247       PERFORM S03-LAES-MEDDELANDE                                        
000248       IF RECV-KDRC = 0                                                   
000249         PERFORM UNTIL RECV-KDRC > 0                                      
000250           MOVE RECV-AREA         TO IN-AREA                              
000251           IF IN-RECTYP = 'M10' OR 'M11' OR 'M13'                         
000252             PERFORM B-KONTO                                              
000253           ELSE                                                           
000254             IF IN-RECTYP = 'M00'                                         
000255               MOVE RECV-AREA      TO IN2-AREA                            
000256               PERFORM C-KUND                                             
000257             ELSE                                                         
000258               IF IN-RECTYP = 'M17'                                       
000259                 MOVE RECV-AREA    TO IN3-AREA                            
000260                 PERFORM D-VAT                                            
000261               END-IF                                                     
000262             END-IF                                                       
000263           END-IF                                                         
000264           PERFORM S03-LAES-MEDDELANDE                                    
000265         END-PERFORM                                                      
000266       END-IF                                                             
000267                                                                          
000268     END-IF                                                               
000269                                                                          
000270     PERFORM S03-LAES-CLOSE                                               
000271                                                                          
000272     PERFORM Z-FINIT                                                      
000273     MOVE ZERO TO RETURN-CODE                                             
000274     GOBACK                                                               
000275     .                                                                    
000276     EJECT                                                                
000277 A-INIT SECTION.                                                          
000278                                                                          
000279                                                                          
000280     OPEN OUTPUT W51014                                                   
000281                 W51016                                                   
000282                 W51017                                                   
000283     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
000284     .                                                                    
000285     EJECT                                                                
000286 B-KONTO SECTION.                                                         
000287                                                                          
000288     MOVE NEJ                  TO WS-KDTRADP-FND-SW                       
000289     EVALUATE IN-RECTYP                                                   
000290       WHEN 'M10'                                                         
000291         MOVE IN-AREA          TO IN-M10-AREA                             
000292         MOVE IN-M10-IDGL      TO W-KDTRADP                               
000293         PERFORM S10-CHECK-KDTRADP                                        
000294         IF WS-KDTRADP-FND                                                
000295           PERFORM BA-SKAPA-IDKONTO                                       
000296         END-IF                                                           
000297       WHEN 'M11'                                                         
000298         MOVE IN-AREA          TO IN-M11-AREA                             
000299         MOVE IN-M11-IDGL      TO W-KDTRADP                               
000300         PERFORM S10-CHECK-KDTRADP                                        
000301         IF WS-KDTRADP-FND                                                
000302           PERFORM BB-SKAPA-IDKST                                         
000303         END-IF                                                           
000304       WHEN 'M13'                                                         
000305         MOVE IN-AREA          TO IN-M13-AREA                             
000306         MOVE IN-M13-IDGL      TO W-KDTRADP                               
000307         PERFORM S10-CHECK-KDTRADP                                        
000308         IF WS-KDTRADP-FND                                                
000309           PERFORM BC-SKAPA-IDANALYSNR                                    
000310         END-IF                                                           
000311      END-EVALUATE                                                        
000312     .                                                                    
000313     EJECT                                                                
000314 BA-SKAPA-IDKONTO SECTION.                                                
000315     MOVE SPACE               TO  UT1-AREA                                
000316     MOVE IN-M10-RECTYP       TO  UT-M10-RECTYP                           
000317     MOVE IN-M10-IDGL         TO  UT-M10-IDGL                             
000318     MOVE IN-M10-IDKONTO      TO  UT-M10-IDKONTO                          
000319     MOVE IN-M10-FLKST        TO  UT-M10-FLKST                            
000320     MOVE IN-M10-FLANALYS     TO  UT-M10-FLANALYS                         
000321     MOVE IN-M10-DAREGDAT     TO  UT-M10-DAREGDAT                         
000322     MOVE IN-M10-DAUPPDAT     TO  UT-M10-DAUPPDAT                         
000323     MOVE IN-M10-DADELDAT     TO  UT-M10-DADELDAT                         
000324     MOVE '2'                 TO  UT-M10-KDSEGKEY                         
000325     PERFORM S11-SKRIV-W51014                                             
000326     .                                                                    
000327     EJECT                                                                
000328 BB-SKAPA-IDKST   SECTION.                                                
000329     MOVE SPACE               TO  UT1-AREA                                
000330     MOVE IN-M11-RECTYP       TO  UT-M11-RECTYP                           
000331     MOVE IN-M11-IDGL         TO  UT-M11-IDGL                             
000332     MOVE IN-M11-IDKST        TO  UT-M11-IDKST                            
000333     MOVE IN-M11-DAREGDAT     TO  UT-M11-DAREGDAT                         
000334     MOVE IN-M11-DADELDAT     TO  UT-M11-DADELDAT                         
000335     MOVE '3'                 TO  UT-M11-KDSEGKEY                         
000336     IF IN-M11-IDKST       NOT = '57586'                                  
000337       PERFORM S11-SKRIV-W51014                                           
000338     END-IF                                                               
000339     .                                                                    
000340     EJECT                                                                
000341 BC-SKAPA-IDANALYSNR SECTION.                                             
000342     MOVE SPACE                  TO UT1-AREA                              
000343     MOVE IN-M13-RECTYP          TO UT-M13-RECTYP                         
000344     MOVE IN-M13-IDGL            TO UT-M13-IDGL                           
000345     MOVE IN-M13-IDANALYS        TO UT-M13-IDANALYS                       
000346     MOVE IN-M13-DAREGDAT        TO UT-M13-DAREGDAT                       
000347     MOVE IN-M13-DADELDAT        TO UT-M13-DADELDAT                       
000348     MOVE '1'                    TO UT-M13-KDSEGKEY                       
000349     PERFORM S11-SKRIV-W51014                                             
000350     .                                                                    
000351     EJECT                                                                
000352 C-KUND SECTION.                                                          
000353     MOVE IN2-IDLEGSEL            TO W-IDLEGSEL                           
000354     PERFORM IMS-GU-WDB601-LEGSEL                                         
000355     IF SEGMENT-FINNS                                                     
000356       MOVE DCS-IDFTG             TO UT2-BET-IDFTG                        
000357     END-IF                                                               
000358     MOVE IN2-IDPARTNR            TO UT2-BET-IDPARTNR                     
000359     MOVE IN2-IDLANDX3(1:2)       TO UT2-BET-IDLANDX2                     
000360     MOVE IN2-IDVAT               TO UT2-BET-IDVAT                        
000361     MOVE IN2-KDBETALV            TO UT2-BET-KDBETVIL                     
000362     IF IN2-KDKREDSP = 'Y' OR 'J'                                         
000363       MOVE '1'                   TO UT2-BET-KDKREDSP                     
000364     ELSE                                                                 
000365       MOVE '0'                   TO UT2-BET-KDKREDSP                     
000366     END-IF                                                               
000367     MOVE IN2-KDTRADP             TO UT2-BET-KDTRADP                      
000368     MOVE IN2-KDVALISO            TO UT2-BET-KDVALISO                     
000369     MOVE IN2-DAREGDAT(3:6)       TO UT2-BET-TISTADAT                     
000370     MOVE IN2-DAUPPDAT(3:6)       TO UT2-BET-TIUPPDAT                     
000371     MOVE IN2-DADELDAT(3:6)       TO UT2-BET-TISTODAT                     
000372     MOVE IN2-IDUSER              TO UT2-BET-IDUSER                       
000373     MOVE ZERO                    TO UT2-BET-RELANDCO                     
000374     MOVE SPACE                   TO UT2-BET-IDPROMR                      
000375     IF IN2-ADBET-STREET = SPACE                                          
000376       IF IN2-ADBET-BOX(1:3) = 'BOX'                                      
000377         MOVE IN2-ADBET-BOX       TO UT2-BET-ADBETRAD-1                   
000378       ELSE                                                               
000379         MOVE 'BOX '              TO UT2-BET-ADBETRAD-1(1:4)              
000380         MOVE IN2-ADBET-BOX       TO UT2-BET-ADBETRAD-1(5:31)             
000381       END-IF                                                             
000382     ELSE                                                                 
000383       MOVE IN2-ADBET-STREET      TO UT2-BET-ADBETRAD-1                   
000384     END-IF                                                               
000385     MOVE IN2-ADBET-PCODE         TO UT2-BET-ADBETRAD-2                   
000386     IF IN2-ADBET-PCODE = SPACE                                           
000387      MOVE IN2-ADBET-CITY         TO UT2-BET-ADBETRAD-2                   
000388     ELSE                                                                 
000389      IF IN2-ADBET-PCODE(4:7) = SPACE                                     
000390       MOVE IN2-ADBET-CITY(1:31)  TO UT2-BET-ADBETRAD-2(5:31)             
000391      ELSE                                                                
000392       IF IN2-ADBET-PCODE(5:6) = SPACE                                    
000393        MOVE IN2-ADBET-CITY(1:30) TO UT2-BET-ADBETRAD-2(6:30)             
000394       ELSE                                                               
000395        IF IN2-ADBET-PCODE(6:5) = SPACE                                   
000396         MOVE IN2-ADBET-CITY(1:29) TO UT2-BET-ADBETRAD-2(7:29)            
000397        ELSE                                                              
000398         IF IN2-ADBET-PCODE(7:4) = SPACE                                  
000399          MOVE IN2-ADBET-CITY(1:28) TO UT2-BET-ADBETRAD-2(8:28)           
000400         ELSE                                                             
000401          IF IN2-ADBET-PCODE(8:3) = SPACE                                 
000402           MOVE IN2-ADBET-CITY(1:27) TO UT2-BET-ADBETRAD-2(9:27)          
000403          ELSE                                                            
000404           MOVE IN2-ADBET-CITY(1:25) TO UT2-BET-ADBETRAD-2(11:25)         
000405          END-IF                                                          
000406         END-IF                                                           
000407        END-IF                                                            
000408       END-IF                                                             
000409      END-IF                                                              
000410     END-IF                                                               
000411     PERFORM CA-KOMPL-BELAND-ENG                                          
000412     MOVE IN2-BEBET-NAME1         TO UT2-BET-BEBETRAD-1                   
000413     MOVE IN2-BEBET-NAME2         TO UT2-BET-BEBETRAD-2                   
000414     MOVE IN2-BEBETVIL            TO UT2-BET-BEBETVIL                     
000415     MOVE IN2-FLRATE              TO UT2-BET-FLRATE                       
000416     MOVE IN2-FLLOCCUR            TO UT2-BET-FLLOCCUR                     
000417     MOVE IN2-KDVALTYP            TO UT2-BET-KDVALTYP                     
000418     MOVE ZERO                    TO UT2-BET-RESERAVG                     
000419     MOVE 'N'                     TO UT2-BET-FLDIRVAT                     
000420     MOVE 'N'                     TO UT2-BET-FLINVGRP                     
000421     MOVE SPACE                   TO UT2-BET-IDLEVNR-FIN                  
000422     PERFORM S12-SKRIV-W51016                                             
000423     .                                                                    
000424     EJECT                                                                
000425 CA-KOMPL-BELAND-ENG SECTION.                                             
000426                                                                          
000427     MOVE IN2-IDLANDX3            TO W-IDLANDX3                           
000428     PERFORM IMS-GU-WDGX4138                                              
000429     MOVE 4138-BELAND             TO UT2-BET-BELAND-SVE                   
000430     .                                                                    
000431     EJECT                                                                
000432 D-VAT SECTION.                                                           
000433                                                                          
000434     MOVE IN3-IDLEGSEL            TO UT3-IDLEGSEL                         
000435     MOVE IN3-IDLAND              TO UT3-IDLAND                           
000436     MOVE IN3-KDVAT               TO UT3-KDVAT                            
000437     MOVE IN3-REVAT               TO UT3-REVAT                            
000438     MOVE IN3-BEVAT               TO UT3-BEVAT                            
000439     MOVE IN3-DAREGDAT            TO UT3-DAREGDAT                         
000440     MOVE IN3-DAUPPDAT            TO UT3-DAUPPDAT                         
000441     MOVE IN3-DADELDAT            TO UT3-DADELDAT                         
000442     PERFORM S13-SKRIV-W51017                                             
000443     .                                                                    
000444     EJECT                                                                
000445 Z-FINIT SECTION.                                                         
000446                                                                          
000447     CLOSE W51014                                                         
000448           W51016                                                         
000449           W51017                                                         
000450                                                                          
000451     MOVE 'S' TO POSTSUM-OPKOD                                            
000452     CALL POSTSUM USING POSTSUM-PARM                                      
000453     .                                                                    
000454     EJECT                                                                
000455*    --- DISPATCHER-SEKTIONER                                             
000456 S03-LAES-OPEN SECTION.                                                   
000457                                                                          
000458     MOVE 'OPEN'                     TO RECV-KDFUNC                       
000459     MOVE 'CARPARTS.PULS.RECFINDOC' TO RECV-ADDISPABS                     
000460     CALL WZ01RECV USING RECV-CONTROL-AREA RECV-OPEN-AREA                 
000461                                                                          
000462     IF RECV-KDRC > 0 AND NOT = 20                                        
000463       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
000464       STRING 'WZ01RECV OPEN ERROR RC=' KDRC-DISPLAY                      
000465       DELIMITED BY SIZE INTO FELTEXT                                     
000466       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
000467     END-IF                                                               
000468     .                                                                    
000469     SKIP3                                                                
000470 S03-LAES-MEDDELANDE SECTION.                                             
000471                                                                          
000472     MOVE 'GET'                      TO RECV-KDFUNC                       
000473     MOVE LENGTH OF RECV-AREA        TO RECV-KVDLEN                       
000474     CALL WZ01RECV USING RECV-CONTROL-AREA RECV-KVDLEN RECV-AREA          
000475                                                                          
000476     IF RECV-KDRC > 1                                                     
000477       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
000478       STRING 'WZ01RECV GET ERROR RC=' KDRC-DISPLAY                       
000479       DELIMITED BY SIZE INTO FELTEXT                                     
000480       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
000481     END-IF                                                               
000482     .                                                                    
000483     SKIP3                                                                
000484 S03-LAES-CLOSE SECTION.                                                  
000485                                                                          
000486     MOVE 'CLOSE'                    TO RECV-KDFUNC                       
000487     CALL WZ01RECV USING RECV-CONTROL-AREA                                
000488                                                                          
000489     IF RECV-KDRC > 0                                                     
000490       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
000491       STRING 'WZ01RECV CLOSE ERROR RC=' KDRC-DISPLAY                     
000492       DELIMITED BY SIZE INTO FELTEXT                                     
000493       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
000494     END-IF                                                               
000495     .                                                                    
000496     EJECT                                                                
000497 S10-CHECK-KDTRADP SECTION.                                               
000498                                                                          
000499     IF W-KDTRADP = WS-SAVE-KDTRADP                                       
000500       MOVE JA         TO WS-KDTRADP-FND-SW                               
000501     ELSE                                                                 
000502       PERFORM IMS-GU-WDB601-TRADP                                        
000503       IF SEGMENT-FINNS                                                   
000504          MOVE JA      TO WS-KDTRADP-FND-SW                               
000505       ELSE                                                               
000506          MOVE NEJ     TO WS-KDTRADP-FND-SW                               
000507       END-IF                                                             
000508       MOVE W-KDTRADP  TO WS-SAVE-KDTRADP                                 
000509     END-IF                                                               
000510     IF W-KDTRADP = 'SE09'                                                
000511        MOVE JA      TO WS-KDTRADP-FND-SW                                 
000512     END-IF                                                               
000513     .                                                                    
000514     EJECT                                                                
000515 S11-SKRIV-W51014 SECTION.                                                
000516     SKIP2                                                                
000517     WRITE UT1-POST FROM UT1-AREA                                         
000518                                                                          
000519     MOVE 'UT1'      TO POSTSUM-TRANSTYP                                  
000520     MOVE 'W51014 ' TO POSTSUM-FDNAMN                                     
000521     MOVE 'W51012D1' TO POSTSUM-DDNAMN2                                   
000522     CALL POSTSUM USING POSTSUM-PARM                                      
000523     .                                                                    
000524     EJECT                                                                
000525 S12-SKRIV-W51016 SECTION.                                                
000526     SKIP2                                                                
000527     WRITE UT2-POST FROM UT2-AREA                                         
000528                                                                          
000529     MOVE 'UT2'      TO POSTSUM-TRANSTYP                                  
000530     MOVE 'W51016 ' TO POSTSUM-FDNAMN                                     
000531     MOVE 'W51012D2' TO POSTSUM-DDNAMN2                                   
000532     CALL POSTSUM USING POSTSUM-PARM                                      
000533     .                                                                    
000534 S13-SKRIV-W51017 SECTION.                                                
000535     SKIP2                                                                
000536     WRITE UT3-POST FROM UT3-AREA                                         
000537                                                                          
000538     MOVE 'UT3'      TO POSTSUM-TRANSTYP                                  
000539     MOVE 'W51017 ' TO POSTSUM-FDNAMN                                     
000540     MOVE 'W51012D3' TO POSTSUM-DDNAMN2                                   
000541     CALL POSTSUM USING POSTSUM-PARM                                      
000542     .                                                                    
000543 IMS-GU-WDGX4138  SECTION.                                                
000544                                                                          
000545     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-4137-X ')'                    
000546          DELIMITED BY SIZE INTO SSA1                                     
000547     STRING 'WDGX4138*F(IDLANDX3 =' W-KY4138-X ')'                        
000548          DELIMITED BY SIZE INTO SSA2                                     
000549     MOVE '    ' TO GODK-STATUSKODER                                      
000550     CALL CBLTDLI USING GU 4138-PCB DLI-IO-WDGX4138 SSA1 SSA2             
000551     MOVE 4138-STATUS-CODE TO STATUS-WS                                   
000552     PERFORM IMS-STATUSKONTROLL                                           
000553     .                                                                    
000554     EJECT                                                                
000555 IMS-GU-WDB601-LEGSEL SECTION.                                            
000556     STRING 'WDB601  (IDLEGSEL =' W-IDLEGSEL ')'                          
000557            DELIMITED BY SIZE INTO SSA1                                   
000558     MOVE '  GB'                 TO GODK-STATUSKODER                      
000559     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
000560     MOVE WDB6-STATUS-CODE       TO STATUS-WS                             
000561     PERFORM IMS-STATUSKONTROLL                                           
000562     .                                                                    
000563     SKIP3                                                                
000564 IMS-GU-WDB601-TRADP SECTION.                                             
000565     STRING 'WDB601  (KDTRADP  =' W-KDTRADP ')'                           
000566            DELIMITED BY SIZE INTO SSA1                                   
000567     MOVE '  GE'                 TO GODK-STATUSKODER                      
000568     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
000569     MOVE WDB6-STATUS-CODE       TO STATUS-WS                             
000570     PERFORM IMS-STATUSKONTROLL                                           
000571     .                                                                    
000572     SKIP3                                                                
000573 IMS-STATUSKONTROLL SECTION.                                              
000574                                                                          
000575     SET STATUS-IX TO 1                                                   
000576     SEARCH GODK-STATUS                                                   
000577       AT END                                                             
000578         STRING 'FEL STATUSKOD FRÅN IMS: IDLANDX3 FEL' STATUS-WS          
000579         DELIMITED BY SIZE INTO FELTEXT                                   
000580         CALL FELLOG                                                      
000581       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
000582         CONTINUE                                                         
000583     END-SEARCH                                                           
000584     .                                                                    
000585     SKIP2                                                                
