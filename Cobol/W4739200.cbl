000001 ID  DIVISION.                                                            
000002     SKIP2                                                                
000003 PROGRAM-ID.    W4739200.                                                 
000004 AUTHOR.        GUNNAR L, IDK.                                            
000005 DATE-WRITTEN.  JAN 1981.                                                 
000006                                                                          
000007     REMARKS.                                                             
000008*                                                                         
000009*    FUNKTION:                                                            
000010*                                                                         
000011*        PROGRAMMET SKRIVER EMBALLAGE-PROFORMA                            
000012*        SORTERAS ENLIGT  DISTRIKT, KUNDNR, ORDERNR                       
000013*                                                                         
000014*        OMSKRIVET JAN. 2014 FÖR ATT PASSA IN I NYA EMBALLAGE             
000015*        FLÖDET TILL TMS (CO).                                            
000016*        SORTERAS ENLIGT  DC-SEND, DC-REC,  DISTRIKT, KUNDNR,             
000017*                         PRODNR                                          
000018*    ABENDKODER:                                                          
000019*        U0016    - OM RETURKOD FRÅN SORT                                 
000020*    EJECT                                                                
000021*                                                                         
000022* ETRACKER 1570221  EMBALLAGE INFO TO VTAB FEB. 2006                      
000023* ETRACKER 10221367 EMBALLAGE INFO TO TMS, JAN. 2014                      
000024*                                                                         
000025 ENVIRONMENT DIVISION.                                                    
000026     SKIP2                                                                
000027 INPUT-OUTPUT SECTION.                                                    
000028*                                                                         
000029 FILE-CONTROL.                                                            
000030     SKIP2                                                                
000031*--- RAPPORTFILER:                                                        
000032*                                                                         
000033     SELECT W47390-EMBIN                 ASSIGN TO UT-S-W47392D1.         
000034     SELECT W4739BI                      ASSIGN TO UT-S-W47392D2.         
000035     SELECT W4739A                       ASSIGN TO UT-S-W47392D3.         
000036     SELECT W4739BU                      ASSIGN TO UT-S-W47392D4.         
000037     SKIP2                                                                
000038*--- SORTFIL:                                                             
000039*                                                                         
000040     SELECT SORTFIL                      ASSIGN TO UT-S-W47392DS.         
000041     EJECT                                                                
000042 DATA DIVISION.                                                           
000043     SKIP2                                                                
000044 FILE SECTION.                                                            
000045     SKIP3                                                                
000046 FD  W47390-EMBIN                                                         
000047     LABEL RECORD    STANDARD                                             
000048     RECORDING       F                                                    
000049     BLOCK CONTAINS 0.                                                    
000050     SKIP2                                                                
000051*01  -COPY W47390      -L.                                                
000052     SKIP3                                                                
000053 FD  W4739BI                                                              
000054     LABEL RECORD     STANDARD                                            
000055     RECORDING        F                                                   
000056     BLOCK CONTAINS   0.                                                  
000057*01  -COPY W4739B    -L.                                                  
000058     EJECT                                                                
000059 FD  W4739A                                                               
000060     LABEL RECORD     STANDARD                                            
000070     RECORDING        V                                                   
000071     BLOCK CONTAINS   0.                                                  
000072     SKIP2                                                                
000073 01  UT-STA-POST.                                                         
000074*    03  -COPY WTMSSTA  -PRE UT-                                          
000075                                                                          
000076 01  UT-HUV-POST.                                                         
000077*    03  -COPY WTMSHEA  -PRE UT-                                          
000078                                                                          
000079 01  UT-DET-POST.                                                         
000080*    03  -COPY WTMSDET  -PRE UT-                                          
000081     EJECT                                                                
000082 FD  W4739BU                                                              
000083     LABEL RECORD     STANDARD                                            
000084     RECORDING        F                                                   
000085     BLOCK CONTAINS   0.                                                  
000086 01  U01NUM-POST.                                                         
000087*    03  -COPY W4739B   -L.                                               
000088     SKIP2                                                                
000089 SD  SORTFIL                                                              
000090     RECORDING       V.                                                   
000091     SKIP2                                                                
000092 01  SORT-POST.                                                           
000093*    03  -COPY W4739201  -PRE SORT-.                                      
000094     EJECT                                                                
000095 WORKING-STORAGE SECTION.                                                 
000096*    -COPY WY2000W1                                                       
000097     SKIP3                                                                
000098*                                                                         
000099 77  PROGRAM-NAMN                PIC X(8) VALUE 'W4739200'.               
000100                                                                          
000101*                                      GENERERAT PROGRAM-NAMN.            
000102     SKIP3                                                                
000103 01  WS-IDDC-SEND                PIC X(02)  VALUE SPACE.                  
000104 01  WS-IDDC-REC                 PIC X(02)  VALUE SPACE.                  
000105 01  WS-SKRIV-START              PIC X(01)  VALUE 'J'.                    
000106                                                                          
000107 01  WS-IDEMBNR-AKT              PIC S9(7)  COMP-3.                       
000108                                                                          
000109     SKIP2                                                                
000110 01  GENERELLA-KONSTANTER.                                                
000120*                                                                         
000121     03  JA                      PIC X(1)    VALUE 'J'.                   
000122     03  NEJ                     PIC X(1)    VALUE 'N'.                   
000123     SKIP3                                                                
000124 01  RETURKODER.                                                          
000125*                                                                         
000126     03  RKOD                    PIC S9(4)   COMP SYNC VALUE ZERO.        
000127     03  RKOD-16                 PIC S9(4)   COMP SYNC VALUE +16.         
000128     SKIP3                                                                
000129 01  WS-DAGENS-YYMMDDHHMMSS.                                              
000130     03 WS-DAGENS-DATUM.                                                  
000140        05 DAGENS-DATUM-AA       PIC X(2).                                
000141        05 DAGENS-DATUM-MM       PIC X(2).                                
000142        05 DAGENS-DATUM-DD       PIC X(2).                                
000143     03 WS-DAGENS-TID.                                                    
000144        05 DAGENS-TID-HH         PIC X(2).                                
000145        05 DAGENS-TID-MM         PIC X(2).                                
000146        05 DAGENS-TID-SS         PIC X(2).                                
000147     SKIP3                                                                
000148 01  WS-DAGENS-DDMMYYYY.                                                  
000149     03 WS-DAGENS-DAT.                                                    
000150        05 DAGENS-DAT-DD         PIC X(2).                                
000151        05 FILLER                PIC X(1)    VALUE '.'.                   
000152        05 DAGENS-DAT-MM         PIC X(2).                                
000153        05 FILLER                PIC X(1)    VALUE '.'.                   
000154        05 FILLER                PIC X(2)    VALUE '20'.                  
000155        05 DAGENS-DAT-AA         PIC X(2).                                
000156 01  WS-TMS-IDBOKN.                                                       
000157     03  WS-TMS-YYMMDDHHMMSS     PIC X(12).                               
000158     03  WS-TMS-IDLOPNR          PIC 9(4).                                
000159     SKIP3                                                                
000160 01  DYNAMISKA-SUBPROGRAM.                                                
000161*                                                                         
000162     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
000163     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
000164     03 CBLTDLI                  PIC X(8)    VALUE 'CBLTDLI '.            
000165     03 FELLOG                   PIC X(8)    VALUE 'FELLOG  '.            
000166     SKIP3                                                                
000167 01  END-OF-FILE-SWITCHAR.                                                
000168*                                                                         
000169     03  SORTFIL-EOF             PIC X(1)    VALUE 'N'.                   
000170     03  EOF-W47390              PIC X(1)    VALUE 'N'.                   
000171     03  EOF-W4739B              PIC X(1)    VALUE 'N'.                   
000172     SKIP2                                                                
000173 01  WS-AVD                      PIC X(5)    VALUE SPACE.                 
000174     EJECT                                                                
000175 01  W-IDKUNDRF.                                                          
000176     03  FILLER                  PIC X(3).                                
000177     03  W-IDKUNDRF-4-5          PIC 9(2).                                
000178     03  FILLER                  PIC X(5).                                
000179 01  W-IDDISTR-X.                                                         
000180     03  FILLER                  PIC 9(1)    VALUE 8.                     
000181     03  W-IDDISTR-2-3           PIC 9(2).                                
000182 01  W-IDDISTR-N REDEFINES W-IDDISTR-X PIC S9(3).                         
000183     EJECT                                                                
000184*                                                                         
000185*      --- VALID IDDC CODES                                               
000186*                                                                         
000187*01    -COPY WWDC99                                                       
000188*                                                                         
000189*                                                                         
000190*01  AREA -COPY W47390     -PRE EMBIN-.                                   
000191     EJECT                                                                
000192 01  FILLER                      PIC X(24)   VALUE                        
000193                                            'EMB-AREA-START  '.           
000194     SKIP3                                                                
000195*01  AREA  -COPY W4739201 -PRE EMB-.                                      
000196     EJECT                                                                
000197 01  FILLER                      PIC X(24)   VALUE                        
000198                                            'UT-AREA-START  '.            
000199     SKIP3                                                                
000200 01  FILLER                      PIC X(24) VALUE 'STA-AREA'.              
000201 01  STA-AREA.                                                            
000202*    03  -COPY WTMSSTA.                                                   
000203                                                                          
000204 01  FILLER                      PIC X(24) VALUE 'HUV-AREA'.              
000205 01  HUV-AREA.                                                            
000206*    03  -COPY WTMSHEA.                                                   
000207                                                                          
000208 01  FILLER                      PIC X(24) VALUE 'DET-AREA'.              
000209 01  DET-AREA.                                                            
000210*    03  -COPY WTMSDET.                                                   
000211                                                                          
000212     EJECT                                                                
000213*-------------------------------PARAMETRAR TILL NUMMERSERIE               
000214*01  -COPY W4739B  -PRE  W01NUM-.                                         
000215     EJECT                                                                
000216 01  NYCKLAR-TILL-DLI.                                                    
000217*                                                                         
000218   03  W-IDDC-B6-S-X.                                                     
000219     05  W-IDDC-B6-S             PIC X(2)    VALUE SPACE.                 
000220     SKIP2                                                                
000230   03  W-IDDC-B6-R-X.                                                     
000231     05  W-IDDC-B6-R             PIC X(2)    VALUE SPACE.                 
000232     EJECT                                                                
000233*        ARBETSAREOR TILL IMS-SEKTIONERNA                                 
000234*                                                                         
000235 01  IMS-WS.                                                              
000236   03  FILLER                    PIC X(8)    VALUE 'IMS-WS  '.            
000237     SKIP3                                                                
000238*                            *** STATUSKOD FRÅN IMS                       
000239   03  STATUS-WS                 PIC XX.                                  
000240     88  SEGMENT-FINNS                       VALUE '  '.                  
000241     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
000242     SKIP3                                                                
000243   03  GODK-STATUSKODER.                                                  
000244     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC X(2).              
000245     SKIP3                                                                
000246   03  SSA1                      PIC X(64).                               
000247     EJECT                                                                
000248*01  -COPY W0003                                                          
000249     EJECT                                                                
000250 01  FILLER                      PIC X(16)  VALUE 'WDB6-S-AREA'.          
000251 01  DLI-IO-AREA-B601-SEND.                                               
000252*    03  -COPY WDB601  -PRE SEND-                                         
000253     EJECT                                                                
000254 01  FILLER                      PIC X(16)  VALUE 'WDB6-R-AREA'.          
000255 01  DLI-IO-AREA-B601-REC.                                                
000256*    03  -COPY WDB601  -PRE REC-                                          
000257     EJECT                                                                
000258 LINKAGE SECTION.                                                         
000259     SKIP3                                                                
000260*01      -COPY W0008     -PRE WDB6-                                       
000261      05 FILLER          PIC X.                                           
000262     EJECT                                                                
000263 PROCEDURE DIVISION USING WDB6-PCB.                                       
000264     ENTRY 'DLITCBL' USING WDB6-PCB.                                      
000265                                                                          
000266 STYR SECTION.                                                            
000267                                                                          
000268     PERFORM A-INIT                                                       
000269                                                                          
000270     SORT SORTFIL                                                         
000271     ASCENDING SORT-IDDC-SEND                                             
000272               SORT-IDDC-REC                                              
000273               SORT-IDDISTR-4768                                          
000274               SORT-IDKUNDNR-4768                                         
000275               SORT-IDPRODNR-4768                                         
000276     INPUT  PROCEDURE B-FORE-SORTERING                                    
000277     OUTPUT PROCEDURE C-EFTER-SORTERING                                   
000278                                                                          
000279     IF  SORT-RETURN > ZERO                                               
000280         DISPLAY '*** W4739200 - FEL VID SORTERING'                       
000281         CALL ABEND USING RKOD-16                                         
000282     ELSE                                                                 
000283         PERFORM Z-FINIT                                                  
000284         MOVE ZERO TO RETURN-CODE                                         
000285         GOBACK                                                           
000286     END-IF.                                                              
000287     SKIP2                                                                
000288 A-INIT SECTION.                                                          
000289                                                                          
000290     OPEN INPUT  W47390-EMBIN                                             
000291                 W4739BI                                                  
000292     OPEN OUTPUT W4739A                                                   
000293                 W4739BU                                                  
000294     PERFORM S01-LAS-W4739B                                               
000295                                                                          
000296     ACCEPT WS-DAGENS-DATUM FROM DATE                                     
000297     ACCEPT WS-DAGENS-TID   FROM TIME                                     
000298                                                                          
000299     MOVE JA TO WS-SKRIV-START                                            
000300     .                                                                    
000301     EJECT                                                                
000302 B-FORE-SORTERING  SECTION.                                               
000303                                                                          
000304     PERFORM BA-LAS-W47390                                                
000305     PERFORM UNTIL EOF-W47390 = JA                                        
000306         MOVE SPACE                 TO SORT-POST                          
000307         MOVE EMBIN-IDDISTR         TO SORT-IDDISTR-4768                  
000308         MOVE EMBIN-IDKUNDNR        TO SORT-IDKUNDNR-4768                 
000309         MOVE EMBIN-IDPRODNR        TO SORT-IDPRODNR-4768                 
000310         MOVE EMBIN-IDDC-SEND       TO SORT-IDDC-SEND                     
000311         MOVE EMBIN-IDKUNDRF        TO SORT-IDKUNDRF-4768                 
000312         MOVE EMBIN-KDORDKL         TO SORT-4768-KDORDKL                  
000313         MOVE EMBIN-KVPALL (1)      TO SORT-4768-KVEMBTYP (1, 1)          
000314         MOVE EMBIN-KVPALL (2)      TO SORT-4768-KVEMBTYP (1, 2)          
000315         MOVE EMBIN-KVPALL (3)      TO SORT-4768-KVEMBTYP (1, 3)          
000316         MOVE EMBIN-KVPALL (4)      TO SORT-4768-KVEMBTYP (1, 4)          
000317         MOVE EMBIN-KVPALL (5)      TO SORT-4768-KVEMBTYP (1, 5)          
000318         MOVE EMBIN-KVPALL (6)      TO SORT-4768-KVEMBTYP (1, 6)          
000319         MOVE EMBIN-KVPALL (7)      TO SORT-4768-KVEMBTYP (1, 7)          
000320         MOVE EMBIN-KVPALL (8)      TO SORT-4768-KVEMBTYP (1, 8)          
000321         MOVE EMBIN-KVPALL (9)      TO SORT-4768-KVEMBTYP (1, 9)          
000322         MOVE EMBIN-KVPALL (10)     TO SORT-4768-KVEMBTYP (1, 10)         
000323         MOVE EMBIN-KVPALL (11)     TO SORT-4768-KVEMBTYP (1, 11)         
000324         MOVE EMBIN-KVPALL (12)     TO SORT-4768-KVEMBTYP (1, 12)         
000325         MOVE EMBIN-KVPALL (13)     TO SORT-4768-KVEMBTYP (1, 13)         
000326         MOVE EMBIN-KVPALL (14)     TO SORT-4768-KVEMBTYP (1, 14)         
000327         MOVE EMBIN-KVPALL (15)     TO SORT-4768-KVEMBTYP (1, 15)         
000328         MOVE EMBIN-KVPALL (16)     TO SORT-4768-KVEMBTYP (1, 16)         
000329         MOVE EMBIN-KVPALL (17)     TO SORT-4768-KVEMBTYP (1, 17)         
000330         MOVE EMBIN-KVPALL (18)     TO SORT-4768-KVEMBTYP (1, 18)         
000331         MOVE EMBIN-KVPALL (19)     TO SORT-4768-KVEMBTYP (1, 19)         
000332         MOVE EMBIN-KVPALL (20)     TO SORT-4768-KVEMBTYP (1, 20)         
000333         MOVE EMBIN-KVPALL (21)     TO SORT-4768-KVEMBTYP (1, 21)         
000334         MOVE EMBIN-KVPALL (22)     TO SORT-4768-KVEMBTYP (1, 22)         
000335         MOVE EMBIN-KVPALL (23)     TO SORT-4768-KVEMBTYP (1, 23)         
000336         MOVE EMBIN-KVPALL (24)     TO SORT-4768-KVEMBTYP (1, 24)         
000337         MOVE EMBIN-KVRAM  (1)      TO SORT-4768-KVEMBTYP (2, 1)          
000338         MOVE EMBIN-KVRAM  (2)      TO SORT-4768-KVEMBTYP (2, 2)          
000339         MOVE EMBIN-KVRAM  (3)      TO SORT-4768-KVEMBTYP (2, 3)          
000340         MOVE EMBIN-KVRAM  (4)      TO SORT-4768-KVEMBTYP (2, 4)          
000341         MOVE EMBIN-KVRAM  (5)      TO SORT-4768-KVEMBTYP (2, 5)          
000342         MOVE EMBIN-KVRAM  (6)      TO SORT-4768-KVEMBTYP (2, 6)          
000343         MOVE EMBIN-KVRAM  (7)      TO SORT-4768-KVEMBTYP (2, 7)          
000344         MOVE EMBIN-KVRAM  (8)      TO SORT-4768-KVEMBTYP (2, 8)          
000345         MOVE EMBIN-KVRAM  (9)      TO SORT-4768-KVEMBTYP (2, 9)          
000346         MOVE EMBIN-KVRAM  (10)     TO SORT-4768-KVEMBTYP (2, 10)         
000347         MOVE EMBIN-KVRAM  (11)     TO SORT-4768-KVEMBTYP (2, 11)         
000348         MOVE EMBIN-KVRAM  (12)     TO SORT-4768-KVEMBTYP (2, 12)         
000349         MOVE EMBIN-KVRAM  (13)     TO SORT-4768-KVEMBTYP (2, 13)         
000350         MOVE EMBIN-KVRAM  (14)     TO SORT-4768-KVEMBTYP (2, 14)         
000351         MOVE EMBIN-KVRAM  (15)     TO SORT-4768-KVEMBTYP (2, 15)         
000352         MOVE EMBIN-KVRAM  (16)     TO SORT-4768-KVEMBTYP (2, 16)         
000353         MOVE EMBIN-KVRAM  (17)     TO SORT-4768-KVEMBTYP (2, 17)         
000354         MOVE EMBIN-KVRAM  (18)     TO SORT-4768-KVEMBTYP (2, 18)         
000355         MOVE EMBIN-KVRAM  (19)     TO SORT-4768-KVEMBTYP (2, 19)         
000356         MOVE EMBIN-KVRAM  (20)     TO SORT-4768-KVEMBTYP (2, 20)         
000357         MOVE EMBIN-KVRAM  (21)     TO SORT-4768-KVEMBTYP (2, 21)         
000358         MOVE EMBIN-KVRAM  (22)     TO SORT-4768-KVEMBTYP (2, 22)         
000359         MOVE EMBIN-KVRAM  (23)     TO SORT-4768-KVEMBTYP (2, 23)         
000360         MOVE EMBIN-KVRAM  (24)     TO SORT-4768-KVEMBTYP (2, 24)         
000361         MOVE EMBIN-KVLOCK (1)      TO SORT-4768-KVEMBTYP (3, 1)          
000362         MOVE EMBIN-KVLOCK (2)      TO SORT-4768-KVEMBTYP (3, 2)          
000363         MOVE EMBIN-KVLOCK (3)      TO SORT-4768-KVEMBTYP (3, 3)          
000364         MOVE EMBIN-KVLOCK (4)      TO SORT-4768-KVEMBTYP (3, 4)          
000365         MOVE EMBIN-KVLOCK (5)      TO SORT-4768-KVEMBTYP (3, 5)          
000366         MOVE EMBIN-KVLOCK (6)      TO SORT-4768-KVEMBTYP (3, 6)          
000367         MOVE EMBIN-KVLOCK (7)      TO SORT-4768-KVEMBTYP (3, 7)          
000368         MOVE EMBIN-KVLOCK (8)      TO SORT-4768-KVEMBTYP (3, 8)          
000369         MOVE EMBIN-KVLOCK (9)      TO SORT-4768-KVEMBTYP (3, 9)          
000370         MOVE EMBIN-KVLOCK (10)     TO SORT-4768-KVEMBTYP (3, 10)         
000371         MOVE EMBIN-KVLOCK (11)     TO SORT-4768-KVEMBTYP (3, 11)         
000372         MOVE EMBIN-KVLOCK (12)     TO SORT-4768-KVEMBTYP (3, 12)         
000373         MOVE EMBIN-KVLOCK (13)     TO SORT-4768-KVEMBTYP (3, 13)         
000374         MOVE EMBIN-KVLOCK (14)     TO SORT-4768-KVEMBTYP (3, 14)         
000375         MOVE EMBIN-KVLOCK (15)     TO SORT-4768-KVEMBTYP (3, 15)         
000376         MOVE EMBIN-KVLOCK (16)     TO SORT-4768-KVEMBTYP (3, 16)         
000377         MOVE EMBIN-KVLOCK (17)     TO SORT-4768-KVEMBTYP (3, 17)         
000378         MOVE EMBIN-KVLOCK (18)     TO SORT-4768-KVEMBTYP (3, 18)         
000379         MOVE EMBIN-KVLOCK (19)     TO SORT-4768-KVEMBTYP (3, 19)         
000380         MOVE EMBIN-KVLOCK (20)     TO SORT-4768-KVEMBTYP (3, 20)         
000381         MOVE EMBIN-KVLOCK (21)     TO SORT-4768-KVEMBTYP (3, 21)         
000382         MOVE EMBIN-KVLOCK (22)     TO SORT-4768-KVEMBTYP (3, 22)         
000383         MOVE EMBIN-KVLOCK (23)     TO SORT-4768-KVEMBTYP (3, 23)         
000384         MOVE EMBIN-KVLOCK (24)     TO SORT-4768-KVEMBTYP (3, 24)         
000385         MOVE EMBIN-KVEMBSPA-02(1)  TO SORT-4768-KVEMBTYP(4, 1)           
000386         MOVE EMBIN-KVEMBSPA-02(2)  TO SORT-4768-KVEMBTYP(4, 2)           
000387         MOVE EMBIN-KVEMBSPA-02(3)  TO SORT-4768-KVEMBTYP(4, 3)           
000388         MOVE EMBIN-KVEMBSPA-02(4)  TO SORT-4768-KVEMBTYP(4, 4)           
000389         MOVE EMBIN-KVEMBSPA-02(5)  TO SORT-4768-KVEMBTYP(4, 5)           
000390         MOVE EMBIN-KVEMBSPA-02(6)  TO SORT-4768-KVEMBTYP(4, 6)           
000391         MOVE EMBIN-KVEMBSPA-02(7)  TO SORT-4768-KVEMBTYP(4, 7)           
000392         MOVE EMBIN-KVEMBSPA-02(8)  TO SORT-4768-KVEMBTYP(4, 8)           
000393         MOVE EMBIN-KVEMBSPA-02(9)  TO SORT-4768-KVEMBTYP(4, 9)           
000394         MOVE EMBIN-KVEMBSPA-02(10) TO SORT-4768-KVEMBTYP(4, 10)          
000395         MOVE EMBIN-KVEMBSPA-02(11) TO SORT-4768-KVEMBTYP(4, 11)          
000396         MOVE EMBIN-KVEMBSPA-02(12) TO SORT-4768-KVEMBTYP(4, 12)          
000397         MOVE EMBIN-KVEMBSPA-02(13) TO SORT-4768-KVEMBTYP(4, 13)          
000398         MOVE EMBIN-KVEMBSPA-02(14) TO SORT-4768-KVEMBTYP(4, 14)          
000399         MOVE EMBIN-KVEMBSPA-02(15) TO SORT-4768-KVEMBTYP(4, 15)          
000400         MOVE EMBIN-KVEMBSPA-02(16) TO SORT-4768-KVEMBTYP(4, 16)          
000401         MOVE EMBIN-KVEMBSPA-02(17) TO SORT-4768-KVEMBTYP(4, 17)          
000402         MOVE EMBIN-KVEMBSPA-02(18) TO SORT-4768-KVEMBTYP(4, 18)          
000403         MOVE EMBIN-KVEMBSPA-02(19) TO SORT-4768-KVEMBTYP(4, 19)          
000404         MOVE EMBIN-KVEMBSPA-02(20) TO SORT-4768-KVEMBTYP(4, 20)          
000405         MOVE EMBIN-KVEMBSPA-02(21) TO SORT-4768-KVEMBTYP(4, 21)          
000406         MOVE EMBIN-KVEMBSPA-02(22) TO SORT-4768-KVEMBTYP(4, 22)          
000407         MOVE EMBIN-KVEMBSPA-02(23) TO SORT-4768-KVEMBTYP(4, 23)          
000408         MOVE EMBIN-KVEMBSPA-02(24) TO SORT-4768-KVEMBTYP(4, 24)          
000409         MOVE EMBIN-IDFAKT          TO SORT-4768-IDFAKT                   
000410         MOVE EMBIN-KDFAKTYP        TO SORT-4768-KDFAKTYP                 
000411         MOVE EMBIN-IDDC-REC        TO SORT-IDDC-REC                      
000412                                                                          
000413         RELEASE SORT-POST                                                
000414         PERFORM BA-LAS-W47390                                            
000415     END-PERFORM.                                                         
000416     SKIP2                                                                
000417 BA-LAS-W47390      SECTION.                                              
000418                                                                          
000419     READ W47390-EMBIN INTO EMBIN-AREA                                    
000420     AT END                                                               
000421         MOVE JA TO EOF-W47390                                            
000422     END-READ.                                                            
000423     SKIP2                                                                
000424 C-EFTER-SORTERING  SECTION.                                              
000425                                                                          
000426     PERFORM CA-LAS-SORTFIL                                               
000427     PERFORM CB-SKAPA-START-POST                                          
000428*CO-FIX                                                                   
000429*   TILLFÄLLIG KOD SOM GÄLLER BARA UNDER PILOT TESTPERIODEN               
000430*   IF-SATSERNA UNDER PERFORM-UNTIL TAS BORT NÄR HELA                     
000431*   URVALET TRÄDER I KRAFT                                                
000432*   OBS: KOLLA ÄVEN KOD SOM GÄLLER WS-SKRIV-START!                        
000433*                                                                         
000434     PERFORM UNTIL SORTFIL-EOF = JA                                       
000435                                                                          
000436       MOVE EMB-IDDC-SEND        TO WS-IDDC                               
000437       IF EMB-IDDC-SEND NOT = W-IDDC-B6-S                                 
000438         MOVE EMB-IDDC-SEND      TO W-IDDC-B6-S                           
000439         PERFORM IMS-GU-WDB601-SEND                                       
000440       END-IF                                                             
000441       IF EMB-IDDC-REC  NOT = W-IDDC-B6-R                                 
000442         MOVE EMB-IDDC-REC       TO W-IDDC-B6-R                           
000443         PERFORM IMS-GU-WDB601-REC                                        
000444       END-IF                                                             
000445                                                                          
000446       PERFORM CC-HAMTA-EMBNR                                             
000447                                                                          
000448       PERFORM CD-SKAPA-HUVUD-POST                                        
000449                                                                          
000450       PERFORM CE-SKAPA-DETALJ-POST                                       
000460                                                                          
000470       PERFORM CA-LAS-SORTFIL                                             
000480     END-PERFORM                                                          
000490     .                                                                    
000500     EJECT                                                                
000510 CA-LAS-SORTFIL  SECTION.                                                 
000520                                                                          
000530     RETURN SORTFIL INTO EMB-AREA                                         
000531       AT END                                                             
000532       MOVE JA TO SORTFIL-EOF                                             
000533     END-RETURN                                                           
000534     .                                                                    
000535     EJECT                                                                
000536 CB-SKAPA-START-POST SECTION.                                             
000537                                                                          
000538     IF SORTFIL-EOF = NEJ                                                 
000539       MOVE WS-DAGENS-YYMMDDHHMMSS TO TMS-IDBATCH                         
000540                                      WS-TMS-YYMMDDHHMMSS                 
000550     END-IF                                                               
000551     .                                                                    
000552     EJECT                                                                
000553 CC-HAMTA-EMBNR SECTION.                                                  
000554                                                                          
000555     IF (SEND-DCS-CDC OR                                                  
000556        (SEND-DCS-SDC AND SEND-DCS-IDLANDX2 = 'SE'))                      
000557       IF W01NUM-IDEMBNR(1) > W01NUM-IDEMBNR-MAX(1)                       
000558         MOVE W01NUM-IDEMBNR-MIN(1) TO W01NUM-IDEMBNR(1)                  
000559       END-IF                                                             
000560       MOVE W01NUM-IDEMBNR(1) TO WS-IDEMBNR-AKT                           
000570       ADD +1 TO W01NUM-IDEMBNR(1)                                        
000580     ELSE                                                                 
000590       IF W01NUM-IDEMBNR(2) > W01NUM-IDEMBNR-MAX(2)                       
000600         MOVE W01NUM-IDEMBNR-MIN(2) TO W01NUM-IDEMBNR(2)                  
000610       END-IF                                                             
000620       MOVE W01NUM-IDEMBNR(2) TO WS-IDEMBNR-AKT                           
000630       ADD +1 TO W01NUM-IDEMBNR(2)                                        
000640     END-IF                                                               
000650     .                                                                    
000660     EJECT                                                                
000670 CD-SKAPA-HUVUD-POST SECTION.                                             
000680                                                                          
000681     ADD +1                   TO TMS-IDLOPNR                              
000682     MOVE TMS-IDLOPNR         TO WS-TMS-IDLOPNR                           
000683     MOVE WS-TMS-IDBOKN       TO TMS-IDBOKN                               
000684     MOVE DAGENS-DATUM-DD     TO DAGENS-DAT-DD                            
000685     MOVE DAGENS-DATUM-MM     TO DAGENS-DAT-MM                            
000686     MOVE DAGENS-DATUM-AA     TO DAGENS-DAT-AA                            
000687     MOVE WS-DAGENS-DDMMYYYY  TO TMS-DATUM-ISSUED                         
000688                                 TMS-DATUM-BOOKED                         
000689     IF SEND-DCS-IDLEVNR-EMB = SPACE                                      
000690       MOVE SEND-DCS-IDLEVNR-DC                                           
000691                              TO TMS-IDLEVNR-GSDB-SEND                    
000692     ELSE                                                                 
000693       MOVE SEND-DCS-IDLEVNR-EMB                                          
000694                              TO TMS-IDLEVNR-GSDB-SEND                    
000695     END-IF                                                               
000696     IF REC-DCS-IDLEVNR-EMB = SPACE                                       
000697       MOVE REC-DCS-IDLEVNR-DC                                            
000698                              TO TMS-IDLEVNR-GSDB-REC                     
000699     ELSE                                                                 
000700       MOVE REC-DCS-IDLEVNR-EMB                                           
000701                              TO TMS-IDLEVNR-GSDB-REC                     
000702     END-IF                                                               
000703                                                                          
000704     IF WS-SKRIV-START = JA                                               
000705       PERFORM S02-SKRIV-W4739A-START                                     
000706       MOVE NEJ TO WS-SKRIV-START                                         
000707     END-IF                                                               
000708                                                                          
000709     PERFORM S03-SKRIV-W4739A-HUVUD                                       
000710     .                                                                    
000711     EJECT                                                                
000712 CE-SKAPA-DETALJ-POST SECTION.                                            
000713                                                                          
000714     MOVE WS-IDEMBNR-AKT TO TMS-FSEDELNR                                  
000715                                                                          
000716     IF EMB-4768-KVEMBTYP (1, 1) > ZERO                                   
000717         MOVE 1                         TO TMS-EMBTYP                     
000718         MOVE EMB-4768-KVEMBTYP (1, 1)  TO TMS-EMBANTAL                   
000719     PERFORM S04-SKRIV-W4739A-DETALJ                                      
000720     END-IF                                                               
000730     IF EMB-4768-KVEMBTYP (1, 2) > ZERO                                   
000731         MOVE 2                         TO TMS-EMBTYP                     
000732         MOVE EMB-4768-KVEMBTYP (1, 2)  TO TMS-EMBANTAL                   
000733     PERFORM S04-SKRIV-W4739A-DETALJ                                      
000734     END-IF                                                               
000735     IF EMB-4768-KVEMBTYP (1, 3) > ZERO                                   
000736         MOVE 5                         TO TMS-EMBTYP                     
000737         MOVE EMB-4768-KVEMBTYP (1, 3)  TO TMS-EMBANTAL                   
000738         PERFORM S04-SKRIV-W4739A-DETALJ                                  
000739     END-IF                                                               
000740     IF EMB-4768-KVEMBTYP (1, 4) > ZERO                                   
000741         MOVE 6                         TO TMS-EMBTYP                     
000742         MOVE EMB-4768-KVEMBTYP (1, 4)  TO TMS-EMBANTAL                   
000743         PERFORM S04-SKRIV-W4739A-DETALJ                                  
000744     END-IF                                                               
000745     IF EMB-4768-KVEMBTYP (1, 5) > ZERO                                   
000746         MOVE 9                         TO TMS-EMBTYP                     
000747         MOVE EMB-4768-KVEMBTYP (1, 5)  TO TMS-EMBANTAL                   
000748         PERFORM S04-SKRIV-W4739A-DETALJ                                  
000749     END-IF                                                               
000750     IF EMB-4768-KVEMBTYP (1, 6) > ZERO                                   
000751         MOVE 2181                      TO TMS-EMBTYP                     
000752         MOVE EMB-4768-KVEMBTYP (1, 6)  TO TMS-EMBANTAL                   
000753         PERFORM S04-SKRIV-W4739A-DETALJ                                  
000754     END-IF                                                               
000755     IF EMB-4768-KVEMBTYP (1, 7) > ZERO                                   
000756         MOVE 701                       TO TMS-EMBTYP                     
000757         MOVE EMB-4768-KVEMBTYP (1, 7)  TO TMS-EMBANTAL                   
000758         PERFORM S04-SKRIV-W4739A-DETALJ                                  
000759     END-IF                                                               
000760     IF EMB-4768-KVEMBTYP (1, 8) > ZERO                                   
000761         MOVE 780                       TO TMS-EMBTYP                     
000762         MOVE EMB-4768-KVEMBTYP (1, 8)  TO TMS-EMBANTAL                   
000763         PERFORM S04-SKRIV-W4739A-DETALJ                                  
000764     END-IF                                                               
000765     IF EMB-4768-KVEMBTYP (1, 9) > ZERO                                   
000766         MOVE 5                         TO TMS-EMBTYP                     
000767         MOVE EMB-4768-KVEMBTYP (1, 9)  TO TMS-EMBANTAL                   
000768         PERFORM S04-SKRIV-W4739A-DETALJ                                  
000769     END-IF                                                               
000770     IF EMB-4768-KVEMBTYP (1, 10) > ZERO                                  
000771         MOVE 5                         TO TMS-EMBTYP                     
000772         MOVE EMB-4768-KVEMBTYP (1, 10) TO TMS-EMBANTAL                   
000773         PERFORM S04-SKRIV-W4739A-DETALJ                                  
000774     END-IF                                                               
000775     IF EMB-4768-KVEMBTYP (1, 11) > ZERO                                  
000776         MOVE 790                       TO TMS-EMBTYP                     
000777         MOVE EMB-4768-KVEMBTYP (1, 11) TO TMS-EMBANTAL                   
000778         PERFORM S04-SKRIV-W4739A-DETALJ                                  
000779     END-IF                                                               
000780     IF EMB-4768-KVEMBTYP (1, 12) > ZERO                                  
000781         MOVE 750                       TO TMS-EMBTYP                     
000782         MOVE EMB-4768-KVEMBTYP (1, 12) TO TMS-EMBANTAL                   
000783         PERFORM S04-SKRIV-W4739A-DETALJ                                  
000784     END-IF                                                               
000785     IF EMB-4768-KVEMBTYP (1, 13) > ZERO                                  
000786         MOVE 1421                      TO TMS-EMBTYP                     
000787         MOVE EMB-4768-KVEMBTYP (1, 13) TO TMS-EMBANTAL                   
000788         PERFORM S04-SKRIV-W4739A-DETALJ                                  
000789     END-IF                                                               
000790     IF EMB-4768-KVEMBTYP (1, 14) > ZERO                                  
000791         MOVE 107                       TO TMS-EMBTYP                     
000792         MOVE EMB-4768-KVEMBTYP (1, 14) TO TMS-EMBANTAL                   
000793         PERFORM S04-SKRIV-W4739A-DETALJ                                  
000794     END-IF                                                               
000795     IF EMB-4768-KVEMBTYP (1, 15) > ZERO                                  
000796         MOVE 460                       TO TMS-EMBTYP                     
000797         MOVE EMB-4768-KVEMBTYP (1, 15) TO TMS-EMBANTAL                   
000798         PERFORM S04-SKRIV-W4739A-DETALJ                                  
000799     END-IF                                                               
000800     IF EMB-4768-KVEMBTYP (1, 16) > ZERO                                  
000801         MOVE 576                       TO TMS-EMBTYP                     
000802         MOVE EMB-4768-KVEMBTYP (1, 16) TO TMS-EMBANTAL                   
000803         PERFORM S04-SKRIV-W4739A-DETALJ                                  
000804     END-IF                                                               
000805     IF EMB-4768-KVEMBTYP (1, 17) > ZERO                                  
000806         MOVE 595                       TO TMS-EMBTYP                     
000807         MOVE EMB-4768-KVEMBTYP (1, 17) TO TMS-EMBANTAL                   
000808         PERFORM S04-SKRIV-W4739A-DETALJ                                  
000809     END-IF                                                               
000810     IF EMB-4768-KVEMBTYP (1, 18) > ZERO                                  
000811         MOVE 724                       TO TMS-EMBTYP                     
000812         MOVE EMB-4768-KVEMBTYP (1, 18) TO TMS-EMBANTAL                   
000813         PERFORM S04-SKRIV-W4739A-DETALJ                                  
000814     END-IF                                                               
000815     IF EMB-4768-KVEMBTYP (1, 19) > ZERO                                  
000816         MOVE 742                       TO TMS-EMBTYP                     
000817         MOVE EMB-4768-KVEMBTYP (1, 19) TO TMS-EMBANTAL                   
000818         PERFORM S04-SKRIV-W4739A-DETALJ                                  
000819     END-IF                                                               
000820     IF EMB-4768-KVEMBTYP (1, 20) > ZERO                                  
000821         MOVE 743                       TO TMS-EMBTYP                     
000822         MOVE EMB-4768-KVEMBTYP (1, 20) TO TMS-EMBANTAL                   
000823         PERFORM S04-SKRIV-W4739A-DETALJ                                  
000824     END-IF                                                               
000825     IF EMB-4768-KVEMBTYP (1, 21) > ZERO                                  
000826         MOVE 840                       TO TMS-EMBTYP                     
000827         MOVE EMB-4768-KVEMBTYP (1, 21) TO TMS-EMBANTAL                   
000828         PERFORM S04-SKRIV-W4739A-DETALJ                                  
000829     END-IF                                                               
000830     IF EMB-4768-KVEMBTYP (1, 22) > ZERO                                  
000831         MOVE 9140                      TO TMS-EMBTYP                     
000832         MOVE EMB-4768-KVEMBTYP (1, 22) TO TMS-EMBANTAL                   
000833         PERFORM S04-SKRIV-W4739A-DETALJ                                  
000834     END-IF                                                               
000835     IF EMB-4768-KVEMBTYP (1, 23) > ZERO                                  
000836         MOVE 9150                      TO TMS-EMBTYP                     
000837         MOVE EMB-4768-KVEMBTYP (1, 23) TO TMS-EMBANTAL                   
000838         PERFORM S04-SKRIV-W4739A-DETALJ                                  
000839     END-IF                                                               
000840     IF EMB-4768-KVEMBTYP (2, 1) > ZERO                                   
000841         MOVE 21                        TO TMS-EMBTYP                     
000842         MOVE EMB-4768-KVEMBTYP (2, 1)  TO TMS-EMBANTAL                   
000843         PERFORM S04-SKRIV-W4739A-DETALJ                                  
000844     END-IF                                                               
000845     IF EMB-4768-KVEMBTYP (2, 2) > ZERO                                   
000846         MOVE 22                        TO TMS-EMBTYP                     
000847         MOVE EMB-4768-KVEMBTYP (2, 2)  TO TMS-EMBANTAL                   
000848        PERFORM S04-SKRIV-W4739A-DETALJ                                   
000849     END-IF                                                               
000850     IF EMB-4768-KVEMBTYP (2, 3) > ZERO                                   
000851         MOVE 25                        TO TMS-EMBTYP                     
000852         MOVE EMB-4768-KVEMBTYP (2, 3)  TO TMS-EMBANTAL                   
000853         PERFORM S04-SKRIV-W4739A-DETALJ                                  
000854     END-IF                                                               
000855     IF EMB-4768-KVEMBTYP (2, 4) > ZERO                                   
000856         MOVE 26                        TO TMS-EMBTYP                     
000857         MOVE EMB-4768-KVEMBTYP (2, 4)  TO TMS-EMBANTAL                   
000858         PERFORM S04-SKRIV-W4739A-DETALJ                                  
000859     END-IF                                                               
000860     IF EMB-4768-KVEMBTYP (2, 5) > ZERO                                   
000861         MOVE 29                        TO TMS-EMBTYP                     
000862         MOVE EMB-4768-KVEMBTYP (2, 5)  TO TMS-EMBANTAL                   
000863         PERFORM S04-SKRIV-W4739A-DETALJ                                  
000864     END-IF                                                               
000865     IF EMB-4768-KVEMBTYP (2, 7) > ZERO                                   
000866         MOVE 702                       TO TMS-EMBTYP                     
000867         MOVE EMB-4768-KVEMBTYP (2, 7)  TO TMS-EMBANTAL                   
000868         PERFORM S04-SKRIV-W4739A-DETALJ                                  
000869     END-IF                                                               
000870     IF EMB-4768-KVEMBTYP (2, 9) > ZERO                                   
000871         MOVE 419                       TO TMS-EMBTYP                     
000872         MOVE EMB-4768-KVEMBTYP (2, 9)  TO TMS-EMBANTAL                   
000873         PERFORM S04-SKRIV-W4739A-DETALJ                                  
000874     END-IF                                                               
000875     IF EMB-4768-KVEMBTYP (2, 10) > ZERO                                  
000876         MOVE 422                       TO TMS-EMBTYP                     
000877         MOVE EMB-4768-KVEMBTYP (2, 10) TO TMS-EMBANTAL                   
000878         PERFORM S04-SKRIV-W4739A-DETALJ                                  
000879     END-IF                                                               
000880     IF EMB-4768-KVEMBTYP (2, 18) > ZERO                                  
000881         MOVE 725                       TO TMS-EMBTYP                     
000882         MOVE EMB-4768-KVEMBTYP (2, 18) TO TMS-EMBANTAL                   
000883         PERFORM S04-SKRIV-W4739A-DETALJ                                  
000884     END-IF                                                               
000885     IF EMB-4768-KVEMBTYP (2, 22) > ZERO                                  
000886         MOVE 9832                      TO TMS-EMBTYP                     
000887         MOVE EMB-4768-KVEMBTYP (2, 22) TO TMS-EMBANTAL                   
000888         PERFORM S04-SKRIV-W4739A-DETALJ                                  
000889     END-IF                                                               
000890     IF EMB-4768-KVEMBTYP (2, 23) > ZERO                                  
000891         MOVE 6826                      TO TMS-EMBTYP                     
000892         MOVE EMB-4768-KVEMBTYP (2, 23) TO TMS-EMBANTAL                   
000893         PERFORM S04-SKRIV-W4739A-DETALJ                                  
000894     END-IF                                                               
000895     IF EMB-4768-KVEMBTYP (3, 1) > ZERO                                   
000896         MOVE 71                        TO TMS-EMBTYP                     
000897         MOVE EMB-4768-KVEMBTYP (3, 1)  TO TMS-EMBANTAL                   
000898         PERFORM S04-SKRIV-W4739A-DETALJ                                  
000899     END-IF                                                               
000900     IF EMB-4768-KVEMBTYP (3, 2) > ZERO                                   
000901         MOVE 72                        TO TMS-EMBTYP                     
000902         MOVE EMB-4768-KVEMBTYP (3, 2)  TO TMS-EMBANTAL                   
000903         PERFORM S04-SKRIV-W4739A-DETALJ                                  
000904     END-IF                                                               
000905     IF EMB-4768-KVEMBTYP (3, 3) > ZERO                                   
000906         MOVE 75                        TO TMS-EMBTYP                     
000907         MOVE EMB-4768-KVEMBTYP (3, 3)  TO TMS-EMBANTAL                   
000908         PERFORM S04-SKRIV-W4739A-DETALJ                                  
000909     END-IF                                                               
000910     IF EMB-4768-KVEMBTYP (3, 4) > ZERO                                   
000911         MOVE 76                        TO TMS-EMBTYP                     
000912         MOVE EMB-4768-KVEMBTYP (3, 4)  TO TMS-EMBANTAL                   
000913         PERFORM S04-SKRIV-W4739A-DETALJ                                  
000914     END-IF                                                               
000915     IF EMB-4768-KVEMBTYP (3, 5) > ZERO                                   
000916         MOVE 79                        TO TMS-EMBTYP                     
000917         MOVE EMB-4768-KVEMBTYP (3, 5)  TO TMS-EMBANTAL                   
000918         PERFORM S04-SKRIV-W4739A-DETALJ                                  
000919     END-IF                                                               
000920     IF EMB-4768-KVEMBTYP (3, 7) > ZERO                                   
000921         MOVE 706                       TO TMS-EMBTYP                     
000922         MOVE EMB-4768-KVEMBTYP (3, 7)  TO TMS-EMBANTAL                   
000923         PERFORM S04-SKRIV-W4739A-DETALJ                                  
000924     END-IF                                                               
000925     IF EMB-4768-KVEMBTYP (3, 8) > ZERO                                   
000926         MOVE 781                       TO TMS-EMBTYP                     
000927         MOVE EMB-4768-KVEMBTYP (3, 8)  TO TMS-EMBANTAL                   
000928         PERFORM S04-SKRIV-W4739A-DETALJ                                  
000929     END-IF                                                               
000930     IF EMB-4768-KVEMBTYP (3, 9) > ZERO                                   
000931         MOVE 236                       TO TMS-EMBTYP                     
000932         MOVE EMB-4768-KVEMBTYP (3, 9)  TO TMS-EMBANTAL                   
000933         PERFORM S04-SKRIV-W4739A-DETALJ                                  
000934     END-IF                                                               
000935     IF EMB-4768-KVEMBTYP (3, 10) > ZERO                                  
000936         MOVE 236                       TO TMS-EMBTYP                     
000937         MOVE EMB-4768-KVEMBTYP (3, 10) TO TMS-EMBANTAL                   
000938         PERFORM S04-SKRIV-W4739A-DETALJ                                  
000939     END-IF                                                               
000940     IF EMB-4768-KVEMBTYP (3, 11) > ZERO                                  
000941         MOVE 791                       TO TMS-EMBTYP                     
000942         MOVE EMB-4768-KVEMBTYP (3, 11) TO TMS-EMBANTAL                   
000943         PERFORM S04-SKRIV-W4739A-DETALJ                                  
000944     END-IF                                                               
000945     IF EMB-4768-KVEMBTYP (3, 12) > ZERO                                  
000946         MOVE 751                       TO TMS-EMBTYP                     
000947         MOVE EMB-4768-KVEMBTYP (3, 12) TO TMS-EMBANTAL                   
000948         PERFORM S04-SKRIV-W4739A-DETALJ                                  
000949     END-IF                                                               
000950     IF EMB-4768-KVEMBTYP (3, 13) > ZERO                                  
000951         MOVE 1422                      TO TMS-EMBTYP                     
000952         MOVE EMB-4768-KVEMBTYP (3, 13) TO TMS-EMBANTAL                   
000953         PERFORM S04-SKRIV-W4739A-DETALJ                                  
000954     END-IF                                                               
000955     IF EMB-4768-KVEMBTYP (3, 15) > ZERO                                  
000956         MOVE 461                       TO TMS-EMBTYP                     
000957         MOVE EMB-4768-KVEMBTYP (3, 15) TO TMS-EMBANTAL                   
000958         PERFORM S04-SKRIV-W4739A-DETALJ                                  
000959     END-IF                                                               
000960     IF EMB-4768-KVEMBTYP (3, 18) > ZERO                                  
000961         MOVE 726                       TO TMS-EMBTYP                     
000962         MOVE EMB-4768-KVEMBTYP (3, 18) TO TMS-EMBANTAL                   
000963         PERFORM S04-SKRIV-W4739A-DETALJ                                  
000964     END-IF                                                               
000965     IF EMB-4768-KVEMBTYP (3, 21) > ZERO                                  
000966         MOVE 841                       TO TMS-EMBTYP                     
000967         MOVE EMB-4768-KVEMBTYP (3, 21) TO TMS-EMBANTAL                   
000968         PERFORM S04-SKRIV-W4739A-DETALJ                                  
000969     END-IF                                                               
000970     IF EMB-4768-KVEMBTYP (3, 22) > ZERO                                  
000971         MOVE 9141                      TO TMS-EMBTYP                     
000972         MOVE EMB-4768-KVEMBTYP (3, 22) TO TMS-EMBANTAL                   
000973         PERFORM S04-SKRIV-W4739A-DETALJ                                  
000974     END-IF                                                               
000975     IF EMB-4768-KVEMBTYP (3, 23) > ZERO                                  
000976         MOVE 9151                      TO TMS-EMBTYP                     
000977         MOVE EMB-4768-KVEMBTYP (3, 23) TO TMS-EMBANTAL                   
000978         PERFORM S04-SKRIV-W4739A-DETALJ                                  
000979     END-IF                                                               
000980     IF EMB-4768-KVEMBTYP (4, 1) > ZERO                                   
000981         MOVE 61                        TO TMS-EMBTYP                     
000982         MOVE EMB-4768-KVEMBTYP (4, 1)  TO TMS-EMBANTAL                   
000983         PERFORM S04-SKRIV-W4739A-DETALJ                                  
000984     END-IF                                                               
000985     IF EMB-4768-KVEMBTYP (4, 2) > ZERO                                   
000986         MOVE 62                        TO TMS-EMBTYP                     
000987         MOVE EMB-4768-KVEMBTYP (4, 2)  TO TMS-EMBANTAL                   
000988         PERFORM S04-SKRIV-W4739A-DETALJ                                  
000989     END-IF                                                               
000990     IF EMB-4768-KVEMBTYP (4, 3) > ZERO                                   
000991         MOVE 65                        TO TMS-EMBTYP                     
000992         MOVE EMB-4768-KVEMBTYP (4, 3)  TO TMS-EMBANTAL                   
000993         PERFORM S04-SKRIV-W4739A-DETALJ                                  
000994     END-IF                                                               
000995     IF EMB-4768-KVEMBTYP (4, 4) > ZERO                                   
000996         MOVE 66                        TO TMS-EMBTYP                     
000997         MOVE EMB-4768-KVEMBTYP (4, 4)  TO TMS-EMBANTAL                   
000998         PERFORM S04-SKRIV-W4739A-DETALJ                                  
000999     END-IF                                                               
001000     IF EMB-4768-KVEMBTYP (4, 5) > ZERO                                   
001001         MOVE 69                        TO TMS-EMBTYP                     
001002         MOVE EMB-4768-KVEMBTYP (4, 5)  TO TMS-EMBANTAL                   
001003         PERFORM S04-SKRIV-W4739A-DETALJ                                  
001004     END-IF                                                               
001005     IF EMB-4768-KVEMBTYP (4, 9) > ZERO                                   
001006         MOVE 65                        TO TMS-EMBTYP                     
001007         MOVE EMB-4768-KVEMBTYP (4, 9)  TO TMS-EMBANTAL                   
001008         PERFORM S04-SKRIV-W4739A-DETALJ                                  
001009     END-IF                                                               
001010     IF EMB-4768-KVEMBTYP (4, 10) > ZERO                                  
001011         MOVE 65                        TO TMS-EMBTYP                     
001012         MOVE EMB-4768-KVEMBTYP (4, 10) TO TMS-EMBANTAL                   
001013         PERFORM S04-SKRIV-W4739A-DETALJ                                  
001014     END-IF                                                               
001015     IF EMB-4768-KVEMBTYP (4, 22) > ZERO                                  
001016         MOVE 61                        TO TMS-EMBTYP                     
001017         MOVE EMB-4768-KVEMBTYP (4, 22) TO TMS-EMBANTAL                   
001018         PERFORM S04-SKRIV-W4739A-DETALJ                                  
001019     END-IF                                                               
001020     IF EMB-4768-KVEMBTYP (4, 23) > ZERO                                  
001021         MOVE 65                        TO TMS-EMBTYP                     
001022         MOVE EMB-4768-KVEMBTYP (4, 23) TO TMS-EMBANTAL                   
001023         PERFORM S04-SKRIV-W4739A-DETALJ                                  
001024     END-IF                                                               
001025     .                                                                    
001026     SKIP2                                                                
001027 Z-FINIT  SECTION.                                                        
001028                                                                          
001029     PERFORM S05-SKRIV-W4739BU                                            
001030     CLOSE W47390-EMBIN                                                   
001031           W4739A                                                         
001032           W4739BI                                                        
001033           W4739BU                                                        
001034     .                                                                    
001035     SKIP2                                                                
001036 S01-LAS-W4739B SECTION.                                                  
001037                                                                          
001038     READ W4739BI INTO W01NUM-W4739B                                      
001039     AT END                                                               
001040         MOVE JA TO EOF-W4739B                                            
001041     END-READ                                                             
001042     .                                                                    
001043     EJECT                                                                
001044 S02-SKRIV-W4739A-START SECTION.                                          
001045                                                                          
001046     WRITE UT-STA-POST FROM STA-AREA                                      
001047     .                                                                    
001048     SKIP2                                                                
001049 S03-SKRIV-W4739A-HUVUD SECTION.                                          
001050                                                                          
001051     WRITE UT-HUV-POST FROM HUV-AREA                                      
001052     .                                                                    
001053     SKIP2                                                                
001054 S04-SKRIV-W4739A-DETALJ SECTION.                                         
001055                                                                          
001056     WRITE UT-DET-POST FROM DET-AREA                                      
001057     .                                                                    
001058     SKIP2                                                                
001059 S05-SKRIV-W4739BU SECTION.                                               
001060                                                                          
001061     WRITE U01NUM-POST FROM W01NUM-W4739B                                 
001062     .                                                                    
001063     EJECT                                                                
001064*****                                                                     
001065     EJECT                                                                
001066* IMS SECTIONER                                                           
001067     SKIP3                                                                
001068 IMS-GU-WDB601-SEND SECTION.                                              
001069     STRING 'WDB601  (IDDC     =' W-IDDC-B6-S-X ')'                       
001070            DELIMITED BY SIZE INTO SSA1                                   
001071     MOVE '  GE' TO GODK-STATUSKODER                                      
001072     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601-SEND SSA1            
001073     MOVE WDB6-STATUS-CODE     TO STATUS-WS                               
001074     PERFORM IMS-STATUSKONTROLL                                           
001075     IF SEGMENT-SAKNAS                                                    
001076        MOVE SPACE TO SEND-DCS-KDDC                                       
001077     END-IF                                                               
001078     .                                                                    
001079     EJECT                                                                
001080 IMS-GU-WDB601-REC    SECTION.                                            
001081     STRING 'WDB601  (IDDC     =' W-IDDC-B6-R-X ')'                       
001082            DELIMITED BY SIZE INTO SSA1                                   
001083     MOVE '  GE' TO GODK-STATUSKODER                                      
001084     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601-REC SSA1             
001085     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
001086     PERFORM IMS-STATUSKONTROLL                                           
001087     IF SEGMENT-SAKNAS                                                    
001088        MOVE SPACE TO REC-DCS-KDDC                                        
001089     END-IF                                                               
001090     .                                                                    
001091     EJECT                                                                
001092 IMS-STATUSKONTROLL SECTION.                                              
001093     SET STATUS-IX TO 1                                                   
001094     SEARCH GODK-STATUS                                                   
001095       AT END                                                             
001096         CALL FELLOG                                                      
001097       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
001098         CONTINUE                                                         
001099     END-SEARCH                                                           
001100     .                                                                    
