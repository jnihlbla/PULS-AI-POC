000001 ID DIVISION.                                                             
000002 PROGRAM-ID.     WF210600.                                                
000003 AUTHOR.         ANDERS HENRIKSSON.                                       
000004 DATE-WRITTEN.   2003-05-20.                                              
000005 DATE-COMPILED.                                                           
000006                                                                          
000007*    FUNCTION:                                                            
000008*    THE PGM                                                              
000009*    - READS FILE WITH GL/AR DATA RECORDS                                 
000010*    - SENDS GL/AR DATA RECORDS FOR VCCS TO:                              
000011*      W510 (PULS)      BY WZ01SEND (CARPARTS.PULS.RECGL)                 
000012                                                                          
000013 ENVIRONMENT DIVISION.                                                    
000014                                                                          
000015 INPUT-OUTPUT SECTION.                                                    
000016                                                                          
000017 FILE-CONTROL.                                                            
000018*          --- GL/AR DATA RECORDS                                         
000019     SELECT WF2014                     ASSIGN TO WF2106D1.                
000020     EJECT                                                                
000021 DATA DIVISION.                                                           
000022                                                                          
000023 FILE SECTION.                                                            
000024 FD  WF2014                                                               
000025     RECORDING       F                                                    
000026     BLOCK CONTAINS  0.                                                   
000027                                                                          
000028*01  -COPY WF2014      -L.                                                
000029     EJECT                                                                
000030                                                                          
000031 WORKING-STORAGE SECTION.                                                 
000032*** - CONSTANTS                                                           
000033 77  IDPGM                       PIC X(8)    VALUE 'WF210600'.            
000034 77  WS-IDDISTR-1                PIC 9(5)    VALUE ZERO.                  
000035 77  WS-IDEXCUST                 PIC X(15)   VALUE SPACE.                 
000036 77  W-ANT                       PIC S9(3)  VALUE ZERO COMP-3.            
000037 77  WS-IDLEGSEL-VCCS            PIC X(4)    VALUE 'VCCS'.                
000038 77  WS-ADRESS-PULS              PIC X(50)                                
000039                                VALUE 'CARPARTS.PULS.RECGL'.              
000040 77  WS-IDSYSTEM-VSS             PIC X(4)    VALUE 'VSS'.                 
000041 77  WS-IDSYSTEM-VIPS            PIC X(4)    VALUE 'VIPS'.                
000042                                                                          
000043 77  YES                         PIC X(1)    VALUE 'J'.                   
000044 77  NOO                         PIC X(1)    VALUE 'N'.                   
000045                                                                          
000046 77  WF2014-EOF-SW               PIC X       VALUE 'N'.                   
000047     88  END-OF-WF2014                       VALUE 'J'.                   
000048                                                                          
000049 77  FIRST-TIME-SW               PIC X       VALUE 'J'.                   
000050     88  FIRST-TIME                          VALUE 'J'.                   
000051                                                                          
000052 77  SEND-TO-GL-SW               PIC X       VALUE 'N'.                   
000053     88  SEND-TO-GL                          VALUE 'J'.                   
000054     88  DONT-SEND-TO-GL                     VALUE 'N'.                   
000055                                                                          
000056 01  WS-SEND-IDCOM               PIC S9(9)   COMP VALUE +0.               
000057 01  WS-DATE                     PIC S9(8)   COMP VALUE +0.               
000058 01  WS-TIME                     PIC S9(6)   COMP VALUE +0.               
000059 01  WS-DAFINDOC                 PIC S9(8)   COMP VALUE +0.               
000060 01  WS-IDFINDOC                 PIC S9(9)   COMP VALUE +0.               
000061 01  WS-IDFINDOC-ALFA            PIC X(9).                                
000062                                                                          
000063 01  WS-IDPARTNR-NUM1            PIC 9(1).                                
000064 01  WS-IDPARTNR-NUM2            PIC 9(2).                                
000065 01  WS-IDPARTNR-NUM3            PIC 9(3).                                
000066 01  WS-IDPARTNR-NUM4            PIC 9(4).                                
000067 01  WS-IDPARTNR-NUM5            PIC 9(5).                                
000068 01  WS-IDPARTNR-NUM6            PIC 9(6).                                
000069 01  WS-IDPARTNR-NUM7            PIC 9(7).                                
000070                                                                          
000071 01  WS-IDACCNT-NUM              PIC 9(6).                                
000072                                                                          
000073 01  WS-IDKST                    PIC X(10).                               
000074                                                                          
000075 01  WS-SUBEL-LINE               PIC 9(9)V99 VALUE ZERO.                  
000076 01  WS-SUBEL-LINE-VSS           PIC 9(9)V99 VALUE ZERO.                  
000077 01  WS-DIFF                     PIC S9(9)V99.                            
000078 01  WS-DIFF-VSS                 PIC S9(9)V99.                            
000079                                                                          
000080 01  ERRTEXT.                                                             
000081     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
000082     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
000083 01  KDRC-DISPLAY                PIC Z(5).                                
000084     EJECT                                                                
000085                                                                          
000086 01  GENERAL-SUBPROGRAMS.                                                 
000087     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
000088     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
000089     EJECT                                                                
000090                                                                          
000091*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
000092                                                                          
000093 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
000094 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
000095 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
000096     EJECT                                                                
000097                                                                          
000098*    --- AREOR FÖR KOMMUNIKATION                                          
000099 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
000100*01  -COPY WZ01SEND                                                       
000101     EJECT                                                                
000102                                                                          
000103 01  IN-AREA-START            PIC X(24)   VALUE 'IN-AREA      '.          
000104 01  IN-AREA.                                                             
000105*    03  -COPY WF2014                                                     
000106     EJECT                                                                
000107                                                                          
000108 01  SPAR-AREA-START          PIC X(24)   VALUE 'SPAR-AREA    '.          
000109 01  SPAR-AREA.                                                           
000110*    03  -COPY WF2014     -PRE SPAR-                                      
000111     EJECT                                                                
000112                                                                          
000113 01  UT-AREA-START         PIC X(24)   VALUE                              
000114                                              'UT-AREA      '.            
000115 01  UT-AREA.                                                             
000116*    03  -COPY WZ01REQU   -PRE UT-                                        
000117*    03  -COPY WF2106     -PRE UT-                                        
000118*      05  -COPY WDR901   -PRE UT- -RED UT-FIL-WF2106-DATA                
000119*       09 -COPY W510EKHA -PRE UT- -RED UT-FIL-WDR901-DATA                
000120     EJECT                                                                
000121                                                                          
000122 LINKAGE SECTION.                                                         
000123*01  -COPY W0009   -PRE MSG-                                              
000124     EJECT                                                                
000125                                                                          
000126 PROCEDURE DIVISION  USING MSG-PCB.                                       
000127 MAIN SECTION.                                                            
000128     ENTRY 'DLITCBL' USING MSG-PCB.                                       
000129                                                                          
000130     PERFORM A-INIT                                                       
000131     PERFORM B-EXECUTE                                                    
000132     PERFORM Z-FINIT                                                      
000133                                                                          
000134     MOVE ZERO TO RETURN-CODE                                             
000135     GOBACK                                                               
000136     .                                                                    
000137     EJECT                                                                
000138                                                                          
000139 A-INIT SECTION.                                                          
000140     MOVE 1                TO UT-REQU-IDMSGVER                            
000141     MOVE SPACE            TO UT-REQU-KDPGMACT                            
000142     MOVE IDPGM            TO UT-REQU-IDUSER                              
000143                                                                          
000144     MOVE FUNCTION CURRENT-DATE (1:8)  TO WS-DATE                         
000145     MOVE FUNCTION CURRENT-DATE (9:6)  TO WS-TIME                         
000146                                                                          
000147     OPEN INPUT WF2014                                                    
000148     .                                                                    
000149     EJECT                                                                
000150                                                                          
000151 B-EXECUTE SECTION.                                                       
000152     PERFORM S01-READ-WF2014                                              
000153                                                                          
000154     IF END-OF-WF2014                                                     
000155       CONTINUE                                                           
000156     ELSE                                                                 
000157                                                                          
000158       PERFORM UNTIL END-OF-WF2014                                        
000159         IF GL-IDLEGSEL = WS-IDLEGSEL-VCCS AND                            
000160           (GL-IDSYSTEM-SEND = WS-IDSYSTEM-VSS  OR                        
000161                               WS-IDSYSTEM-VIPS)                          
000162           MOVE YES TO SEND-TO-GL-SW                                      
000163           IF FIRST-TIME                                                  
000164             PERFORM S70-OPEN                                             
000165             MOVE SEND-IDCOM  TO WS-SEND-IDCOM                            
000166             MOVE GL-DAFINDOC TO WS-DAFINDOC                              
000167             MOVE GL-IDFINDOC TO WS-IDFINDOC                              
000168             MOVE IN-AREA     TO SPAR-AREA                                
000169             MOVE 'N'         TO FIRST-TIME-SW                            
000170           END-IF                                                         
000171           IF GL-DAFINDOC NOT = WS-DAFINDOC OR                            
000172              GL-IDFINDOC NOT = WS-IDFINDOC                               
000173             PERFORM BA-HANDLE-SUM                                        
000174             IF SPAR-GL-IDSYSTEM-SEND = WS-IDSYSTEM-VIPS                  
000175               IF WS-DIFF NOT = ZERO                                      
000176                 PERFORM BD-HANDLE-DIFF                                   
000177               END-IF                                                     
000178             END-IF                                                       
000179             IF SPAR-GL-IDSYSTEM-SEND = WS-IDSYSTEM-VSS                   
000180               IF WS-DIFF-VSS NOT = ZERO                                  
000181                 PERFORM BE-HANDLE-DIFF-VSS                               
000182               END-IF                                                     
000183             END-IF                                                       
000184             MOVE ZERO        TO WS-SUBEL-LINE                            
000185             MOVE ZERO        TO WS-SUBEL-LINE-VSS                        
000186             MOVE GL-DAFINDOC TO WS-DAFINDOC                              
000187             MOVE GL-IDFINDOC TO WS-IDFINDOC                              
000188             MOVE IN-AREA     TO SPAR-AREA                                
000189           END-IF                                                         
000190           IF GL-IDSYSTEM-SEND = WS-IDSYSTEM-VSS                          
000191             IF GL-IDARTNR-FINANCE = SPACE                                
000192               PERFORM BC-HANDLE-FRAKT                                    
000193             ELSE                                                         
000194               PERFORM BB-HANDLE-DET                                      
000195             END-IF                                                       
000196           ELSE                                                           
000197             PERFORM BB-HANDLE-DET                                        
000198           END-IF                                                         
000199         END-IF                                                           
000200         PERFORM S01-READ-WF2014                                          
000201       END-PERFORM                                                        
000202       IF SEND-TO-GL                                                      
000203         PERFORM BA-HANDLE-SUM                                            
000204         IF SPAR-GL-IDSYSTEM-SEND = WS-IDSYSTEM-VIPS                      
000205           IF WS-DIFF NOT = ZERO                                          
000206             PERFORM BD-HANDLE-DIFF                                       
000207           END-IF                                                         
000208         END-IF                                                           
000209         IF SPAR-GL-IDSYSTEM-SEND = WS-IDSYSTEM-VSS                       
000210           IF WS-DIFF-VSS NOT = ZERO                                      
000211             PERFORM BE-HANDLE-DIFF-VSS                                   
000212           END-IF                                                         
000213         END-IF                                                           
000214       END-IF                                                             
000215     END-IF                                                               
000216     .                                                                    
000217                                                                          
000218 BA-HANDLE-SUM SECTION.                                                   
000219     MOVE IDPGM                 TO UT-FIL-IDPGM                           
000220     MOVE WS-DATE               TO UT-FIL-DAREGDAT                        
000221     MOVE WS-TIME               TO UT-FIL-TIKLOCK                         
000222     MOVE 1                     TO UT-FIL-IDSEKVNR                        
000223     MOVE 'W510EKHA'            TO UT-FIL-IDCPYTXT                        
000224     MOVE SPAR-GL-KDVAT         TO UT-EKH-BEVAT                           
000225     MOVE WS-DAFINDOC           TO UT-EKH-DAVERDAT                        
000226     MOVE SPACE                 TO UT-EKH-FLLSBOK                         
000227     MOVE SPACE                 TO UT-EKH-IDANALYS                        
000228     MOVE ZERO                  TO UT-EKH-IDARTNR                         
000229     MOVE SPAR-GL-IDEXCUST(1)   TO WS-IDEXCUST                            
000230     MOVE +0                    TO W-ANT                                  
000231     INSPECT WS-IDEXCUST TALLYING W-ANT FOR CHARACTERS                    
000232             BEFORE INITIAL ' '                                           
000233     IF W-ANT = ZERO                                                      
000234       MOVE ZERO                  TO WS-IDDISTR-1                         
000235     ELSE                                                                 
000236       MOVE WS-IDEXCUST(1:W-ANT)  TO WS-IDDISTR-1                         
000237     END-IF                                                               
000238     IF SPAR-GL-IDSYSTEM-SEND = 'VSS'                                     
000239       MOVE 0778                  TO WS-IDDISTR-1                         
000240     END-IF                                                               
000241     MOVE WS-IDDISTR-1          TO UT-EKH-IDDISTR                         
000242     MOVE ZERO                  TO UT-EKH-IDKONTO                         
000243     MOVE SPACE                 TO UT-EKH-IDKST                           
000244                                                                          
000245     IF SPAR-GL-IDPARTNR = SPACE                                          
000246       MOVE ZERO                TO UT-EKH-IDKUNDNR                        
000247     ELSE                                                                 
000248       COMPUTE UT-EKH-IDKUNDNR =                                          
000249               FUNCTION NUMVAL(SPAR-GL-IDPARTNR(1:7))                     
000250       END-COMPUTE                                                        
000251     END-IF                                                               
000252     MOVE SPACE                 TO UT-EKH-IDTRANS                         
000253                                                                          
000254     MOVE WS-IDFINDOC           TO WS-IDFINDOC-ALFA                       
000255     IF WS-IDFINDOC-ALFA(1:1) NOT = '0'                                   
000256       MOVE WS-IDFINDOC-ALFA      TO UT-EKH-IDVERGL                       
000257     ELSE                                                                 
000258     IF WS-IDFINDOC-ALFA(2:1) NOT = '0'                                   
000259       MOVE WS-IDFINDOC-ALFA(2:8) TO UT-EKH-IDVERGL                       
000260     ELSE                                                                 
000261     IF WS-IDFINDOC-ALFA(3:1) NOT = '0'                                   
000262       MOVE WS-IDFINDOC-ALFA(3:7) TO UT-EKH-IDVERGL                       
000263     ELSE                                                                 
000264     IF WS-IDFINDOC-ALFA(4:1) NOT = '0'                                   
000265       MOVE WS-IDFINDOC-ALFA(4:6) TO UT-EKH-IDVERGL                       
000266     ELSE                                                                 
000267     IF WS-IDFINDOC-ALFA(5:1) NOT = '0'                                   
000268       MOVE WS-IDFINDOC-ALFA(5:5) TO UT-EKH-IDVERGL                       
000269     ELSE                                                                 
000270     IF WS-IDFINDOC-ALFA(6:1) NOT = '0'                                   
000271       MOVE WS-IDFINDOC-ALFA(6:4) TO UT-EKH-IDVERGL                       
000272     ELSE                                                                 
000273     IF WS-IDFINDOC-ALFA(7:1) NOT = '0'                                   
000274       MOVE WS-IDFINDOC-ALFA(7:3) TO UT-EKH-IDVERGL                       
000275     ELSE                                                                 
000276     IF WS-IDFINDOC-ALFA(8:1) NOT = '0'                                   
000277       MOVE WS-IDFINDOC-ALFA(8:2) TO UT-EKH-IDVERGL                       
000278     ELSE                                                                 
000279       MOVE WS-IDFINDOC-ALFA(9:1) TO UT-EKH-IDVERGL                       
000280     END-IF                                                               
000281     END-IF                                                               
000282     END-IF                                                               
000283     END-IF                                                               
000284     END-IF                                                               
000285     END-IF                                                               
000286     END-IF                                                               
000287     END-IF                                                               
000288                                                                          
000289     MOVE SPACE                 TO UT-EKH-KDANMORS                        
000290                                                                          
000291     IF SPAR-GL-IDSYSTEM-SEND = 'VIPS'                                    
000292       IF SPAR-GL-KDFINDOC = 'INV'                                        
000293         MOVE '204'             TO UT-EKH-KDEKHHT                         
000294         MOVE '253'             TO UT-EKH-KDEKSHT                         
000295       END-IF                                                             
000296       MOVE SPACE               TO UT-EKH-IDDC-SEND                       
000297       MOVE SPAR-GL-KDVALISO    TO UT-EKH-KDVALISO                        
000298       MOVE SPAR-GL-PRKURS      TO UT-EKH-PRKURS                          
000299     END-IF                                                               
000300                                                                          
000301     IF SPAR-GL-IDSYSTEM-SEND = 'VSS'                                     
000302       IF SPAR-GL-KDFINDOC = 'INV2'                                       
000303         MOVE '205'             TO UT-EKH-KDEKHHT                         
000304         IF SPAR-GL-IDARTNR-FINANCE(1:4) = 'WIFI'                         
000305           MOVE '202'             TO UT-EKH-KDEKSHT                       
000306         ELSE                                                             
000307           IF SPAR-GL-IDARTNR-FINANCE(1:4) = 'VIDA'                       
000308             MOVE '202'             TO UT-EKH-KDEKSHT                     
000309           ELSE                                                           
000310             MOVE '201'             TO UT-EKH-KDEKSHT                     
000311           END-IF                                                         
000312         END-IF                                                           
000313       ELSE                                                               
000314         IF SPAR-GL-KDFINDOC = 'INT2'                                     
000315           MOVE '205'             TO UT-EKH-KDEKHHT                       
000316           IF SPAR-GL-IDARTNR-FINANCE(1:4) = 'WIFI'                       
000317             MOVE '202'               TO UT-EKH-KDEKSHT                   
000318           ELSE                                                           
000319             IF SPAR-GL-IDARTNR-FINANCE(1:4) = 'VIDA'                     
000320               MOVE '202'             TO UT-EKH-KDEKSHT                   
000321             ELSE                                                         
000322               MOVE '201'             TO UT-EKH-KDEKSHT                   
000323             END-IF                                                       
000324           END-IF                                                         
000325         ELSE                                                             
000326           MOVE '304'             TO UT-EKH-KDEKHHT                       
000327           MOVE '301'             TO UT-EKH-KDEKSHT                       
000328         END-IF                                                           
000329       END-IF                                                             
000330* WEB-SHOP DOCUMENTS ALWAYS HAS '11'  IN IDDC                             
000331       MOVE '11'                  TO UT-EKH-IDDC-SEND                     
000332* WEB-SHOP DOCUMENTS ALWAYS HAS 'SEK' IN KDVALISO                         
000333*      MOVE 'SEK'               TO UT-EKH-KDVALISO                        
000334* WEB-SHOP DOCUMENTS ALWAYS HAS '1.0' IN PRKURS                           
000335*      MOVE 1.0                 TO UT-EKH-PRKURS                          
000336                                                                          
000337       IF (SPAR-GL-KDPARTTY = 'EXT')     AND                              
000338          (SPAR-GL-KDPARTGR = 'CARD-EU'  OR                               
000339           SPAR-GL-KDPARTGR = 'CARD-NOT-EU')                              
000340         MOVE SPAR-GL-IDREF       TO UT-EKH-IDREF                         
000341         MOVE 'SEK'               TO UT-EKH-KDVALISO                      
000342         MOVE 1.0                 TO UT-EKH-PRKURS                        
000343       ELSE                                                               
000344         MOVE SPAR-GL-KDVALISO    TO UT-EKH-KDVALISO                      
000345         COMPUTE UT-EKH-PRKURS = SPAR-GL-PRKURS / SPAR-GL-REVALUTA        
000346       END-IF                                                             
000347     END-IF                                                               
000348                                                                          
000349     MOVE 'SUM'                 TO UT-EKH-KDEKNIVA                        
000350     MOVE ZERO                  TO UT-EKH-KDFRAKT                         
000351     MOVE ZERO                  TO UT-EKH-KDPRODSL                        
000352     MOVE ZERO                  TO UT-EKH-KDPSLLOC                        
000353     MOVE ZERO                  TO UT-EKH-KVANTAL                         
000354     MOVE ZERO                  TO UT-EKH-PRARTNTO                        
000355     MOVE ZERO                  TO UT-EKH-PRARTSJK                        
000356     MOVE ZERO                  TO UT-EKH-PRHEMTAG                        
000357     MOVE ZERO                  TO UT-EKH-PRARTSTD                        
000358     MOVE ZERO                  TO UT-EKH-PRDIRLON                        
000359     MOVE ZERO                  TO UT-EKH-PRDMTRL                         
000360     MOVE ZERO                  TO UT-EKH-PRINK                           
000361     MOVE ZERO                  TO UT-EKH-PRLANDCO                        
000362     MOVE ZERO                  TO UT-EKH-PROVRPAL                        
000363     MOVE ZERO                  TO UT-EKH-DAAVIDAT                        
000364     MOVE ZERO                  TO UT-EKH-IDAVINR                         
000365     MOVE SPACE                 TO UT-EKH-IDLEVNR                         
000366     MOVE ZERO                  TO UT-EKH-KDAVVTYP                        
000367     MOVE ZERO                  TO UT-EKH-KDRT                            
000368     MOVE ZERO                  TO UT-EKH-KVANTMOT                        
000369     MOVE ZERO                  TO UT-EKH-KVAVIS                          
000370     MOVE SPACE                 TO UT-EKH-KDSORT                          
000371     MOVE SPAR-GL-KDTRADP       TO UT-EKH-KDTRADP                         
000372     MOVE SPACE                 TO UT-EKH-FLOVRLEV                        
000373     MOVE ZERO                  TO UT-EKH-IDORDNR5                        
000374     MOVE SPACE                 TO UT-EKH-IDUSER                          
000375     IF SPAR-GL-IDSYSTEM-SEND = WS-IDSYSTEM-VSS                           
000376       IF SPAR-GL-KDVALISO     = 'SEK'                                    
000377         MOVE SPAR-GL-SUBTO-TOT        TO UT-EKH-SUBEL                    
000378         MOVE SPAR-GL-SUVAT-BILLIT-TOT TO UT-EKH-SUVAT                    
000379       ELSE                                                               
000380         COMPUTE UT-EKH-SUBEL ROUNDED =                                   
000381                 SPAR-GL-SUBTO-TOT *                                      
000382                 (SPAR-GL-PRKURS     / SPAR-GL-REVALUTA    )              
000383         END-COMPUTE                                                      
000384         COMPUTE UT-EKH-SUVAT ROUNDED =                                   
000385                 SPAR-GL-SUVAT-BILLIT-TOT *                               
000386                 (SPAR-GL-PRKURS     / SPAR-GL-REVALUTA    )              
000387         END-COMPUTE                                                      
000388       END-IF                                                             
000389     END-IF                                                               
000390     IF SPAR-GL-IDSYSTEM-SEND = WS-IDSYSTEM-VIPS                          
000391       COMPUTE UT-EKH-SUBEL ROUNDED =                                     
000392               SPAR-GL-SUBTO-TOT *                                        
000393               (SPAR-GL-PRKURS / SPAR-GL-REVALUTA)                        
000394       END-COMPUTE                                                        
000395       COMPUTE UT-EKH-SUVAT ROUNDED =                                     
000396               SPAR-GL-SUVAT-BILLIT-TOT *                                 
000397               (SPAR-GL-PRKURS / SPAR-GL-REVALUTA)                        
000398       END-COMPUTE                                                        
000399     END-IF                                                               
000400                                                                          
000401     IF SPAR-GL-IDSYSTEM-SEND = WS-IDSYSTEM-VIPS                          
000402       COMPUTE WS-DIFF = UT-EKH-SUBEL -                                   
000403                         UT-EKH-SUVAT -                                   
000404                         WS-SUBEL-LINE                                    
000405       END-COMPUTE                                                        
000406     END-IF                                                               
000407                                                                          
000408     IF SPAR-GL-IDSYSTEM-SEND = WS-IDSYSTEM-VSS                           
000409       COMPUTE WS-DIFF-VSS = UT-EKH-SUBEL -                               
000410                         UT-EKH-SUVAT -                                   
000411                         WS-SUBEL-LINE-VSS                                
000412       END-COMPUTE                                                        
000413     END-IF                                                               
000414     MOVE SPACE                 TO UT-EKH-FLDCET                          
000415     MOVE SPACE                 TO UT-EKH-IDKUNDRF                        
000415     MOVE SPACE                 TO UT-EKH-IDFAKT-EXP                      
000416                                                                          
000417     PERFORM S80-PUT                                                      
000418     .                                                                    
000419     EJECT                                                                
000420                                                                          
000421 BB-HANDLE-DET SECTION.                                                   
000422     MOVE IDPGM                 TO UT-FIL-IDPGM                           
000423     MOVE WS-DATE               TO UT-FIL-DAREGDAT                        
000424     MOVE WS-TIME               TO UT-FIL-TIKLOCK                         
000425     MOVE 1                     TO UT-FIL-IDSEKVNR                        
000426     MOVE 'W510EKHA'            TO UT-FIL-IDCPYTXT                        
000427     MOVE GL-KDVAT              TO UT-EKH-BEVAT                           
000428     MOVE GL-DAFINDOC           TO UT-EKH-DAVERDAT                        
000429     MOVE 'N'                   TO UT-EKH-FLLSBOK                         
000430     MOVE SPACE                 TO UT-EKH-IDANALYS                        
000431     IF SPAR-GL-IDSYSTEM-SEND = WS-IDSYSTEM-VSS                           
000432       MOVE ZERO                TO UT-EKH-IDARTNR                         
000433     ELSE                                                                 
000434       MOVE ZERO                TO UT-EKH-IDARTNR                         
000435**** FLYTTA ENBART DET NUMERISKA VÄRDET                                   
000436*      MOVE GL-IDARTNR-FINANCE(1:9) TO UT-EKH-IDARTNR                     
000437     END-IF                                                               
000438     MOVE GL-IDEXCUST(1)        TO WS-IDEXCUST                            
000439     MOVE +0                    TO W-ANT                                  
000440     INSPECT WS-IDEXCUST TALLYING W-ANT FOR CHARACTERS                    
000441             BEFORE INITIAL ' '                                           
000442     IF W-ANT = ZERO                                                      
000443       MOVE ZERO                  TO WS-IDDISTR-1                         
000444     ELSE                                                                 
000445       MOVE WS-IDEXCUST(1:W-ANT)  TO WS-IDDISTR-1                         
000446     END-IF                                                               
000447     IF SPAR-GL-IDSYSTEM-SEND = 'VSS'                                     
000448       MOVE 0778                  TO WS-IDDISTR-1                         
000449     END-IF                                                               
000450     MOVE WS-IDDISTR-1          TO UT-EKH-IDDISTR                         
000451     MOVE SPACE                 TO UT-EKH-IDKST                           
000452                                                                          
000453     IF GL-IDPARTNR = SPACE                                               
000454       MOVE ZERO TO UT-EKH-IDKUNDNR                                       
000455     ELSE                                                                 
000456       COMPUTE UT-EKH-IDKUNDNR =                                          
000457               FUNCTION NUMVAL(GL-IDPARTNR(1:7))                          
000458       END-COMPUTE                                                        
000459     END-IF                                                               
000460                                                                          
000461     MOVE SPACE                   TO UT-EKH-IDTRANS                       
000462                                                                          
000463     MOVE GL-IDFINDOC             TO WS-IDFINDOC-ALFA                     
000464     IF WS-IDFINDOC-ALFA(1:1) NOT = '0'                                   
000465       MOVE WS-IDFINDOC-ALFA      TO UT-EKH-IDVERGL                       
000466     ELSE                                                                 
000467     IF WS-IDFINDOC-ALFA(2:1) NOT = '0'                                   
000468       MOVE WS-IDFINDOC-ALFA(2:8) TO UT-EKH-IDVERGL                       
000469     ELSE                                                                 
000470     IF WS-IDFINDOC-ALFA(3:1) NOT = '0'                                   
000471       MOVE WS-IDFINDOC-ALFA(3:7) TO UT-EKH-IDVERGL                       
000472     ELSE                                                                 
000473     IF WS-IDFINDOC-ALFA(4:1) NOT = '0'                                   
000474       MOVE WS-IDFINDOC-ALFA(4:6) TO UT-EKH-IDVERGL                       
000475     ELSE                                                                 
000476     IF WS-IDFINDOC-ALFA(5:1) NOT = '0'                                   
000477       MOVE WS-IDFINDOC-ALFA(5:5) TO UT-EKH-IDVERGL                       
000478     ELSE                                                                 
000479     IF WS-IDFINDOC-ALFA(6:1) NOT = '0'                                   
000480       MOVE WS-IDFINDOC-ALFA(6:4) TO UT-EKH-IDVERGL                       
000481     ELSE                                                                 
000482     IF WS-IDFINDOC-ALFA(7:1) NOT = '0'                                   
000483       MOVE WS-IDFINDOC-ALFA(7:3) TO UT-EKH-IDVERGL                       
000484     ELSE                                                                 
000485     IF WS-IDFINDOC-ALFA(8:1) NOT = '0'                                   
000486       MOVE WS-IDFINDOC-ALFA(8:2) TO UT-EKH-IDVERGL                       
000487     ELSE                                                                 
000488       MOVE WS-IDFINDOC-ALFA(9:1) TO UT-EKH-IDVERGL                       
000489     END-IF                                                               
000490     END-IF                                                               
000491     END-IF                                                               
000492     END-IF                                                               
000493     END-IF                                                               
000494     END-IF                                                               
000495     END-IF                                                               
000496     END-IF                                                               
000497                                                                          
000498     MOVE SPACE                 TO UT-EKH-KDANMORS                        
000499                                                                          
000500     IF GL-IDSYSTEM-SEND = 'VIPS'                                         
000501       MOVE '204'               TO UT-EKH-KDEKHHT                         
000502       MOVE '253'               TO UT-EKH-KDEKSHT                         
000503       MOVE SPACE               TO UT-EKH-IDDC-SEND                       
000504       MOVE GL-KDVALISO         TO UT-EKH-KDVALISO                        
000505       MOVE GL-PRKURS           TO UT-EKH-PRKURS                          
000506       MOVE ZERO                TO UT-EKH-IDKONTO                         
000507       COMPUTE UT-EKH-PRARTNTO ROUNDED = GL-PRARTNTO *                    
000508                            (GL-PRKURS / GL-REVALUTA)                     
000509       END-COMPUTE                                                        
000510       COMPUTE UT-EKH-SUBEL ROUNDED =                                     
000511                           (GL-KVLEVART * GL-PRARTNTO) *                  
000512                            (GL-PRKURS / GL-REVALUTA)                     
000513       END-COMPUTE                                                        
000514       COMPUTE WS-SUBEL-LINE = WS-SUBEL-LINE +                            
000515                               UT-EKH-SUBEL                               
000516       END-COMPUTE                                                        
000517       COMPUTE UT-EKH-PRARTSJK ROUNDED = GL-PRARTNTO *                    
000518                                        (GL-PRKURS / GL-REVALUTA)         
000519       END-COMPUTE                                                        
000520       COMPUTE UT-EKH-PRARTSTD ROUNDED = GL-PRARTNTO *                    
000521                                        (GL-PRKURS / GL-REVALUTA)         
000522       END-COMPUTE                                                        
000523       COMPUTE UT-EKH-PRINK    ROUNDED = GL-PRARTNTO *                    
000524                                        (GL-PRKURS / GL-REVALUTA)         
000525       END-COMPUTE                                                        
000526     END-IF                                                               
000527                                                                          
000528     IF GL-IDSYSTEM-SEND = 'VSS'                                          
000529       IF GL-KDFINDOC = 'INV2'                                            
000530         MOVE '205'             TO UT-EKH-KDEKHHT                         
000531         IF GL-IDARTNR-FINANCE(1:4) = 'WIFI'                              
000532           MOVE '202'             TO UT-EKH-KDEKSHT                       
000533           MOVE 'WIFI'            TO UT-EKH-IDTRANS                       
000534         ELSE                                                             
000535           IF GL-IDARTNR-FINANCE(1:4) = 'VIDA'                            
000536             MOVE '202'           TO UT-EKH-KDEKSHT                       
000537             MOVE 'VIDA'          TO UT-EKH-IDTRANS                       
000538           ELSE                                                           
000539             MOVE '201'           TO UT-EKH-KDEKSHT                       
000540           END-IF                                                         
000541         END-IF                                                           
000542       ELSE                                                               
000543         IF GL-KDFINDOC = 'INV2'                                          
000544           MOVE '205'             TO UT-EKH-KDEKHHT                       
000545           IF GL-IDARTNR-FINANCE(1:4) = 'WIFI'                            
000546             MOVE '202'             TO UT-EKH-KDEKSHT                     
000547           ELSE                                                           
000548             IF GL-IDARTNR-FINANCE(1:4) = 'VIDA'                          
000549               MOVE '202'           TO UT-EKH-KDEKSHT                     
000550             ELSE                                                         
000551               MOVE '201'           TO UT-EKH-KDEKSHT                     
000552             END-IF                                                       
000553           END-IF                                                         
000554         ELSE                                                             
000555           MOVE '304'             TO UT-EKH-KDEKHHT                       
000556           MOVE '301'             TO UT-EKH-KDEKSHT                       
000557         END-IF                                                           
000558       END-IF                                                             
000559* WEB-SHOP DOCUMENTS ALWAYS HAS '11'  IN IDDC                             
000560       MOVE '11'                TO UT-EKH-IDDC-SEND                       
000561* WEB-SHOP DOCUMENTS ALWAYS HAS 'SEK' IN KDVALISO                         
000562*      MOVE 'SEK'               TO UT-EKH-KDVALISO                        
000563* WEB-SHOP DOCUMENTS ALWAYS HAS '1.0' IN PRKURS                           
000564*      MOVE 1.0                 TO UT-EKH-PRKURS                          
000565                                                                          
000566       IF (SPAR-GL-KDPARTTY = 'EXT')     AND                              
000567          (SPAR-GL-KDPARTGR = 'CARD-EU'  OR                               
000568           SPAR-GL-KDPARTGR = 'CARD-NOT-EU')                              
000569         MOVE 'SEK'               TO UT-EKH-KDVALISO                      
000570         MOVE 1.0                 TO UT-EKH-PRKURS                        
000571       ELSE                                                               
000572         MOVE SPAR-GL-KDVALISO    TO UT-EKH-KDVALISO                      
000573         MOVE SPAR-GL-PRKURS      TO UT-EKH-PRKURS                        
000574       END-IF                                                             
000575                                                                          
000576       IF GL-IDACCNT(1) = SPACE                                           
000577         MOVE ZERO TO UT-EKH-IDKONTO                                      
000578       ELSE                                                               
000579         COMPUTE UT-EKH-IDKONTO =                                         
000580                 FUNCTION NUMVAL(GL-IDACCNT(1)(1:10))                     
000581         END-COMPUTE                                                      
000582       END-IF                                                             
000583       MOVE GL-PRARTNTO         TO UT-EKH-PRARTNTO                        
000584       IF SPAR-GL-IDSYSTEM-SEND = WS-IDSYSTEM-VSS                         
000585         IF SPAR-GL-KDVALISO     = 'SEK'                                  
000586           MOVE GL-PRARTNTO       TO UT-EKH-PRARTNTO                      
000587         ELSE                                                             
000588           COMPUTE UT-EKH-PRARTNTO ROUNDED =                              
000589                   GL-PRARTNTO *                                          
000590                   (SPAR-GL-PRKURS     / SPAR-GL-REVALUTA    )            
000591           END-COMPUTE                                                    
000592         END-IF                                                           
000593         COMPUTE UT-EKH-SUBEL ROUNDED =                                   
000594                              GL-KVLEVART * UT-EKH-PRARTNTO               
000595         END-COMPUTE                                                      
000596         COMPUTE WS-SUBEL-LINE-VSS = WS-SUBEL-LINE-VSS +                  
000597                                     UT-EKH-SUBEL                         
000598         END-COMPUTE                                                      
000599       END-IF                                                             
000600       MOVE ZERO                TO UT-EKH-PRARTSJK                        
000601                                   UT-EKH-PRARTSTD                        
000602                                   UT-EKH-PRINK                           
000603     END-IF                                                               
000604                                                                          
000605     MOVE 'DET'                 TO UT-EKH-KDEKNIVA                        
000606     MOVE ZERO                  TO UT-EKH-KDFRAKT                         
000607     MOVE ZERO                  TO UT-EKH-KDPRODSL                        
000608     MOVE ZERO                  TO UT-EKH-KDPSLLOC                        
000609     MOVE GL-KVLEVART           TO UT-EKH-KVANTAL                         
000610     MOVE ZERO                  TO UT-EKH-PRHEMTAG                        
000611     MOVE ZERO                  TO UT-EKH-PRDIRLON                        
000612     MOVE ZERO                  TO UT-EKH-PRDMTRL                         
000613     MOVE ZERO                  TO UT-EKH-PRLANDCO                        
000614     MOVE ZERO                  TO UT-EKH-PROVRPAL                        
000615     MOVE ZERO                  TO UT-EKH-SUVAT                           
000616     MOVE GL-DAREFDAT           TO UT-EKH-DAAVIDAT                        
000617     MOVE ZERO                  TO UT-EKH-IDAVINR                         
000618     MOVE SPACE                 TO UT-EKH-IDLEVNR                         
000619     MOVE ZERO                  TO UT-EKH-KDAVVTYP                        
000620     MOVE ZERO                  TO UT-EKH-KDRT                            
000621     MOVE ZERO                  TO UT-EKH-KVANTMOT                        
000622     MOVE ZERO                  TO UT-EKH-KVAVIS                          
000623     MOVE SPACE                 TO UT-EKH-KDSORT                          
000624     IF GL-FLSOFT = 'J'                                                   
000625       MOVE 'SW'                TO UT-EKH-KDSORT                          
000626     END-IF                                                               
000627     IF GL-FLSOFT = 'Y'                                                   
000628       MOVE 'SW'                TO UT-EKH-KDSORT                          
000629     END-IF                                                               
000630     MOVE GL-KDTRADP            TO UT-EKH-KDTRADP                         
000631     MOVE 'N'                   TO UT-EKH-FLOVRLEV                        
000632                                                                          
000633     IF GL-IDREF = SPACE                                                  
000634       MOVE ZERO TO UT-EKH-IDORDNR5                                       
000635     ELSE                                                                 
000636       COMPUTE UT-EKH-IDORDNR5 =                                          
000637               FUNCTION NUMVAL(GL-IDREF(1:5))                             
000638       END-COMPUTE                                                        
000639     END-IF                                                               
000640     MOVE SPACE                 TO UT-EKH-IDUSER                          
000641     MOVE SPACE                 TO UT-EKH-IDREF                           
000642     MOVE SPACE                 TO UT-EKH-FLDCET                          
000643     MOVE SPACE                 TO UT-EKH-IDKUNDRF                        
000643     MOVE SPACE                 TO UT-EKH-IDFAKT-EXP                      
000644                                                                          
000645     PERFORM S80-PUT                                                      
000646     .                                                                    
000647     EJECT                                                                
000648                                                                          
000649 BC-HANDLE-FRAKT SECTION.                                                 
000650     MOVE IDPGM                 TO UT-FIL-IDPGM                           
000651     MOVE IDPGM                 TO UT-FIL-IDPGM                           
000652     MOVE WS-DATE               TO UT-FIL-DAREGDAT                        
000653     MOVE WS-TIME               TO UT-FIL-TIKLOCK                         
000654     MOVE 1                     TO UT-FIL-IDSEKVNR                        
000655     MOVE 'W510EKHA'            TO UT-FIL-IDCPYTXT                        
000656     MOVE GL-KDVAT              TO UT-EKH-BEVAT                           
000657     MOVE GL-DAFINDOC           TO UT-EKH-DAVERDAT                        
000658     MOVE SPACE                 TO UT-EKH-FLLSBOK                         
000659     MOVE GL-IDACCNT(3) (1:12)  TO UT-EKH-IDANALYS                        
000660     MOVE GL-IDACCNT(2) (1:10)  TO UT-EKH-IDKST                           
000661     MOVE ZERO                  TO UT-EKH-IDARTNR                         
000662     MOVE SPACE                 TO UT-EKH-IDDC-SEND                       
000663     MOVE SPACE                 TO UT-EKH-IDDC-REC                        
000664     MOVE ZERO                  TO UT-EKH-IDDISTR                         
000665                                                                          
000666     IF GL-IDACCNT(1) = SPACE                                             
000667       MOVE ZERO TO UT-EKH-IDKONTO                                        
000668     ELSE                                                                 
000669       COMPUTE UT-EKH-IDKONTO  =                                          
000670               FUNCTION NUMVAL(GL-IDACCNT(1)(1:10))                       
000671       END-COMPUTE                                                        
000672     END-IF                                                               
000673                                                                          
000674     MOVE ZERO                  TO UT-EKH-IDKUNDNR                        
000675                                                                          
000676     IF GL-IDPARTNR = SPACE                                               
000677       MOVE ZERO TO UT-EKH-IDKUNDNR                                       
000678     ELSE                                                                 
000679       COMPUTE UT-EKH-IDKUNDNR =                                          
000680               FUNCTION NUMVAL(GL-IDPARTNR(1:7))                          
000681       END-COMPUTE                                                        
000682     END-IF                                                               
000683                                                                          
000684     MOVE SPACE                 TO UT-EKH-IDTRANS                         
000685                                                                          
000686     MOVE GL-IDFINDOC           TO WS-IDFINDOC-ALFA                       
000687     IF WS-IDFINDOC-ALFA(1:1) NOT = '0'                                   
000688       MOVE WS-IDFINDOC-ALFA      TO UT-EKH-IDVERGL                       
000689     ELSE                                                                 
000690     IF WS-IDFINDOC-ALFA(2:1) NOT = '0'                                   
000691       MOVE WS-IDFINDOC-ALFA(2:8) TO UT-EKH-IDVERGL                       
000692     ELSE                                                                 
000693     IF WS-IDFINDOC-ALFA(3:1) NOT = '0'                                   
000694       MOVE WS-IDFINDOC-ALFA(3:7) TO UT-EKH-IDVERGL                       
000695     ELSE                                                                 
000696     IF WS-IDFINDOC-ALFA(4:1) NOT = '0'                                   
000697       MOVE WS-IDFINDOC-ALFA(4:6) TO UT-EKH-IDVERGL                       
000698     ELSE                                                                 
000699     IF WS-IDFINDOC-ALFA(5:1) NOT = '0'                                   
000700       MOVE WS-IDFINDOC-ALFA(5:5) TO UT-EKH-IDVERGL                       
000701     ELSE                                                                 
000702     IF WS-IDFINDOC-ALFA(6:1) NOT = '0'                                   
000703       MOVE WS-IDFINDOC-ALFA(6:4) TO UT-EKH-IDVERGL                       
000704     ELSE                                                                 
000705     IF WS-IDFINDOC-ALFA(7:1) NOT = '0'                                   
000706       MOVE WS-IDFINDOC-ALFA(7:3) TO UT-EKH-IDVERGL                       
000707     ELSE                                                                 
000708     IF WS-IDFINDOC-ALFA(8:1) NOT = '0'                                   
000709       MOVE WS-IDFINDOC-ALFA(8:2) TO UT-EKH-IDVERGL                       
000710     ELSE                                                                 
000711       MOVE WS-IDFINDOC-ALFA(9:1) TO UT-EKH-IDVERGL                       
000712     END-IF                                                               
000713     END-IF                                                               
000714     END-IF                                                               
000715     END-IF                                                               
000716     END-IF                                                               
000717     END-IF                                                               
000718     END-IF                                                               
000719     END-IF                                                               
000720                                                                          
000721     MOVE SPACE                 TO UT-EKH-KDANMORS                        
000722                                                                          
000723     IF GL-KDFINDOC = 'INV2'                                              
000724       MOVE '205'               TO UT-EKH-KDEKHHT                         
000725       MOVE '201'               TO UT-EKH-KDEKSHT                         
000726     ELSE                                                                 
000727       MOVE '304'               TO UT-EKH-KDEKHHT                         
000728       MOVE '301'               TO UT-EKH-KDEKSHT                         
000729     END-IF                                                               
000730                                                                          
000731     MOVE 'FRAKT'               TO UT-EKH-KDEKNIVA                        
000732     MOVE ZERO                  TO UT-EKH-KDFRAKT                         
000733     MOVE ZERO                  TO UT-EKH-KDPRODSL                        
000734     MOVE ZERO                  TO UT-EKH-KDPSLLOC                        
000735* WEB-SHOP DOCUMENTS ALWAYS HAS 'SEK' IN KDVALISO                         
000736*    MOVE 'SEK'                 TO UT-EKH-KDVALISO                        
000737* WEB-SHOP DOCUMENTS ALWAYS HAS '1.0' IN PRKURS                           
000738*    MOVE 1.0                   TO UT-EKH-PRKURS                          
000739     IF (SPAR-GL-KDPARTTY = 'EXT')     AND                                
000740        (SPAR-GL-KDPARTGR = 'CARD-EU'  OR                                 
000741         SPAR-GL-KDPARTGR = 'CARD-NOT-EU')                                
000742       MOVE 'SEK'               TO UT-EKH-KDVALISO                        
000743       MOVE 1.0                 TO UT-EKH-PRKURS                          
000744     ELSE                                                                 
000745       MOVE SPAR-GL-KDVALISO    TO UT-EKH-KDVALISO                        
000746       MOVE SPAR-GL-PRKURS      TO UT-EKH-PRKURS                          
000747     END-IF                                                               
000748     MOVE ZERO                  TO UT-EKH-KVANTAL                         
000749     MOVE ZERO                  TO UT-EKH-PRARTNTO                        
000750     MOVE ZERO                  TO UT-EKH-PRARTSJK                        
000751     MOVE ZERO                  TO UT-EKH-PRHEMTAG                        
000752     MOVE ZERO                  TO UT-EKH-PRARTSTD                        
000753     MOVE ZERO                  TO UT-EKH-PRDIRLON                        
000754     MOVE ZERO                  TO UT-EKH-PRDMTRL                         
000755     MOVE ZERO                  TO UT-EKH-PRINK                           
000756     MOVE ZERO                  TO UT-EKH-PRLANDCO                        
000757     MOVE ZERO                  TO UT-EKH-PROVRPAL                        
000758     MOVE GL-SUNTO              TO UT-EKH-SUBEL                           
000759     IF SPAR-GL-IDSYSTEM-SEND = WS-IDSYSTEM-VSS                           
000760       IF SPAR-GL-KDVALISO     = 'SEK'                                    
000761         MOVE GL-SUNTO          TO UT-EKH-SUBEL                           
000762       ELSE                                                               
000763         COMPUTE UT-EKH-SUBEL ROUNDED =                                   
000764                 GL-SUNTO *                                               
000765                 (SPAR-GL-PRKURS     / SPAR-GL-REVALUTA    )              
000766         END-COMPUTE                                                      
000767       END-IF                                                             
000768       COMPUTE WS-SUBEL-LINE-VSS = WS-SUBEL-LINE-VSS +                    
000769                                   UT-EKH-SUBEL                           
000770       END-COMPUTE                                                        
000771     END-IF                                                               
000772     MOVE ZERO                  TO UT-EKH-SUVAT                           
000773     MOVE GL-DAREFDAT           TO UT-EKH-DAAVIDAT                        
000774     MOVE ZERO                  TO UT-EKH-IDAVINR                         
000775     MOVE SPACE                 TO UT-EKH-IDLEVNR                         
000776     MOVE ZERO                  TO UT-EKH-KDAVVTYP                        
000777     MOVE ZERO                  TO UT-EKH-KDRT                            
000778     MOVE ZERO                  TO UT-EKH-KVANTMOT                        
000779     MOVE ZERO                  TO UT-EKH-KVAVIS                          
000780     MOVE SPACE                 TO UT-EKH-KDSORT                          
000781     MOVE GL-KDTRADP            TO UT-EKH-KDTRADP                         
000782     MOVE SPACE                 TO UT-EKH-FLOVRLEV                        
000783     MOVE ZERO                  TO UT-EKH-IDORDNR5                        
000784     MOVE SPACE                 TO UT-EKH-IDUSER                          
000785     MOVE SPACE                 TO UT-EKH-IDREF                           
000786     MOVE SPACE                 TO UT-EKH-FLDCET                          
000787     MOVE SPACE                 TO UT-EKH-IDKUNDRF                        
000787     MOVE SPACE                 TO UT-EKH-IDFAKT-EXP                      
000788                                                                          
000789     PERFORM S80-PUT                                                      
000790     .                                                                    
000791     EJECT                                                                
000792                                                                          
000793 BD-HANDLE-DIFF SECTION.                                                  
000794     MOVE IDPGM                 TO UT-FIL-IDPGM                           
000795     MOVE WS-DATE               TO UT-FIL-DAREGDAT                        
000796     MOVE WS-TIME               TO UT-FIL-TIKLOCK                         
000797     MOVE 1                     TO UT-FIL-IDSEKVNR                        
000798     MOVE 'W510EKHA'            TO UT-FIL-IDCPYTXT                        
000799     MOVE SPAR-GL-KDVAT         TO UT-EKH-BEVAT                           
000800     MOVE SPAR-GL-DAFINDOC      TO UT-EKH-DAVERDAT                        
000801     MOVE 'N'                   TO UT-EKH-FLLSBOK                         
000802     MOVE SPACE                 TO UT-EKH-IDANALYS                        
000803     MOVE ZERO                  TO UT-EKH-IDARTNR                         
000804     MOVE SPAR-GL-IDEXCUST(1)   TO WS-IDEXCUST                            
000805     MOVE +0                    TO W-ANT                                  
000806     INSPECT WS-IDEXCUST TALLYING W-ANT FOR CHARACTERS                    
000807             BEFORE INITIAL ' '                                           
000808     IF W-ANT = ZERO                                                      
000809       MOVE ZERO                  TO WS-IDDISTR-1                         
000810     ELSE                                                                 
000811       MOVE WS-IDEXCUST(1:W-ANT)  TO WS-IDDISTR-1                         
000812     END-IF                                                               
000813     IF SPAR-GL-IDSYSTEM-SEND = 'VSS'                                     
000814       MOVE 0778                  TO WS-IDDISTR-1                         
000815     END-IF                                                               
000816     MOVE WS-IDDISTR-1          TO UT-EKH-IDDISTR                         
000817     MOVE SPACE                 TO UT-EKH-IDKST                           
000818                                                                          
000819     IF SPAR-GL-IDPARTNR = SPACE                                          
000820       MOVE ZERO TO UT-EKH-IDKUNDNR                                       
000821     ELSE                                                                 
000822       COMPUTE UT-EKH-IDKUNDNR =                                          
000823               FUNCTION NUMVAL(SPAR-GL-IDPARTNR(1:7))                     
000824       END-COMPUTE                                                        
000825     END-IF                                                               
000826                                                                          
000827     MOVE SPACE                   TO UT-EKH-IDTRANS                       
000828                                                                          
000829     MOVE SPAR-GL-IDFINDOC        TO WS-IDFINDOC-ALFA                     
000830     IF WS-IDFINDOC-ALFA(1:1) NOT = '0'                                   
000831       MOVE WS-IDFINDOC-ALFA      TO UT-EKH-IDVERGL                       
000832     ELSE                                                                 
000833     IF WS-IDFINDOC-ALFA(2:1) NOT = '0'                                   
000834       MOVE WS-IDFINDOC-ALFA(2:8) TO UT-EKH-IDVERGL                       
000835     ELSE                                                                 
000836     IF WS-IDFINDOC-ALFA(3:1) NOT = '0'                                   
000837       MOVE WS-IDFINDOC-ALFA(3:7) TO UT-EKH-IDVERGL                       
000838     ELSE                                                                 
000839     IF WS-IDFINDOC-ALFA(4:1) NOT = '0'                                   
000840       MOVE WS-IDFINDOC-ALFA(4:6) TO UT-EKH-IDVERGL                       
000841     ELSE                                                                 
000842     IF WS-IDFINDOC-ALFA(5:1) NOT = '0'                                   
000843       MOVE WS-IDFINDOC-ALFA(5:5) TO UT-EKH-IDVERGL                       
000844     ELSE                                                                 
000845     IF WS-IDFINDOC-ALFA(6:1) NOT = '0'                                   
000846       MOVE WS-IDFINDOC-ALFA(6:4) TO UT-EKH-IDVERGL                       
000847     ELSE                                                                 
000848     IF WS-IDFINDOC-ALFA(7:1) NOT = '0'                                   
000849       MOVE WS-IDFINDOC-ALFA(7:3) TO UT-EKH-IDVERGL                       
000850     ELSE                                                                 
000851     IF WS-IDFINDOC-ALFA(8:1) NOT = '0'                                   
000852       MOVE WS-IDFINDOC-ALFA(8:2) TO UT-EKH-IDVERGL                       
000853     ELSE                                                                 
000854       MOVE WS-IDFINDOC-ALFA(9:1) TO UT-EKH-IDVERGL                       
000855     END-IF                                                               
000856     END-IF                                                               
000857     END-IF                                                               
000858     END-IF                                                               
000859     END-IF                                                               
000860     END-IF                                                               
000861     END-IF                                                               
000862     END-IF                                                               
000863                                                                          
000864     MOVE SPACE                 TO UT-EKH-KDANMORS                        
000865                                                                          
000866     MOVE '204'                 TO UT-EKH-KDEKHHT                         
000867     MOVE '253'                 TO UT-EKH-KDEKSHT                         
000868     MOVE SPACE                 TO UT-EKH-IDDC-SEND                       
000869     MOVE SPAR-GL-KDVALISO      TO UT-EKH-KDVALISO                        
000870     MOVE SPAR-GL-PRKURS        TO UT-EKH-PRKURS                          
000871     MOVE ZERO                  TO UT-EKH-IDKONTO                         
000872     MOVE ZERO                  TO UT-EKH-PRARTNTO                        
000873     MOVE ZERO                  TO UT-EKH-PRARTSJK                        
000874     MOVE ZERO                  TO UT-EKH-PRARTSTD                        
000875     MOVE ZERO                  TO UT-EKH-PRINK                           
000876                                                                          
000877     MOVE 'DDI'                 TO UT-EKH-KDEKNIVA                        
000878     MOVE ZERO                  TO UT-EKH-KDFRAKT                         
000879     MOVE ZERO                  TO UT-EKH-KDPRODSL                        
000880     MOVE ZERO                  TO UT-EKH-KDPSLLOC                        
000881     MOVE ZERO                  TO UT-EKH-KVANTAL                         
000882     MOVE ZERO                  TO UT-EKH-PRHEMTAG                        
000883     MOVE ZERO                  TO UT-EKH-PRDIRLON                        
000884     MOVE ZERO                  TO UT-EKH-PRDMTRL                         
000885     MOVE ZERO                  TO UT-EKH-PRLANDCO                        
000886     MOVE ZERO                  TO UT-EKH-PROVRPAL                        
000887     MOVE WS-DIFF               TO UT-EKH-SUBEL                           
000888     MOVE ZERO                  TO UT-EKH-SUVAT                           
000889     MOVE SPAR-GL-DAREFDAT      TO UT-EKH-DAAVIDAT                        
000890     MOVE ZERO                  TO UT-EKH-IDAVINR                         
000891     MOVE SPACE                 TO UT-EKH-IDLEVNR                         
000892     MOVE ZERO                  TO UT-EKH-KDAVVTYP                        
000893     MOVE ZERO                  TO UT-EKH-KDRT                            
000894     MOVE ZERO                  TO UT-EKH-KVANTMOT                        
000895     MOVE ZERO                  TO UT-EKH-KVAVIS                          
000896     MOVE SPACE                 TO UT-EKH-KDSORT                          
000897     MOVE SPAR-GL-KDTRADP       TO UT-EKH-KDTRADP                         
000898     MOVE 'N'                   TO UT-EKH-FLOVRLEV                        
000899                                                                          
000900     IF SPAR-GL-IDREF = SPACE                                             
000901       MOVE ZERO TO UT-EKH-IDORDNR5                                       
000902     ELSE                                                                 
000903       COMPUTE UT-EKH-IDORDNR5 =                                          
000904               FUNCTION NUMVAL(SPAR-GL-IDREF(1:5))                        
000905       END-COMPUTE                                                        
000906     END-IF                                                               
000907     MOVE SPACE                 TO UT-EKH-IDUSER                          
000908     MOVE SPACE                 TO UT-EKH-IDREF                           
000909     MOVE SPACE                 TO UT-EKH-FLDCET                          
000910     MOVE SPACE                 TO UT-EKH-IDKUNDRF                        
000910     MOVE SPACE                 TO UT-EKH-IDFAKT-EXP                      
000911                                                                          
000912     PERFORM S80-PUT                                                      
000913     .                                                                    
000914     EJECT                                                                
000915                                                                          
000916 BE-HANDLE-DIFF-VSS SECTION.                                              
000917     MOVE IDPGM                 TO UT-FIL-IDPGM                           
000918     MOVE WS-DATE               TO UT-FIL-DAREGDAT                        
000919     MOVE WS-TIME               TO UT-FIL-TIKLOCK                         
000920     MOVE 1                     TO UT-FIL-IDSEKVNR                        
000921     MOVE 'W510EKHA'            TO UT-FIL-IDCPYTXT                        
000922     MOVE SPAR-GL-KDVAT         TO UT-EKH-BEVAT                           
000923     MOVE SPAR-GL-DAFINDOC      TO UT-EKH-DAVERDAT                        
000924     MOVE 'N'                   TO UT-EKH-FLLSBOK                         
000925     MOVE SPACE                 TO UT-EKH-IDANALYS                        
000926     MOVE ZERO                  TO UT-EKH-IDARTNR                         
000927     MOVE SPAR-GL-IDEXCUST(1)   TO WS-IDEXCUST                            
000928     MOVE +0                    TO W-ANT                                  
000929     INSPECT WS-IDEXCUST TALLYING W-ANT FOR CHARACTERS                    
000930             BEFORE INITIAL ' '                                           
000931     IF W-ANT = ZERO                                                      
000932       MOVE ZERO                  TO WS-IDDISTR-1                         
000933     ELSE                                                                 
000934       MOVE WS-IDEXCUST(1:W-ANT)  TO WS-IDDISTR-1                         
000935     END-IF                                                               
000936     IF SPAR-GL-IDSYSTEM-SEND = 'VSS'                                     
000937       MOVE 0778                  TO WS-IDDISTR-1                         
000938     END-IF                                                               
000939     MOVE WS-IDDISTR-1          TO UT-EKH-IDDISTR                         
000940     MOVE SPACE                 TO UT-EKH-IDKST                           
000941                                                                          
000942     IF SPAR-GL-IDPARTNR = SPACE                                          
000943       MOVE ZERO TO UT-EKH-IDKUNDNR                                       
000944     ELSE                                                                 
000945       COMPUTE UT-EKH-IDKUNDNR =                                          
000946               FUNCTION NUMVAL(SPAR-GL-IDPARTNR(1:7))                     
000947       END-COMPUTE                                                        
000948     END-IF                                                               
000949                                                                          
000950     MOVE SPACE                   TO UT-EKH-IDTRANS                       
000951                                                                          
000952     MOVE SPAR-GL-IDFINDOC        TO WS-IDFINDOC-ALFA                     
000953     IF WS-IDFINDOC-ALFA(1:1) NOT = '0'                                   
000954       MOVE WS-IDFINDOC-ALFA      TO UT-EKH-IDVERGL                       
000955     ELSE                                                                 
000956     IF WS-IDFINDOC-ALFA(2:1) NOT = '0'                                   
000957       MOVE WS-IDFINDOC-ALFA(2:8) TO UT-EKH-IDVERGL                       
000958     ELSE                                                                 
000959     IF WS-IDFINDOC-ALFA(3:1) NOT = '0'                                   
000960       MOVE WS-IDFINDOC-ALFA(3:7) TO UT-EKH-IDVERGL                       
000961     ELSE                                                                 
000962     IF WS-IDFINDOC-ALFA(4:1) NOT = '0'                                   
000963       MOVE WS-IDFINDOC-ALFA(4:6) TO UT-EKH-IDVERGL                       
000964     ELSE                                                                 
000965     IF WS-IDFINDOC-ALFA(5:1) NOT = '0'                                   
000966       MOVE WS-IDFINDOC-ALFA(5:5) TO UT-EKH-IDVERGL                       
000967     ELSE                                                                 
000968     IF WS-IDFINDOC-ALFA(6:1) NOT = '0'                                   
000969       MOVE WS-IDFINDOC-ALFA(6:4) TO UT-EKH-IDVERGL                       
000970     ELSE                                                                 
000971     IF WS-IDFINDOC-ALFA(7:1) NOT = '0'                                   
000972       MOVE WS-IDFINDOC-ALFA(7:3) TO UT-EKH-IDVERGL                       
000973     ELSE                                                                 
000974     IF WS-IDFINDOC-ALFA(8:1) NOT = '0'                                   
000975       MOVE WS-IDFINDOC-ALFA(8:2) TO UT-EKH-IDVERGL                       
000976     ELSE                                                                 
000977       MOVE WS-IDFINDOC-ALFA(9:1) TO UT-EKH-IDVERGL                       
000978     END-IF                                                               
000979     END-IF                                                               
000980     END-IF                                                               
000981     END-IF                                                               
000982     END-IF                                                               
000983     END-IF                                                               
000984     END-IF                                                               
000985     END-IF                                                               
000986                                                                          
000987     MOVE SPACE                 TO UT-EKH-KDANMORS                        
000988                                                                          
000989     IF GL-KDFINDOC = 'INV2'                                              
000990       MOVE '205'             TO UT-EKH-KDEKHHT                           
000991       MOVE '201'             TO UT-EKH-KDEKSHT                           
000992     ELSE                                                                 
000993       MOVE '304'             TO UT-EKH-KDEKHHT                           
000994       MOVE '301'             TO UT-EKH-KDEKSHT                           
000995     END-IF                                                               
000996     MOVE SPACE                 TO UT-EKH-IDDC-SEND                       
000997     IF (SPAR-GL-KDPARTTY = 'EXT')     AND                                
000998        (SPAR-GL-KDPARTGR = 'CARD-EU'  OR                                 
000999         SPAR-GL-KDPARTGR = 'CARD-NOT-EU')                                
001000       MOVE 'SEK'               TO UT-EKH-KDVALISO                        
001001       MOVE 1.0                 TO UT-EKH-PRKURS                          
001002     ELSE                                                                 
001003       MOVE SPAR-GL-KDVALISO    TO UT-EKH-KDVALISO                        
001004       MOVE SPAR-GL-PRKURS      TO UT-EKH-PRKURS                          
001005     END-IF                                                               
001006     MOVE ZERO                  TO UT-EKH-IDKONTO                         
001007     MOVE ZERO                  TO UT-EKH-PRARTNTO                        
001008     MOVE ZERO                  TO UT-EKH-PRARTSJK                        
001009     MOVE ZERO                  TO UT-EKH-PRARTSTD                        
001010     MOVE ZERO                  TO UT-EKH-PRINK                           
001011                                                                          
001012     MOVE 'DDI'                 TO UT-EKH-KDEKNIVA                        
001013     MOVE ZERO                  TO UT-EKH-KDFRAKT                         
001014     MOVE ZERO                  TO UT-EKH-KDPRODSL                        
001015     MOVE ZERO                  TO UT-EKH-KDPSLLOC                        
001016     MOVE ZERO                  TO UT-EKH-KVANTAL                         
001017     MOVE ZERO                  TO UT-EKH-PRHEMTAG                        
001018     MOVE ZERO                  TO UT-EKH-PRDIRLON                        
001019     MOVE ZERO                  TO UT-EKH-PRDMTRL                         
001020     MOVE ZERO                  TO UT-EKH-PRLANDCO                        
001021     MOVE ZERO                  TO UT-EKH-PROVRPAL                        
001022     MOVE WS-DIFF-VSS           TO UT-EKH-SUBEL                           
001023     MOVE ZERO                  TO UT-EKH-SUVAT                           
001024     MOVE SPAR-GL-DAREFDAT      TO UT-EKH-DAAVIDAT                        
001025     MOVE ZERO                  TO UT-EKH-IDAVINR                         
001026     MOVE SPACE                 TO UT-EKH-IDLEVNR                         
001027     MOVE ZERO                  TO UT-EKH-KDAVVTYP                        
001028     MOVE ZERO                  TO UT-EKH-KDRT                            
001029     MOVE ZERO                  TO UT-EKH-KVANTMOT                        
001030     MOVE ZERO                  TO UT-EKH-KVAVIS                          
001031     MOVE SPACE                 TO UT-EKH-KDSORT                          
001032     MOVE SPAR-GL-KDTRADP       TO UT-EKH-KDTRADP                         
001033     MOVE 'N'                   TO UT-EKH-FLOVRLEV                        
001034                                                                          
001035     IF SPAR-GL-IDREF = SPACE                                             
001036       MOVE ZERO TO UT-EKH-IDORDNR5                                       
001037     ELSE                                                                 
001038       COMPUTE UT-EKH-IDORDNR5 =                                          
001039               FUNCTION NUMVAL(SPAR-GL-IDREF(1:5))                        
001040       END-COMPUTE                                                        
001041     END-IF                                                               
001042     MOVE SPACE                 TO UT-EKH-IDUSER                          
001043     MOVE SPACE                 TO UT-EKH-IDREF                           
001044     MOVE SPACE                 TO UT-EKH-FLDCET                          
001045     MOVE SPACE                 TO UT-EKH-IDKUNDRF                        
001045     MOVE SPACE                 TO UT-EKH-IDFAKT-EXP                      
001046                                                                          
001047     PERFORM S80-PUT                                                      
001048     .                                                                    
001049     EJECT                                                                
001050                                                                          
001051 Z-FINIT SECTION.                                                         
001052     CLOSE WF2014                                                         
001053                                                                          
001054     IF WS-SEND-IDCOM > ZERO                                              
001055       PERFORM S90-CLOSE                                                  
001056     END-IF                                                               
001057     .                                                                    
001058     EJECT                                                                
001059                                                                          
001060 S01-READ-WF2014  SECTION.                                                
001061     READ WF2014 INTO IN-AREA                                             
001062       AT END                                                             
001063         SET END-OF-WF2014 TO TRUE                                        
001064     END-READ                                                             
001065     .                                                                    
001066     EJECT                                                                
001067                                                                          
001068 S70-OPEN SECTION.                                                        
001069     MOVE WS-ADRESS-PULS                  TO SEND-ADDISPABS               
001070     MOVE 'OPEN'                          TO SEND-KDFUNC                  
001071     CALL WZ01SEND USING SEND-CONTROL-AREA                                
001072                         SEND-OPEN-AREA                                   
001073     IF SEND-KDRC > ZERO                                                  
001074       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
001075       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
001076       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
001077       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
001078     END-IF                                                               
001079     .                                                                    
001080                                                                          
001081 S80-PUT SECTION.                                                         
001082     MOVE 'PUT'                           TO SEND-KDFUNC                  
001083     MOVE WS-SEND-IDCOM                   TO SEND-IDCOM                   
001084     MOVE LENGTH OF UT-AREA               TO SEND-KVDLEN                  
001085     CALL WZ01SEND USING SEND-CONTROL-AREA                                
001086                         SEND-KVDLEN                                      
001087                         UT-AREA                                          
001088     IF SEND-KDRC > ZERO                                                  
001089       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
001090       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
001091       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
001092       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
001093     END-IF                                                               
001094     .                                                                    
001095                                                                          
001096 S90-CLOSE SECTION.                                                       
001097     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
001098     MOVE WS-SEND-IDCOM                   TO SEND-IDCOM                   
001099     CALL WZ01SEND USING SEND-CONTROL-AREA                                
001100     .                                                                    
