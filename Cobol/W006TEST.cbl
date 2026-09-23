000001 ID  DIVISION.                                                            
000003 PROGRAM-ID.    W006TEST.                                                 
000004 AUTHOR.        RICHARD.                                                  
000005 DATE-WRITTEN.  SEP   1992.                                               
000006 DATE-COMPILED.                                                           
000007                                                                          
000008*    FUNKTION:                                                            
000009*        TEST-PROGRAM FÖR W006XXXX (STANDARDSUB-PROGRAM).                 
000010*             SUBPROGRAM W006PRT                                          
000011*                       (PRINTER OCH NODE)                                
000016*                    OCH W006PRC1                                         
000017*                       (PRINTNING GENOM VCOM OCH SPOOL-API)              
000018*                    OCH W006PRR1                                         
000019*                       (PRINTNING GENOM SPOOL-API MED ÅTERSTART)         
000020*                    OCH W006PRS1                                         
000021*                       (PRINTNING GENOM SPOOL-API UTAN ÅTERSTART)        
000022*                    OCH W006ASCI                                         
000023*                       (ÖVERSÄTTNING MELLAN ASCI OCH EBCDIC)             
000024     SKIP3                                                                
000025 ENVIRONMENT DIVISION.                                                    
000026 CONFIGURATION SECTION.                                                   
000027 SPECIAL-NAMES.                                                           
000028     C01 IS NY-SIDA.                                                      
000029     SKIP3                                                                
000030 INPUT-OUTPUT SECTION.                                                    
000031 FILE-CONTROL.                                                            
000032     SELECT W006STYR ASSIGN TO W006STYR.                                  
000033     EJECT                                                                
000034 DATA DIVISION.                                                           
000035 FILE SECTION.                                                            
000036                                                                          
000037 FD  W006STYR                                                             
000038     RECORDING      F                                                     
000039     BLOCK CONTAINS 0.                                                    
000040                                                                          
000041 01  STYR-POST.                                                           
000050   03  STYR-IDPGM                PIC X(8).                                
000060   03  FILLER                    PIC X(72).                               
000105     EJECT                                                                
000106 WORKING-STORAGE SECTION.                                                 
000107                                                                          
000108                                                                          
000109*    -- CHECKED BY WY2000                                                 
000110 77    IDPGM                     PIC X(8)    VALUE 'W006TEST'.            
000111 77    JA                        PIC X(1)    VALUE 'J'.                   
000120 77    NEJ                       PIC X(1)    VALUE 'N'.                   
000123 77    W-LISTNR-1                PIC X(10)   VALUE 'LISTA-1   '.          
000124 77    W-LISTNR-2                PIC X(10)   VALUE 'LISTA-2   '.          
000125 77    W-DUMMY                   PIC X(8)    VALUE SPACE.                 
000126 77    MSG-IO-AREA-L             PIC S9(9)   VALUE +32  COMP SYNC.        
000127 77    MSG-IO-AREA               PIC X(32)   VALUE SPACE.                 
000128 77    CHKP-ID                   PIC X(8)    VALUE 'W006TEST'.            
000129 77    CHKP-AREA-1-L             PIC S9(9)   VALUE +32  COMP SYNC.        
000130 77    CHKP-AREA-1               PIC X(32)   VALUE SPACE.                 
000131 77    TEST-36000-1              PIC X(8)    VALUE '013     '.            
000132 77    TEST-36000-2              PIC X(8)    VALUE '015     '.            
000133     EJECT                                                                
000134 01  DYNAMISKA-SUBPROGRAM.                                                
000135   03  W006ASCI                  PIC X(8)    VALUE 'W006ASCI'.            
000136   03  W006PRT                   PIC X(8)    VALUE 'W006PRT '.            
000137   03  W006PRC1                  PIC X(8)    VALUE 'W006PRC1'.            
000142   03  W006PRR1                  PIC X(8)    VALUE 'W006PRR1'.            
000143   03  W006PRR2                  PIC X(8)    VALUE 'W006PRR2'.            
000144   03  W006PRS1                  PIC X(8)    VALUE 'W006PRS1'.            
000145   03  W006PRS2                  PIC X(8)    VALUE 'W006PRS2'.            
000146   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
000147   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
000148     EJECT                                                                
000149 01    ASCI-PARAMETRAR.                                                   
000150   03    ASCI-001                PIC X(3)    VALUE '001'.                 
000151   03    ASCI-LL                 PIC S9(9)   VALUE +120   COMP.           
000152 01    ASCI-DATA.                                                         
000153   03  FILLER                    PIC X(12)   VALUE '1234567890+é'.        
000154   03  FILLER                    PIC X(12)   VALUE '!"§¤%&/()=?É'.        
000155   03  FILLER                    PIC X(12)   VALUE 'qwertyuiopåü'.        
000156   03  FILLER                    PIC X(12)   VALUE 'QWERTYUIOPÅ^'.        
000157   03  FILLER                    PIC X(12)   VALUE 'asdfghjklöä '.        
000158   03  FILLER                    PIC X(12)   VALUE 'ASDFGHJKLÖÄ*'.        
000159   03  FILLER                    PIC X(12)   VALUE '<zxcvbnm,.- '.        
000160   03  FILLER                    PIC X(12)   VALUE '>ZXCVBNM;:_ '.        
000161   03  FILLER                    PIC X(50)   VALUE space.                 
000162     EJECT                                                                
000163 01  PRT-AREA-START              PIC X(16)   VALUE 'PRT-START'.           
000164*01    -COPY W006PRT                                                      
000165     EJECT                                                                
000166 01    SIDA.                                                              
000167   03  RAD1                      PIC X(132)  VALUE 'RAD 001               
000168-      '    20        30        40        50        60        70          
000169-      '      80        90       100       110       120         1        
000170-      '32'.                                                              
000171   03  RAD2.                                                              
000172     05  FILLER                  PIC X(4)    VALUE 'RAD '.                
000173     05  RAD                     PIC 9(3)    VALUE 001.                   
000174     05  FILLER                  PIC X(40)   VALUE SPACE.                 
000175     05  FILLER                  PIC X(3)    VALUE '50A'.                 
000176     05  FILLER                  PIC X(27)   VALUE SPACE.                 
000177     05  FILLER                  PIC X(3)    VALUE '80A'.                 
000178     05  FILLER                  PIC X(33)   VALUE SPACE.                 
000179     05  FILLER                  PIC X(3)    VALUE '116'.                 
000180     05  FILLER                  PIC X(13)   VALUE SPACE.                 
000181     05  FILLER                  PIC X(3)    VALUE '132'.                 
000182   03  SLUTRAD                   PIC X(132)  VALUE 'SLUTRAD               
000183-      '    20        30        40        50        60        70          
000184-      '      80        90       100       110       120         1        
000185-      '32'.                                                              
000186     EJECT                                                                
000187*01    -COPY W006PRAR                                                     
000188     EJECT                                                                
000189 01  STATUS-WS                   PIC XX.                                  
000190     88  SEGMENT-FINNS                       VALUE '  '.                  
000191     88  IMS-EJ-OK                           VALUE 'XD'.                  
000192     SKIP3                                                                
000193 01  GODK-STATUSKODER.                                                    
000194   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
000195     EJECT                                                                
000196*01    -COPY W0003                                                        
000197     EJECT                                                                
000198 LINKAGE SECTION.                                                         
000199*01    -COPY W0009     -PRE MSG-                                          
000200*01    -COPY W0009     -PRE ALT-                                          
000201     EJECT                                                                
000202*01    -COPY W0008     -PRE LISB-                                         
000203     05  FILLER                  PIC X(1).                                
000204     EJECT                                                                
000205 PROCEDURE DIVISION USING MSG-PCB ALT-PCB LISB-PCB.                       
000206 MAIN SECTION.                                                            
000207     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB LISB-PCB.                      
000208                                                                          
000209     DISPLAY 'TEST STARTAR'                                               
000210     DISPLAY '            '                                               
000211                                                                          
000212     PERFORM A-INIT                                                       
000213     EVALUATE STYR-IDPGM                                                  
000214       WHEN 'W006PRT '                                                    
000215         PERFORM B-W006PRT                                                
000220       WHEN 'W006PRC1'                                                    
000221         PERFORM E-W006PRC1                                               
000228       WHEN 'W006PRR1'                                                    
000229         PERFORM H-W006PRR1                                               
000230       WHEN 'W006PRR2'                                                    
000231         PERFORM I-W006PRR2                                               
000232       WHEN 'W006PRS1'                                                    
000233         PERFORM J-W006PRS1                                               
000234       WHEN 'W006PRS2'                                                    
000235         PERFORM K-W006PRS2                                               
000236       WHEN 'W006ASCI'                                                    
000237         PERFORM L-W006ASCI                                               
000238       WHEN OTHER                                                         
000239         DISPLAY 'FEL STYR-PARAMETER'                                     
000240     END-EVALUATE                                                         
000241                                                                          
000242     DISPLAY '            '                                               
000243     DISPLAY 'TEST SLUTAR '                                               
000244                                                                          
000245     MOVE ZERO TO RETURN-CODE                                             
000246     GOBACK                                                               
000247     .                                                                    
000250     EJECT                                                                
000260 A-INIT SECTION.                                                          
000270                                                                          
000280     OPEN INPUT W006STYR                                                  
000290                                                                          
000300     READ W006STYR                                                        
000400       AT END                                                             
000410         DISPLAY 'STYR-PARAMETER SAKNAS'                                  
000420         CALL FELLOG                                                      
000430       NOT AT END                                                         
000440         DISPLAY STYR-IDPGM                                               
000450     END-READ                                                             
000500     .                                                                    
008500     EJECT                                                                
008600 B-W006PRT SECTION.                                                       
008700                                                                          
008800     MOVE 001 TO PRT-KDCALL                                               
008900     MOVE '12345678' TO PRT-IDPRTLST                                      
009000     CALL W006PRT USING PRT-W006PRT                                       
009100     PERFORM BA-W006PRT                                                   
009300                                                                          
009400     MOVE 001 TO PRT-KDCALL                                               
009500     MOVE 'SPA     ' TO PRT-IDPRTLST                                      
009600     CALL W006PRT USING PRT-W006PRT                                       
009610     PERFORM BA-W006PRT                                                   
010000                                                                          
010010     MOVE 001 TO PRT-KDCALL                                               
010020     MOVE '461     ' TO PRT-IDPRTLST                                      
010030     CALL W006PRT USING PRT-W006PRT                                       
010031     PERFORM BA-W006PRT                                                   
010096                                                                          
010100     MOVE 002 TO PRT-KDCALL                                               
010200     MOVE 'W1234567' TO PRT-IDLTERM                                       
010300     CALL W006PRT USING PRT-W006PRT                                       
010310     PERFORM BA-W006PRT                                                   
010700                                                                          
010800     MOVE 002 TO PRT-KDCALL                                               
010900     MOVE 'R17850  ' TO PRT-IDLTERM                                       
011000     CALL W006PRT USING PRT-W006PRT                                       
011010     PERFORM BA-W006PRT                                                   
011020                                                                          
011030     MOVE 003 TO PRT-KDCALL                                               
011040     MOVE 'AAAAAAAA' TO PRT-IDPRTLST                                      
011050     CALL W006PRT USING PRT-W006PRT                                       
011060     PERFORM BA-W006PRT                                                   
011070                                                                          
011080     MOVE 003 TO PRT-KDCALL                                               
011090     MOVE '001     ' TO PRT-IDPRTLST                                      
011100     CALL W006PRT USING PRT-W006PRT                                       
011200     PERFORM BA-W006PRT                                                   
017100     .                                                                    
019700     EJECT                                                                
019720 BA-W006PRT SECTION.                                                      
019730                                                                          
019770     DISPLAY PRT-KDCALL   ' ' PRT-IDPRTLST                                
019780         ' ' PRT-IDLTERM  ' ' PRT-IDNODE                                  
019790         ' ' PRT-BEPRTLST ' ' PRT-KDSVAR                                  
019791     .                                                                    
039203     EJECT                                                                
039204 E-W006PRC1 SECTION.                                                      
039205                                                                          
039206*    PERFORM IMS-RESTART                                                  
039207                                                                          
039208     CALL W006PRC1 USING PRT-SPOOL-OVR PRT-OPEN  TEST-36000-2             
039209                         ALT-PCB                                          
039210                         W-DUMMY W-DUMMY                                  
039211                                                                          
039212     CALL W006PRC1 USING PRT-SPOOL-OVR PRT-WRITE TEST-36000-2             
039213                         ALT-PCB                                          
039214                         PRT-NYSIDA-RAD1 RAD1                             
039215                                                                          
039216     MOVE 1 TO RAD                                                        
039217     PERFORM 69 TIMES                                                     
039218       ADD 1 TO RAD                                                       
039219       CALL W006PRC1 USING PRT-SPOOL-OVR PRT-WRITE TEST-36000-2           
039220                           ALT-PCB                                        
039221                           PRT-AFTER-1 RAD2                               
039222     END-PERFORM                                                          
039223                                                                          
039224     CALL W006PRC1 USING PRT-SPOOL-OVR PRT-PURGE TEST-36000-2             
039225                         ALT-PCB                                          
039226                         W-DUMMY W-DUMMY                                  
039227                                                                          
039228*    PERFORM IMS-CHECKPOINT                                               
039229                                                                          
039230     CALL W006PRC1 USING PRT-SPOOL-OVR PRT-OPEN  TEST-36000-2             
039231                         ALT-PCB                                          
039232                         W-DUMMY W-DUMMY                                  
039233                                                                          
039234     CALL W006PRC1 USING PRT-SPOOL-OVR PRT-WRITE TEST-36000-2             
039235                         ALT-PCB                                          
039236                         PRT-NYSIDA-RAD1 RAD1                             
039237                                                                          
039238     MOVE 1 TO RAD                                                        
039239     PERFORM 40 TIMES                                                     
039240       ADD 1 TO RAD                                                       
039241       CALL W006PRC1 USING PRT-SPOOL-OVR PRT-WRITE TEST-36000-2           
039242                           ALT-PCB                                        
039243                           PRT-AFTER-1 RAD2                               
039244     END-PERFORM                                                          
039245                                                                          
039246*    CALL W006PRC1 USING PRT-SPOOL-OVR PRT-PURGE TEST-36000-2             
039247*                        ALT-PCB                                          
039248*                        W-DUMMY W-DUMMY                                  
039249                                                                          
039250     CALL W006PRC1 USING PRT-SPOOL-OVR PRT-WRITE TEST-36000-2             
039251                         ALT-PCB                                          
039252                         PRT-NYSIDA-RAD1 RAD1                             
039253                                                                          
039254     MOVE 1 TO RAD                                                        
039255     PERFORM 47 TIMES                                                     
039256       ADD 1 TO RAD                                                       
039257       CALL W006PRC1 USING PRT-SPOOL-OVR PRT-WRITE TEST-36000-2           
039258                           ALT-PCB                                        
039259                           PRT-AFTER-1 RAD2                               
039260     END-PERFORM                                                          
039261                                                                          
039262     CALL W006PRC1 USING PRT-SPOOL-OVR PRT-WRITE TEST-36000-2             
039263                         ALT-PCB                                          
039264                         PRT-AFTER-1 SLUTRAD                              
039265                                                                          
039278     CALL W006PRC1 USING PRT-SPOOL-OVR PRT-CLOSE TEST-36000-2             
039279                         ALT-PCB                                          
039280                         W-DUMMY W-DUMMY                                  
039339     .                                                                    
039577     EJECT                                                                
039578 H-W006PRR1 SECTION.                                                      
039579                                                                          
039580     PERFORM IMS-RESTART                                                  
039581                                                                          
039582     CALL W006PRR1 USING PRT-SPOOL-OVR PRT-OPEN TEST-36000-1              
039583                         ALT-PCB LISB-PCB W-DUMMY                         
039584                         W-DUMMY W-DUMMY                                  
039585                                                                          
039586     CALL W006PRR1 USING PRT-SPOOL-OVR PRT-WRITE TEST-36000-1             
039587                         ALT-PCB LISB-PCB W-LISTNR-1                      
039588                         PRT-NYSIDA-RAD1 RAD1                             
039589                                                                          
039590     MOVE 1 TO RAD                                                        
039591     PERFORM 47 TIMES                                                     
039592       ADD 1 TO RAD                                                       
039593       CALL W006PRR1 USING PRT-SPOOL-OVR PRT-WRITE TEST-36000-1           
039594                           ALT-PCB LISB-PCB W-LISTNR-1                    
039595                           PRT-AFTER-1 RAD2                               
039596     END-PERFORM                                                          
039597                                                                          
039598     CALL W006PRR1 USING PRT-SPOOL-OVR PRT-PURGE TEST-36000-1             
039599                         ALT-PCB LISB-PCB W-LISTNR-1                      
039600                         W-DUMMY W-DUMMY                                  
039601                                                                          
039602     CALL W006PRR1 USING PRT-SPOOL-OVR PRT-WRITE TEST-36000-2             
039603                         ALT-PCB LISB-PCB W-LISTNR-2                      
039604                         PRT-NYSIDA-RAD1 RAD1                             
039605                                                                          
039606     MOVE 1 TO RAD                                                        
039607     PERFORM 47 TIMES                                                     
039608       ADD 1 TO RAD                                                       
039609       CALL W006PRR1 USING PRT-SPOOL-OVR PRT-WRITE TEST-36000-2           
039610                           ALT-PCB LISB-PCB W-LISTNR-2                    
039611                           PRT-AFTER-1 RAD2                               
039612     END-PERFORM                                                          
039613                                                                          
039614*    CALL W006PRR1 USING PRT-SPOOL-OVR PRT-PURGE TEST-36000-2             
039615*                        ALT-PCB LISB-PCB W-LIATNR-2                      
039616*                        W-DUMMY W-DUMMY                                  
039617                                                                          
039618     PERFORM 10 TIMES                                                     
039619                                                                          
039620       CALL W006PRR1 USING PRT-SPOOL-OVR PRT-WRITE TEST-36000-2           
039621                           ALT-PCB LISB-PCB W-LISTNR-2                    
039622                           PRT-NYSIDA-RAD1 RAD1                           
039623                                                                          
039624       MOVE 1 TO RAD                                                      
039625       PERFORM 47 TIMES                                                   
039626         ADD 1 TO RAD                                                     
039627        CALL W006PRR1 USING PRT-SPOOL-OVR PRT-WRITE   TEST-36000-2        
039628                             ALT-PCB LISB-PCB W-LISTNR-2                  
039629                             PRT-AFTER-1 RAD2                             
039630       END-PERFORM                                                        
039631                                                                          
039632     END-PERFORM                                                          
039633                                                                          
039634     CALL W006PRR1 USING PRT-SPOOL-OVR PRT-CLOSE TEST-36000-2             
039635                         ALT-PCB LISB-PCB W-DUMMY                         
039636                         W-DUMMY W-DUMMY                                  
039637     .                                                                    
039638     EJECT                                                                
039639 I-W006PRR2 SECTION.                                                      
039640                                                                          
039641     PERFORM IMS-RESTART                                                  
039642                                                                          
039643     CALL W006PRR2 USING PRT-SPOOL-OVR PRT-OPEN TEST-36000-2              
039644                         ALT-PCB LISB-PCB W-DUMMY                         
039645                         W-DUMMY W-DUMMY                                  
039646                                                                          
039647     CALL W006PRR2 USING PRT-SPOOL-OVR PRT-WRITE TEST-36000-2             
039648                         ALT-PCB LISB-PCB W-LISTNR-1                      
039649                         PRT-NYSIDA-RAD1 RAD1                             
039650                                                                          
039651     MOVE 1 TO RAD                                                        
039652     PERFORM 47 TIMES                                                     
039653       ADD 1 TO RAD                                                       
039654       CALL W006PRR2 USING PRT-SPOOL-OVR PRT-WRITE TEST-36000-2           
039655                           ALT-PCB LISB-PCB W-LISTNR-1                    
039656                           PRT-AFTER-1 RAD2                               
039657     END-PERFORM                                                          
039658                                                                          
039659     CALL W006PRR2 USING PRT-SPOOL-OVR PRT-PURGE TEST-36000-2             
039660                         ALT-PCB LISB-PCB W-LISTNR-1                      
039661                         W-DUMMY W-DUMMY                                  
039662                                                                          
039663     CALL W006PRR2 USING PRT-SPOOL-OVR PRT-WRITE TEST-36000-2             
039664                         ALT-PCB LISB-PCB W-LISTNR-2                      
039665                         PRT-NYSIDA-RAD1 RAD1                             
039666                                                                          
039667     MOVE 1 TO RAD                                                        
039668     PERFORM 47 TIMES                                                     
039669       ADD 1 TO RAD                                                       
039670       CALL W006PRR2 USING PRT-SPOOL-OVR PRT-WRITE TEST-36000-2           
039671                           ALT-PCB LISB-PCB W-LISTNR-2                    
039672                           PRT-AFTER-1 RAD2                               
039673     END-PERFORM                                                          
039674                                                                          
039675*    CALL W006PRR2 USING PRT-SPOOL-OVR PRT-PURGE TEST-36000-2             
039676*                        ALT-PCB LISB-PCB W-LIATNR-2                      
039677*                        W-DUMMY W-DUMMY                                  
039678                                                                          
039679     PERFORM 10 TIMES                                                     
039680                                                                          
039681       CALL W006PRR2 USING PRT-SPOOL-OVR PRT-WRITE TEST-36000-2           
039682                           ALT-PCB LISB-PCB W-LISTNR-2                    
039683                           PRT-NYSIDA-RAD1 RAD1                           
039684                                                                          
039685       MOVE 1 TO RAD                                                      
039686       PERFORM 47 TIMES                                                   
039687         ADD 1 TO RAD                                                     
039688        CALL W006PRR2 USING PRT-SPOOL-OVR PRT-WRITE   TEST-36000-2        
039689                             ALT-PCB LISB-PCB W-LISTNR-2                  
039690                             PRT-AFTER-1 RAD2                             
039691       END-PERFORM                                                        
039692                                                                          
039693     END-PERFORM                                                          
039694                                                                          
039695     CALL W006PRR2 USING PRT-SPOOL-OVR PRT-CLOSE TEST-36000-2             
039696                         ALT-PCB LISB-PCB W-DUMMY                         
039697                         W-DUMMY W-DUMMY                                  
039698     .                                                                    
039699     EJECT                                                                
039700 J-W006PRS1 SECTION.                                                      
039701                                                                          
039702*    PERFORM IMS-RESTART                                                  
039703                                                                          
039704     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-OPEN TEST-36000-2              
039705                         ALT-PCB                                          
039706                         W-DUMMY W-DUMMY                                  
039707                                                                          
039708     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE TEST-36000-2             
039709                         ALT-PCB                                          
039710                         PRT-NYSIDA-RAD1 RAD1                             
039711                                                                          
039712     MOVE 1 TO RAD                                                        
039713     PERFORM 69 TIMES                                                     
039714       ADD 1 TO RAD                                                       
039715       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE TEST-36000-2           
039716                           ALT-PCB                                        
039717                           PRT-AFTER-1 RAD2                               
039718     END-PERFORM                                                          
039719                                                                          
039720     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-PURGE TEST-36000-2             
039721                         ALT-PCB                                          
039722                         W-DUMMY W-DUMMY                                  
039723                                                                          
039724*    PERFORM IMS-CHECKPOINT                                               
039725                                                                          
039726     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-OPEN TEST-36000-2              
039727                         ALT-PCB                                          
039728                         W-DUMMY W-DUMMY                                  
039729                                                                          
039730     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE TEST-36000-2             
039731                         ALT-PCB                                          
039732                         PRT-NYSIDA-RAD1 RAD1                             
039733                                                                          
039734     MOVE 1 TO RAD                                                        
039735     PERFORM 40 TIMES                                                     
039736       ADD 1 TO RAD                                                       
039737       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE TEST-36000-2           
039738                           ALT-PCB                                        
039739                           PRT-AFTER-1 RAD2                               
039740     END-PERFORM                                                          
039741                                                                          
039742*    CALL W006PRS1 USING PRT-SPOOL-OVR PRT-PURGE TEST-36000-2             
039743*                        ALT-PCB                                          
039744*                        W-DUMMY W-DUMMY                                  
039745                                                                          
039746     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE TEST-36000-2             
039747                         ALT-PCB                                          
039748                         PRT-NYSIDA-RAD1 RAD1                             
039749                                                                          
039750     MOVE 1 TO RAD                                                        
039751     PERFORM 47 TIMES                                                     
039752       ADD 1 TO RAD                                                       
039753       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE TEST-36000-2           
039754                           ALT-PCB                                        
039755                           PRT-AFTER-1 RAD2                               
039756     END-PERFORM                                                          
039757                                                                          
039758     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE TEST-36000-2             
039759                         ALT-PCB                                          
039760                         PRT-AFTER-1 SLUTRAD                              
039761                                                                          
039762     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-CLOSE TEST-36000-2             
039763                         ALT-PCB                                          
039764                         W-DUMMY W-DUMMY                                  
039765     .                                                                    
039766     EJECT                                                                
039767 K-W006PRS2 SECTION.                                                      
039768                                                                          
039769*    PERFORM IMS-RESTART                                                  
039770                                                                          
039771     CALL W006PRS2 USING PRT-SPOOL-OVR PRT-OPEN TEST-36000-2              
039772                         ALT-PCB                                          
039773                         W-DUMMY W-DUMMY                                  
039774                                                                          
039775     CALL W006PRS2 USING PRT-SPOOL-OVR PRT-WRITE TEST-36000-2             
039776                         ALT-PCB                                          
039777                         PRT-NYSIDA-RAD1 RAD1                             
039778                                                                          
039779     MOVE 1 TO RAD                                                        
039780     PERFORM 69 TIMES                                                     
039781       ADD 1 TO RAD                                                       
039782       CALL W006PRS2 USING PRT-SPOOL-OVR PRT-WRITE TEST-36000-2           
039783                           ALT-PCB                                        
039784                           PRT-AFTER-1 RAD2                               
039785     END-PERFORM                                                          
039786                                                                          
039787     CALL W006PRS2 USING PRT-SPOOL-OVR PRT-PURGE TEST-36000-2             
039788                         ALT-PCB                                          
039789                         W-DUMMY W-DUMMY                                  
039790                                                                          
039791*    PERFORM IMS-CHECKPOINT                                               
039792                                                                          
039793     CALL W006PRS2 USING PRT-SPOOL-OVR PRT-OPEN TEST-36000-2              
039794                         ALT-PCB                                          
039795                         W-DUMMY W-DUMMY                                  
039796                                                                          
039797     CALL W006PRS2 USING PRT-SPOOL-OVR PRT-WRITE TEST-36000-2             
039798                         ALT-PCB                                          
039799                         PRT-NYSIDA-RAD1 RAD1                             
039800                                                                          
039801     MOVE 1 TO RAD                                                        
039802     PERFORM 40 TIMES                                                     
039803       ADD 1 TO RAD                                                       
039804       CALL W006PRS2 USING PRT-SPOOL-OVR PRT-WRITE TEST-36000-2           
039805                           ALT-PCB                                        
039806                           PRT-AFTER-1 RAD2                               
039807     END-PERFORM                                                          
039808                                                                          
039809*    CALL W006PRS2 USING PRT-SPOOL-OVR PRT-PURGE TEST-36000-2             
039810*                        ALT-PCB                                          
039811*                        W-DUMMY W-DUMMY                                  
039812                                                                          
039813     CALL W006PRS2 USING PRT-SPOOL-OVR PRT-WRITE TEST-36000-2             
039814                         ALT-PCB                                          
039815                         PRT-NYSIDA-RAD1 RAD1                             
039816                                                                          
039817     MOVE 1 TO RAD                                                        
039818     PERFORM 47 TIMES                                                     
039819       ADD 1 TO RAD                                                       
039820       CALL W006PRS2 USING PRT-SPOOL-OVR PRT-WRITE TEST-36000-2           
039821                           ALT-PCB                                        
039822                           PRT-AFTER-1 RAD2                               
039823     END-PERFORM                                                          
039824                                                                          
039825     CALL W006PRS2 USING PRT-SPOOL-OVR PRT-WRITE TEST-36000-2             
039826                         ALT-PCB                                          
039827                         PRT-AFTER-1 SLUTRAD                              
039828                                                                          
039829     CALL W006PRS2 USING PRT-SPOOL-OVR PRT-CLOSE TEST-36000-2             
039830                         ALT-PCB                                          
039831                         W-DUMMY W-DUMMY                                  
039832     .                                                                    
039833     EJECT                                                                
039834 L-W006ASCI SECTION.                                                      
039835                                                                          
039836     DISPLAY 'ASCI-TEST STARTAR '                                         
039837                                                                          
039838     CALL W006ASCI USING ASCI-001 ASCI-LL ASCI-DATA                       
039839     PERFORM CA-ASCI                                                      
039840                                                                          
039841     .                                                                    
039842     EJECT                                                                
039843 CA-ASCI SECTION.                                                         
039844                                                                          
039845     DISPLAY '             '                                              
039846                                                                          
039847     DISPLAY ASCI-001                                                     
039848         ' ' ASCI-LL                                                      
039849         ' ' ASCI-DATA                                                    
039850     .                                                                    
039851     EJECT                                                                
039852 IMS-RESTART SECTION.                                                     
039853                                                                          
039854     MOVE SPACE TO MSG-IO-AREA                                            
039855     MOVE '  ' TO GODK-STATUSKODER                                        
039856     CALL CBLTDLI USING XRST MSG-PCB MSG-IO-AREA-L MSG-IO-AREA            
039857                        CHKP-AREA-1-L CHKP-AREA-1                         
039860     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
039900     PERFORM IMS-STATUSKONTROLL                                           
040000     .                                                                    
040100     SKIP3                                                                
040200 IMS-CHECKPOINT SECTION.                                                  
040300                                                                          
040400     MOVE CHKP-ID TO MSG-IO-AREA                                          
040500     MOVE '  XD' TO GODK-STATUSKODER                                      
040600     CALL CBLTDLI USING CHKP MSG-PCB MSG-IO-AREA-L MSG-IO-AREA            
040700                        CHKP-AREA-1-L CHKP-AREA-1                         
040800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
040900     PERFORM IMS-STATUSKONTROLL                                           
041000     IF IMS-EJ-OK                                                         
041100       DISPLAY 'IMS-KONTROLLREGION EJ TILLGÄNGLIG'                        
041200       CALL FELLOG                                                        
041300     END-IF                                                               
041400     .                                                                    
041500     SKIP3                                                                
041600 IMS-STATUSKONTROLL SECTION.                                              
041700                                                                          
041800     SET STATUS-IX TO 1                                                   
041900     SEARCH GODK-STATUS                                                   
042000       AT END                                                             
042100         CALL FELLOG                                                      
042200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
042300         CONTINUE                                                         
042400     END-SEARCH                                                           
042500     .                                                                    
