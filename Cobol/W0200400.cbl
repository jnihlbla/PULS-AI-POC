000001 ID DIVISION.                                                             
000002                                                                          
000003 PROGRAM-ID.     W0200400.                                                
000004 AUTHOR.         STEFANO GIOBBI.                                          
000005 DATE-WRITTEN.   95/07/04.                                                
000006 DATE-COMPILED.                                                           
000007                                                                          
000008*    FUNKTION:                                                            
000009*        PROGRAMMET LÄSER NER SAMTLIGA WDB201-SEGMENT                     
000010*        UT PÅ FIL.                                                       
000011*                                                                         
000012*        PROGRAMMET LÄSER      WLKNDA (WDB2)                              
000013*                                                                         
000014     SKIP3                                                                
000015 ENVIRONMENT DIVISION.                                                    
000016     SKIP2                                                                
000017 INPUT-OUTPUT SECTION.                                                    
000018                                                                          
000019 FILE-CONTROL.                                                            
000020     SKIP2                                                                
000021*          --- VISS KUNDINFO FRÅN WDB201                                  
000022     SELECT W02007                     ASSIGN TO W02004D1.                
000023     SKIP3                                                                
000024*          --- LDC INFO FRÅN WDB201                                       
000025     SELECT W02013                     ASSIGN TO W02004D2.                
000026     EJECT                                                                
000027 DATA DIVISION.                                                           
000028     SKIP2                                                                
000029 FILE SECTION.                                                            
000030     SKIP3                                                                
000031 FD  W02007                                                               
000032     RECORDING       F                                                    
000033     BLOCK CONTAINS  0.                                                   
000034                                                                          
000035*01  POST -COPY W02007 -PRE  UT-  -L.                                     
000036     SKIP3                                                                
000037 FD  W02013                                                               
000038     RECORDING       F                                                    
000039     BLOCK CONTAINS  0.                                                   
000040 01  LDC-POST             PIC X(28).                                      
000041     EJECT                                                                
000042 WORKING-STORAGE SECTION.                                                 
000043                                                                          
000044                                                                          
000045*    -- CHECKED BY WY2000                                                 
000046 77  IDPGM                       PIC X(8)    VALUE 'W0200400'.            
000047 77  JA                          PIC X       VALUE 'J'.                   
000048 77  NEJ                         PIC X       VALUE 'N'.                   
000049 77  SPAR-IDDISTR                PIC S9(5)   VALUE +0.                    
000050     EJECT                                                                
000051 01  W-1AAR-INNAN                PIC 9(6)    VALUE ZERO.                  
000052 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
000053 01  FILLER REDEFINES DAGENS-DATUM.                                       
000054     03  DAGENS-DATUM-AAR        PIC 9(2).                                
000055     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
000056     03  DAGENS-DATUM-DAG        PIC 9(2).                                
000057     EJECT                                                                
000058 01  DYNAMISKA-SUBPROGRAM.                                                
000059*                                                                         
000060     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
000061     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000062     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000063     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
000064     SKIP2                                                                
000065*    --- PARAMETRAR TILL ABEND                                            
000066                                                                          
000067 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
000068 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
000069     SKIP2                                                                
000070 01  FELTEXT.                                                             
000071     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
000072     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
000073     EJECT                                                                
000074*    --- PARAMETRAR TILL POSTSUM                                          
000075*                                                                         
000076*01  -COPY W0005   -PRE  POSTSUM-                                         
000077     EJECT                                                                
000078*01    -COPY WWDC99                                                       
000079       EJECT                                                              
000080 01  UT-AREA-START               PIC X(24)   VALUE                        
000081                                 'UT-AREA-START  '.                       
000082     SKIP2                                                                
000083                                                                          
000084*01  AREA -COPY W02007     -PRE UT-                                       
000085     EJECT                                                                
000086 01  LDC-AREA-START              PIC X(24)   VALUE                        
000087                                 'LDC-AREA-START  '.                      
000088     SKIP2                                                                
000089*01  AREA -COPY W02013     -PRE LDC-                                      
000090     EJECT                                                                
000091*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
000092*                                                                         
000093     EJECT                                                                
000094 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000095     SKIP3                                                                
000096 01  NYCKLAR-TILL-DLI.                                                    
000097     03  W-IDDISTR-X.                                                     
000098         05  W-IDDISTR           PIC S9(3)   VALUE ZERO COMP-3.           
000099     03  W-IDKUNDNR-X.                                                    
000100         05  W-IDKUNDNR          PIC S9(8)   VALUE ZERO COMP-3.           
000101     SKIP2                                                                
000102*    --- STATUS-KOD FRÅN IMS                                              
000103 01  STATUS-WS                   PIC XX.                                  
000104     88  SEGMENT-FINNS                       VALUE '  '.                  
000105     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
000106     88  BASEN-SLUT                          VALUE 'GB'.                  
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
000118 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
000119     SKIP3                                                                
000120 01  DLI-IO-AREA.                                                         
000121*    03  -COPY WDB201                                                     
000122     EJECT                                                                
000123 LINKAGE SECTION.                                                         
000124                                                                          
000125     EJECT                                                                
000126*01  -COPY W0008  -PRE KNDA-                                              
000127     05  FILLER                  PIC X.                                   
000128     EJECT                                                                
000129 PROCEDURE DIVISION  USING KNDA-PCB.                                      
000130     ENTRY 'DLITCBL' USING KNDA-PCB.                                      
000131                                                                          
000132     PERFORM A-INIT                                                       
000133     PERFORM IMS-GET-KNDA                                                 
000134     PERFORM UNTIL BASEN-SLUT                                             
000135       EVALUATE KNDA-SEG-NAME-FB                                          
000136         WHEN 'WDB201  '                                                  
000137           PERFORM B-FLYTTA-WDB201-INFO                                   
000138           PERFORM S11-SKRIV-W02007                                       
000139           IF GMT-FLLDCKND = 'N' OR                                       
000140             (GMT-TISTODAT > 0 AND GMT-TISTODAT < W-1AAR-INNAN)           
000141             CONTINUE                                                     
000142           ELSE                                                           
000143             PERFORM C-SKAPA-SKRIV-LDC-FIL                                
000144           END-IF                                                         
000145       END-EVALUATE                                                       
000146       PERFORM IMS-GET-KNDA                                               
000147     END-PERFORM                                                          
000148                                                                          
000149     PERFORM Z-FINIT                                                      
000150                                                                          
000151     MOVE ZERO TO RETURN-CODE                                             
000152     GOBACK                                                               
000153     .                                                                    
000154     EJECT                                                                
000155 A-INIT SECTION.                                                          
000156                                                                          
000157     OPEN OUTPUT W02007                                                   
000158                 W02013                                                   
000159                                                                          
000160     ACCEPT DAGENS-DATUM  FROM DATE                                       
000161     COMPUTE W-1AAR-INNAN = DAGENS-DATUM - 10000                          
000162     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
000163     MOVE SPACE TO LDC-AREA                                               
000164     MOVE ZERO TO LDC-IDDISTR-1                                           
000165                  LDC-IDDISTR-2                                           
000166                  LDC-IDKUNDNR-1                                          
000167                  LDC-IDKUNDNR-2                                          
000168                  LDC-KOD                                                 
000169     .                                                                    
000170     EJECT                                                                
000171 B-FLYTTA-WDB201-INFO SECTION.                                            
000172                                                                          
000173     MOVE GMT-IDDISTR       TO UT-IDDISTR                                 
000174     MOVE GMT-IDKUNDNR      TO UT-IDKUNDNR                                
000175     MOVE GMT-IDDC-BULK(1)  TO UT-IDDC-BULK                               
000176     MOVE GMT-IDDC-DAY (1)  TO UT-IDDC-DAY                                
000177     MOVE GMT-IDDC-VOR (1)  TO UT-IDDC-VOR                                
000178     .                                                                    
000179     EJECT                                                                
000180 C-SKAPA-SKRIV-LDC-FIL SECTION.                                           
000181                                                                          
000182     IF GMT-IDDISTR = 778                                                 
000183        MOVE GMT-IDDC-BULK(1) TO WS-IDDC                                  
000184        IF LDC-SE                                                         
000185           MOVE GMT-IDDISTR  TO LDC-IDDISTR-1                             
000186                                LDC-IDDISTR-2                             
000187           MOVE GMT-IDKUNDNR TO LDC-IDKUNDNR-1                            
000188                                LDC-IDKUNDNR-2                            
000189           MOVE GMT-IDDC-BULK(1) TO WS-IDDC                               
000190           IF LDC-SE-1A                                                   
000191             MOVE 100 TO LDC-KOD                                          
000192           ELSE                                                           
000193            IF LDC-SE-1B                                                  
000194              MOVE 101 TO LDC-KOD                                         
000195            ELSE                                                          
000196              IF LDC-SE-1C                                                
000197                MOVE 102 TO LDC-KOD                                       
000198              ELSE                                                        
000199                IF LDC-SE-1D                                              
000200                  MOVE 103 TO LDC-KOD                                     
000201                ELSE                                                      
000202                  IF LDC-SE-1E                                            
000203                    MOVE 104 TO LDC-KOD                                   
000204                  END-IF                                                  
000205                END-IF                                                    
000206              END-IF                                                      
000207            END-IF                                                        
000208           END-IF                                                         
000209           MOVE GMT-IDDC-DAY(1) TO WS-IDDC                                
000210           IF CDC-SE                                                      
000211              MOVE 'J' TO LDC-SPECKUND                                    
000212              PERFORM S12-SKRIV-W02013                                    
000213           ELSE                                                           
000214              MOVE 'N' TO LDC-SPECKUND                                    
000215              PERFORM S12-SKRIV-W02013                                    
000216           END-IF                                                         
000217        END-IF                                                            
000218     END-IF                                                               
000219     IF GMT-IDDISTR = 878                                                 
000220****** DISTRIKT NORGE                                                     
000221        MOVE GMT-IDDC-DAY(1) TO WS-IDDC                                   
000222        IF LDC-NO                                                         
000223           MOVE GMT-IDDISTR  TO LDC-IDDISTR-1                             
000224                                LDC-IDDISTR-2                             
000225           MOVE GMT-IDKUNDNR TO LDC-IDKUNDNR-1                            
000226                                LDC-IDKUNDNR-2                            
000227           MOVE 'N'          TO LDC-SPECKUND                              
000228           IF LDC-NO-3J                                                   
000229              MOVE 312 TO LDC-KOD                                         
000230              PERFORM S12-SKRIV-W02013                                    
000231           END-IF                                                         
000232        END-IF                                                            
000233     END-IF                                                               
000234     IF GMT-IDDISTR = 1378                                                
000235        MOVE GMT-IDDC-DAY(1) TO WS-IDDC                                   
000236        IF LDC-GB OR LDC-GB-3A                                            
000237           MOVE GMT-IDDISTR  TO LDC-IDDISTR-1                             
000238                                LDC-IDDISTR-2                             
000239           MOVE GMT-IDKUNDNR TO LDC-IDKUNDNR-1                            
000240                                LDC-IDKUNDNR-2                            
000241           MOVE 'N'          TO LDC-SPECKUND                              
000250           IF LDC-GB-2C                                                   
000251            MOVE 202 TO LDC-KOD                                           
000252            PERFORM S12-SKRIV-W02013                                      
000253           ELSE                                                           
000258             IF LDC-GB-3A                                                 
000259              MOVE 300 TO LDC-KOD                                         
000260              PERFORM S12-SKRIV-W02013                                    
000261             ELSE                                                         
000262              IF LDC-GB-3B                                                
000263               MOVE 301 TO LDC-KOD                                        
000264               PERFORM S12-SKRIV-W02013                                   
000265              ELSE                                                        
000278                IF LDC-GB-2H                                              
000279                 MOVE 207 TO LDC-KOD                                      
000280                 PERFORM S12-SKRIV-W02013                                 
000281                END-IF                                                    
000285              END-IF                                                      
000286             END-IF                                                       
000288           END-IF                                                         
000291        END-IF                                                            
000292     END-IF                                                               
000293     IF GMT-IDDISTR = 2278                                                
000294***    DISTRIKT TYSKLAND                                                  
000295        MOVE GMT-IDDC-DAY(1) TO WS-IDDC                                   
000296        IF LDC-DE                                                         
000297           MOVE GMT-IDDISTR  TO LDC-IDDISTR-1                             
000298                                LDC-IDDISTR-2                             
000299           MOVE GMT-IDKUNDNR TO LDC-IDKUNDNR-1                            
000300                                LDC-IDKUNDNR-2                            
000301           MOVE 'N'          TO LDC-SPECKUND                              
000302           IF LDC-DE-2I                                                   
000303            MOVE 208 TO LDC-KOD                                           
000304            PERFORM S12-SKRIV-W02013                                      
000305           ELSE                                                           
000306            IF LDC-DE-3C                                                  
000307             MOVE 302 TO LDC-KOD                                          
000308             PERFORM S12-SKRIV-W02013                                     
000309            ELSE                                                          
000310             IF LDC-DE-3E                                                 
000311               MOVE 304 TO LDC-KOD                                        
000312               PERFORM S12-SKRIV-W02013                                   
000313             ELSE                                                         
000314              IF LDC-DE-2J                                                
000315                MOVE 209 TO LDC-KOD                                       
000316                PERFORM S12-SKRIV-W02013                                  
000317              ELSE                                                        
000322                IF LDC-DE-2L                                              
000323                 MOVE 306 TO LDC-KOD                                      
000324                 PERFORM S12-SKRIV-W02013                                 
000325                ELSE                                                      
000326                 IF LDC-DE-3G                                             
000327                  MOVE 307 TO LDC-KOD                                     
000328                  PERFORM S12-SKRIV-W02013                                
000329                 ELSE                                                     
000330                  IF LDC-DE-3K                                            
000331                   MOVE 313 TO LDC-KOD                                    
000332                   PERFORM S12-SKRIV-W02013                               
000333                  ELSE                                                    
000334                   IF LDC-DE-3M                                           
000335                    MOVE 315 TO LDC-KOD                                   
000336                    PERFORM S12-SKRIV-W02013                              
000337                   ELSE                                                   
000338                    IF LDC-DE-3T                                          
000339                     MOVE 321 TO LDC-KOD                                  
000340                     PERFORM S12-SKRIV-W02013                             
000341                    END-IF                                                
000342                   END-IF                                                 
000343                  END-IF                                                  
000344                 END-IF                                                   
000345                END-IF                                                    
000347              END-IF                                                      
000348             END-IF                                                       
000349           END-IF                                                         
000350         END-IF                                                           
000351       END-IF                                                             
000352     END-IF                                                               
000353     IF GMT-IDDISTR = 1822                                                
000354****** DISTRIKT ITALIEN                                                   
000355        MOVE GMT-IDDC-DAY(1) TO WS-IDDC                                   
000356        IF LDC-IT                                                         
000357           MOVE GMT-IDDISTR  TO LDC-IDDISTR-1                             
000358                                LDC-IDDISTR-2                             
000359           MOVE GMT-IDKUNDNR TO LDC-IDKUNDNR-1                            
000360                                LDC-IDKUNDNR-2                            
000361           MOVE 'N'          TO LDC-SPECKUND                              
000362           IF LDC-IT-3D                                                   
000363              MOVE 303 TO LDC-KOD                                         
000364              PERFORM S12-SKRIV-W02013                                    
000365           END-IF                                                         
000366           IF LDC-IT-3F                                                   
000367              MOVE 305 TO LDC-KOD                                         
000368              PERFORM S12-SKRIV-W02013                                    
000369           END-IF                                                         
000370        END-IF                                                            
000371     END-IF                                                               
000372     IF GMT-IDDISTR = 2078                                                
000373****** DISTRIKT SCHWEIZ                                                   
000374        MOVE GMT-IDDC-DAY(1) TO WS-IDDC                                   
000375        IF LDC-CH                                                         
000376           MOVE GMT-IDDISTR  TO LDC-IDDISTR-1                             
000377                                LDC-IDDISTR-2                             
000378           MOVE GMT-IDKUNDNR TO LDC-IDKUNDNR-1                            
000379                                LDC-IDKUNDNR-2                            
000380           MOVE 'N'          TO LDC-SPECKUND                              
000381           IF LDC-CH-3H                                                   
000382              MOVE 308 TO LDC-KOD                                         
000383              PERFORM S12-SKRIV-W02013                                    
000384           END-IF                                                         
000385        END-IF                                                            
000386     END-IF                                                               
000387     IF GMT-IDDISTR = 1258                                                
000388****** DISTRIKT BELGIEN                                                   
000389        MOVE GMT-IDDC-DAY(1) TO WS-IDDC                                   
000390        IF LDC-BE                                                         
000391           MOVE GMT-IDDISTR  TO LDC-IDDISTR-1                             
000392                                LDC-IDDISTR-2                             
000393           MOVE GMT-IDKUNDNR TO LDC-IDKUNDNR-1                            
000394                                LDC-IDKUNDNR-2                            
000395           MOVE 'N'          TO LDC-SPECKUND                              
000396           IF LDC-BE-3L                                                   
000397              MOVE 314 TO LDC-KOD                                         
000398              PERFORM S12-SKRIV-W02013                                    
000399           END-IF                                                         
000400        END-IF                                                            
000401     END-IF                                                               
000402     IF GMT-IDDISTR = 1678                                                
000403****** DISTRIKT HOLLAND                                                   
000404        MOVE GMT-IDDC-DAY(1) TO WS-IDDC                                   
000405        IF LDC-NL                                                         
000406           MOVE GMT-IDDISTR  TO LDC-IDDISTR-1                             
000407                                LDC-IDDISTR-2                             
000408           MOVE GMT-IDKUNDNR TO LDC-IDKUNDNR-1                            
000409                                LDC-IDKUNDNR-2                            
000410           MOVE 'N'          TO LDC-SPECKUND                              
000415           IF LDC-NL-2M                                                   
000416              MOVE 310 TO LDC-KOD                                         
000417              PERFORM S12-SKRIV-W02013                                    
000418           END-IF                                                         
000419           IF LDC-NL-3N                                                   
000420              MOVE 316 TO LDC-KOD                                         
000421              PERFORM S12-SKRIV-W02013                                    
000422           END-IF                                                         
000423           IF LDC-NL-3R                                                   
000424              MOVE 319 TO LDC-KOD                                         
000425              PERFORM S12-SKRIV-W02013                                    
000426           END-IF                                                         
000427        END-IF                                                            
000428     END-IF                                                               
000429     IF GMT-IDDISTR = 1090                                                
000430****** DISTRIKT FINLAND                                                   
000431        MOVE GMT-IDDC-DAY(1) TO WS-IDDC                                   
000432        IF LDC-FI                                                         
000433           MOVE GMT-IDDISTR  TO LDC-IDDISTR-1                             
000434                                LDC-IDDISTR-2                             
000435           MOVE GMT-IDKUNDNR TO LDC-IDKUNDNR-1                            
000436                                LDC-IDKUNDNR-2                            
000437           MOVE 'N'          TO LDC-SPECKUND                              
000438           IF LDC-FI-3O                                                   
000439              MOVE 317 TO LDC-KOD                                         
000440              PERFORM S12-SKRIV-W02013                                    
000441           END-IF                                                         
000442        END-IF                                                            
000443     END-IF                                                               
000444     IF GMT-IDDISTR = 1478                                                
000445****** DISTRIKT FRANKRIKE                                                 
000446        MOVE GMT-IDDC-DAY(1) TO WS-IDDC                                   
000447        IF LDC-FR                                                         
000448           MOVE GMT-IDDISTR  TO LDC-IDDISTR-1                             
000449                                LDC-IDDISTR-2                             
000450           MOVE GMT-IDKUNDNR TO LDC-IDKUNDNR-1                            
000451                                LDC-IDKUNDNR-2                            
000452           MOVE 'N'          TO LDC-SPECKUND                              
000453           IF LDC-FR-3P                                                   
000454              MOVE 318 TO LDC-KOD                                         
000455              PERFORM S12-SKRIV-W02013                                    
000456           END-IF                                                         
000457        END-IF                                                            
000458     END-IF                                                               
000459     IF GMT-IDDISTR = 2878                                                
000460****** DISTRIKT POLEN                                                     
000461        MOVE GMT-IDDC-DAY(1) TO WS-IDDC                                   
000462        IF LDC-PL                                                         
000463           MOVE GMT-IDDISTR  TO LDC-IDDISTR-1                             
000464                                LDC-IDDISTR-2                             
000465           MOVE GMT-IDKUNDNR TO LDC-IDKUNDNR-1                            
000466                                LDC-IDKUNDNR-2                            
000467           MOVE 'N'          TO LDC-SPECKUND                              
000468           IF LDC-PL-3S                                                   
000469              MOVE 320 TO LDC-KOD                                         
000470              PERFORM S12-SKRIV-W02013                                    
000471           END-IF                                                         
000472        END-IF                                                            
000473     END-IF                                                               
000474     .                                                                    
000475     EJECT                                                                
000476 Z-FINIT SECTION.                                                         
000477                                                                          
000478     MOVE ZERO TO LDC-IDDISTR-1                                           
000479                  LDC-IDKUNDNR-1                                          
000480     MOVE 9999 TO LDC-IDDISTR-2                                           
000481     MOVE 9999999 TO LDC-IDKUNDNR-2                                       
000482     MOVE 311 TO LDC-KOD                                                  
000483     PERFORM S12-SKRIV-W02013                                             
000484                                                                          
000485     CLOSE W02007                                                         
000486           W02013                                                         
000487                                                                          
000488     MOVE 'S' TO POSTSUM-OPKOD                                            
000489     CALL POSTSUM USING POSTSUM-PARM                                      
000490     .                                                                    
000491     EJECT                                                                
000492 S11-SKRIV-W02007 SECTION.                                                
000493                                                                          
000494     WRITE UT-POST   FROM  UT-AREA                                        
000495                                                                          
000496     MOVE 'WDB2'     TO    POSTSUM-TRANSTYP                               
000497     MOVE 'W02007'   TO    POSTSUM-FDNAMN                                 
000498     MOVE 'W02004D1' TO    POSTSUM-DDNAMN2                                
000499     CALL  POSTSUM   USING POSTSUM-PARM                                   
000500     .                                                                    
000501     EJECT                                                                
000502 S12-SKRIV-W02013 SECTION.                                                
000503                                                                          
000504     WRITE LDC-POST  FROM  LDC-AREA                                       
000505                                                                          
000506     MOVE 'LDC'      TO    POSTSUM-TRANSTYP                               
000507     MOVE 'W02013'   TO    POSTSUM-FDNAMN                                 
000508     MOVE 'W02004D2' TO    POSTSUM-DDNAMN2                                
000509     CALL  POSTSUM   USING POSTSUM-PARM                                   
000510     .                                                                    
000511     EJECT                                                                
000512*                                                                         
000513*                 IIIIIIIIIII MMMMMMMMMMM SSSSSSSSSSS                     
000514*                 III     III MM MMMMM MM SSSS   SSSS                     
000515*                 IIIII IIIII MM  MMM  MM SSS SSS SSS                     
000516*                 IIIII IIIII MM M M M MM SSS  SSSSSS                     
000517*                 IIIII IIIII MM MM MM MM SSSSSS  SSS                     
000518*                 IIIII IIIII MM MMMMM MM SSS SSS SSS                     
000519*                 III     III MM MMMMM MM SSSS   SSSS                     
000520*                 IIIIIIIIIII MMMMMMMMMMM SSSSSSSSSSS                     
000521*                                                                         
000522*                                                                         
000523     EJECT                                                                
000524 IMS-GET-KNDA   SECTION.                                                  
000525                                                                          
000526     CALL    CBLTDLI          USING GN                                    
000527                                    KNDA-PCB                              
000528                                    DLI-IO-AREA                           
000529     MOVE    KNDA-STATUS-CODE TO    STATUS-WS                             
000530     MOVE    '  GAGKGB'       TO    GODK-STATUSKODER                      
000531     PERFORM IMS-STATUSKONTROLL                                           
000532     .                                                                    
000533     EJECT                                                                
000534 IMS-STATUSKONTROLL SECTION.                                              
000535                                                                          
000536     SET STATUS-IX TO 1                                                   
000537     SEARCH GODK-STATUS                                                   
000538       AT END                                                             
000539         CALL FELLOG                                                      
000540       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
000541         CONTINUE                                                         
000542     END-SEARCH                                                           
000543     .                                                                    
