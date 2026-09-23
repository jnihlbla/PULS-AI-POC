000001 ID DIVISION.                                                             
000002 PROGRAM-ID.     W6132200.                                                
000003 AUTHOR.         UMESH JAIN.                                              
000004 DATE-WRITTEN.   08/11/04.                                                
000005 DATE-COMPILED.                                                           
000006                                                                          
000007*    FUNCTION:                                                            
000008*        READ WDT101 AND CREATE BELOW THREE FILES                         
000009*        1. W613.W613V3.W61322(+1) - COMPLETE SEGMENTS WDT101             
000010*        2. W613.W613V3.W61323(+1) - W6132301                             
000011*        3. W613.W613V3.W61324(+1) - W6132401                             
000012*                                                                         
000013*        THE PROGRAM READS     WDT1                                       
000014*                                                                         
000015*                                                                         
000016                                                                          
000017     SKIP3                                                                
000018 ENVIRONMENT DIVISION.                                                    
000019     SKIP2                                                                
000020 INPUT-OUTPUT SECTION.                                                    
000021                                                                          
000022 FILE-CONTROL.                                                            
000023     SKIP2                                                                
000024*          --- ALL SEGMENTS WITH STATUS 'A'                               
000025     SELECT W61322                     ASSIGN TO W61322D1.                
000026     SKIP2                                                                
000027*          --- FILE WITH ELEMENTS IN W6132301                             
000028     SELECT W61323                     ASSIGN TO W61322D2.                
000029     SKIP2                                                                
000030*          --- FILE WITH ELEMENTS IN W6132401                             
000031     SELECT W61324                     ASSIGN TO W61322D3.                
000032     EJECT                                                                
000033 DATA DIVISION.                                                           
000034     SKIP2                                                                
000035 FILE SECTION.                                                            
000036     SKIP3                                                                
000037 FD  W61322                                                               
000038     RECORDING       F                                                    
000039     BLOCK CONTAINS  0.                                                   
000040                                                                          
000041*01  POST -COPY WDT101 -PRE  W61322-  -L.                                 
000042     SKIP3                                                                
000043 FD  W61323                                                               
000044     RECORDING       F                                                    
000045     BLOCK CONTAINS  0.                                                   
000046                                                                          
000047*01  POST -COPY W6132301 -PRE  W61323-  -L.                               
000048     SKIP3                                                                
000049 FD  W61324                                                               
000050     RECORDING       F                                                    
000051     BLOCK CONTAINS  0.                                                   
000052                                                                          
000053*01  POST -COPY W6132401 -PRE  W61324-  -L.                               
000054     EJECT                                                                
000055 WORKING-STORAGE SECTION.                                                 
000056                                                                          
000057 77  IDPGM                       PIC X(8)    VALUE 'W6132200'.            
000058 77  YES                         PIC X       VALUE 'J'.                   
000059 77  NOO                         PIC X       VALUE 'N'.                   
000060     EJECT                                                                
000061 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
000062 01  FILLER REDEFINES TODAYS-DATE.                                        
000063     03  TODAYS-DATE-YEAR        PIC 9(2).                                
000064     03  TODAYS-DATE-MONTH       PIC 9(2).                                
000065     03  TODAYS-DATE-DAY         PIC 9(2).                                
000066     EJECT                                                                
000067 01  GENERAL-SUBPROGRAMS.                                                 
000068*                                                                         
000069     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
000070     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000071     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000072     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
000073     SKIP2                                                                
000074*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
000075                                                                          
000076 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
000077 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
000078 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
000079     SKIP2                                                                
000080 01  ERRTEXT.                                                             
000081     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
000082     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
000083     EJECT                                                                
000084*    --- PARAMETRAR TILL POSTSUM                                          
000085*                                                                         
000086*01  -COPY W0005   -PRE  POSTSUM-                                         
000087     EJECT                                                                
000088 01  W61322-AREA-START           PIC X(24)   VALUE                        
000089                                 'W61322-AREA-START  '.                   
000090     SKIP2                                                                
000091                                                                          
000092*01  AREA -COPY WDT101     -PRE W61322-                                   
000093     EJECT                                                                
000094 01  W61323-AREA-START           PIC X(24)   VALUE                        
000095                                 'W61323-AREA-START  '.                   
000096     SKIP2                                                                
000097                                                                          
000098*01  AREA -COPY W6132301     -PRE W61323-                                 
000099     EJECT                                                                
000100 01  W61324-AREA-START           PIC X(24)   VALUE                        
000101                                 'W61324-AREA-START  '.                   
000102     SKIP2                                                                
000103                                                                          
000104*01  AREA -COPY W6132401     -PRE W61324-                                 
000105     EJECT                                                                
000106*    --- AREAS FOR IMS-SECTIONS                                           
000107*                                                                         
000108     EJECT                                                                
000109 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000110     SKIP3                                                                
000111 01  KEYS-FOR-DLI.                                                        
000112     03  W-WDT101KY-X.                                                    
000113         05  W-WDT101KY          PIC X(26)    VALUE SPACE.                
000114     SKIP2                                                                
000115*    --- STATUS CODE FROM IMS                                             
000116 01  STATUS-WS                   PIC XX.                                  
000117     88  SEGMENT-FOUND                       VALUE '  '.                  
000118     88  SEGMENT-MISSING                     VALUE 'GB'.                  
000119     SKIP2                                                                
000120 01  GOOD-STATUSCODES.                                                    
000121     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000122     SKIP3                                                                
000123 01  SSA1                        PIC X(64).                               
000124 01  SSA2                        PIC X(64).                               
000125     EJECT                                                                
000126*    --- IMS FUNCTION CODES                                               
000127*01  -COPY W0003                                                          
000128     EJECT                                                                
000129*    ---  DLI INPUT-OUTPUT AREA                                           
000130 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT1'.                        
000131 01  DLI-IO-WDT1.                                                         
000132*    03  -COPY WDT101                                                     
000133     EJECT                                                                
000134 LINKAGE SECTION.                                                         
000135                                                                          
000136                                                                          
000137*01  -COPY W0008  -PRE WDT1-                                              
000138     05  FILLER                  PIC X.                                   
000139     EJECT                                                                
000140 PROCEDURE DIVISION  USING WDT1-PCB.                                      
000141 MAIN SECTION.                                                            
000142     ENTRY 'DLITCBL' USING WDT1-PCB.                                      
000143                                                                          
000144     PERFORM A-INIT                                                       
000145                                                                          
000146     PERFORM IMS-GN-WDT1                                                  
000147     PERFORM UNTIL SEGMENT-MISSING                                        
000148       IF PF-KDSTAPF = 'A'                                                
000149          MOVE PF-WDT101          TO W61322-AREA                          
000150          PERFORM S11-WRITE-W61322                                        
000151          MOVE PF-IDARTNR         TO W61323-2301-IDARTNR                  
000152          MOVE PF-KVBEST          TO W61323-2301-KVBEST                   
000153          MOVE PF-TIORDTIME(1:10) TO W61323-2301-TIORDTIME                
000154          MOVE PF-TIHOTIME(1:10)  TO W61323-2301-TIHOTIME                 
000155          MOVE PF-KVBEST-ANDR     TO W61323-2301-KVBEST-ANDR              
000156          MOVE PF-TIAVSL(1:10)    TO W61323-2301-TIAVSL                   
000157          MOVE PF-ADART-FOM       TO W61323-2301-ADART-FOM                
000158          MOVE PF-ADART-TOM       TO W61323-2301-ADART-TOM                
000159          PERFORM S12-WRITE-W61323                                        
000160       ELSE                                                               
000161          MOVE PF-IDARTNR         TO W61324-2401-IDARTNR                  
000162          MOVE PF-KVBEST          TO W61324-2401-KVBEST                   
000163          MOVE PF-TIORDTIME(1:10) TO W61324-2401-TIORDTIME                
000164          MOVE PF-KDSTAPF         TO W61324-2401-KDSTAPF                  
000165          MOVE PF-ADART-FOM       TO W61324-2401-ADART-FOM                
000166          MOVE PF-ADART-TOM       TO W61324-2401-ADART-TOM                
000167          PERFORM S13-WRITE-W61324                                        
000168       END-IF                                                             
000169       PERFORM IMS-GN-WDT1                                                
000170     END-PERFORM                                                          
000171     PERFORM Z-FINIT                                                      
000172                                                                          
000173     MOVE ZERO TO RETURN-CODE                                             
000174     GOBACK                                                               
000175     .                                                                    
000176     EJECT                                                                
000177 A-INIT SECTION.                                                          
000178                                                                          
000179     OPEN OUTPUT W61322                                                   
000180                 W61323                                                   
000181                 W61324                                                   
000182                                                                          
000183     ACCEPT TODAYS-DATE  FROM DATE                                        
000184     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
000185     .                                                                    
000186     EJECT                                                                
000187 Z-FINIT SECTION.                                                         
000188     CLOSE W61322                                                         
000189           W61323                                                         
000190           W61324                                                         
000191     SKIP2                                                                
000192     MOVE 'S' TO POSTSUM-OPKOD                                            
000193     CALL POSTSUM USING POSTSUM-PARM                                      
000194     .                                                                    
000195     EJECT                                                                
000196 S11-WRITE-W61322 SECTION.                                                
000197                                                                          
000198     WRITE W61322-POST FROM W61322-AREA                                   
000199                                                                          
000200     MOVE SPACE         TO POSTSUM-TRANSTYP                               
000201     MOVE 'W61322' TO POSTSUM-FDNAMN                                      
000202     MOVE 'W61322D1' TO POSTSUM-DDNAMN2                                   
000203     CALL POSTSUM USING POSTSUM-PARM                                      
000204     .                                                                    
000205     EJECT                                                                
000206 S12-WRITE-W61323 SECTION.                                                
000207                                                                          
000208     WRITE W61323-POST FROM W61323-AREA                                   
000209                                                                          
000210     MOVE SPACE         TO POSTSUM-TRANSTYP                               
000211     MOVE 'W61323' TO POSTSUM-FDNAMN                                      
000212     MOVE 'W61322D2' TO POSTSUM-DDNAMN2                                   
000213     CALL POSTSUM USING POSTSUM-PARM                                      
000214     .                                                                    
000215     EJECT                                                                
000216 S13-WRITE-W61324 SECTION.                                                
000217                                                                          
000218     WRITE W61324-POST FROM W61324-AREA                                   
000219                                                                          
000220     MOVE SPACE         TO POSTSUM-TRANSTYP                               
000221     MOVE 'W61324' TO POSTSUM-FDNAMN                                      
000222     MOVE 'W61322D3' TO POSTSUM-DDNAMN2                                   
000223     CALL POSTSUM USING POSTSUM-PARM                                      
000224     .                                                                    
000225     EJECT                                                                
000226 S99-ABEND SECTION.                                                       
000227                                                                          
000228     SKIP2                                                                
000229     MOVE 'S' TO POSTSUM-OPKOD                                            
000230     CALL POSTSUM USING POSTSUM-PARM                                      
000231     CALL ABEND USING RKOD-ABEND                                          
000232     .                                                                    
000233     EJECT                                                                
000234* --- IMS SECTIONS  ---                                                   
000235                                                                          
000236 IMS-GN-WDT1   SECTION.                                                   
000237                                                                          
000238     CALL CBLTDLI USING GN WDT1-PCB DLI-IO-WDT1                           
000239     MOVE WDT1-STATUS-CODE TO STATUS-WS                                   
000240     MOVE '  GAGKGB' TO GOOD-STATUSCODES                                  
000241     PERFORM IMS-STATUSCHECK                                              
000242     .                                                                    
000243     EJECT                                                                
000244 IMS-STATUSCHECK SECTION.                                                 
000245                                                                          
000246     SET STATUS-IX TO 1                                                   
000247     SEARCH GOOD-STATUS                                                   
000248       AT END                                                             
000249         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
000250           DELIMITED BY SIZE INTO ERRTEXT                                 
000251         DISPLAY ERRTEXT                                                  
000252         CALL FELLOG                                                      
000253       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
000254         CONTINUE                                                         
000255     END-SEARCH                                                           
000256     .                                                                    
