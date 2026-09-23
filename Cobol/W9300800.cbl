000001 ID DIVISION.                                                             
000002 PROGRAM-ID.     W9300800.                                                
000003 AUTHOR.         REDDY RAHUL.                                             
000004 DATE-WRITTEN.   14/02/11.                                                
000005 DATE-COMPILED.                                                           
000006                                                                          
000007                                                                          
000008*    FUNCTION:                                                            
000009*        DELETE CURRENCY DATA OLDER THAN 3 YEARS FROM WDG2(9305)          
000010*                                                                         
000011*        THE PROGRAM UPDATES   WDG2 (9305/9308)                           
000012*                                                                         
000013*    CHANGE LOG:                                                          
000014*      YY/MM/DD                                                           
000015*      14/02/11 - REDDY RAHUL     - INITIAL VERSION                       
000016*                                   SCR 10222254                          
000017*                                                                         
000018     SKIP3                                                                
000019 ENVIRONMENT DIVISION.                                                    
000020     SKIP2                                                                
000021 INPUT-OUTPUT SECTION.                                                    
000022                                                                          
000023 FILE-CONTROL.                                                            
000024*          --- THREE YEARS OLD CURRENCY DATA                              
000025     SELECT W93010                     ASSIGN TO W93008D1.                
000026     EJECT                                                                
000027     EJECT                                                                
000028 DATA DIVISION.                                                           
000029     SKIP3                                                                
000030 FILE SECTION.                                                            
000031 FD  W93010                                                               
000032      RECORDING       F                                                   
000033      BLOCK CONTAINS  0.                                                  
000034                                                                          
000035*01  RECORD -COPY W93010 -PRE  IN-  -L.                                   
000036      EJECT                                                               
000037 WORKING-STORAGE SECTION.                                                 
000038                                                                          
000039 77  IDPGM                       PIC X(8)    VALUE 'W9300800'.            
000040 01  CHKP-VAR.                                                            
000041     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
000042     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
000043     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
000044     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
000045     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
000046     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
000047 77  YES                         PIC X       VALUE 'J'.                   
000048 77  NOO                         PIC X       VALUE 'N'.                   
000049     SKIP2                                                                
000050 01  ERROR-TEXT.                                                          
000051     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
000052     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
000053     EJECT                                                                
000054  77  W93010-EOF-SW               PIC X       VALUE 'N'.                  
000055      88  END-OF-W93010                       VALUE 'J'.                  
000056      EJECT                                                               
000057*    -COPY WY2000W1                                                       
000058     SKIP3                                                                
000059     EJECT                                                                
000060 01  GENERAL-SUBPROGRAMS.                                                 
000061*                                                                         
000062     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000063     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000064     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
000065*                                                                         
000066*    --- PARAMETRAR TILL POSTSUM                                          
000067*01  -COPY W0005   -PRE  POSTSUM-                                         
000068     EJECT                                                                
000069 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000070     SKIP3                                                                
000071  01  IN-AREA-START               PIC X(24)   VALUE                       
000072                                              'IN-AREA-START'.            
000073      SKIP2                                                               
000074                                                                          
000075*01  AREA -COPY W93010   -PRE IN-                                         
000076 01  KEYS-TILL-DLI.                                                       
000077     03  W-WDGXKEY-X.                                                     
000078         05  W-IDHTYP            PIC X(4)    VALUE '9305'.                
000079         05  W-KDVALISO-HUV      PIC X(3)    VALUE LOW-VALUE.             
000080         05  W-KDVALTYP          PIC X(1)    VALUE 'M'.                   
000081         05  FILLER              PIC X(22)   VALUE LOW-VALUE.             
000082     03  W-KDVALISO-X.                                                    
000083         05  W-KDVALISO          PIC X(3)    VALUE SPACE.                 
000084     03  W-TISTADA9-X.                                                    
000085         05  W-TISTADAT-9KOMPL   PIC S9(7)   VALUE ZERO COMP-3.           
000086     SKIP2                                                                
000087*    --- STATUS-KOD FRÅN IMS                                              
000088 01  STATUS-WS                   PIC XX.                                  
000089     88  SEGMENT-FOUND                       VALUE '  '.                  
000090     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
000091     88  SEGMENT-MISSING                     VALUE 'GE'.                  
000092     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
000093     88  IMS-NOT-OK                          VALUE 'XD'.                  
000094     SKIP2                                                                
000095 01  GOOD-STATUSCODES.                                                    
000096     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000097     SKIP3                                                                
000098 01  SSA1                        PIC X(64).                               
000099 01  SSA2                        PIC X(64).                               
000100 01  SSA3                        PIC X(64).                               
000101     EJECT                                                                
000102*    --- IMS FUNCTION CODES                                               
000103*01  -COPY W0003                                                          
000104     EJECT                                                                
000105*    ---  DLI INPUT-OUTPUT AREA                                           
000106                                                                          
000107                                                                          
000108 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX9308'.                    
000109 01  DLI-IO-WDGX9308.                                                     
000110*    03  -COPY WDGX9308                                                   
000111                                                                          
000112     EJECT                                                                
000113 LINKAGE SECTION.                                                         
000114                                                                          
000115*01  -COPY W0009   -PRE MSG-                                              
000116                                                                          
000117*01  -COPY W0008  -PRE 9305-                                              
000118     05  FILLER                  PIC X.                                   
000119     EJECT                                                                
000120 PROCEDURE DIVISION  USING MSG-PCB 9305-PCB.                              
000121 MAIN SECTION.                                                            
000122     ENTRY 'DLITCBL' USING MSG-PCB 9305-PCB.                              
000123                                                                          
000124     SKIP2                                                                
000125     PERFORM A-INIT                                                       
000126     PERFORM S07-READ-W93010                                              
000127     PERFORM UNTIL END-OF-W93010                                          
000128        IF CHKP-ANT > CHKP-MAX                                            
000129          PERFORM X-TAKE-CHECKPOINT                                       
000130        END-IF                                                            
000131        MOVE IN-KDVALISO-HUV     TO W-KDVALISO-HUV                        
000133        MOVE IN-KDVALISO         TO W-KDVALISO                            
000134        MOVE IN-TISTADAT-9KOMPL  TO W-TISTADAT-9KOMPL                     
000135                                                                          
000136                                                                          
000137        PERFORM IMS-GHU-WDGX9308                                          
000138        IF SEGMENT-FOUND                                                  
000139           PERFORM IMS-DLET-WDGX9308                                      
000140           ADD +1                TO CHKP-ANT                              
000141        END-IF                                                            
000142        PERFORM S07-READ-W93010                                           
000143     END-PERFORM                                                          
000144                                                                          
000145     MOVE ZERO                   TO RETURN-CODE                           
000146     GOBACK                                                               
000147     .                                                                    
000148     EJECT                                                                
000149 A-INIT SECTION.                                                          
000150     SKIP2                                                                
000151     PERFORM IMS-RESTART                                                  
000152     OPEN INPUT W93010                                                    
000153                                                                          
000154     .                                                                    
000155     EJECT                                                                
000156 X-TAKE-CHECKPOINT   SECTION.                                             
000157                                                                          
000158     PERFORM IMS-CHECKPOINT                                               
000159     MOVE ZERO                   TO CHKP-ANT                              
000160     .                                                                    
000161     EJECT                                                                
000162* --- IMS SECTIONS  ---                                                   
000163                                                                          
000164     PERFORM IMS-STATUSCHECK                                              
000165     .                                                                    
000166     EJECT                                                                
000167 Z-FINIT SECTION.                                                         
000168                                                                          
000169     CLOSE W93010                                                         
000170                                                                          
000171                                                                          
000172                                                                          
000173     MOVE 'S'                    TO POSTSUM-OPKOD                         
000174     CALL POSTSUM             USING POSTSUM-PARM                          
000175     .                                                                    
000176     EJECT                                                                
000177 S07-READ-W93010  SECTION.                                                
000178     SKIP2                                                                
000179     READ W93010 INTO IN-AREA                                             
000180     AT END                                                               
000181        SET END-OF-W93010 TO TRUE                                         
000182     NOT AT END                                                           
000183        MOVE 'W93010'  TO POSTSUM-FDNAMN                                  
000184        MOVE 'W93008D1'TO POSTSUM-DDNAMN2                                 
000185        CALL POSTSUM USING POSTSUM-PARM                                   
000186     END-READ                                                             
000187     .                                                                    
000188     EJECT                                                                
000189 IMS-GHU-WDGX9308 SECTION.                                                
000190                                                                          
000191     STRING 'WDG201  (WDGXKEY  =' W-WDGXKEY-X ')'                         
000192             DELIMITED BY SIZE INTO SSA1                                  
000193     STRING 'WDGX9306(KDVALISO =' W-KDVALISO-X ')'                        
000194             DELIMITED BY SIZE INTO SSA2                                  
000195     STRING 'WDGX9308(TISTADA9 =' W-TISTADA9-X ')'                        
000196             DELIMITED BY SIZE INTO SSA3                                  
000197     MOVE '  GEGB'               TO GOOD-STATUSCODES                      
000198     CALL CBLTDLI             USING GHU                                   
000199                                    9305-PCB                              
000200                                    DLI-IO-WDGX9308                       
000201                                    SSA1 SSA2 SSA3                        
000202     MOVE 9305-STATUS-CODE       TO STATUS-WS                             
000203     PERFORM IMS-STATUSCHECK                                              
000204     .                                                                    
000205     SKIP3                                                                
000206 IMS-DLET-WDGX9308 SECTION.                                               
000207                                                                          
000208     MOVE '  '                   TO GOOD-STATUSCODES                      
000209     CALL CBLTDLI             USING DLET                                  
000210                                    9305-PCB                              
000211                                    DLI-IO-WDGX9308                       
000212     MOVE 9305-STATUS-CODE       TO STATUS-WS                             
000213     PERFORM IMS-STATUSCHECK                                              
000214     .                                                                    
000215     EJECT                                                                
000216 IMS-RESTART SECTION.                                                     
000217     SKIP2                                                                
000218     MOVE SPACE                  TO CHKP-MSG-IO-AREA                      
000219     MOVE '  '                   TO GOOD-STATUSCODES                      
000220     CALL CBLTDLI             USING XRST                                  
000221                                    MSG-PCB                               
000222                                    CHKP-MSG-IO-AREA-LENGTH               
000223                                    CHKP-MSG-IO-AREA                      
000224                                    CHKP-AREA-LENGTH                      
000225                                    CHKP-AREA                             
000226     MOVE MSG-STATUS-CODE        TO STATUS-WS                             
000227     PERFORM IMS-STATUSCHECK                                              
000228     .                                                                    
000229     SKIP3                                                                
000230 IMS-CHECKPOINT SECTION.                                                  
000231     SKIP2                                                                
000232     MOVE SPACE                  TO CHKP-MSG-IO-AREA                      
000233     MOVE '  XD'                 TO GOOD-STATUSCODES                      
000234     CALL CBLTDLI             USING CHKP                                  
000235                                    MSG-PCB                               
000236                                    CHKP-MSG-IO-AREA-LENGTH               
000237                                    CHKP-MSG-IO-AREA                      
000238                                    CHKP-AREA-LENGTH                      
000239                                    CHKP-AREA                             
000240     MOVE MSG-STATUS-CODE        TO STATUS-WS                             
000241     PERFORM IMS-STATUSCHECK                                              
000242                                                                          
000243     IF IMS-NOT-OK                                                        
000244       MOVE 'IMS CONTROL REGION IS NOT ACCESSIBLE'                        
000245                                 TO ERROR-TEXT-STR                        
000246       DISPLAY ERROR-TEXT                                                 
000247       CALL FELLOG                                                        
000248     END-IF                                                               
000249     .                                                                    
000250     EJECT                                                                
000251 IMS-STATUSCHECK SECTION.                                                 
000252     SKIP2                                                                
000253     SET STATUS-IX               TO 1                                     
000254     SEARCH GOOD-STATUS                                                   
000255       AT END                                                             
000256         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
000257             DELIMITED BY SIZE INTO ERROR-TEXT                            
000258         DISPLAY ERROR-TEXT                                               
000259         CALL FELLOG                                                      
000260       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
000261         CONTINUE                                                         
000262     END-SEARCH                                                           
000263     .                                                                    
